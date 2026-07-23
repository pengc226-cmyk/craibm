// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <map>
#include <vector>
#include <string>
#include <cmath>
#include <random>
#include <thread>     
#include <atomic> 
#include <cstdint>
#include <utility>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;


struct SimulationRNG {
  std::mt19937 gen;
  std::uniform_real_distribution<> dis_uniform;
  std::exponential_distribution<> dis_exp;
  
  SimulationRNG(unsigned int seed)
    : gen(seed), dis_uniform(0.0, 1.0), dis_exp(1.0) {}
  
  SimulationRNG(const SimulationRNG& other)
    : gen(other.gen), dis_uniform(0.0, 1.0), dis_exp(1.0) {}
  
  std::mt19937 get_state() const { return gen; }
  void set_state(const std::mt19937& state) { gen = state; }
  
  double uniform() { return dis_uniform(gen); }
  
  double exponential(double rate) {
    if (rate <= 0) return 1e9;
    std::exponential_distribution<> dis(rate);
    return dis(gen);
  }
  
  double normal(double mean, double sd) {
    std::normal_distribution<> dis(mean, sd);
    return dis(gen);
  }
  
  int poisson(double lambda) {
    if (lambda <= 0) return 0;
    if (lambda > 1e6) lambda = 1e6;
    std::poisson_distribution<> dis(lambda);
    return dis(gen);
  }
};


// (： Compliance)

inline int legal_mask(double len, double min_len, double max_len) {
  if (min_len <= max_len) {
    return (len >= min_len && len <= max_len) ? 1 : 0;
  } else {
    return (len < max_len || len > min_len) ? 1 : 0;
  }
}

inline double willing_prob(double len, double p_max, double L50, double slope) {
  double kp = p_max / (1.0 + std::exp(-slope * (len - L50)));
  return std::max(0.0, std::min(1.0, kp));
}



inline double compliance_prob(double len, double min_len, double max_len,
                              int comp_mode,
                              const std::vector<double>& thresholds,
                              const std::vector<double>& probs) {
  
  if (legal_mask(len, min_len, max_len) == 1) return 0.0;
  
  
  if (comp_mode == 0) return 1.0;
  
  // 3. (comp_mode != 0, )
  
  if (thresholds.empty()) return 1.0;
  
  double p = 1.0;
  // thresholds (0, 254, ...)
  // <= len
  for (size_t i = 0; i < thresholds.size(); ++i) {
    if (len >= thresholds[i]) {
      if (i < probs.size()) {
        p = probs[i];
      }
    } else {
      // , len,,
      break;
    }
  }
  return p;
}

// [] vectors
inline double keep_prob_with_compliance(double len, double min_len, double max_len,
                                        double p_max, double L50, double slope,
                                        int comp_mode,
                                        const std::vector<double>& thresholds,
                                        const std::vector<double>& probs) {
  const double w = willing_prob(len, p_max, L50, slope);
  const int g = legal_mask(len, min_len, max_len);
  const double c = compliance_prob(len, min_len, max_len, comp_mode, thresholds, probs);
  double K = w * (g + (1.0 - c) * (1.0 - g));
  return std::max(0.0, std::min(1.0, K));
}


struct MonthlyStats {
  int year, month, month_of_year;
  std::string phase;
  double Sden, Rden;
  int AdultN, AgeFRN, Yield_n, N_pop;
  double PSD_Q, PSD_P, PSD_M, PSD_T;
  double Enc_Q, Enc_P, Enc_M, Enc_T;
  bool trophy_seen;
  double maxage;
  double prop_annual_encounters, min_len_mm, max_len_mm;
  int comp_mode;
  double release_mortality;
  int policy_combo_id;
};

struct SurvivalResults {
  arma::mat survivors;
  int yield_n, enc_n_total, enc_n_Q, enc_n_P, enc_n_M, enc_n_T;
};


inline arma::vec csample_num_cpp(const arma::vec& x, int size, SimulationRNG& rng) {
  arma::vec result(size);
  for (int i = 0; i < size; ++i) {
    arma::uword idx = static_cast<arma::uword>(rng.uniform() * x.n_elem);
    if (idx >= x.n_elem) idx = x.n_elem - 1;
    result(i) = x(idx);
  }
  return result;
}


inline arma::mat init_population_cpp_arma(
    const arma::mat& W1_alk, const arma::mat& Theta_clean,
    const arma::vec& zr_w_dist, const int initial_pop_size,
    const double F_over_Z_ratio, SimulationRNG& rng) {
  if (initial_pop_size <= 0) return arma::mat(0, 6);
  
  const int n_age_rows = W1_alk.n_rows;
  std::vector<int> ages(n_age_rows);
  arma::vec counts(n_age_rows);
  
  for (int r = 0; r < n_age_rows; ++r) {
    ages[r] = static_cast<int>(W1_alk(r, 0));
    counts[r] = W1_alk(r, 1);
  }
  
  double sum_n = arma::accu(counts);
  arma::vec prob = counts / sum_n;
  
  arma::ivec fish_ages(initial_pop_size);
  for (int i = 0; i < initial_pop_size; ++i) {
    double u = rng.uniform();
    double csum = 0.0;
    bool assigned = false;
    for (int j = 0; j < n_age_rows; ++j) {
      csum += prob[j];
      if (u <= csum) { fish_ages[i] = ages[j]; assigned = true; break; }
    }
    if (!assigned) fish_ages[i] = ages[n_age_rows - 1];
  }
  
  std::map<int, std::pair<double,double>> age2msd;
  for (int r = 0; r < n_age_rows; ++r) {
    int a = ages[r];
    double mn = W1_alk(r, 2);
    double sd = std::max(W1_alk(r, 3), 1e-6);
    age2msd[a] = std::make_pair(mn, sd);
  }
  
  arma::vec fish_lengths(initial_pop_size);
  for (int i = 0; i < initial_pop_size; ++i) {
    int a = fish_ages[i];
    if (age2msd.find(a) != age2msd.end()) {
      auto msd = age2msd[a];
      double len = rng.normal(msd.first, msd.second);
      fish_lengths[i] = (len > 0.0) ? len : 1.0;
    } else {
      fish_lengths[i] = 50.0;
    }
  }
  
  const arma::uword nTheta = Theta_clean.n_rows;
  arma::uvec idx(initial_pop_size);
  for (int i = 0; i < initial_pop_size; ++i) {
    double r_val = std::floor(rng.uniform() * nTheta);
    if (r_val >= nTheta) r_val = nTheta - 1;
    idx[i] = static_cast<arma::uword>(r_val);
  }
  
  arma::vec Linf(initial_pop_size), K(initial_pop_size), t0(initial_pop_size);
  for (int i = 0; i < initial_pop_size; ++i) {
    arma::uword r = idx[i];
    Linf[i] = Theta_clean(r, 0);
    K[i] = Theta_clean(r, 1);
    t0[i] = Theta_clean(r, 2);
  }
  
  arma::vec z0i = csample_num_cpp(zr_w_dist, initial_pop_size, rng);
  arma::vec mi(initial_pop_size);
  for (int i = 0; i < initial_pop_size; ++i) mi[i] = z0i[i] * (1.0 - F_over_Z_ratio);
  
  arma::mat out(initial_pop_size, 6);
  out.col(0) = fish_lengths;
  out.col(1) = arma::conv_to<arma::vec>::from(fish_ages);
  out.col(2) = K; out.col(3) = Linf; out.col(4) = t0; out.col(5) = mi;
  
  return out;
}


inline arma::mat recruits_init_cpp_arma(
    const arma::mat& Theta_clean, const arma::vec& zr_w_dist,
    const int Rall_real, const double F_over_Z_ratio, SimulationRNG& rng) {
  if (Rall_real == 0) return arma::mat(0, 6);
  
  const arma::uword nTheta = Theta_clean.n_rows;
  arma::uvec idx(Rall_real);
  for (int i = 0; i < Rall_real; ++i) {
    double r_val = std::floor(rng.uniform() * nTheta);
    if (r_val >= nTheta) r_val = nTheta - 1;
    idx[i] = static_cast<arma::uword>(r_val);
  }
  
  arma::vec Linf_r(Rall_real), K_r(Rall_real), t0_r(Rall_real);
  for (int i = 0; i < Rall_real; ++i) {
    arma::uword r = idx[i];
    Linf_r[i] = Theta_clean(r, 0);
    K_r[i] = Theta_clean(r, 1);
    t0_r[i] = Theta_clean(r, 2);
  }
  
  arma::vec z0i_r = csample_num_cpp(zr_w_dist, Rall_real, rng);
  arma::vec mi_r(Rall_real);
  for (int i = 0; i < Rall_real; ++i) mi_r[i] = z0i_r[i] * (1.0 - F_over_Z_ratio);
  
  arma::vec RL(Rall_real, arma::fill::zeros);
  arma::vec Ra(Rall_real, arma::fill::zeros);
  
  arma::mat out(Rall_real, 6);
  out.col(0) = RL; out.col(1) = Ra;
  out.col(2) = K_r; out.col(3) = Linf_r; out.col(4) = t0_r; out.col(5) = mi_r;
  
  return out;
}


SurvivalResults survival_ibm_competing_risks(
    const arma::mat& y1, double prop_enc_month,
    double min_len, double max_len,
    double p_max, double L50, double slope,
    int comp_mode,
    const std::vector<double>& comp_thresholds,
    const std::vector<double>& comp_probs,
    double PV_adult, double PV_juv, double vmonthly_avg,
    double env_mult_juv, double env_mult_adlt,
    double juv_onlyM_len, double release_mortality,
    double max_encounters_per_month,
    double psd_quality, double psd_preferred, double psd_memorable, double psd_trophy,
    double min_adult_age,
    bool Fagemode, double age_recruit,
    SimulationRNG& rng) {
  
  const int n = y1.n_rows;
  const int p = y1.n_cols;
  
  SurvivalResults results;
  results.yield_n = 0; results.enc_n_total = 0;
  results.enc_n_Q = 0; results.enc_n_P = 0; results.enc_n_M = 0; results.enc_n_T = 0;
  
  if (n == 0) { results.survivors = arma::mat(0, p); return results; }
  
  auto pos = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
  
  const double p_enc = std::max(0.0, std::min(1.0, prop_enc_month));
  const double lambdaE = (p_enc >= 1.0) ? 1e6 : (p_enc <= 0.0 ? 0.0 : -std::log(1.0 - p_enc));
  const double env_juv = pos(env_mult_juv, 1e-9);
  const double env_adlt = pos(env_mult_adlt, 1e-9);
  const double PVJ = pos(PV_juv, 1e-9);
  const double PVA = pos(PV_adult, 1e-9);
  const double month_duration = 1.0;
  
  std::vector<int> idx_surv;
  idx_surv.reserve(n);
  
  for (int i = 0; i < n; ++i) {
    const double age_end = y1(i, 1);
    const double age0 = age_end - 1.0 / 12.0;
    const bool isAdult = (age0 >= min_adult_age);
    const double L0i = y1(i, 0);
    
    double M_monthly;
    if (isAdult) {
      double annual_mortality_coef = pos(y1(i, 5));
      double annual_survival = std::exp(-annual_mortality_coef);
      double monthly_survival = std::pow(annual_survival, 1.0 / 12.0);
      double M0_ad = -std::log(monthly_survival);
      M_monthly = pos(M0_ad * PVA * env_adlt);
    } else {
      M_monthly = pos(vmonthly_avg * PVJ * env_juv);
    }
    
    const double tN = rng.exponential(M_monthly);
    
    // Fagemode: use age_recruit threshold; otherwise use juv_onlyM_len
    bool juv_skip_enc;
    if (Fagemode) {
      juv_skip_enc = (age_end < age_recruit);
    } else {
      juv_skip_enc = (!isAdult && L0i < juv_onlyM_len);
    }
    
    if (lambdaE <= 1e-9 || juv_skip_enc) {
      if (tN >= month_duration) idx_surv.push_back(i);
      continue;
    }
    
    double t_curr = 0.0;
    bool survived = true;
    int enc_count = 0;
    
    while (t_curr < month_duration && enc_count < max_encounters_per_month) {
      double gap = rng.exponential(lambdaE);
      double t_next_enc = t_curr + gap;
      
      if (tN < t_next_enc && tN < month_duration) { survived = false; break; }
      if (t_next_enc >= month_duration) break;
      
      enc_count++;
      results.enc_n_total++;
      
      if (L0i >= psd_quality) {
        if (L0i < psd_preferred) results.enc_n_Q++;
        else if (L0i < psd_memorable) results.enc_n_P++;
        else if (L0i < psd_trophy) results.enc_n_M++;
        else results.enc_n_T++;
      }
      
      // [Use Vector Logic]
      double kp = keep_prob_with_compliance(L0i, min_len, max_len, p_max, L50, slope,
                                            comp_mode, comp_thresholds, comp_probs);
      
      // Fagemode: use age_recruit threshold; otherwise use juv_onlyM_len
      if (Fagemode) {
        if (age_end < age_recruit) kp = 0.0;
      } else {
        if (L0i < juv_onlyM_len) kp = 0.0;
      }
      
      if (rng.uniform() < kp) { results.yield_n++; survived = false; break; }
      if (release_mortality > 0.0 && rng.uniform() < release_mortality) { survived = false; break; }
      
      t_curr = t_next_enc;
    }
    
    if (survived && tN >= month_duration) idx_surv.push_back(i);
  }
  
  const size_t n_surv = idx_surv.size();
  results.survivors.set_size(n_surv, p);
  for (size_t r = 0; r < n_surv; ++r) results.survivors.row(r) = y1.row(idx_surv[r]);
  
  return results;
}


inline arma::mat growthf_arma(const arma::mat& y1,
                              double g1_d_avg, double g1_a, double g1_b, double g1_c,
                              double g2_d_avg, double g2_a, double g2_b, double g2_c,
                              double lake_area_ha,double min_adult_age,
                              double extra_juvN = 0.0) {
  int n_fish = y1.n_rows;
  if (n_fish == 0) return y1;
  
  arma::mat y2 = y1;
  const double dt = 1.0 / 12.0;
  
  arma::vec age = y2.col(1);
  arma::uvec idx_juv = arma::find(age < min_adult_age);
  arma::uvec idx_ad  = arma::find(age >= min_adult_age);
  
  double n_juv = static_cast<double>(idx_juv.n_elem) + std::max(0.0, extra_juvN);
  double n_ad  = static_cast<double>(idx_ad.n_elem);
  
  double dens_juv = (lake_area_ha > 1e-6) ? (n_juv / lake_area_ha) : 0.0;
  double dens_ad  = (lake_area_ha > 1e-6) ? (n_ad  / lake_area_ha) : 0.0;
  
  double PD_juv = (g1_d_avg > 1e-6) ? (dens_juv / g1_d_avg) : 0.0;
  double PG_juv = g1_a + g1_b * std::exp(-g1_c * PD_juv);
  double PD_ad  = (g2_d_avg > 1e-6) ? (dens_ad  / g2_d_avg) : 0.0;
  double PG_ad  = g2_a + g2_b * std::exp(-g2_c * PD_ad);
  
  arma::vec k = y2.col(2), Linf = y2.col(3), t0 = y2.col(4);
  arma::vec base = Linf % arma::exp(-k % (age - t0));
  arma::vec inc_factor = 1.0 - arma::exp(-k * dt);
  
  arma::vec PG_vec(n_fish);
  PG_vec.elem(idx_juv).fill(PG_juv);
  PG_vec.elem(idx_ad).fill(PG_ad);
  
  y2.col(0) += PG_vec % base % inc_factor;
  y2.col(1) += dt;
  
  return y2;
}

inline arma::mat growthf0_arma(const arma::mat& y1) {
  int n = y1.n_rows;
  if (n == 0) return y1;
  
  arma::mat y2 = y1;
  arma::vec ki = y2.col(2), Lii = y2.col(3), t0i = y2.col(4);
  arma::vec L0 = Lii % (1.0 - arma::exp(-ki % (0.0 - t0i)));
  
  for (int i = 0; i < n; ++i) {
    if (!R_finite(L0[i]) || L0[i] < 0.0) L0[i] = 0.0;
  }
  y2.col(0) = L0;
  return y2;
}


// =============================================================================
// Juvenile Fast-Forward System
//
// New recruits enter a lightweight "batch" instead of the main population.
// During T_safe months, each batch only tracks expected surviving count and
// the monthly density-dependent growth factor (pg_history). No individuals
// are stored. When t_elapsed reaches T_safe, the batch "graduates":
// round(N_surviving) individuals are generated, and each fish's length is
// reconstructed by replaying pg_history with its own K/Linf/t0.
//
// Statistically exact (law of large numbers) because juvenile mortality is
// identical for all fish in a given month:
//   M_monthly = vmonthly_avg * PVJ * env_juv  (no per-fish term)
//
// Recruits enter at age 0 (hardcoded, matching recruits_init_cpp_arma which
// sets Ra=0).
// =============================================================================

struct FastForwardBatch {
  int    t_elapsed;                // months already fast-forwarded
  double N_surviving;              // expected surviving count (fractional)
  std::vector<double> pg_history;  // per-month juvenile growth factor PG_juv
  
  FastForwardBatch(double N0)
    : t_elapsed(0), N_surviving(N0) {
    pg_history.reserve(24);
  }
};

struct FastForwardManager {
  std::vector<FastForwardBatch> batches;
  int T_safe;
  
  FastForwardManager(int t_safe) : T_safe(t_safe) {}
  
  bool enabled() const { return T_safe > 0; }
  
  // Total juveniles currently in fast-forward (contributes to density)
  double total_juveniles() const {
    double sum = 0.0;
    for (const auto& b : batches) sum += b.N_surviving;
    return sum;
  }
  
  // Biological age represented by an active batch. The recruit-entry month
  // has survival but no growth, so age equals the number of replayed growth
  // steps rather than t_elapsed.
  static double batch_age_years(const FastForwardBatch& batch) {
    return static_cast<double>(batch.pg_history.size()) / 12.0;
  }
  
  double total_below_age(double upper_age) const {
    double sum = 0.0;
    for (const auto& batch : batches) {
      if (batch_age_years(batch) < upper_age) sum += batch.N_surviving;
    }
    return sum;
  }
  
  double total_in_age_range(double lower_age, double upper_age) const {
    double sum = 0.0;
    for (const auto& batch : batches) {
      const double age = batch_age_years(batch);
      if (age >= lower_age && age < upper_age) sum += batch.N_surviving;
    }
    return sum;
  }
  
  double max_batch_age() const {
    double out = 0.0;
    for (const auto& batch : batches) {
      out = std::max(out, batch_age_years(batch));
    }
    return out;
  }
  
  // Add a new recruit batch (called at recruit_entry_month)
  void add_batch(double N0) {
    if (N0 > 0.0) batches.emplace_back(N0);
  }
  
  // Graduate a batch: generate n_final individuals, reconstruct length by
  // replaying pg_history with each fish's own K/Linf/t0. Recruits start at age 0.
  arma::mat graduate_batch(
      const FastForwardBatch& batch, int n_final,
      const arma::mat& Theta_clean, const arma::vec& zr_w_dist,
      double F_over_Z_ratio, SimulationRNG& rng) {
    
    // recruits_init returns [length=0, age=0, K, Linf, t0, mi]
    arma::mat fish = recruits_init_cpp_arma(Theta_clean, zr_w_dist, n_final,
                                            F_over_Z_ratio, rng);
    const double dt = 1.0 / 12.0;
    const int T = static_cast<int>(batch.pg_history.size());
    
    for (arma::uword i = 0; i < fish.n_rows; ++i) {
      double K    = fish(i, 2);
      double Linf = fish(i, 3);
      double t0   = fish(i, 4);
      
      // Initial length (growthf0: age=0 VBGF length)
      double L = Linf * (1.0 - std::exp(-K * (0.0 - t0)));
      if (!R_finite(L) || L < 0.0) L = 0.0;
      
      double age = 0.0;  // recruits start at age 0
      for (int t = 0; t < T; ++t) {
        double base = Linf * std::exp(-K * (age - t0));
        double inc  = 1.0 - std::exp(-K * dt);
        L += batch.pg_history[t] * base * inc;
        age += dt;
      }
      fish(i, 0) = L;              // reconstructed length
      fish(i, 1) = age;            // age after replaying growth (entry at age 0, no growth on entry month)
    }
    return fish;
  }
  
  // Monthly update: apply expected survival + record growth factor to all
  // batches, advance elapsed time, graduate finished batches.
  // M_monthly and PG_juv are computed by the CALLER using TOTAL juvenile density.
  // Returns graduated recruits ready to join the main population (empty if none).
  arma::mat update_and_graduate(
      double M_monthly, double PG_juv,
      const arma::mat& Theta_clean, const arma::vec& zr_w_dist,
      double F_over_Z_ratio, SimulationRNG& rng) {
    
    double survival_rate = std::exp(-M_monthly);
    if (!R_finite(survival_rate) || survival_rate < 0.0) survival_rate = 0.0;
    if (survival_rate > 1.0) survival_rate = 1.0;
    
    std::vector<arma::mat> graduated_mats;
    
    for (auto it = batches.begin(); it != batches.end(); ) {
      // Entry month has survival but no growth.
      // In the original IBM, new recruits enter after growthf_arma().
      if (it->t_elapsed > 0) {
        it->pg_history.push_back(PG_juv);
      }
      it->N_surviving *= survival_rate;
      it->t_elapsed++;
      
      if (it->t_elapsed >= T_safe) {
        int n_final = static_cast<int>(std::floor(it->N_surviving + 0.5));
        if (n_final > 0) {
          graduated_mats.push_back(
            graduate_batch(*it, n_final, Theta_clean, zr_w_dist, F_over_Z_ratio, rng));
        }
        it = batches.erase(it);
      } else {
        ++it;
      }
    }
    
    if (graduated_mats.empty()) return arma::mat(0, 6);
    arma::mat combined = graduated_mats[0];
    for (size_t i = 1; i < graduated_mats.size(); ++i)
      combined = arma::join_vert(combined, graduated_mats[i]);
    return combined;
  }
};


inline void validate_fastforward_age_window(int T_safe,
                                            double min_adult_age,
                                            double age_spawn,
                                            bool Fagemode,
                                            double age_recruit) {
  if (T_safe < 0) Rcpp::stop("T_safe must be >= 0.");
  
  // Hidden batches must remain juvenile and pre-spawning. In age-based
  // vulnerability mode they must also remain below fishery recruit age.
  double age_bound = std::min(min_adult_age, age_spawn);
  if (Fagemode) age_bound = std::min(age_bound, age_recruit);
  if (!R_finite(age_bound) || age_bound < 0.0) {
    Rcpp::stop("Fast-forward age bounds must be finite and non-negative.");
  }
  
  const int max_safe_months = static_cast<int>(std::ceil(age_bound * 12.0));
  if (T_safe > max_safe_months) {
    Rcpp::stop("T_safe exceeds the juvenile, maturity, or vulnerability safe window.");
  }
}


// ====== DataFrame ======
DataFrame monthly_stats_to_dataframe(const std::vector<MonthlyStats>& monthly_data) {
  int n = monthly_data.size();
  IntegerVector years(n), months(n), months_of_year(n);
  CharacterVector phases(n);
  NumericVector Sdens(n), Rdens(n);
  IntegerVector AdultNs(n), AgeFRNs(n), Yield_ns(n), N_pops(n);
  NumericVector PSD_Qs(n), PSD_Ps(n), PSD_Ms(n), PSD_Ts(n);
  NumericVector Enc_Qs(n), Enc_Ps(n), Enc_Ms(n), Enc_Ts(n);
  LogicalVector trophy_seens(n);
  NumericVector maxages(n), prop_encounters(n), min_lens(n), max_lens(n);
  IntegerVector comp_modes(n), policy_combo_ids(n);
  NumericVector release_morts(n);
  
  for (int i = 0; i < n; ++i) {
    years[i] = monthly_data[i].year;
    months[i] = monthly_data[i].month;
    months_of_year[i] = monthly_data[i].month_of_year;
    phases[i] = monthly_data[i].phase;
    Sdens[i] = monthly_data[i].Sden;
    Rdens[i] = monthly_data[i].Rden;
    AdultNs[i] = monthly_data[i].AdultN;
    AgeFRNs[i] = monthly_data[i].AgeFRN;
    Yield_ns[i] = monthly_data[i].Yield_n;
    N_pops[i] = monthly_data[i].N_pop;
    PSD_Qs[i] = monthly_data[i].PSD_Q;
    PSD_Ps[i] = monthly_data[i].PSD_P;
    PSD_Ms[i] = monthly_data[i].PSD_M;
    PSD_Ts[i] = monthly_data[i].PSD_T;
    Enc_Qs[i] = monthly_data[i].Enc_Q;
    Enc_Ps[i] = monthly_data[i].Enc_P;
    Enc_Ms[i] = monthly_data[i].Enc_M;
    Enc_Ts[i] = monthly_data[i].Enc_T;
    trophy_seens[i] = monthly_data[i].trophy_seen;
    maxages[i] = monthly_data[i].maxage;
    prop_encounters[i] = monthly_data[i].prop_annual_encounters;
    min_lens[i] = monthly_data[i].min_len_mm;
    max_lens[i] = monthly_data[i].max_len_mm;
    comp_modes[i] = monthly_data[i].comp_mode;
    release_morts[i] = monthly_data[i].release_mortality;
    policy_combo_ids[i] = monthly_data[i].policy_combo_id;
  }
  
  Rcpp::List out;
  out["year"] = years;
  out["month"] = months;
  out["month_of_year"] = months_of_year;
  out["phase"] = phases;
  out["Sden"] = Sdens;
  out["Rden"] = Rdens;
  out["AdultN"] = AdultNs;
  out["AgeFRN"] = AgeFRNs;
  out["Yield_n"] = Yield_ns;
  out["N_pop"] = N_pops;
  out["PSD_Q"] = PSD_Qs;
  out["PSD_P"] = PSD_Ps;
  out["PSD_M"] = PSD_Ms;
  out["PSD_T"] = PSD_Ts;
  out["Enc_Q"] = Enc_Qs;
  out["Enc_P"] = Enc_Ps;
  out["Enc_M"] = Enc_Ms;
  out["Enc_T"] = Enc_Ts;
  out["trophy_seen"] = trophy_seens;
  out["maxage"] = maxages;
  out["prop_annual_encounters"] = prop_encounters;
  out["min_len_mm"] = min_lens;
  out["max_len_mm"] = max_lens;
  out["comp_mode"] = comp_modes;
  out["release_mortality"] = release_morts;
  out["policy_combo_id"] = policy_combo_ids;
  
  // make it a data.frame with correct rownames
  out.attr("class") = "data.frame";
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
  
  return Rcpp::DataFrame(out);
}

// ====== [] Policy ======
std::vector<MonthlyStats> run_policy_phase(
    arma::mat dd1_start, arma::mat pending_Rd1_start,
    double Rden_year_start, int Rall_year_start,
    SimulationRNG rng_copy, double env_mult_juv_start, double env_mult_adlt_start,
    const arma::mat& Theta_clean, const arma::vec& zr_w_dist_arma,
    const arma::vec& month_weights_arma, const arma::vec& prop_enc_month_vec,
    double h_p_max, double h_L50, double h_slope,
    double g1_d_avg, double g1_a, double g1_b, double g1_c,
    double g2_d_avg, double g2_a, double g2_b, double g2_c,
    double s_a, double s_b, double s_c, double s_d1, double s_d2,
    double vmonthly_avg, double lake_area_ha,
    double rec_a, double rec_b, double F_over_Z_ratio,
    double juv_onlyM_len,
    const std::vector<double>& comp_thresholds,
    const std::vector<double>& comp_probs,
    double psd_stock, double psd_quality, double psd_preferred, double psd_memorable, double psd_trophy,
    double min_adult_age,
    double age_recruit,
    double age_spawn,
    bool Fagemode,
    int spawn_month, int recruit_entry_month, double ESD,
    int policy_years, int before_policy_years,
    double prop_annual_encounters_val,
    double min_len_mm_val, double max_len_mm_val,
    int comp_mode_val, double release_mortality_val, int policy_combo_id,
    bool use_ricker,
    const FastForwardManager& ff_mgr_start,
    int pending_ff_recruits_start) {
  
  std::vector<MonthlyStats> monthly_data;
  monthly_data.reserve(policy_years * 12);
  
  arma::mat dd1 = dd1_start;
  arma::mat pending_Rd1 = pending_Rd1_start;
  double Rden_year = Rden_year_start;
  int Rall_year = Rall_year_start;
  double env_mult_juv = env_mult_juv_start;
  double env_mult_adlt = env_mult_adlt_start;
  
  // Each policy combo gets an independent copy of the before-policy cohort state.
  FastForwardManager ff_mgr = ff_mgr_start;
  int pending_ff_recruits = pending_ff_recruits_start;
  
  int start_month = before_policy_years * 12 + 1;
  int end_month = (before_policy_years + policy_years) * 12;
  
  for (int m = start_month; m <= end_month; ++m) {
    int year = (m - 1) / 12 + 1;
    int month_of_year = (m - 1) % 12 + 1;
    
    if (month_of_year == 1) {
      const double lower_bound = 1.0 - 1.645 * ESD;
      const double upper_bound = 1.0 + 1.645 * ESD;
      double envy_raw = rng_copy.normal(1.0, ESD);
      double envy = std::max(lower_bound, std::min(envy_raw, upper_bound));
      env_mult_juv = envy;
      env_mult_adlt = envy;
    }
    
    // Density used for this month's growth is evaluated BEFORE the new
    // recruit-entry batch is added (entry recruits don't grow this month).
    double ff_juv_before_growth = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
    
    arma::mat growth_results = growthf_arma(dd1, g1_d_avg, g1_a, g1_b, g1_c,
                                            g2_d_avg, g2_a, g2_b, g2_c, lake_area_ha, 
                                            min_adult_age, ff_juv_before_growth);
    
    // Batch growth factor PG must use the SAME pre-entry density as main-pop growth
    double juvN_growth = arma::accu(dd1.col(1) < min_adult_age) + ff_juv_before_growth;
    double juv_dens_growth = (lake_area_ha > 1e-6) ? (juvN_growth / lake_area_ha) : 0.0;
    double PD_juv_growth = (g1_d_avg > 1e-6) ? (juv_dens_growth / g1_d_avg) : 0.0;
    double PG_juv_growth = g1_a + g1_b * std::exp(-g1_c * PD_juv_growth);
    
    if (month_of_year == spawn_month) {
      double Spawners = arma::accu(growth_results.col(1) >= age_spawn);
      double Sden_spawn = Spawners / lake_area_ha;
      // Stock-recruitment: Ricker or Beverton-Holt
      if (use_ricker) {
        Rden_year = rec_a * Sden_spawn * std::exp(-rec_b * Sden_spawn);
      } else {
        Rden_year = rec_a * Sden_spawn / (1.0 + rec_b * Sden_spawn);
      }
      if (Rden_year < 0.0 || !R_finite(Rden_year)) Rden_year = 0.0;
      Rall_year = static_cast<int>(std::floor(Rden_year * lake_area_ha));
      
      if (Rall_year > 0) {
        if (ff_mgr.enabled()) {
          // Store only the expected recruit count until entry month (no huge matrix)
          pending_ff_recruits = Rall_year;
          pending_Rd1.set_size(0, 6);
        } else {
          pending_Rd1 = recruits_init_cpp_arma(Theta_clean, zr_w_dist_arma, Rall_year, F_over_Z_ratio, rng_copy);
          pending_ff_recruits = 0;
        }
      } else {
        pending_Rd1.set_size(0, 6);
        pending_ff_recruits = 0;
      }
    }
    
    arma::mat dd1_all = growth_results;
    if (month_of_year == recruit_entry_month) {
      if (ff_mgr.enabled() && pending_ff_recruits > 0) {
        ff_mgr.add_batch(static_cast<double>(pending_ff_recruits));
        pending_ff_recruits = 0;
      } else if (pending_Rd1.n_rows > 0) {
        arma::mat Rd1real = growthf0_arma(pending_Rd1);
        dd1_all = arma::join_vert(growth_results, Rd1real);
        pending_Rd1.set_size(0, 6);
      }
    }
    
    double juvN_main = arma::accu(dd1_all.col(1) < min_adult_age);
    double adN       = arma::accu(dd1_all.col(1) >= min_adult_age);
    double juvN_ff   = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
    double juvN      = juvN_main + juvN_ff;
    double juv_dens = (lake_area_ha > 1e-6) ? (juvN / lake_area_ha) : 0.0;
    double ad_dens  = (lake_area_ha > 1e-6) ? (adN  / lake_area_ha) : 0.0;
    double PV_juv   = s_a + s_b * (1.0 - std::exp(-s_c * (juv_dens / s_d1)));
    double PV_adult = s_a + s_b * (1.0 - std::exp(-s_c * (ad_dens  / s_d2)));
    
    // ===== Update fast-forward batches; capture graduates for post-survival merge =====
    arma::mat ff_graduates(0, 6);
    if (ff_mgr.enabled() && !ff_mgr.batches.empty()) {
      auto pos_ff = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
      double env_juv_ff = pos_ff(env_mult_juv, 1e-9);
      double PVJ_ff = pos_ff(PV_juv, 1e-9);
      double M_juv_monthly = pos_ff(vmonthly_avg * PVJ_ff * env_juv_ff);
      // PG uses pre-entry growth density (consistent with main-pop growth above)
      ff_graduates = ff_mgr.update_and_graduate(
        M_juv_monthly, PG_juv_growth, Theta_clean, zr_w_dist_arma, F_over_Z_ratio, rng_copy);
    }
    
    double prop_enc_month = prop_enc_month_vec(month_of_year - 1);
    
    // [Pass Vectors]
    SurvivalResults fishing_results = survival_ibm_competing_risks(
      dd1_all, prop_enc_month, min_len_mm_val, max_len_mm_val,
      h_p_max, h_L50, h_slope, comp_mode_val,
      comp_thresholds, comp_probs,
      PV_adult, PV_juv, vmonthly_avg, env_mult_juv, env_mult_adlt,
      juv_onlyM_len, release_mortality_val, 50,
      psd_quality, psd_preferred, psd_memorable, psd_trophy,
      min_adult_age,
      Fagemode, age_recruit,
      rng_copy);
    
    double prop_Q = 0.0, prop_P = 0.0, prop_M = 0.0, prop_T = 0.0;
    bool any_trophy = false;
    if (fishing_results.enc_n_total > 0) {
      prop_Q = 100.0 * fishing_results.enc_n_Q / fishing_results.enc_n_total;
      prop_P = 100.0 * fishing_results.enc_n_P / fishing_results.enc_n_total;
      prop_M = 100.0 * fishing_results.enc_n_M / fishing_results.enc_n_total;
      prop_T = 100.0 * fishing_results.enc_n_T / fishing_results.enc_n_total;
      any_trophy = (fishing_results.enc_n_T > 0);
    }
    
    dd1 = fishing_results.survivors;
    
    // Merge graduated fast-forward fish AFTER survival (they already completed
    // T_safe months of expected juvenile survival during fast-forward).
    if (ff_graduates.n_rows > 0) {
      dd1 = arma::join_vert(dd1, ff_graduates);
    }
    
    MonthlyStats stats;
    stats.year = year; stats.month = m; stats.month_of_year = month_of_year;
    stats.phase = "policy";
    
    double AdultN_curr = arma::accu(dd1.col(1) >= min_adult_age);
    double AgeFRN = arma::accu((dd1.col(1) >= age_recruit) && (dd1.col(1) < (1+age_recruit)))
      + ff_mgr.total_in_age_range(age_recruit, 1.0 + age_recruit);
    stats.AgeFRN = static_cast<int>(std::llround(AgeFRN));
    
    double Spawners_curr = arma::accu(dd1.col(1) >= age_spawn);
    stats.Sden = (lake_area_ha > 1e-6) ? (Spawners_curr / lake_area_ha) : 0.0;
    double Larve_curr = arma::accu(dd1.col(1) < 1.0) + ff_mgr.total_below_age(1.0);
    stats.Rden = (lake_area_ha > 1e-6) ? (Larve_curr / lake_area_ha) : 0.0;
    stats.AdultN = static_cast<int>(AdultN_curr);
    stats.Yield_n = fishing_results.yield_n;
    stats.N_pop = static_cast<int>(dd1.n_rows) + static_cast<int>(std::llround(ff_mgr.total_juveniles()));
    
    arma::vec lengths = dd1.col(0);
    int n_stock = arma::accu(lengths >= psd_stock);
    stats.PSD_Q = (n_stock > 0) ? 100.0 * arma::accu(lengths >= psd_quality) / n_stock : 0.0;
    stats.PSD_P = (n_stock > 0) ? 100.0 * arma::accu(lengths >= psd_preferred) / n_stock : 0.0;
    stats.PSD_M = (n_stock > 0) ? 100.0 * arma::accu(lengths >= psd_memorable) / n_stock : 0.0;
    stats.PSD_T = (n_stock > 0) ? 100.0 * arma::accu(lengths >= psd_trophy) / n_stock : 0.0;
    
    stats.Enc_Q = prop_Q; stats.Enc_P = prop_P; stats.Enc_M = prop_M; stats.Enc_T = prop_T;
    stats.trophy_seen = any_trophy;
    stats.maxage = std::max((dd1.n_rows > 0) ? dd1.col(1).max() : 0.0,
                            ff_mgr.max_batch_age());
    stats.prop_annual_encounters = prop_annual_encounters_val;
    stats.min_len_mm = min_len_mm_val;
    stats.max_len_mm = max_len_mm_val;
    stats.comp_mode = comp_mode_val;
    stats.release_mortality = release_mortality_val;
    stats.policy_combo_id = policy_combo_id;
    
    monthly_data.push_back(stats);
  }
  
  return monthly_data;
}

//' Run IBM Simulation with Size Limits
 // [[Rcpp::export]]
 List run_simulation_sizelimit_cpp(
     NumericVector zr_w_dist, NumericVector month_weights,
     NumericMatrix W1_alk, NumericMatrix agedata,
     List harvest_params_in, List growth_params_dd_in1, List growth_params_dd_in2,
     List survival_params, List scenario_to_run,
     DataFrame policy_combos,
     DataFrame compliance_structure,
     int before_policy_years = 50, int policy_years = 30,
     double lake_area_ha = 2818.635, int initial_pop_size = 10000,
     double rec_a = 2.901, double rec_b = 134.78, double rec_v = 0.68,
     double F_over_Z_ratio = 0.5, double juv_onlyM_len = 130.0,
     int spawn_month = 4,
     int recruit_entry_month = 8, int rep = 1,
     double vmonthly_avg = 0.15,
     double min_adult_age = 1.0,
     double age_recruit = 1.0,
     double age_spawn = 1.0,
     double psd_stock = 130.0, double psd_quality = 200.0,
     double psd_preferred = 250.0, double psd_memorable = 300.0, double psd_trophy = 380.0,
     bool Fagemode = false,
     bool use_ricker = true,
     int T_safe = 0) {
   validate_fastforward_age_window(T_safe, min_adult_age, age_spawn, Fagemode, age_recruit);
   
   NumericVector thres_nv = compliance_structure["Threshold_mm"];
   NumericVector probs_nv = compliance_structure["Probability"];
   std::vector<double> comp_thresholds = as<std::vector<double>>(thres_nv);
   std::vector<double> comp_probs = as<std::vector<double>>(probs_nv);
   
   int scenario_id = scenario_to_run["scenario_id"];
   unsigned int seed = static_cast<unsigned int>(rep * 123456 + scenario_id * 789);
   SimulationRNG rng(seed);
   
   double h_p_max = harvest_params_in["p_max"];
   double h_L50 = harvest_params_in["L50"];
   double h_slope = harvest_params_in["slope"];
   
   double g1_a = growth_params_dd_in1["a"], g1_b = growth_params_dd_in1["b"];
   double g1_c = growth_params_dd_in1["c"], g1_d_avg = growth_params_dd_in1["d_avg"];
   double g2_a = growth_params_dd_in2["a"], g2_b = growth_params_dd_in2["b"];
   double g2_c = growth_params_dd_in2["c"], g2_d_avg = growth_params_dd_in2["d_avg"];
   
   double s_a = survival_params["a"], s_b = survival_params["b"];
   double s_c = survival_params["c"], s_d1 = survival_params["d_avg1"], s_d2 = survival_params["d_avg2"];
   
   double prop_annual_encounters_val = scenario_to_run["prop_annual_encounters"];
   double ESD = scenario_to_run["ESD"];
   int burnin_comp_mode = scenario_to_run["burnin_comp_mode"];
   double burnin_release_mortality = scenario_to_run["burnin_release_mortality"];
   double min_len_mm_val = scenario_to_run["min_len_mm"];
   double max_len_mm_val = scenario_to_run["max_len_mm"];
   
   arma::mat W1_alk_arma = as<arma::mat>(W1_alk);
   arma::mat Theta_clean = as<arma::mat>(agedata);
   arma::vec zr_w_dist_arma = as<arma::vec>(zr_w_dist);
   
   arma::mat dd1;
   if (initial_pop_size > 0) {
     dd1 = init_population_cpp_arma(W1_alk_arma, Theta_clean, zr_w_dist_arma, initial_pop_size, F_over_Z_ratio, rng);
   } else {
     dd1 = arma::mat(0, 6);
   }
   
   arma::vec month_weights_arma = as<arma::vec>(month_weights);
   double wsum = arma::accu(month_weights_arma);
   double p_year = std::max(0.0, std::min(0.999999, prop_annual_encounters_val));
   double lambda_year = -std::log(1.0 - p_year);
   arma::vec rel_w = month_weights_arma / wsum;
   arma::vec lambda_month = lambda_year * rel_w;
   arma::vec prop_enc_month_vec = 1.0 - arma::exp(-lambda_month);
   for (arma::uword i = 0; i < prop_enc_month_vec.n_elem; ++i) {
     prop_enc_month_vec[i] = std::max(0.0, std::min(0.999999, prop_enc_month_vec[i]));
   }
   
   // ============ BEFORE-POLICY (Transient + Stable) ============
   std::vector<MonthlyStats> before_policy_data;
   before_policy_data.reserve(before_policy_years * 12);
   
   double Rden_year = 0.0;
   int Rall_year = 0;
   arma::mat pending_Rd1 = arma::mat(0, 6);
   int pending_ff_recruits = 0;
   double env_mult_juv = 1.0, env_mult_adlt = 1.0;
   
   // Burn-in, compliance ,
   // min/max len , compliance check
   std::vector<double> dummy_thres; std::vector<double> dummy_probs;
   
   FastForwardManager ff_mgr(T_safe);
   
   for (int m = 1; m <= before_policy_years * 12; ++m) {
     int year = (m - 1) / 12 + 1;
     int month_of_year = (m - 1) % 12 + 1;
     
     if (month_of_year == 1) {
       const double lower_bound = 1.0 - 1.645 * ESD;
       const double upper_bound = 1.0 + 1.645 * ESD;
       double envy_raw = rng.normal(1.0, ESD);
       double envy = std::max(lower_bound, std::min(envy_raw, upper_bound));
       env_mult_juv = envy;
       env_mult_adlt = envy;
     }
     
     // Density used for growth is evaluated BEFORE recruit-entry batch is added.
     double ff_juv_before_growth = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
     
     arma::mat growth_results = growthf_arma(dd1, g1_d_avg, g1_a, g1_b, g1_c,
                                             g2_d_avg, g2_a, g2_b, g2_c, lake_area_ha,
                                             min_adult_age, ff_juv_before_growth);
     
     // Batch growth factor PG uses same pre-entry density as main-pop growth
     double juvN_growth = arma::accu(dd1.col(1) < min_adult_age) + ff_juv_before_growth;
     double juv_dens_growth = (lake_area_ha > 1e-6) ? (juvN_growth / lake_area_ha) : 0.0;
     double PD_juv_growth = (g1_d_avg > 1e-6) ? (juv_dens_growth / g1_d_avg) : 0.0;
     double PG_juv_growth = g1_a + g1_b * std::exp(-g1_c * PD_juv_growth);
     
     if (month_of_year == spawn_month) {
       double Spawners = arma::accu(growth_results.col(1) >= age_spawn);
       double Sden_spawn = Spawners / lake_area_ha;
       if (use_ricker) {
         Rden_year = rec_a * Sden_spawn * std::exp(-rec_b * Sden_spawn);
       } else {
         Rden_year = rec_a * Sden_spawn / (1.0 + rec_b * Sden_spawn);
       }
       if (Rden_year < 0.0 || !R_finite(Rden_year)) Rden_year = 0.0;
       Rall_year = static_cast<int>(std::floor(Rden_year * lake_area_ha));
       
       if (Rall_year > 0) {
         if (ff_mgr.enabled()) {
           pending_ff_recruits = Rall_year;
           pending_Rd1.set_size(0, 6);
         } else {
           pending_Rd1 = recruits_init_cpp_arma(Theta_clean, zr_w_dist_arma, Rall_year, F_over_Z_ratio, rng);
           pending_ff_recruits = 0;
         }
       } else {
         pending_Rd1.set_size(0, 6);
         pending_ff_recruits = 0;
       }
     }
     
     arma::mat dd1_all = growth_results;
     if (month_of_year == recruit_entry_month) {
       if (ff_mgr.enabled() && pending_ff_recruits > 0) {
         ff_mgr.add_batch(static_cast<double>(pending_ff_recruits));
         pending_ff_recruits = 0;
       } else if (pending_Rd1.n_rows > 0) {
         arma::mat Rd1real = growthf0_arma(pending_Rd1);
         dd1_all = arma::join_vert(growth_results, Rd1real);
         pending_Rd1.set_size(0, 6);
       }
     }
     
     double juvN_main = arma::accu(dd1_all.col(1) < min_adult_age);
     double adN       = arma::accu(dd1_all.col(1) >= min_adult_age);
     double juvN_ff   = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
     double juvN      = juvN_main + juvN_ff;
     double juv_dens = (lake_area_ha > 1e-6) ? (juvN / lake_area_ha) : 0.0;
     double ad_dens  = (lake_area_ha > 1e-6) ? (adN  / lake_area_ha) : 0.0;
     double PV_juv   = s_a + s_b * (1.0 - std::exp(-s_c * (juv_dens / s_d1)));
     double PV_adult = s_a + s_b * (1.0 - std::exp(-s_c * (ad_dens  / s_d2)));
     
     // ===== Update fast-forward batches; capture graduates for post-survival merge =====
     arma::mat ff_graduates(0, 6);
     if (ff_mgr.enabled() && !ff_mgr.batches.empty()) {
       auto pos_ff = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
       double env_juv_ff = pos_ff(env_mult_juv, 1e-9);
       double PVJ_ff = pos_ff(PV_juv, 1e-9);
       double M_juv_monthly = pos_ff(vmonthly_avg * PVJ_ff * env_juv_ff);
       ff_graduates = ff_mgr.update_and_graduate(
         M_juv_monthly, PG_juv_growth, Theta_clean, zr_w_dist_arma, F_over_Z_ratio, rng);
     }
     
     double prop_enc_month = prop_enc_month_vec(month_of_year - 1);
     
     // Before-policy survival (dummy compliance)
     SurvivalResults fishing_results = survival_ibm_competing_risks(
       dd1_all, prop_enc_month, 0.0, 1e9,
       h_p_max, h_L50, h_slope, burnin_comp_mode,
       dummy_thres, dummy_probs,
       PV_adult, PV_juv, vmonthly_avg, env_mult_juv, env_mult_adlt,
       juv_onlyM_len, burnin_release_mortality, 50,
       psd_quality, psd_preferred, psd_memorable, psd_trophy,
       min_adult_age,
       Fagemode, age_recruit,
       rng);
     
     double prop_Q = 0.0, prop_P = 0.0, prop_M = 0.0, prop_T = 0.0;
     bool any_trophy = false;
     if (fishing_results.enc_n_total > 0) {
       prop_Q = 100.0 * fishing_results.enc_n_Q / fishing_results.enc_n_total;
       prop_P = 100.0 * fishing_results.enc_n_P / fishing_results.enc_n_total;
       prop_M = 100.0 * fishing_results.enc_n_M / fishing_results.enc_n_total;
       prop_T = 100.0 * fishing_results.enc_n_T / fishing_results.enc_n_total;
       any_trophy = (fishing_results.enc_n_T > 0);
     }
     
     dd1 = fishing_results.survivors;
     
     // Merge graduated fast-forward fish AFTER survival (they already completed
     // T_safe months of expected juvenile survival during fast-forward).
     if (ff_graduates.n_rows > 0) {
       dd1 = arma::join_vert(dd1, ff_graduates);
     }
     
     MonthlyStats stats;
     stats.year = year; stats.month = m; stats.month_of_year = month_of_year;
     stats.phase = "before_policy";
     
     double AdultN_curr = arma::accu(dd1.col(1) >= min_adult_age);
     double AgeFRN = arma::accu((dd1.col(1) >= age_recruit) && (dd1.col(1) < (1+age_recruit)))
       + ff_mgr.total_in_age_range(age_recruit, 1.0 + age_recruit);
     stats.AgeFRN = static_cast<int>(std::llround(AgeFRN));
     
     double Spawners_curr = arma::accu(dd1.col(1) >= age_spawn);
     stats.Sden = (lake_area_ha > 1e-6) ? (Spawners_curr / lake_area_ha) : 0.0;
     
     double Larve_curr = arma::accu(dd1.col(1) < 1.0) + ff_mgr.total_below_age(1.0);
     stats.Rden = (lake_area_ha > 1e-6) ? (Larve_curr / lake_area_ha) : 0.0;
     stats.AdultN = static_cast<int>(AdultN_curr);
     stats.Yield_n = fishing_results.yield_n;
     stats.N_pop = static_cast<int>(dd1.n_rows) + static_cast<int>(std::llround(ff_mgr.total_juveniles()));
     
     arma::vec lengths = dd1.col(0);
     int n_stock = arma::accu(lengths >= psd_stock);
     int n_q = arma::accu(lengths >= psd_quality);
     int n_pref = arma::accu(lengths >= psd_preferred);
     int n_m = arma::accu(lengths >= psd_memorable);
     int n_t = arma::accu(lengths >= psd_trophy);
     
     stats.PSD_Q = (n_stock > 0) ? 100.0 * n_q / n_stock : 0.0;
     stats.PSD_P = (n_stock > 0) ? 100.0 * n_pref / n_stock : 0.0;
     stats.PSD_M = (n_stock > 0) ? 100.0 * n_m / n_stock : 0.0;
     stats.PSD_T = (n_stock > 0) ? 100.0 * n_t / n_stock : 0.0;
     
     stats.Enc_Q = prop_Q; stats.Enc_P = prop_P; stats.Enc_M = prop_M; stats.Enc_T = prop_T;
     stats.trophy_seen = any_trophy;
     stats.maxage = std::max((dd1.n_rows > 0) ? dd1.col(1).max() : 0.0,
                             ff_mgr.max_batch_age());
     stats.prop_annual_encounters = prop_annual_encounters_val;
     stats.min_len_mm = 0.0;
     stats.max_len_mm = 1e9;
     stats.comp_mode = burnin_comp_mode;
     stats.release_mortality = burnin_release_mortality;
     stats.policy_combo_id = 0;
     
     before_policy_data.push_back(stats);
   }
   
   // Save state at end of before-policy phase
   arma::mat dd1_bp_end = dd1;
   arma::mat pending_Rd1_bp_end = pending_Rd1;
   double Rden_year_bp_end = Rden_year;
   int Rall_year_bp_end = Rall_year;
   double env_mult_juv_bp_end = env_mult_juv;
   double env_mult_adlt_bp_end = env_mult_adlt;
   FastForwardManager ff_mgr_bp_end = ff_mgr;
   int pending_ff_recruits_bp_end = pending_ff_recruits;
   std::mt19937 rng_state_bp_end = rng.get_state();
   
   // ============ POLICY ============
   IntegerVector policy_combo_ids = policy_combos["policy_combo_id"];
   IntegerVector policy_comp_modes = policy_combos["comp_mode"];
   NumericVector policy_release_morts = policy_combos["release_mortality"];
   int n_combos = policy_combo_ids.size();
   
   List result;
   result["before_policy"] = monthly_stats_to_dataframe(before_policy_data);
   
   for (int c = 0; c < n_combos; ++c) {
     SimulationRNG rng_copy(0);
     rng_copy.set_state(rng_state_bp_end);
     
     std::vector<MonthlyStats> policy_data = run_policy_phase(
       dd1_bp_end, pending_Rd1_bp_end, Rden_year_bp_end, Rall_year_bp_end,
       rng_copy, env_mult_juv_bp_end, env_mult_adlt_bp_end,
       Theta_clean, zr_w_dist_arma, month_weights_arma, prop_enc_month_vec,
       h_p_max, h_L50, h_slope, g1_d_avg, g1_a, g1_b, g1_c,
       g2_d_avg, g2_a, g2_b, g2_c, s_a, s_b, s_c, s_d1, s_d2,
       vmonthly_avg, lake_area_ha, rec_a, rec_b, F_over_Z_ratio,
       juv_onlyM_len,
       comp_thresholds, comp_probs, 
       psd_stock, psd_quality, psd_preferred, psd_memorable, psd_trophy,
       min_adult_age,
       age_recruit,
       age_spawn,
       Fagemode,
       spawn_month, recruit_entry_month, ESD,
       policy_years, before_policy_years, prop_annual_encounters_val,
       min_len_mm_val, max_len_mm_val,
       policy_comp_modes[c], policy_release_morts[c], policy_combo_ids[c],
                                                                      use_ricker, ff_mgr_bp_end, pending_ff_recruits_bp_end);
     
     std::string name = "policy_" + std::to_string(policy_combo_ids[c]);
     result[name] = monthly_stats_to_dataframe(policy_data);
   }
   
   return result;
 }


// =============================================================================
// GPU Detection 
// =============================================================================
namespace gpu_detect {

struct GpuInfo {
  bool available;
  std::string name, platform, backend;
  int compute_units;
  size_t global_mem_mb;
};

inline GpuInfo detect_gpu() {
  GpuInfo info{false, "None", "None", "CPU-Threads", 0, 0};
  
#ifdef _WIN32
  FILE* pipe = _popen(
    "nvidia-smi --query-gpu=name,memory.total --format=csv,noheader,nounits 2>NUL", "r");
  if (pipe) {
    char buf[256];
    if (fgets(buf, sizeof(buf), pipe)) {
      std::string line(buf);
      size_t comma = line.find(',');
      if (comma != std::string::npos) {
        info.available = true;
        info.name = line.substr(0, comma);
        while (!info.name.empty() && (info.name.back()==' ' || info.name.back()=='\n'))
          info.name.pop_back();
        info.global_mem_mb = static_cast<size_t>(std::atol(line.substr(comma+1).c_str()));
        info.platform = "NVIDIA CUDA";
        info.backend = "GPU-Threads";
      }
    }
    _pclose(pipe);
  }
#else
  FILE* pipe = popen(
    "nvidia-smi --query-gpu=name,memory.total --format=csv,noheader,nounits 2>/dev/null", "r");
  if (pipe) {
    char buf[256];
    if (fgets(buf, sizeof(buf), pipe)) {
      std::string line(buf);
      size_t comma = line.find(',');
      if (comma != std::string::npos) {
        info.available = true;
        info.name = line.substr(0, comma);
        while (!info.name.empty() && (info.name.back()==' ' || info.name.back()=='\n'))
          info.name.pop_back();
        info.global_mem_mb = static_cast<size_t>(std::atol(line.substr(comma+1).c_str()));
        info.platform = "NVIDIA CUDA";
        info.backend = "GPU-Threads";
      }
    }
    pclose(pipe);
  }
#endif
  return info;
}

} // namespace gpu_detect


// [[Rcpp::export]]
List detect_gpu_info() {
  auto info = gpu_detect::detect_gpu();
  return List::create(
    Named("gpu_available")     = info.available,
    Named("gpu_name")          = info.name,
    Named("gpu_platform")      = info.platform,
    Named("gpu_memory_mb")     = static_cast<int>(info.global_mem_mb),
    Named("gpu_compute_units") = info.compute_units,
    Named("backend")           = info.backend,
    Named("cpu_thread_fallback") = !info.available
  );
}


// [[Rcpp::export]]
List detect_openmp_info() {
#ifdef _OPENMP
  return List::create(
    Named("openmp_available") = true,
    Named("max_threads") = omp_get_max_threads(),
    Named("openmp_version") = static_cast<int>(_OPENMP)
  );
#else
  return List::create(
    Named("openmp_available") = false,
    Named("max_threads") = 1,
    Named("openmp_version") = 0
  );
#endif
}




// [[Rcpp::export]]
List run_simulation_gpu(
    NumericVector zr_w_dist, NumericVector month_weights,
    NumericMatrix W1_alk, NumericMatrix agedata,
    List harvest_params_in, List growth_params_dd_in1, List growth_params_dd_in2,
    List survival_params, List scenario_to_run,
    DataFrame policy_combos,
    DataFrame compliance_structure,
    int before_policy_years = 50, int policy_years = 30,
    double lake_area_ha = 2818.635, int initial_pop_size = 10000,
    double rec_a = 2.901, double rec_b = 134.78, double rec_v = 0.68,
    double F_over_Z_ratio = 0.5, double juv_onlyM_len = 130.0,
    int spawn_month = 4,
    int recruit_entry_month = 8, int rep = 1,
    double vmonthly_avg = 0.15,
    double min_adult_age = 1.0,
    double age_recruit = 1.0,
    double age_spawn = 1.0,
    double psd_stock = 130.0, double psd_quality = 200.0,
    double psd_preferred = 250.0, double psd_memorable = 300.0, double psd_trophy = 380.0,
    bool Fagemode = false,
    bool use_ricker = true,
    int T_safe = 0,
    int gpu_threads = 0) {
  validate_fastforward_age_window(T_safe, min_adult_age, age_spawn, Fagemode, age_recruit);
  if (gpu_threads < 0) Rcpp::stop("gpu_threads must be >= 0.");
  
  // ==== STEP 1: Extract R → C++ (MAIN THREAD ONLY) ====
  NumericVector thres_nv = compliance_structure["Threshold_mm"];
  NumericVector probs_nv = compliance_structure["Probability"];
  std::vector<double> comp_thresholds = as<std::vector<double>>(thres_nv);
  std::vector<double> comp_probs      = as<std::vector<double>>(probs_nv);
  
  int    scenario_id              = scenario_to_run["scenario_id"];
  double prop_annual_encounters_val = scenario_to_run["prop_annual_encounters"];
  double ESD                      = scenario_to_run["ESD"];
  int    burnin_comp_mode         = scenario_to_run["burnin_comp_mode"];
  double burnin_release_mortality = scenario_to_run["burnin_release_mortality"];
  double min_len_mm_val           = scenario_to_run["min_len_mm"];
  double max_len_mm_val           = scenario_to_run["max_len_mm"];
  
  double h_p_max = harvest_params_in["p_max"];
  double h_L50   = harvest_params_in["L50"];
  double h_slope = harvest_params_in["slope"];
  
  double g1_a = growth_params_dd_in1["a"], g1_b = growth_params_dd_in1["b"];
  double g1_c = growth_params_dd_in1["c"], g1_d_avg = growth_params_dd_in1["d_avg"];
  double g2_a = growth_params_dd_in2["a"], g2_b = growth_params_dd_in2["b"];
  double g2_c = growth_params_dd_in2["c"], g2_d_avg = growth_params_dd_in2["d_avg"];
  
  double s_a = survival_params["a"], s_b = survival_params["b"];
  double s_c = survival_params["c"];
  double s_d1 = survival_params["d_avg1"], s_d2 = survival_params["d_avg2"];
  
  IntegerVector pcid_r = policy_combos["policy_combo_id"];
  IntegerVector pcm_r  = policy_combos["comp_mode"];
  NumericVector prm_r  = policy_combos["release_mortality"];
  int n_combos = pcid_r.size();
  std::vector<int>    policy_combo_ids(n_combos), policy_comp_modes(n_combos);
  std::vector<double> policy_release_morts(n_combos);
  for (int i = 0; i < n_combos; ++i) {
    policy_combo_ids[i] = pcid_r[i];
    policy_comp_modes[i] = pcm_r[i];
    policy_release_morts[i] = prm_r[i];
  }
  
  arma::mat W1_alk_arma       = as<arma::mat>(W1_alk);
  arma::mat Theta_clean        = as<arma::mat>(agedata);
  arma::vec zr_w_dist_arma     = as<arma::vec>(zr_w_dist);
  arma::vec month_weights_arma = as<arma::vec>(month_weights);
  
  // ==== STEP 2: Thread count ====
  int hw_threads = std::thread::hardware_concurrency();
  if (hw_threads == 0) hw_threads = 4;
  int n_threads;
  if (gpu_threads > 0) n_threads = std::min(gpu_threads, n_combos);
  else n_threads = std::min(n_combos, hw_threads);
  if (n_combos <= 1) n_threads = 1;
  
  // ==== STEP 3: Before-policy phase (MAIN THREAD, pure C++) ====
  unsigned int seed = static_cast<unsigned int>(
    rep * 123456 + scenario_id * 789);
  SimulationRNG rng(seed);
  
  arma::mat dd1;
  if (initial_pop_size > 0)
    dd1 = init_population_cpp_arma(W1_alk_arma, Theta_clean, zr_w_dist_arma,
                                   initial_pop_size, F_over_Z_ratio, rng);
  else
    dd1 = arma::mat(0, 6);
  
  double wsum = arma::accu(month_weights_arma);
  double p_year = std::max(0.0, std::min(0.999999, prop_annual_encounters_val));
  double lambda_year = -std::log(1.0 - p_year);
  arma::vec rel_w = month_weights_arma / wsum;
  arma::vec lambda_month = lambda_year * rel_w;
  arma::vec prop_enc_month_vec = 1.0 - arma::exp(-lambda_month);
  for (arma::uword i = 0; i < prop_enc_month_vec.n_elem; ++i)
    prop_enc_month_vec[i] = std::max(0.0, std::min(0.999999, prop_enc_month_vec[i]));
  
  std::vector<MonthlyStats> before_policy_data;
  before_policy_data.reserve(before_policy_years * 12);
  
  double Rden_year = 0.0;
  int Rall_year = 0;
  arma::mat pending_Rd1 = arma::mat(0, 6);
  int pending_ff_recruits = 0;
  double env_mult_juv = 1.0, env_mult_adlt = 1.0;
  std::vector<double> dummy_thres, dummy_probs;
  
  FastForwardManager ff_mgr(T_safe);
  
  for (int m = 1; m <= before_policy_years * 12; ++m) {
    int year = (m-1)/12 + 1;
    int moy  = (m-1)%12 + 1;
    
    if (moy == 1) {
      double lo = 1.0 - 1.645*ESD, hi = 1.0 + 1.645*ESD;
      double e = std::max(lo, std::min(rng.normal(1.0, ESD), hi));
      env_mult_juv = e; env_mult_adlt = e;
    }
    
    // Density used for growth is evaluated BEFORE recruit-entry batch is added.
    double ff_juv_before_growth = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
    
    arma::mat gr = growthf_arma(dd1, g1_d_avg, g1_a, g1_b, g1_c,
                                g2_d_avg, g2_a, g2_b, g2_c, lake_area_ha, min_adult_age,
                                ff_juv_before_growth);
    
    // Batch growth factor PG uses same pre-entry density as main-pop growth
    double juvN_growth = arma::accu(dd1.col(1) < min_adult_age) + ff_juv_before_growth;
    double juv_dens_growth = (lake_area_ha > 1e-6) ? (juvN_growth / lake_area_ha) : 0.0;
    double PD_juv_growth = (g1_d_avg > 1e-6) ? (juv_dens_growth / g1_d_avg) : 0.0;
    double PG_juv_growth = g1_a + g1_b * std::exp(-g1_c * PD_juv_growth);
    
    if (moy == spawn_month) {
      double S = arma::accu(gr.col(1) >= age_spawn);
      double Sd = S / lake_area_ha;
      Rden_year = use_ricker ? rec_a*Sd*std::exp(-rec_b*Sd) : rec_a*Sd/(1.0+rec_b*Sd);
      if (Rden_year < 0.0 || !R_finite(Rden_year)) Rden_year = 0.0;
      Rall_year = static_cast<int>(std::floor(Rden_year * lake_area_ha));
      if (Rall_year > 0) {
        if (ff_mgr.enabled()) {
          pending_ff_recruits = Rall_year;
          pending_Rd1.set_size(0,6);
        } else {
          pending_Rd1 = recruits_init_cpp_arma(Theta_clean, zr_w_dist_arma, Rall_year, F_over_Z_ratio, rng);
          pending_ff_recruits = 0;
        }
      } else {
        pending_Rd1.set_size(0,6);
        pending_ff_recruits = 0;
      }
    }
    
    arma::mat dd1_all = gr;
    if (moy == recruit_entry_month) {
      if (ff_mgr.enabled() && pending_ff_recruits > 0) {
        ff_mgr.add_batch(static_cast<double>(pending_ff_recruits));
        pending_ff_recruits = 0;
      } else if (pending_Rd1.n_rows > 0) {
        dd1_all = arma::join_vert(gr, growthf0_arma(pending_Rd1));
        pending_Rd1.set_size(0,6);
      }
    }
    
    double jN_main = arma::accu(dd1_all.col(1) < min_adult_age);
    double aN = arma::accu(dd1_all.col(1) >= min_adult_age);
    double jN_ff = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
    double jN = jN_main + jN_ff;
    double jd = (lake_area_ha>1e-6)?(jN/lake_area_ha):0.0;
    double ad = (lake_area_ha>1e-6)?(aN/lake_area_ha):0.0;
    double PVj = s_a + s_b*(1.0-std::exp(-s_c*(jd/s_d1)));
    double PVa = s_a + s_b*(1.0-std::exp(-s_c*(ad/s_d2)));
    
    // Update fast-forward batches; graduates merged after survival
    arma::mat ff_graduates(0, 6);
    if (ff_mgr.enabled() && !ff_mgr.batches.empty()) {
      auto pos_ff = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
      double M_juv_monthly = pos_ff(vmonthly_avg * pos_ff(PVj,1e-9) * pos_ff(env_mult_juv,1e-9));
      ff_graduates = ff_mgr.update_and_graduate(
        M_juv_monthly, PG_juv_growth, Theta_clean, zr_w_dist_arma, F_over_Z_ratio, rng);
    }
    
    SurvivalResults fr = survival_ibm_competing_risks(
      dd1_all, prop_enc_month_vec(moy-1), 0.0, 1e9,
      h_p_max, h_L50, h_slope, burnin_comp_mode,
      dummy_thres, dummy_probs,
      PVa, PVj, vmonthly_avg, env_mult_juv, env_mult_adlt,
      juv_onlyM_len, burnin_release_mortality, 50,
      psd_quality, psd_preferred, psd_memorable, psd_trophy,
      min_adult_age, Fagemode, age_recruit, rng);
    
    dd1 = fr.survivors;
    if (ff_graduates.n_rows > 0) dd1 = arma::join_vert(dd1, ff_graduates);
    
    MonthlyStats st;
    st.year = year; st.month = m; st.month_of_year = moy;
    st.phase = "before_policy";
    st.AdultN = static_cast<int>(arma::accu(dd1.col(1) >= min_adult_age));
    st.AgeFRN = static_cast<int>(std::llround(
      arma::accu((dd1.col(1)>=age_recruit)&&(dd1.col(1)<(1+age_recruit))) +
        ff_mgr.total_in_age_range(age_recruit, 1.0 + age_recruit)));
    double Sp = arma::accu(dd1.col(1) >= age_spawn);
    st.Sden = (lake_area_ha>1e-6)?(Sp/lake_area_ha):0.0;
    double Larve_total = arma::accu(dd1.col(1)<1.0) + ff_mgr.total_below_age(1.0);
    st.Rden = (lake_area_ha>1e-6)?(Larve_total/lake_area_ha):0.0;
    st.Yield_n = fr.yield_n;
    st.N_pop = static_cast<int>(dd1.n_rows) + static_cast<int>(std::llround(ff_mgr.total_juveniles()));
    arma::vec lens = dd1.col(0);
    int nst = arma::accu(lens >= psd_stock);
    st.PSD_Q = nst>0 ? 100.0*arma::accu(lens>=psd_quality)/nst : 0.0;
    st.PSD_P = nst>0 ? 100.0*arma::accu(lens>=psd_preferred)/nst : 0.0;
    st.PSD_M = nst>0 ? 100.0*arma::accu(lens>=psd_memorable)/nst : 0.0;
    st.PSD_T = nst>0 ? 100.0*arma::accu(lens>=psd_trophy)/nst : 0.0;
    if (fr.enc_n_total>0) {
      st.Enc_Q = 100.0*fr.enc_n_Q/fr.enc_n_total;
      st.Enc_P = 100.0*fr.enc_n_P/fr.enc_n_total;
      st.Enc_M = 100.0*fr.enc_n_M/fr.enc_n_total;
      st.Enc_T = 100.0*fr.enc_n_T/fr.enc_n_total;
      st.trophy_seen = (fr.enc_n_T>0);
    } else { st.Enc_Q=st.Enc_P=st.Enc_M=st.Enc_T=0; st.trophy_seen=false; }
    st.maxage = std::max(dd1.n_rows>0 ? dd1.col(1).max() : 0.0,
                         ff_mgr.max_batch_age());
    st.prop_annual_encounters = prop_annual_encounters_val;
    st.min_len_mm = 0.0; st.max_len_mm = 1e9;
    st.comp_mode = burnin_comp_mode;
    st.release_mortality = burnin_release_mortality;
    st.policy_combo_id = 0;
    before_policy_data.push_back(st);
  }
  
  // ==== STEP 4: Save snapshot ====
  arma::mat dd1_bp_end = dd1;
  arma::mat pending_Rd1_bp_end = pending_Rd1;
  double Rden_bp = Rden_year, env_j_bp = env_mult_juv, env_a_bp = env_mult_adlt;
  int Rall_bp = Rall_year;
  FastForwardManager ff_mgr_bp = ff_mgr;
  int pending_ff_recruits_bp = pending_ff_recruits;
  std::mt19937 rng_state_bp = rng.get_state();
  
  // ==== STEP 5: Parallel policy combos (PURE C++ — thread safe) ====
  std::vector<std::vector<MonthlyStats>> combo_results(n_combos);
  std::vector<std::string> combo_errors(n_combos);
  
  auto worker = [&](int c) {
    try {
      SimulationRNG rng_c(0);
      rng_c.set_state(rng_state_bp);
      combo_results[c] = run_policy_phase(
        dd1_bp_end, pending_Rd1_bp_end, Rden_bp, Rall_bp,
        rng_c, env_j_bp, env_a_bp,
        Theta_clean, zr_w_dist_arma, month_weights_arma, prop_enc_month_vec,
        h_p_max, h_L50, h_slope, g1_d_avg, g1_a, g1_b, g1_c,
        g2_d_avg, g2_a, g2_b, g2_c, s_a, s_b, s_c, s_d1, s_d2,
        vmonthly_avg, lake_area_ha, rec_a, rec_b, F_over_Z_ratio,
        juv_onlyM_len, comp_thresholds, comp_probs,
        psd_stock, psd_quality, psd_preferred, psd_memorable, psd_trophy,
        min_adult_age, age_recruit, age_spawn, Fagemode,
        spawn_month, recruit_entry_month, ESD,
        policy_years, before_policy_years, prop_annual_encounters_val,
        min_len_mm_val, max_len_mm_val,
        policy_comp_modes[c], policy_release_morts[c], policy_combo_ids[c],
                                                                       use_ricker, ff_mgr_bp, pending_ff_recruits_bp);
    } catch (std::exception& e) {
      combo_errors[c] = e.what();
    }
  };
  
  if (n_threads <= 1) {
    for (int c = 0; c < n_combos; ++c) worker(c);
  } else {
    for (int b = 0; b < n_combos; b += n_threads) {
      int be = std::min(b + n_threads, n_combos);
      std::vector<std::thread> thr;
      for (int c = b; c < be; ++c) thr.emplace_back(worker, c);
      for (auto& t : thr) t.join();
    }
  }
  
  // ==== STEP 6: Convert back to R (MAIN THREAD ONLY) ====
  List result;
  result["before_policy"] = monthly_stats_to_dataframe(before_policy_data);
  for (int c = 0; c < n_combos; ++c) {
    if (!combo_errors[c].empty()) {
      Rcpp::warning("Policy combo %d error: %s",
                    policy_combo_ids[c], combo_errors[c].c_str());
      continue;
    }
    std::string key = "policy_" + std::to_string(policy_combo_ids[c]);
    result[key] = monthly_stats_to_dataframe(combo_results[c]);
  }
  return result;
}



// [[Rcpp::export]]
List run_simulation_hybrid(
    NumericVector zr_w_dist, NumericVector month_weights,
    NumericMatrix W1_alk, NumericMatrix agedata,
    List harvest_params_in, List growth_params_dd_in1, List growth_params_dd_in2,
    List survival_params, List scenario_to_run,
    DataFrame policy_combos,
    DataFrame compliance_structure,
    IntegerVector cpu_combo_indices,
    IntegerVector gpu_combo_indices,
    int gpu_thread_count = 4,
    int before_policy_years = 50, int policy_years = 30,
    double lake_area_ha = 2818.635, int initial_pop_size = 10000,
    double rec_a = 2.901, double rec_b = 134.78, double rec_v = 0.68,
    double F_over_Z_ratio = 0.5, double juv_onlyM_len = 130.0,
    int spawn_month = 4,
    int recruit_entry_month = 8, int rep = 1,
    double vmonthly_avg = 0.15,
    double min_adult_age = 1.0,
    double age_recruit = 1.0,
    double age_spawn = 1.0,
    double psd_stock = 130.0, double psd_quality = 200.0,
    double psd_preferred = 250.0, double psd_memorable = 300.0, double psd_trophy = 380.0,
    bool Fagemode = false,
    bool use_ricker = true,
    int T_safe = 0) {
  
  return run_simulation_gpu(
    zr_w_dist, month_weights, W1_alk, agedata,
    harvest_params_in, growth_params_dd_in1, growth_params_dd_in2,
    survival_params, scenario_to_run, policy_combos, compliance_structure,
    before_policy_years, policy_years, lake_area_ha, initial_pop_size,
    rec_a, rec_b, rec_v, F_over_Z_ratio, juv_onlyM_len,
    spawn_month, recruit_entry_month, rep, vmonthly_avg,
    min_adult_age, age_recruit, age_spawn,
    psd_stock, psd_quality, psd_preferred, psd_memorable, psd_trophy,
    Fagemode, use_ricker, T_safe,gpu_thread_count);
}




struct FishPool {
  arma::mat data;                // capacity × 6, pre-allocated
  std::vector<uint8_t> alive;    // 1=alive, 0=dead
  int n_alive;
  int capacity;
  int next_dead_hint;            // hint for next dead slot search
  
  // Create from existing arma::mat (all rows alive)
  static FishPool from_mat(const arma::mat& src, int extra_capacity = 0) {
    FishPool pool;
    int n = static_cast<int>(src.n_rows);
    extra_capacity = std::max(0, extra_capacity);
    pool.capacity = n + extra_capacity;
    pool.data.set_size(pool.capacity, 6);
    pool.alive.assign(pool.capacity, 0);
    pool.n_alive = n;
    pool.next_dead_hint = n;
    
    if (n > 0) {
      pool.data.rows(0, n - 1) = src;
      for (int i = 0; i < n; ++i) pool.alive[i] = 1;
    }
    return pool;
  }
  
  // Convert back to arma::mat (compact, only alive rows)
  arma::mat to_mat() const {
    if (n_alive == 0) return arma::mat(0, 6);
    arma::mat out(n_alive, 6);
    int j = 0;
    for (int i = 0; i < capacity && j < n_alive; ++i) {
      if (alive[i]) { out.row(j++) = data.row(i); }
    }
    return out;
  }
  
  // Kill fish at index i
  void kill(int i) {
    if (alive[i]) { alive[i] = 0; n_alive--; if (i < next_dead_hint) next_dead_hint = i; }
  }
  
  // Find next dead slot starting from hint
  int find_dead_slot() {
    for (int i = next_dead_hint; i < capacity; ++i) {
      if (!alive[i]) { next_dead_hint = i + 1; return i; }
    }
    return -1; // pool full
  }
  
  // Add a fish (recruit) — returns slot index or -1 if full
  int add_fish(const arma::rowvec& fish_row) {
    int slot = find_dead_slot();
    if (slot < 0) return -1;
    data.row(slot) = fish_row;
    alive[slot] = 1;
    n_alive++;
    return slot;
  }
  
  // Add multiple recruits from a matrix
  int add_recruits(const arma::mat& recruits) {
    int added = 0;
    for (arma::uword r = 0; r < recruits.n_rows; ++r) {
      if (add_fish(recruits.row(r)) >= 0) added++;
      else break; // pool full
    }
    return added;
  }
  
  // Compact: move all alive fish to front, reset dead slots
  // Call periodically (e.g., once per year) to improve cache locality
  void compact() {
    if (n_alive == 0) { next_dead_hint = 0; return; }
    int write = 0;
    for (int read = 0; read < capacity; ++read) {
      if (alive[read]) {
        if (write != read) {
          data.row(write) = data.row(read);
          alive[write] = 1;
          alive[read] = 0;
        }
        write++;
      }
    }
    next_dead_hint = write;
  }
  
  // Ensure capacity for at least n_needed total slots
  void ensure_capacity(int n_needed) {
    if (n_needed <= capacity) return;
    int new_cap = std::max(n_needed, std::max(1, capacity * 2));
    arma::mat new_data(new_cap, 6, arma::fill::zeros);
    if (capacity > 0) {
      new_data.rows(0, capacity - 1) = data;
    }
    data = std::move(new_data);
    alive.resize(new_cap, 0);
    capacity = new_cap;
    if (next_dead_hint < 0 || next_dead_hint > capacity) next_dead_hint = 0;
  }
};


// =============================================================================
// Survival V2: RNG pre-gen + OpenMP static + alive marking
//
// Parameters:
//   pool         - FishPool (modified in-place: dead fish marked)
//   omp_nthreads - number of OpenMP threads (1 = serial fallback)
//   rng          - master RNG (used ONLY to generate per-thread seeds)
//   [all other params same as original]
//
// Returns:
//   SurvivalResults with yield_n, enc counts (survivors mat is EMPTY —
//   the pool itself IS the survivor state after this call)
// =============================================================================

SurvivalResults survival_ibm_v2(
    FishPool& pool,
    int omp_nthreads,
    double prop_enc_month,
    double min_len, double max_len,
    double p_max, double L50, double slope,
    int comp_mode,
    const std::vector<double>& comp_thresholds,
    const std::vector<double>& comp_probs,
    double PV_adult, double PV_juv, double vmonthly_avg,
    double env_mult_juv, double env_mult_adlt,
    double juv_onlyM_len, double release_mortality,
    double max_encounters_per_month,
    double psd_quality, double psd_preferred, double psd_memorable, double psd_trophy,
    double min_adult_age,
    bool Fagemode, double age_recruit,
    SimulationRNG& rng) {
  
  SurvivalResults results;
  results.yield_n = 0; results.enc_n_total = 0;
  results.enc_n_Q = 0; results.enc_n_P = 0; results.enc_n_M = 0; results.enc_n_T = 0;
  results.survivors = arma::mat(0, 6); // empty — pool IS the state
  
  if (pool.n_alive == 0) return results;
  
  auto pos = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
  
  const double p_enc = std::max(0.0, std::min(1.0, prop_enc_month));
  const double lambdaE = (p_enc >= 1.0) ? 1e6 : (p_enc <= 0.0 ? 0.0 : -std::log(1.0 - p_enc));
  const double env_juv = pos(env_mult_juv, 1e-9);
  const double env_adlt = pos(env_mult_adlt, 1e-9);
  const double PVJ = pos(PV_juv, 1e-9);
  const double PVA = pos(PV_adult, 1e-9);
  const double month_duration = 1.0;
  const int max_enc = static_cast<int>(max_encounters_per_month);
  
  const int cap = pool.capacity;
  
  // =========================================================================
  // M 2: Generate per-thread RNG seeds from master RNG
  // Master RNG advances by exactly omp_nthreads steps (deterministic)
  // =========================================================================
  
  int actual_threads = std::max(1, omp_nthreads);
  actual_threads = std::min(actual_threads, std::max(1, pool.n_alive));
#ifdef _OPENMP
  actual_threads = std::min(actual_threads, std::max(1, omp_get_max_threads()));
#else
  actual_threads = 1;
#endif
  
  std::vector<unsigned int> thread_seeds(actual_threads);
  for (int t = 0; t < actual_threads; ++t) {
    // Use master RNG to generate deterministic seeds
    thread_seeds[t] = static_cast<unsigned int>(rng.uniform() * 4294967295.0);
  }
  
  // Per-thread accumulators (avoid false sharing with padding)
  struct alignas(64) ThreadAcc {
    int yield_n = 0;
    int enc_total = 0, enc_Q = 0, enc_P = 0, enc_M = 0, enc_T = 0;
    int deaths = 0;
  };
  std::vector<ThreadAcc> thread_acc(actual_threads);
  
  // =========================================================================
  // M A: OpenMP parallel fish loop with static scheduling
  // Each thread processes a contiguous chunk of the pool array
  // =========================================================================
  
#ifdef _OPENMP
#pragma omp parallel if(actual_threads > 1) num_threads(actual_threads) 
#endif
{
#ifdef _OPENMP
  int tid = omp_get_thread_num();
#else
  int tid = 0;
#endif
  
  // Per-thread RNG (deterministic seed from master)
  SimulationRNG local_rng(thread_seeds[tid]);
  ThreadAcc& acc = thread_acc[tid];
  
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
  for (int i = 0; i < cap; ++i) {
    if (!pool.alive[i]) continue;  // skip dead slots
    
    const double age_end = pool.data(i, 1);
    const double age0 = age_end - 1.0 / 12.0;
    const bool isAdult = (age0 >= min_adult_age);
    const double L0i = pool.data(i, 0);
    
    // --- Mortality rate calculation ---
    double M_monthly;
    if (isAdult) {
      double annual_mortality_coef = pos(pool.data(i, 5));
      double annual_survival = std::exp(-annual_mortality_coef);
      double monthly_survival = std::pow(annual_survival, 1.0 / 12.0);
      double M0_ad = -std::log(monthly_survival);
      M_monthly = pos(M0_ad * PVA * env_adlt);
    } else {
      M_monthly = pos(vmonthly_avg * PVJ * env_juv);
    }
    
    // --- Natural death time (exponential from per-thread RNG) ---
    const double tN = local_rng.exponential(M_monthly);
    
    // --- Skip encounter for juveniles ---
    bool juv_skip_enc;
    if (Fagemode) juv_skip_enc = (age_end < age_recruit);
    else juv_skip_enc = (!isAdult && L0i < juv_onlyM_len);
    
    if (lambdaE <= 1e-9 || juv_skip_enc) {
      if (tN < month_duration) {
        pool.alive[i] = 0;  // 方案 B: mark dead in-place
        acc.deaths++;
      }
      continue;
    }
    
    // --- Encounter loop ---
    double t_curr = 0.0;
    bool survived = true;
    int enc_count = 0;
    
    while (t_curr < month_duration && enc_count < max_enc) {
      double gap = local_rng.exponential(lambdaE);
      double t_next_enc = t_curr + gap;
      
      if (tN < t_next_enc && tN < month_duration) { survived = false; break; }
      if (t_next_enc >= month_duration) break;
      
      enc_count++;
      acc.enc_total++;
      
      // PSD classification
      if (L0i >= psd_quality) {
        if (L0i < psd_preferred) acc.enc_Q++;
        else if (L0i < psd_memorable) acc.enc_P++;
        else if (L0i < psd_trophy) acc.enc_M++;
        else acc.enc_T++;
      }
      
      // Keep probability with compliance
      double kp = keep_prob_with_compliance(L0i, min_len, max_len, p_max, L50, slope,
                                            comp_mode, comp_thresholds, comp_probs);
      if (Fagemode) { if (age_end < age_recruit) kp = 0.0; }
      else { if (L0i < juv_onlyM_len) kp = 0.0; }
      
      if (local_rng.uniform() < kp) { acc.yield_n++; survived = false; break; }
      if (release_mortality > 0.0 && local_rng.uniform() < release_mortality) { survived = false; break; }
      
      t_curr = t_next_enc;
    }
    
    if (!survived || tN < month_duration) {
      pool.alive[i] = 0;
      acc.deaths++;
    }
  }
} // end omp parallel

// =========================================================================
// Merge per-thread accumulators (main thread only)
// =========================================================================
int total_deaths = 0;
for (int t = 0; t < actual_threads; ++t) {
  results.yield_n     += thread_acc[t].yield_n;
  results.enc_n_total += thread_acc[t].enc_total;
  results.enc_n_Q     += thread_acc[t].enc_Q;
  results.enc_n_P     += thread_acc[t].enc_P;
  results.enc_n_M     += thread_acc[t].enc_M;
  results.enc_n_T     += thread_acc[t].enc_T;
  total_deaths        += thread_acc[t].deaths;
}
pool.n_alive -= total_deaths;
if (total_deaths > 0) {
  pool.next_dead_hint = 0;
}
return results;
}


// =============================================================================
// Adapted growth function for FishPool (方案 B)
// Operates in-place on alive fish only
// =============================================================================

inline void growthf_pool(FishPool& pool,
                         double g1_d_avg, double g1_a, double g1_b, double g1_c,
                         double g2_d_avg, double g2_a, double g2_b, double g2_c,
                         double lake_area_ha, double min_adult_age,
                         double extra_juvN = 0.0) {
  if (pool.n_alive == 0) return;
  
  const double dt = 1.0 / 12.0;
  
  // Count juveniles and adults
  int n_juv = 0, n_ad = 0;
  for (int i = 0; i < pool.capacity; ++i) {
    if (!pool.alive[i]) continue;
    if (pool.data(i, 1) < min_adult_age) n_juv++; else n_ad++;
  }
  
  double dens_juv = (lake_area_ha > 1e-6) ? ((n_juv + std::max(0.0, extra_juvN)) / lake_area_ha) : 0.0;
  double dens_ad  = (lake_area_ha > 1e-6) ? (n_ad  / lake_area_ha) : 0.0;
  double PD_juv = (g1_d_avg > 1e-6) ? (dens_juv / g1_d_avg) : 0.0;
  double PG_juv = g1_a + g1_b * std::exp(-g1_c * PD_juv);
  double PD_ad  = (g2_d_avg > 1e-6) ? (dens_ad  / g2_d_avg) : 0.0;
  double PG_ad  = g2_a + g2_b * std::exp(-g2_c * PD_ad);
  
  for (int i = 0; i < pool.capacity; ++i) {
    if (!pool.alive[i]) continue;
    
    double age  = pool.data(i, 1);
    double k    = pool.data(i, 2);
    double Linf = pool.data(i, 3);
    double t0   = pool.data(i, 4);
    
    double PG = (age < min_adult_age) ? PG_juv : PG_ad;
    double base = Linf * std::exp(-k * (age - t0));
    double inc  = 1.0 - std::exp(-k * dt);
    
    pool.data(i, 0) += PG * base * inc;  // length update
    pool.data(i, 1) += dt;               // age update
  }
}


// =============================================================================
// Adapted stats collection for FishPool
// =============================================================================

inline void collect_stats_pool(const FishPool& pool,
                               MonthlyStats& stats,
                               double lake_area_ha,
                               double min_adult_age, double age_recruit, double age_spawn,
                               double psd_stock, double psd_quality, double psd_preferred,
                               double psd_memorable, double psd_trophy) {
  
  double AdultN = 0, AgeFRN = 0, Spawners = 0, Larvae = 0;
  int n_stock = 0, n_q = 0, n_pref = 0, n_m = 0, n_t = 0;
  double max_age = 0;
  
  for (int i = 0; i < pool.capacity; ++i) {
    if (!pool.alive[i]) continue;
    double age = pool.data(i, 1);
    double len = pool.data(i, 0);
    
    if (age >= min_adult_age) AdultN++;
    if (age >= age_recruit && age < (1 + age_recruit)) AgeFRN++;
    if (age >= age_spawn) Spawners++;
    if (age < 1.0) Larvae++;
    if (age > max_age) max_age = age;
    
    if (len >= psd_stock) n_stock++;
    if (len >= psd_quality) n_q++;
    if (len >= psd_preferred) n_pref++;
    if (len >= psd_memorable) n_m++;
    if (len >= psd_trophy) n_t++;
  }
  
  stats.AdultN = static_cast<int>(AdultN);
  stats.AgeFRN = static_cast<int>(AgeFRN);
  stats.Sden = (lake_area_ha > 1e-6) ? (Spawners / lake_area_ha) : 0.0;
  stats.Rden = (lake_area_ha > 1e-6) ? (Larvae / lake_area_ha) : 0.0;
  stats.N_pop = pool.n_alive;
  stats.maxage = max_age;
  
  stats.PSD_Q = (n_stock > 0) ? 100.0 * n_q / n_stock : 0.0;
  stats.PSD_P = (n_stock > 0) ? 100.0 * n_pref / n_stock : 0.0;
  stats.PSD_M = (n_stock > 0) ? 100.0 * n_m / n_stock : 0.0;
  stats.PSD_T = (n_stock > 0) ? 100.0 * n_t / n_stock : 0.0;
}


// =============================================================================
// Adapted density calculation for FishPool
// =============================================================================

inline void calc_density_pool(const FishPool& pool, double lake_area_ha,
                              double min_adult_age,
                              double& juv_dens, double& ad_dens,
                              double extra_juvN = 0.0) {
  int n_juv = 0, n_ad = 0;
  for (int i = 0; i < pool.capacity; ++i) {
    if (!pool.alive[i]) continue;
    if (pool.data(i, 1) < min_adult_age) n_juv++; else n_ad++;
  }
  juv_dens = (lake_area_ha > 1e-6) ? ((n_juv + std::max(0.0, extra_juvN)) / lake_area_ha) : 0.0;
  ad_dens  = (lake_area_ha > 1e-6) ? (n_ad  / lake_area_ha) : 0.0;
}

// Count juveniles (age < min_adult_age) among alive fish in pool
inline double count_juveniles_pool(const FishPool& pool, double min_adult_age) {
  double n = 0;
  for (int i = 0; i < pool.capacity; ++i) {
    if (pool.alive[i] && pool.data(i, 1) < min_adult_age) n++;
  }
  return n;
}

// Count larvae (age < 1.0) among alive fish — for Rden stat
inline double count_larvae_pool(const FishPool& pool) {
  double n = 0;
  for (int i = 0; i < pool.capacity; ++i) {
    if (pool.alive[i] && pool.data(i, 1) < 1.0) n++;
  }
  return n;
}


// =============================================================================
// Adapted recruitment for FishPool
// Returns number of spawners (for S-R calculation)
// =============================================================================

inline double count_spawners_pool(const FishPool& pool, double age_spawn) {
  double count = 0;
  for (int i = 0; i < pool.capacity; ++i) {
    if (pool.alive[i] && pool.data(i, 1) >= age_spawn) count++;
  }
  return count;
}




std::vector<MonthlyStats> run_policy_phase_v2(
    const arma::mat& dd1_start, const arma::mat& pending_Rd1_start,
    double Rden_year_start, int Rall_year_start,
    SimulationRNG rng_copy, double env_mult_juv_start, double env_mult_adlt_start,
    const arma::mat& Theta_clean, const arma::vec& zr_w_dist_arma,
    const arma::vec& month_weights_arma, const arma::vec& prop_enc_month_vec,
    double h_p_max, double h_L50, double h_slope,
    double g1_d_avg, double g1_a, double g1_b, double g1_c,
    double g2_d_avg, double g2_a, double g2_b, double g2_c,
    double s_a, double s_b, double s_c, double s_d1, double s_d2,
    double vmonthly_avg, double lake_area_ha,
    double rec_a, double rec_b, double F_over_Z_ratio,
    double juv_onlyM_len,
    const std::vector<double>& comp_thresholds,
    const std::vector<double>& comp_probs,
    double psd_stock, double psd_quality, double psd_preferred, double psd_memorable, double psd_trophy,
    double min_adult_age,
    double age_recruit, double age_spawn,
    bool Fagemode,
    int spawn_month, int recruit_entry_month, double ESD,
    int policy_years, int before_policy_years,
    double prop_annual_encounters_val,
    double min_len_mm_val, double max_len_mm_val,
    int comp_mode_val, double release_mortality_val, int policy_combo_id,
    bool use_ricker,
    int omp_nthreads,
    const FastForwardManager& ff_mgr_start,
    int pending_ff_recruits_start) {
  
  std::vector<MonthlyStats> monthly_data;
  monthly_data.reserve(policy_years * 12);
  
  // Create FishPool from snapshot — extra capacity for recruitment
  int extra = static_cast<int>(rec_a * dd1_start.n_rows / std::max(1.0, lake_area_ha) * lake_area_ha * 1.5);
  if (extra < 10000) extra = 10000;
  FishPool pool = FishPool::from_mat(dd1_start, extra);
  
  arma::mat pending_Rd1 = pending_Rd1_start;
  double Rden_year = Rden_year_start;
  int Rall_year = Rall_year_start;
  double env_mult_juv = env_mult_juv_start;
  double env_mult_adlt = env_mult_adlt_start;
  
  // Each policy combo gets an independent copy of the before-policy cohort state.
  FastForwardManager ff_mgr = ff_mgr_start;
  int pending_ff_recruits = pending_ff_recruits_start;
  
  int start_month = before_policy_years * 12 + 1;
  int end_month = (before_policy_years + policy_years) * 12;
  
  for (int m = start_month; m <= end_month; ++m) {
    int year = (m - 1) / 12 + 1;
    int month_of_year = (m - 1) % 12 + 1;
    
    // Yearly environment stochasticity
    if (month_of_year == 1) {
      const double lo = 1.0 - 1.645 * ESD;
      const double hi = 1.0 + 1.645 * ESD;
      double envy = std::max(lo, std::min(rng_copy.normal(1.0, ESD), hi));
      env_mult_juv = envy;
      env_mult_adlt = envy;
      
      // Annual compaction — move alive fish to front for cache efficiency
      pool.compact();
    }
    
    // Density for this month's growth is evaluated BEFORE recruit-entry batch
    double ff_juv_before_growth = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
    
    // Count juveniles BEFORE growth runs. growthf_pool advances ages in place,
    // so a count taken afterwards would drop the fish that crossed
    // min_adult_age during this step. The matrix engine counts from its
    // untouched input matrix, and taking the count first keeps the two
    // engines in step.
    double juvN_growth = count_juveniles_pool(pool, min_adult_age) + ff_juv_before_growth;
    double juv_dens_growth = (lake_area_ha > 1e-6) ? (juvN_growth / lake_area_ha) : 0.0;
    double PD_juv_growth = (g1_d_avg > 1e-6) ? (juv_dens_growth / g1_d_avg) : 0.0;
    double PG_juv_growth = g1_a + g1_b * std::exp(-g1_c * PD_juv_growth);
    
    // Growth (in-place on FishPool)
    growthf_pool(pool, g1_d_avg, g1_a, g1_b, g1_c,
                 g2_d_avg, g2_a, g2_b, g2_c, lake_area_ha, min_adult_age,
                 ff_juv_before_growth);
    
    // Recruitment — spawning
    if (month_of_year == spawn_month) {
      double Spawners = count_spawners_pool(pool, age_spawn);
      double Sden_spawn = Spawners / lake_area_ha;
      if (use_ricker) {
        Rden_year = rec_a * Sden_spawn * std::exp(-rec_b * Sden_spawn);
      } else {
        Rden_year = rec_a * Sden_spawn / (1.0 + rec_b * Sden_spawn);
      }
      if (Rden_year < 0.0 || !R_finite(Rden_year)) Rden_year = 0.0;
      Rall_year = static_cast<int>(std::floor(Rden_year * lake_area_ha));
      
      if (Rall_year > 0) {
        if (ff_mgr.enabled()) {
          pending_ff_recruits = Rall_year;
          pending_Rd1.set_size(0, 6);
        } else {
          pending_Rd1 = recruits_init_cpp_arma(Theta_clean, zr_w_dist_arma,
                                               Rall_year, F_over_Z_ratio, rng_copy);
          pending_ff_recruits = 0;
        }
      } else {
        pending_Rd1.set_size(0, 6);
        pending_ff_recruits = 0;
      }
    }
    
    // Recruitment — entry into pool
    if (month_of_year == recruit_entry_month) {
      if (ff_mgr.enabled() && pending_ff_recruits > 0) {
        ff_mgr.add_batch(static_cast<double>(pending_ff_recruits));
        pending_ff_recruits = 0;
      } else if (pending_Rd1.n_rows > 0) {
        arma::mat Rd1real = growthf0_arma(pending_Rd1);
        pool.ensure_capacity(pool.n_alive + Rd1real.n_rows + 1000);
        pool.add_recruits(Rd1real);
        pending_Rd1.set_size(0, 6);
      }
    }
    
    // Density for survival (includes batch juveniles)
    double juv_dens, ad_dens;
    calc_density_pool(pool, lake_area_ha, min_adult_age, juv_dens, ad_dens,
                      ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0);
    double PV_juv   = s_a + s_b * (1.0 - std::exp(-s_c * (juv_dens / s_d1)));
    double PV_adult = s_a + s_b * (1.0 - std::exp(-s_c * (ad_dens  / s_d2)));
    
    // Update fast-forward batches; graduates added after survival
    arma::mat ff_graduates(0, 6);
    if (ff_mgr.enabled() && !ff_mgr.batches.empty()) {
      auto pos_ff = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
      double M_juv_monthly = pos_ff(vmonthly_avg * pos_ff(PV_juv,1e-9) * pos_ff(env_mult_juv,1e-9));
      ff_graduates = ff_mgr.update_and_graduate(
        M_juv_monthly, PG_juv_growth, Theta_clean, zr_w_dist_arma, F_over_Z_ratio, rng_copy);
    }
    
    double prop_enc_month = prop_enc_month_vec(month_of_year - 1);
    
    // Survival V2 (OpenMP + alive marking)
    SurvivalResults fishing_results = survival_ibm_v2(
      pool, omp_nthreads,
      prop_enc_month, min_len_mm_val, max_len_mm_val,
      h_p_max, h_L50, h_slope, comp_mode_val,
      comp_thresholds, comp_probs,
      PV_adult, PV_juv, vmonthly_avg, env_mult_juv, env_mult_adlt,
      juv_onlyM_len, release_mortality_val, 50,
      psd_quality, psd_preferred, psd_memorable, psd_trophy,
      min_adult_age, Fagemode, age_recruit,
      rng_copy);
    
    // Merge graduated fast-forward fish AFTER survival
    if (ff_graduates.n_rows > 0) {
      pool.ensure_capacity(pool.n_alive + ff_graduates.n_rows + 1000);
      pool.add_recruits(ff_graduates);
    }
    
    // Encounter stats
    double prop_Q = 0, prop_P = 0, prop_M = 0, prop_T = 0;
    bool any_trophy = false;
    if (fishing_results.enc_n_total > 0) {
      prop_Q = 100.0 * fishing_results.enc_n_Q / fishing_results.enc_n_total;
      prop_P = 100.0 * fishing_results.enc_n_P / fishing_results.enc_n_total;
      prop_M = 100.0 * fishing_results.enc_n_M / fishing_results.enc_n_total;
      prop_T = 100.0 * fishing_results.enc_n_T / fishing_results.enc_n_total;
      any_trophy = (fishing_results.enc_n_T > 0);
    }
    
    // Collect monthly stats from pool
    MonthlyStats stats;
    stats.year = year; stats.month = m; stats.month_of_year = month_of_year;
    stats.phase = "policy";
    stats.Yield_n = fishing_results.yield_n;
    
    collect_stats_pool(pool, stats, lake_area_ha, min_adult_age, age_recruit, age_spawn,
                       psd_stock, psd_quality, psd_preferred, psd_memorable, psd_trophy);
    
    // Add fast-forward batch juveniles to Rden and N_pop
    if (ff_mgr.enabled()) {
      double ff_juv = ff_mgr.total_juveniles();
      double larvae_total = count_larvae_pool(pool) + ff_mgr.total_below_age(1.0);
      stats.Rden = (lake_area_ha > 1e-6) ? (larvae_total / lake_area_ha) : 0.0;
      stats.AgeFRN += static_cast<int>(std::llround(
        ff_mgr.total_in_age_range(age_recruit, 1.0 + age_recruit)));
      stats.N_pop = static_cast<int>(pool.n_alive) + static_cast<int>(std::llround(ff_juv));
      stats.maxage = std::max(stats.maxage, ff_mgr.max_batch_age());
    }
    
    stats.Enc_Q = prop_Q; stats.Enc_P = prop_P; stats.Enc_M = prop_M; stats.Enc_T = prop_T;
    stats.trophy_seen = any_trophy;
    stats.prop_annual_encounters = prop_annual_encounters_val;
    stats.min_len_mm = min_len_mm_val;
    stats.max_len_mm = max_len_mm_val;
    stats.comp_mode = comp_mode_val;
    stats.release_mortality = release_mortality_val;
    stats.policy_combo_id = policy_combo_id;
    
    monthly_data.push_back(stats);
  }
  
  return monthly_data;
}


// =============================================================================
// Main simulation V2 — exported to R
//
// Same interface as run_simulation_sizelimit_cpp plus:
//   omp_nthreads: OpenMP threads for fish-level parallelism (1 = serial)
//   gpu_threads:  threads for combo-level parallelism (0 = serial)
// =============================================================================

//' @export
 // [[Rcpp::export]]
 List run_simulation_v2_cpp(
     NumericVector zr_w_dist, NumericVector month_weights,
     NumericMatrix W1_alk, NumericMatrix agedata,
     List harvest_params_in, List growth_params_dd_in1, List growth_params_dd_in2,
     List survival_params, List scenario_to_run,
     DataFrame policy_combos,
     DataFrame compliance_structure,
     int before_policy_years = 50, int policy_years = 30,
     double lake_area_ha = 2818.635, int initial_pop_size = 10000,
     double rec_a = 2.901, double rec_b = 134.78, double rec_v = 0.68,
     double F_over_Z_ratio = 0.5, double juv_onlyM_len = 130.0,
     int spawn_month = 4,
     int recruit_entry_month = 8, int rep = 1,
     double vmonthly_avg = 0.15,
     double min_adult_age = 1.0,
     double age_recruit = 1.0,
     double age_spawn = 1.0,
     double psd_stock = 130.0, double psd_quality = 200.0,
     double psd_preferred = 250.0, double psd_memorable = 300.0, double psd_trophy = 380.0,
     bool Fagemode = false,
     bool use_ricker = true,
     int omp_nthreads = 1,
     int gpu_threads = 0,
     int T_safe = 0) {
   validate_fastforward_age_window(T_safe, min_adult_age, age_spawn, Fagemode, age_recruit);
   if (omp_nthreads < 1) Rcpp::stop("omp_nthreads must be >= 1.");
   if (gpu_threads < 0) Rcpp::stop("gpu_threads must be >= 0.");
   
   // ==== Extract R → C++ (MAIN THREAD) ====
   
   NumericVector thres_nv = compliance_structure["Threshold_mm"];
   NumericVector probs_nv = compliance_structure["Probability"];
   std::vector<double> comp_thresholds = as<std::vector<double>>(thres_nv);
   std::vector<double> comp_probs      = as<std::vector<double>>(probs_nv);
   
   int    scenario_id              = scenario_to_run["scenario_id"];
   double prop_annual_encounters_val = scenario_to_run["prop_annual_encounters"];
   double ESD                      = scenario_to_run["ESD"];
   int    burnin_comp_mode         = scenario_to_run["burnin_comp_mode"];
   double burnin_release_mortality = scenario_to_run["burnin_release_mortality"];
   double min_len_mm_val           = scenario_to_run["min_len_mm"];
   double max_len_mm_val           = scenario_to_run["max_len_mm"];
   
   double h_p_max = harvest_params_in["p_max"];
   double h_L50   = harvest_params_in["L50"];
   double h_slope = harvest_params_in["slope"];
   
   double g1_a = growth_params_dd_in1["a"], g1_b = growth_params_dd_in1["b"];
   double g1_c = growth_params_dd_in1["c"], g1_d_avg = growth_params_dd_in1["d_avg"];
   double g2_a = growth_params_dd_in2["a"], g2_b = growth_params_dd_in2["b"];
   double g2_c = growth_params_dd_in2["c"], g2_d_avg = growth_params_dd_in2["d_avg"];
   
   double s_a = survival_params["a"], s_b = survival_params["b"];
   double s_c = survival_params["c"];
   double s_d1 = survival_params["d_avg1"], s_d2 = survival_params["d_avg2"];
   
   // Policy combos → STL
   IntegerVector pcid_r = policy_combos["policy_combo_id"];
   IntegerVector pcm_r  = policy_combos["comp_mode"];
   NumericVector prm_r  = policy_combos["release_mortality"];
   int n_combos = pcid_r.size();
   std::vector<int>    policy_combo_ids(n_combos), policy_comp_modes(n_combos);
   std::vector<double> policy_release_morts(n_combos);
   for (int i = 0; i < n_combos; ++i) {
     policy_combo_ids[i] = pcid_r[i];
     policy_comp_modes[i] = pcm_r[i];
     policy_release_morts[i] = prm_r[i];
   }
   
   arma::mat W1_alk_arma       = as<arma::mat>(W1_alk);
   arma::mat Theta_clean        = as<arma::mat>(agedata);
   arma::vec zr_w_dist_arma     = as<arma::vec>(zr_w_dist);
   arma::vec month_weights_arma = as<arma::vec>(month_weights);
   
   // ==== Seed & RNG ====
   unsigned int seed = static_cast<unsigned int>(
     rep * 123456 + scenario_id * 789);
   SimulationRNG rng(seed);
   
   // ==== Init population → FishPool ====
   arma::mat dd1_init;
   if (initial_pop_size > 0) {
     dd1_init = init_population_cpp_arma(W1_alk_arma, Theta_clean, zr_w_dist_arma,
                                         initial_pop_size, F_over_Z_ratio, rng);
   } else {
     dd1_init = arma::mat(0, 6);
   }
   
   // Estimate max population for FishPool capacity
   // Profiling showed populations can grow to ~1M from 10K initial
   int estimated_max = std::max(initial_pop_size * 3,
                                static_cast<int>(rec_a * initial_pop_size * 2));
   if (estimated_max < 50000) estimated_max = 50000;
   if (estimated_max > 5000000) estimated_max = 5000000; // initial reserve cap
   estimated_max = std::max(estimated_max, initial_pop_size);
   
   FishPool pool = FishPool::from_mat(dd1_init, estimated_max - initial_pop_size);
   
   // Encounter probabilities
   double wsum = arma::accu(month_weights_arma);
   double p_year = std::max(0.0, std::min(0.999999, prop_annual_encounters_val));
   double lambda_year = -std::log(1.0 - p_year);
   arma::vec rel_w = month_weights_arma / wsum;
   arma::vec lambda_month = lambda_year * rel_w;
   arma::vec prop_enc_month_vec = 1.0 - arma::exp(-lambda_month);
   for (arma::uword i = 0; i < prop_enc_month_vec.n_elem; ++i)
     prop_enc_month_vec[i] = std::max(0.0, std::min(0.999999, prop_enc_month_vec[i]));
   
   // ==== BEFORE-POLICY PHASE ====
   std::vector<MonthlyStats> before_policy_data;
   before_policy_data.reserve(before_policy_years * 12);
   
   double Rden_year = 0.0;
   int Rall_year = 0;
   arma::mat pending_Rd1 = arma::mat(0, 6);
   int pending_ff_recruits = 0;
   double env_mult_juv = 1.0, env_mult_adlt = 1.0;
   std::vector<double> dummy_thres, dummy_probs;
   
   FastForwardManager ff_mgr(T_safe);
   
   for (int m = 1; m <= before_policy_years * 12; ++m) {
     int year = (m - 1) / 12 + 1;
     int month_of_year = (m - 1) % 12 + 1;
     
     if (month_of_year == 1) {
       double lo = 1.0 - 1.645 * ESD, hi = 1.0 + 1.645 * ESD;
       double envy = std::max(lo, std::min(rng.normal(1.0, ESD), hi));
       env_mult_juv = envy; env_mult_adlt = envy;
       // Compact pool every year for cache efficiency
       pool.compact();
     }
     
     // Density for this month's growth is evaluated BEFORE recruit-entry batch
     double ff_juv_before_growth = ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0;
     
     // Count juveniles BEFORE growth runs. growthf_pool advances ages in place,
     // so a count taken afterwards would drop the fish that crossed
     // min_adult_age during this step. The matrix engine counts from its
     // untouched input matrix, and taking the count first keeps the two
     // engines in step.
     double juvN_growth = count_juveniles_pool(pool, min_adult_age) + ff_juv_before_growth;
     double juv_dens_growth = (lake_area_ha > 1e-6) ? (juvN_growth / lake_area_ha) : 0.0;
     double PD_juv_growth = (g1_d_avg > 1e-6) ? (juv_dens_growth / g1_d_avg) : 0.0;
     double PG_juv_growth = g1_a + g1_b * std::exp(-g1_c * PD_juv_growth);
     
     // Growth (in-place on pool)
     growthf_pool(pool, g1_d_avg, g1_a, g1_b, g1_c,
                  g2_d_avg, g2_a, g2_b, g2_c, lake_area_ha, min_adult_age,
                  ff_juv_before_growth);
     
     // Spawning
     if (month_of_year == spawn_month) {
       double Spawners = count_spawners_pool(pool, age_spawn);
       double Sden_spawn = Spawners / lake_area_ha;
       if (use_ricker)
         Rden_year = rec_a * Sden_spawn * std::exp(-rec_b * Sden_spawn);
       else
         Rden_year = rec_a * Sden_spawn / (1.0 + rec_b * Sden_spawn);
       if (Rden_year < 0.0 || !R_finite(Rden_year)) Rden_year = 0.0;
       Rall_year = static_cast<int>(std::floor(Rden_year * lake_area_ha));
       if (Rall_year > 0) {
         if (ff_mgr.enabled()) {
           pending_ff_recruits = Rall_year;
           pending_Rd1.set_size(0, 6);
         } else {
           pending_Rd1 = recruits_init_cpp_arma(Theta_clean, zr_w_dist_arma,
                                                Rall_year, F_over_Z_ratio, rng);
           pending_ff_recruits = 0;
         }
       } else {
         pending_Rd1.set_size(0, 6);
         pending_ff_recruits = 0;
       }
     }
     
     // Recruit entry
     if (month_of_year == recruit_entry_month) {
       if (ff_mgr.enabled() && pending_ff_recruits > 0) {
         ff_mgr.add_batch(static_cast<double>(pending_ff_recruits));
         pending_ff_recruits = 0;
       } else if (pending_Rd1.n_rows > 0) {
         arma::mat Rd1real = growthf0_arma(pending_Rd1);
         pool.ensure_capacity(pool.n_alive + Rd1real.n_rows + 1000);
         pool.add_recruits(Rd1real);
         pending_Rd1.set_size(0, 6);
       }
     }
     
     // Density & survival (includes batch juveniles in juv_dens)
     double juv_dens, ad_dens;
     calc_density_pool(pool, lake_area_ha, min_adult_age, juv_dens, ad_dens,
                       ff_mgr.enabled() ? ff_mgr.total_juveniles() : 0.0);
     double PV_juv   = s_a + s_b * (1.0 - std::exp(-s_c * (juv_dens / s_d1)));
     double PV_adult = s_a + s_b * (1.0 - std::exp(-s_c * (ad_dens  / s_d2)));
     double prop_enc_month = prop_enc_month_vec(month_of_year - 1);
     
     // Update fast-forward batches; graduates added to pool after survival
     arma::mat ff_graduates(0, 6);
     if (ff_mgr.enabled() && !ff_mgr.batches.empty()) {
       auto pos_ff = [](double x, double lo=1e-12){ return (R_finite(x) && x>lo) ? x : lo; };
       double M_juv_monthly = pos_ff(vmonthly_avg * pos_ff(PV_juv,1e-9) * pos_ff(env_mult_juv,1e-9));
       ff_graduates = ff_mgr.update_and_graduate(
         M_juv_monthly, PG_juv_growth, Theta_clean, zr_w_dist_arma, F_over_Z_ratio, rng);
     }
     
     // Survival V2 (OpenMP for Mode C/D, serial for Mode A/B)
     SurvivalResults fr = survival_ibm_v2(
       pool, omp_nthreads,
       prop_enc_month, 0.0, 1e9,     // before-policy: no size limits
       h_p_max, h_L50, h_slope, burnin_comp_mode,
       dummy_thres, dummy_probs,
       PV_adult, PV_juv, vmonthly_avg, env_mult_juv, env_mult_adlt,
       juv_onlyM_len, burnin_release_mortality, 50,
       psd_quality, psd_preferred, psd_memorable, psd_trophy,
       min_adult_age, Fagemode, age_recruit,
       rng);
     
     // Merge graduated fast-forward fish AFTER survival (they already completed
     // T_safe months of expected juvenile survival during fast-forward).
     if (ff_graduates.n_rows > 0) {
       pool.ensure_capacity(pool.n_alive + ff_graduates.n_rows + 1000);
       pool.add_recruits(ff_graduates);
     }
     
     // Collect stats
     MonthlyStats stats;
     stats.year = year; stats.month = m; stats.month_of_year = month_of_year;
     stats.phase = "before_policy";
     stats.Yield_n = fr.yield_n;
     
     collect_stats_pool(pool, stats, lake_area_ha, min_adult_age, age_recruit, age_spawn,
                        psd_stock, psd_quality, psd_preferred, psd_memorable, psd_trophy);
     
     // Add fast-forward batch juveniles to Rden and N_pop
     if (ff_mgr.enabled()) {
       double ff_juv = ff_mgr.total_juveniles();
       double larvae_total = count_larvae_pool(pool) + ff_mgr.total_below_age(1.0);
       stats.Rden = (lake_area_ha > 1e-6) ? (larvae_total / lake_area_ha) : 0.0;
       stats.AgeFRN += static_cast<int>(std::llround(
         ff_mgr.total_in_age_range(age_recruit, 1.0 + age_recruit)));
       stats.N_pop = static_cast<int>(pool.n_alive) + static_cast<int>(std::llround(ff_juv));
       stats.maxage = std::max(stats.maxage, ff_mgr.max_batch_age());
     }
     
     double prop_Q = 0, prop_P = 0, prop_M = 0, prop_T = 0;
     bool any_trophy = false;
     if (fr.enc_n_total > 0) {
       prop_Q = 100.0 * fr.enc_n_Q / fr.enc_n_total;
       prop_P = 100.0 * fr.enc_n_P / fr.enc_n_total;
       prop_M = 100.0 * fr.enc_n_M / fr.enc_n_total;
       prop_T = 100.0 * fr.enc_n_T / fr.enc_n_total;
       any_trophy = (fr.enc_n_T > 0);
     }
     stats.Enc_Q = prop_Q; stats.Enc_P = prop_P; stats.Enc_M = prop_M; stats.Enc_T = prop_T;
     stats.trophy_seen = any_trophy;
     stats.prop_annual_encounters = prop_annual_encounters_val;
     stats.min_len_mm = 0.0; stats.max_len_mm = 1e9;
     stats.comp_mode = burnin_comp_mode;
     stats.release_mortality = burnin_release_mortality;
     stats.policy_combo_id = 0;
     
     before_policy_data.push_back(stats);
   }
   
   // ==== SAVE STATE SNAPSHOT ====
   // Convert pool back to arma::mat for snapshot (policy phases need independent copies)
   arma::mat dd1_bp_end = pool.to_mat();
   arma::mat pending_Rd1_bp_end = pending_Rd1;
   double Rden_bp = Rden_year;
   int Rall_bp = Rall_year;
   double env_j_bp = env_mult_juv, env_a_bp = env_mult_adlt;
   FastForwardManager ff_mgr_bp = ff_mgr;
   int pending_ff_recruits_bp = pending_ff_recruits;
   std::mt19937 rng_state_bp = rng.get_state();
   
   // ==== POLICY PHASE ====
   // Combo-level parallelism via std::thread (same as run_simulation_gpu)
   int n_threads_combo = (gpu_threads > 0) ? std::min(gpu_threads, n_combos) : 1;
   if (n_combos <= 1) n_threads_combo = 1;
   
   std::vector<std::vector<MonthlyStats>> combo_results(n_combos);
   std::vector<std::string> combo_errors(n_combos);
   
   auto combo_worker = [&](int c) {
     try {
       SimulationRNG rng_c(0);
       rng_c.set_state(rng_state_bp);
       
       combo_results[c] = run_policy_phase_v2(
         dd1_bp_end, pending_Rd1_bp_end, Rden_bp, Rall_bp,
         rng_c, env_j_bp, env_a_bp,
         Theta_clean, zr_w_dist_arma, month_weights_arma, prop_enc_month_vec,
         h_p_max, h_L50, h_slope, g1_d_avg, g1_a, g1_b, g1_c,
         g2_d_avg, g2_a, g2_b, g2_c, s_a, s_b, s_c, s_d1, s_d2,
         vmonthly_avg, lake_area_ha, rec_a, rec_b, F_over_Z_ratio,
         juv_onlyM_len, comp_thresholds, comp_probs,
         psd_stock, psd_quality, psd_preferred, psd_memorable, psd_trophy,
         min_adult_age, age_recruit, age_spawn, Fagemode,
         spawn_month, recruit_entry_month, ESD,
         policy_years, before_policy_years, prop_annual_encounters_val,
         min_len_mm_val, max_len_mm_val,
         policy_comp_modes[c], policy_release_morts[c], policy_combo_ids[c],
                                                                        use_ricker, omp_nthreads, ff_mgr_bp, pending_ff_recruits_bp);
     } catch (std::exception& e) {
       combo_errors[c] = e.what();
     }
   };
   
   if (n_threads_combo <= 1) {
     for (int c = 0; c < n_combos; ++c) combo_worker(c);
   } else {
     for (int b = 0; b < n_combos; b += n_threads_combo) {
       int be = std::min(b + n_threads_combo, n_combos);
       std::vector<std::thread> thr;
       for (int c = b; c < be; ++c) thr.emplace_back(combo_worker, c);
       for (auto& t : thr) t.join();
     }
   }
   
   // ==== CONVERT RESULTS → R ====
   List result;
   result["before_policy"] = monthly_stats_to_dataframe(before_policy_data);
   
   for (int c = 0; c < n_combos; ++c) {
     if (!combo_errors[c].empty()) {
       Rcpp::warning("Policy combo %d error: %s",
                     policy_combo_ids[c], combo_errors[c].c_str());
       continue;
     }
     std::string key = "policy_" + std::to_string(policy_combo_ids[c]);
     result[key] = monthly_stats_to_dataframe(combo_results[c]);
   }
   
   return result;
 }
