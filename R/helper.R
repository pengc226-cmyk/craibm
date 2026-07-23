#' @useDynLib craibm, .registration = TRUE
#' @title Fish IBM Helper Functions
#' @description Helper functions for the craibm package, including VBGF bootstrapping, Z estimation, and batch simulation runners.
#' @importFrom Rcpp evalCpp
#' @keywords internal
"_PACKAGE"

# ==============================================================================
# Utilities
# ==============================================================================

#' @title Parse Numeric Vector from String
#' @description Parses a comma-separated string into a numeric vector. Handles fullwidth commas.
#' @param x Character string of comma-separated numbers.
#' @return Numeric vector (may be length 0).
#' @export
parse_num_vec <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) return(numeric())
  x <- gsub("\uFF0C", ",", x)
  v <- trimws(unlist(strsplit(x, ",")))
  v <- v[v != ""]
  out <- suppressWarnings(as.numeric(v))
  out[!is.na(out)]
}

# ==============================================================================
# VBGF Bootstrap
# ==============================================================================

#' @title Run VBGF Bootstrap
#' @description Fits the von Bertalanffy Growth Function (VBGF) using bootstrapping to estimate parameter uncertainty.
#' @param wd A dataframe containing the data. Must have columns named 'Length' and 'Age'.
#' @param B Integer. The number of bootstrap replicates to run. Default is 1000.
#' @param phi_obs Numeric. The observation error parameter. Default is 0.1.
#' @return A list containing 'Theta_clean' (bootstrapped parameters) and 'Data' (original data with fits).
#' @export
run_vbgf_bootstrap_full <- function(wd, B = 1000, phi_obs = 0.1, seed = NULL) {
  
  # The bootstrap resamples rows at random, so a seed is recorded to make the
  # fitted parameter distribution reproducible. When none is supplied one is
  # generated and returned alongside the result.
  if (is.null(seed) || !is.finite(suppressWarnings(as.numeric(seed)))) {
    seed <- sample.int(999999L, 1L)
  }
  seed <- as.integer(seed)
  set.seed(seed)
  
  # Ensure clean data
  wd <- wd %>%
    dplyr::select(Length, Age) %>%
    stats::na.omit()
  
  wd$AgeGrp <- wd$Age 
  
  # FSA helper functions
  vbT <- FSA::makeGrowthFun(type = "von Bertalanffy", simple = TRUE)
  start_vals <- FSA::findGrowthStarts(Length ~ Age, data = wd, type = "von Bertalanffy")
  
  # Initial Fit
  fit0 <- try(stats::nls(Length ~ vbT(Age, Linf, K, t0),
                  data = wd,
                  start = start_vals),
              silent = TRUE)
  
  if(inherits(fit0, "try-error")) return(NULL)
  
  wd$fit0 <- stats::predict(fit0)
  wd$res0 <- wd$Length - wd$fit0
  
  # Bootstrap function
  fit_func <- function(d) {
    tryCatch({
      fit <- stats::nls(Length ~ vbT(Age, Linf, K, t0), data = d, start = coef(fit0))
      coef(fit)
    }, error = function(e) return(c(Linf=NA, K=NA, t0=NA)))
  }
  
  # Manual loop
  Theta_mat <- matrix(NA, nrow = B, ncol = 3)
  colnames(Theta_mat) <- c("Linf", "K", "t0")
  
  for(i in 1:B) {
    idx <- sample(1:nrow(wd), replace = TRUE)
    boot_dat <- wd[idx, ]
    res <- fit_func(boot_dat)
    Theta_mat[i, ] <- res
  }
  
  Theta_df <- as.data.frame(Theta_mat)
  Theta_clean <- stats::na.omit(Theta_df)
  
  return(list(Theta_clean = Theta_clean, Data = wd, seed = seed))
}


# ==============================================================================
# Mortality (Z) Estimation
# ==============================================================================

#' @title Internal Z Estimation Logic
#' @description Core logic for Z estimation using car::Boot or lme4.
#' @param extd Data frame with extended age structure.
#' @param BG2 Number of bootstraps.
#' @param method Method string.
#' @return Bootstrapped Z values.
#' @keywords internal
zestimate1_shiny <- function(extd, BG2, method = c("lr","wlr","pois","ripois")) {
  
  if (any(method == "lr")) {
    cc <- stats::lm(log(extd[, 2]) ~ extd[, 1])
    bootD=car::Boot(cc,R=BG2)
    bootTYP2D=-bootD$t[,2]
  }
  
  if (any(method == "wlr")) {
    LRglm_out1 <- try(stats::lm(log(extd[, 2]) ~ extd[, 1]), 
                      silent = TRUE)
    if (!any(class(LRglm_out1) == "try-error")) {
      preds <- stats::predict(LRglm_out1,data=extd)
      
      preds[preds < 0] <- 0
      preds <- preds/sum(preds)
      assign("preds",preds,envir = globalenv())
      LRglm_out2 <- try(stats::lm(log(extd[, 2]) ~ extd[, 
                                                 1], weights = preds), silent = TRUE)
      if (!any(class(LRglm_out2) == "try-error")) {
        bootD<-car::Boot(LRglm_out2,R=BG2)
        bootTYP2D<--bootD$t[,2]
      }
    }
    if (any(class(LRglm_out1) == "try-error")) {
      bootTYP2D<-NA
    }
  }
  
  if (any(method == "pois")) {
    poiglm_out1 <- try(stats::glm(number ~ age, family = stats::poisson(link = log), 
                           data = extd, control = list(maxit = 1000)), silent = TRUE)
    if (!any(class(poiglm_out1) == "try-error")) {
      bootD<-car::Boot(poiglm_out1,R=BG2)
      bootTYP2D<--bootD$t[,2]
    }
  }
  
  if (any(method == "ripois")) {
    poiglm_out1 <- lme4::glmer(number ~ age+(1|age), family = poisson,
                               data = extd, control = lme4::glmerControl(optCtrl = list(maxfun=10000)))
    
    if (!any(class(poiglm_out1) == "try-error")) {
      bootD <- lmeresampler::bootstrap(poiglm_out1, .f = lme4::fixef, type = "residual", B = BG2)
      bootTYP2D<--bootD$replicates$age
    }
    if (any(class(poiglm_out1) == "try-error")) {
      bootTYP2D<-NA
    }
  }
  
  return(bootTYP2D)
  try( rm(list = "pred"),silent=TRUE)
}


#' @title Internal Z Data Prep
#' @description Prepares data for Z estimation.
#' @param age Vector of ages.
#' @param number Vector of counts.
#' @param full Recruitment age.
#' @param last Max age.
#' @param BG2 Bootstraps.
#' @param method Method.
#' @return Bootstrapped Z values.
#' @keywords internal
zestimate_shiny <- function(age, number, full, last, BG2, method) {
  
  st_obj <- data.frame(age = age, number = number)
  st_obj <- st_obj[!is.na(st_obj$age), ]
  
  amin <- min(st_obj[, 1])
  amax <- max(st_obj[, 1])
  agefreq <- data.frame(age = seq(amin, amax, 1))
  st_obj <- merge(agefreq, st_obj, by.x = "age", by.y = "age", 
                  all.x = TRUE, all.y = TRUE)
  names(st_obj)[2] <- "number"
  st_obj$number[is.na(st_obj$number)] <- 0
  if (is.null(last)) last <- max(age) else last <- last
  d <<- subset(st_obj, st_obj[, 1] >= full & st_obj[, 1] <= 
                 last)
  names(d) <- c("age", "number")
  if (d[1, 1] != full|!full>0)  stop("recruit age should be an interger and larger than 0.")
  if (length(d[, 1]) <= 2) {
    stop(paste("Data just have", length(d[, 1]), "ages after fully-recruited"))
  }                                         
  
  if (any(method == "lr")){extd <- d[d[, 2] > 0, ]}
  if (any(method == "wlr")){extd <- d[d[, 2] > 0, ]}
  if (any(method == "pois")){ max.age <- max(d[, 1]); extd <- rbind(d, cbind(age = (max.age + 1):(3 * max.age), 
                                                                             number = rep(0, max.age)))}
  if (any(method == "ripois")){ max.age <- max(d[, 1]); extd <- rbind(d, cbind(age = (max.age + 1):(3 * max.age), 
                                                                               number = rep(0, max.age)))}
  
  assign("extd",extd,envir = globalenv())
  bootTYP2D<-zestimate1_shiny(extd,BG2,method)
  try( rm(list = "extd", envir = globalenv()),silent=TRUE)
  
  return(bootTYP2D)
}


#' @title Run Z Bootstrap (Custom)
#' @description Wrapper function to estimate Z using catch curves or GLMs with bootstrapping.
#' @param raw_data Dataframe with 'Age' and 'n' columns.
#' @param BG2 Integer. Number of bootstrap replicates.
#' @param full Integer. Fully recruited age.
#' @param last Integer. Maximum age.
#' @param method Character. "lr", "wlr", "pois", or "ripois".
#' @return Numeric vector of bootstrapped Z estimates.
#' @export
run_z_bootstrap_custom <- function(raw_data, BG2, full, last, method, seed = NULL) {
  stopifnot(is.data.frame(raw_data))
  if (!all(c("Age","n") %in% names(raw_data))) {
    stop("ALK data must include columns: Age, n")
  }
  
  # The mortality bootstrap draws random resamples. Seeding here makes the
  # resulting Z distribution reproducible. The seed is supplied by the caller,
  # which also records it, so the return value is unchanged.
  if (!is.null(seed) && is.finite(suppressWarnings(as.numeric(seed)))) {
    set.seed(as.integer(seed))
  }
  
  age=raw_data$Age
  number=raw_data$n
  
  res<-zestimate_shiny(
    age    = age,
    number = number,
    full   = full,
    last   = last,
    BG2    = BG2,
    method = method
  )
  return(res)
}

# ==============================================================================
# Batch Run Function (Fixed Signature)
# ==============================================================================

# Internal C++ simulation dispatcher shared by foreground/background workers.
# Selects the legacy matrix engine or the FishPool V2 engine from params$execution.
.run_cpp_simulation_dispatch <- function(params,
                                         data_pack,
                                         compliance_structure,
                                         scenario_params,
                                         policy_combos,
                                         rep,
                                         engine_override = NULL,
                                         omp_threads_override = NULL,
                                         combo_threads_override = NULL) {
  value_or <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) default else x[[1]]
  }
  
  execution <- params$execution
  if (is.null(execution)) execution <- list()
  
  engine <- as.character(value_or(engine_override,
                                  value_or(execution$engine,
                                           value_or(params$other$simulation_engine, "legacy"))))
  omp_threads <- as.integer(value_or(omp_threads_override,
                                     value_or(execution$omp_nthreads,
                                              value_or(params$other$omp_nthreads, 1L))))
  combo_threads <- as.integer(value_or(combo_threads_override,
                                       value_or(execution$combo_threads,
                                                value_or(params$other$combo_threads, 0L))))
  omp_threads <- max(1L, omp_threads)
  combo_threads <- max(0L, combo_threads)
  
  is_age_mode <- identical(params$other$f_age_mode, "age")
  juv_len_val <- value_or(params$other$juv_onlyM_len, params$other$psd_stock)
  t_safe <- as.integer(value_or(params$other$T_safe, 0L))
  seed_base <- as.integer(value_or(params$seed, 1L))
  rep_seed <- seed_base + as.integer(rep) - 1L
  
  common_args <- list(
    zr_w_dist            = data_pack$zr_vec,
    month_weights        = params$month_weights,
    W1_alk               = data_pack$W1_mat,
    agedata              = data_pack$Theta_mat,
    harvest_params_in    = params$harvest,
    growth_params_dd_in1 = params$growth_1,
    growth_params_dd_in2 = params$growth_2,
    survival_params      = params$survival,
    scenario_to_run      = scenario_params,
    policy_combos        = policy_combos,
    compliance_structure = compliance_structure,
    before_policy_years  = params$before_policy_years,
    policy_years         = params$policy_years,
    lake_area_ha         = params$other$lake_area_ha,
    initial_pop_size     = params$other$initial_pop_size,
    rec_a                = params$other$rec_a,
    rec_b                = params$other$rec_b,
    rec_v                = params$other$rec_v,
    F_over_Z_ratio       = params$other$F_over_Z_ratio,
    juv_onlyM_len        = juv_len_val,
    spawn_month          = params$other$spawn_month,
    recruit_entry_month  = params$other$recruit_entry_month,
    rep                  = rep_seed,
    vmonthly_avg         = params$other$vmonthly_avg,
    min_adult_age        = params$other$min_adult_age,
    age_recruit          = params$other$age_recruit,
    age_spawn            = params$other$age_spawn,
    psd_stock            = params$other$psd_stock,
    psd_quality          = params$other$psd_quality,
    psd_preferred        = params$other$psd_preferred,
    psd_memorable        = params$other$psd_memorable,
    psd_trophy           = params$other$psd_trophy,
    Fagemode             = is_age_mode,
    use_ricker           = isTRUE(params$other$use_ricker),
    T_safe               = t_safe
  )
  
  craibm_ns <- asNamespace("craibm")
  
  get_craibm_fun <- function(name) {
    get0(
      name,
      envir = craibm_ns,
      mode = "function",
      inherits = FALSE,
      ifnotfound = NULL
    )
  }
  
  # Large-population optimized engine
  if (identical(engine, "v2")) {
    
    sim_fun <- get_craibm_fun(
      "run_simulation_v2_cpp"
    )
    
    if (is.null(sim_fun)) {
      stop(
        paste0(
          "The large-population optimized method is selected, but ",
          "run_simulation_v2_cpp is unavailable in the installed craibm build."
        )
      )
    }
    
    common_args$omp_nthreads <- omp_threads
    common_args$gpu_threads <- combo_threads
    
    return(
      do.call(sim_fun, common_args)
    )
  }
  
  # Standard engine with policy parallelism
  if (combo_threads > 0L) {
    
    sim_fun <- get_craibm_fun(
      "run_simulation_gpu"
    )
    
    if (is.null(sim_fun)) {
      stop(
        paste0(
          "Policy parallelism is enabled, but the policy-parallel ",
          "simulation function is unavailable in the installed craibm build."
        )
      )
    }
    
    common_args$gpu_threads <- combo_threads
    
    return(
      do.call(sim_fun, common_args)
    )
  }
  
  # Standard sequential engine
  sim_fun <- get_craibm_fun(
    "run_simulation_sizelimit_cpp"
  )
  
  if (is.null(sim_fun)) {
    stop(
      paste0(
        "The standard simulation function run_simulation_sizelimit_cpp ",
        "is unavailable in the installed craibm build."
      )
    )
  }
  
  do.call(sim_fun, common_args)
}


#' @title Run Whole Scenario Job (Shiny Worker)
#' @description Worker function to run a single scenario (with multiple policies) in parallel.
#'   The C++ simulation functions are loaded automatically via \code{library(craibm)} since
#'   \code{miaov6s.cpp} is compiled into the package shared library at install time.
#' @param task_info List. Contains sidx, n_iter, burnin_rm_val.
#' @param scenarios_df Dataframe. All scenario configurations.
#' @param policy_combos_logic Dataframe. Policy combinations.
#' @param all_params List. Contains params (global), data_pack, compliance_structure.
#' @param out_dir_base String. Base output directory.
#' @param cpp_abs_path String. Deprecated - no longer used. Kept for call-site compatibility only.
#' @return Boolean TRUE if successful.
#' @export
run_whole_scenario_job_shiny <- function(task_info, 
                                         scenarios_df, 
                                         policy_combos_logic, 
                                         all_params, 
                                         out_dir_base, 
                                         cpp_abs_path) {
  
  sidx          <- task_info$sidx
  iter_i        <- task_info$iter_i
  burnin_rm_val <- task_info$burnin_rm_val
  
  params               <- all_params$params
  data_pack            <- all_params$data_pack
  compliance_structure <- all_params$compliance_structure
  
  robust_op <- function(expr, attempts = 5, wait_time = 2) {
    for (i in seq_len(attempts)) {
      res <- tryCatch({ force(expr); TRUE }, error = function(e) FALSE)
      if (res) return(TRUE)
      Sys.sleep(wait_time + runif(1, 0, 2))
    }
    stop("Operation failed after multiple attempts")
  }
  
  tryCatch({
    #library(craibm)
    #library(data.table)
    #library(dplyr)
    
    # The dispatcher validates the selected compiled engine.
    
    scen_row   <- scenarios_df[sidx, ]
    clean_name <- as.character(scen_row$run_label)
    
    if (length(out_dir_base) == 0 || is.na(out_dir_base))
      stop("Output directory base is empty")
    
    scenario_dir <- file.path(out_dir_base, clean_name)
    robust_op(if (!dir.exists(scenario_dir)) dir.create(scenario_dir, recursive = TRUE))
    
    current_policy_df <- policy_combos_logic |>
      dplyr::rowwise() |>
      dplyr::mutate(
        release_mortality = if (use_scenario_rm) {
          as.numeric(scen_row$release_mortality)
        } else {
          0.0
        }
      ) |>
      dplyr::select(
        policy_combo_id,
        comp_mode,
        release_mortality
      ) |>
      dplyr::ungroup() |>
      as.data.frame()
    
    # Write scenario metadata once (guarded — other workers may race here)
    if (!file.exists(file.path(scenario_dir, "scenario_info.csv"))) {
      robust_op(data.table::fwrite(scen_row, file.path(scenario_dir, "scenario_info.csv")))
      robust_op(data.table::fwrite(current_policy_df, file.path(scenario_dir, "policy_combos_info.csv")))
    }
    
    execution <- params$execution
    if (is.null(execution)) execution <- list()
    execution_info <- data.frame(
      engine = if (is.null(execution$engine)) "legacy" else as.character(execution$engine),
      T_safe = if (is.null(params$other$T_safe)) 0L else as.integer(params$other$T_safe),
      omp_nthreads = if (is.null(execution$omp_nthreads)) 1L else as.integer(execution$omp_nthreads),
      combo_threads = if (is.null(execution$combo_threads)) 0L else as.integer(execution$combo_threads),
      seed = if (is.null(params$seed)) 1L else as.integer(params$seed),
      stringsAsFactors = FALSE
    )
    execution_file <- file.path(scenario_dir, "execution_info.csv")
    if (!file.exists(execution_file)) {
      robust_op(data.table::fwrite(execution_info, execution_file))
    }
    
    # Skip if this specific iteration already exists
    check_file <- file.path(scenario_dir, sprintf("iter%04d_before_policy.csv", iter_i))
    if (file.exists(check_file)) return(TRUE)
    
    cpp_scenario_params <- list(
      scenario_id              = as.numeric(scen_row$scenario_id),
      scenario_name            = as.character(scen_row$scenario_name),
      prop_annual_encounters   = as.numeric(scen_row$prop_annual_encounters),
      ESD                      = as.numeric(scen_row$ESD),
      burnin_comp_mode         = 0L,
      burnin_release_mortality = as.numeric(burnin_rm_val),
      min_len_mm               = as.numeric(scen_row$min_len_mm),
      max_len_mm               = as.numeric(scen_row$max_len_mm)
    )
    
    tryCatch({
      ts_list <- .run_cpp_simulation_dispatch(
        params               = params,
        data_pack            = data_pack,
        compliance_structure = compliance_structure,
        scenario_params      = cpp_scenario_params,
        policy_combos        = current_policy_df,
        rep                  = iter_i
      )
      
      for (key in names(ts_list)) {
        df_out              <- ts_list[[key]]
        df_out$iteration    <- iter_i
        df_out$scenario_id  <- sidx
        fn                  <- sprintf("iter%04d_%s.csv", iter_i, key)
        robust_op(data.table::fwrite(df_out, file.path(scenario_dir, fn), row.names = FALSE))
      }
      
      gc()
      
    }, error = function(e) {
      try(
        cat(paste0("Error in iter ", iter_i, " scen ", sidx, ": ", e$message, "\n"),
            file = file.path(scenario_dir, "error_log.txt"), append = TRUE),
        silent = TRUE
      )
      stop(e)
    })
    
    return(TRUE)
    
  }, error = function(e) {
    debug_file <- file.path(out_dir_base, "worker_init_error.txt")
    try(
      cat(paste0("Worker Failed (Scen ", sidx, " Iter ", iter_i, "): ", e$message, "\n"),
          file = debug_file, append = TRUE),
      silent = TRUE
    )
    stop(e)
  })
}


# ==============================================================================
# Result Calculation Helpers
# ==============================================================================

#' @title Calculate Burn-in Counts
#' @description Calculates statistics for burn-in periods in simulation results.
#' @param file_list A list or vector of file paths to process.
#' @param group_name String. The name of the group (e.g., "Burn-in").
#' @param var_name String. The variable name to summarize.
#' @param t_blue Integer. Start year for filtering (e.g., stable year start).
#' @param t_red Integer. End year for filtering.
#' @return A dataframe with Group, Mean, and SD.
#' @export
calc_burnin_counts <- function(
    file_list,
    group_name,
    t_blue = 0,
    t_red = 999
) {
  
  if (length(file_list) == 0L) {
    return(NULL)
  }
  
  counts_vec <- vapply(
    file_list,
    FUN.VALUE = numeric(1),
    FUN = function(f) {
      
      d <- data.table::fread(
        f,
        select = c("year", "trophy_seen")
      )
      
      # Explicit column extraction avoids data.table NSE problems
      # inside the package namespace.
      year_values <- suppressWarnings(
        as.numeric(d[["year"]])
      )
      
      trophy_values <- toupper(
        trimws(
          as.character(d[["trophy_seen"]])
        )
      )
      
      keep <- (
        is.finite(year_values) &
          year_values > t_blue &
          year_values <= t_red
      )
      
      # Supports logical TRUE/FALSE and numeric 1/0 output formats.
      sum(
        trophy_values[keep] %in% c(
          "TRUE",
          "T",
          "1"
        )
      )
    }
  )
  
  data.frame(
    Group = group_name,
    Mean = mean(counts_vec),
    SD = stats::sd(counts_vec),
    stringsAsFactors = FALSE
  )
}


#' @title Calculate Policy Counts
#' @description Calculates statistics for policy periods in simulation results.
#' @param file_list A list or vector of file paths.
#' @param group_name String. The group name.
#' @param var_name String. Variable name.
#' @param t_red Integer. Start year for policy filtering.
#' @return A dataframe with Group, Mean, and SD.
#' @export
calc_policy_counts <- function(
    file_list,
    group_name,
    t_red = 0
) {
  
  if (length(file_list) == 0L) {
    return(NULL)
  }
  
  counts_vec <- vapply(
    file_list,
    FUN.VALUE = numeric(1),
    FUN = function(f) {
      
      d <- data.table::fread(
        f,
        select = c("year", "trophy_seen")
      )
      
      year_values <- suppressWarnings(
        as.numeric(d[["year"]])
      )
      
      trophy_values <- toupper(
        trimws(
          as.character(d[["trophy_seen"]])
        )
      )
      
      keep <- (
        is.finite(year_values) &
          year_values > t_red
      )
      
      sum(
        trophy_values[keep] %in% c(
          "TRUE",
          "T",
          "1"
        )
      )
    }
  )
  
  data.frame(
    Group = group_name,
    Mean = mean(counts_vec),
    SD = stats::sd(counts_vec),
    stringsAsFactors = FALSE
  )
}




# ==============================================================================
# GPU Detection (R-level wrapper)
# ==============================================================================
#' @title Detect GPU and System Compute Capability
#' @description Multi-layer hardware detection. Attempts dedicated CLI tools first,
#'   then falls back to OS-level device enumeration that works without any
#'   GPU runtime installed. Always returns a usable recommendation.
#' @return A list with components:
#'   \describe{
#'     \item{gpu_available}{Logical. TRUE if any GPU hardware detected.}
#'     \item{gpu_name}{Character. GPU device name.}
#'     \item{gpu_platform}{Character. Platform/vendor.}
#'     \item{gpu_memory_mb}{Integer. GPU memory in MB (0 if unknown).}
#'     \item{gpu_compute_units}{Integer. Compute units (0 if unknown).}
#'     \item{gpu_type}{Character. "discrete", "integrated", or "none".}
#'     \item{backend}{Character. "GPU-Threads" or "CPU-Threads".}
#'     \item{cpu_cores_logical}{Integer. Total logical CPU cores.}
#'     \item{cpu_cores_physical}{Integer. Physical CPU cores (estimated).}
#'     \item{recommended_gpu_threads}{Integer. Suggested thread count.}
#'     \item{detection_method}{Character. Which layer succeeded.}
#'     \item{detection_details}{Character. Raw detection output for debugging.}
#'   }
#' @export
detect_gpu_r <- function() {
  
  # Initialize result structure
  result <- list(
    gpu_available           = FALSE,
    gpu_name                = "None",
    gpu_platform            = "None",
    gpu_memory_mb           = 0L,
    gpu_compute_units       = 0L,
    gpu_type                = "none",
    backend                 = "CPU-Threads",
    cpu_cores_logical       = max(1L, parallel::detectCores(logical = TRUE)),
    cpu_cores_physical      = max(1L, parallel::detectCores(logical = FALSE)),
    recommended_gpu_threads = 4L,
    detection_method        = "none",
    detection_details       = ""
  )
  
  # Fix NA from detectCores
  if (is.na(result$cpu_cores_logical))  result$cpu_cores_logical  <- 4L
  if (is.na(result$cpu_cores_physical)) result$cpu_cores_physical <- 2L
  
  # ========================================================================
  # Layer 1: C++ detect_gpu_info (compiled from miaov6s_gpu.cpp)
  # ========================================================================
  if (exists("detect_gpu_info", mode = "function")) {
    cpp_info <- tryCatch(detect_gpu_info(), error = function(e) NULL)
    if (!is.null(cpp_info) && isTRUE(cpp_info$gpu_available)) {
      result$gpu_available     <- TRUE
      result$gpu_name          <- cpp_info$gpu_name
      result$gpu_platform      <- cpp_info$gpu_platform
      result$gpu_memory_mb     <- as.integer(cpp_info$gpu_memory_mb)
      result$gpu_compute_units <- as.integer(cpp_info$gpu_compute_units)
      result$gpu_type          <- "discrete"
      result$backend           <- "GPU-Threads"
      result$detection_method  <- "C++ detect_gpu_info"
      return(.finalize_gpu_result(result))
    }
  }
  
  # ========================================================================
  # Layer 2: Dedicated CLI tools
  # ========================================================================
  
  # --- NVIDIA: nvidia-smi ---
  nv <- .try_nvidia_smi()
  if (nv$found) {
    result$gpu_available     <- TRUE
    result$gpu_name          <- nv$name
    result$gpu_platform      <- "NVIDIA CUDA"
    result$gpu_memory_mb     <- nv$memory_mb
    result$gpu_type          <- "discrete"
    result$backend           <- "GPU-Threads"
    result$detection_method  <- "nvidia-smi"
    result$detection_details <- nv$raw
    return(.finalize_gpu_result(result))
  }
  
  # --- AMD: rocm-smi ---
  amd <- .try_rocm_smi()
  if (amd$found) {
    result$gpu_available     <- TRUE
    result$gpu_name          <- amd$name
    result$gpu_platform      <- "AMD ROCm"
    result$gpu_memory_mb     <- amd$memory_mb
    result$gpu_type          <- "discrete"
    result$backend           <- "GPU-Threads"
    result$detection_method  <- "rocm-smi"
    result$detection_details <- amd$raw
    return(.finalize_gpu_result(result))
  }
  
  # --- Intel: intel_gpu_top (requires intel-gpu-tools) ---
  # Just check existence, don't actually run it (needs root usually)
  intel_cli <- .try_command_exists("intel_gpu_top")
  if (intel_cli) {
    # intel_gpu_top exists → Intel GPU with driver installed
    result$gpu_available     <- TRUE
    result$gpu_name          <- "Intel GPU (intel-gpu-tools detected)"
    result$gpu_platform      <- "Intel"
    result$gpu_type          <- "integrated"
    result$backend           <- "GPU-Threads"
    result$detection_method  <- "intel_gpu_top presence"
    # Don't return yet — try OS enumeration for better name
  }
  
  # ========================================================================
  # Layer 3: OS-level device enumeration (no runtime needed)
  # ========================================================================
  
  os_name <- Sys.info()[["sysname"]]
  
  if (os_name == "Windows") {
    win <- .detect_windows_gpu()
    if (win$found) {
      result$gpu_available     <- TRUE
      result$gpu_name          <- win$name
      result$gpu_platform      <- win$platform
      result$gpu_memory_mb     <- win$memory_mb
      result$gpu_type          <- win$gpu_type
      result$backend           <- "GPU-Threads"
      result$detection_method  <- paste0(result$detection_method, 
                                         if (nchar(result$detection_method) > 4) " + " else "",
                                         "Windows PowerShell")
      result$detection_details <- win$raw
      return(.finalize_gpu_result(result))
    }
    
  } else if (os_name == "Darwin") {
    mac <- .detect_macos_gpu()
    if (mac$found) {
      result$gpu_available     <- TRUE
      result$gpu_name          <- mac$name
      result$gpu_platform      <- mac$platform
      result$gpu_memory_mb     <- mac$memory_mb
      result$gpu_type          <- mac$gpu_type
      result$backend           <- "GPU-Threads"
      result$detection_method  <- "macOS system_profiler"
      result$detection_details <- mac$raw
      return(.finalize_gpu_result(result))
    }
    
  } else {
    # Linux
    linux <- .detect_linux_gpu()
    if (linux$found) {
      result$gpu_available     <- TRUE
      result$gpu_name          <- linux$name
      result$gpu_platform      <- linux$platform
      result$gpu_memory_mb     <- linux$memory_mb
      result$gpu_type          <- linux$gpu_type
      result$backend           <- "GPU-Threads"
      result$detection_method  <- paste0(result$detection_method, 
                                         if (nchar(result$detection_method) > 4) " + " else "",
                                         linux$method)
      result$detection_details <- linux$raw
      return(.finalize_gpu_result(result))
    }
  }
  
  # ========================================================================
  # Layer 4: OpenCL runtime probe (clinfo)
  # ========================================================================
  cl <- .try_clinfo()
  if (cl$found) {
    result$gpu_available     <- TRUE
    result$gpu_name          <- cl$name
    result$gpu_platform      <- cl$platform
    result$gpu_memory_mb     <- cl$memory_mb
    result$gpu_compute_units <- cl$compute_units
    result$gpu_type          <- cl$gpu_type
    result$backend           <- "GPU-Threads"
    result$detection_method  <- "clinfo (OpenCL)"
    result$detection_details <- cl$raw
    return(.finalize_gpu_result(result))
  }
  
  # ========================================================================
  # Layer 5: Nothing found — pure CPU assessment
  # ========================================================================
  if (intel_cli) {
    # We detected intel_gpu_top earlier but couldn't get device name
    return(.finalize_gpu_result(result))
  }
  
  result$detection_method <- "CPU-only (no GPU detected)"
  result$detection_details <- paste0(
    "Logical cores: ", result$cpu_cores_logical,
    ", Physical cores: ", result$cpu_cores_physical
  )
  
  .finalize_gpu_result(result)
}


# ==============================================================================
# Internal: Compute recommended thread count
# ==============================================================================
.finalize_gpu_result <- function(r) {
  
  if (r$gpu_available) {
    r$recommended_gpu_threads <- if (r$gpu_type == "discrete") {
      # Discrete GPU → system is likely powerful, be aggressive
      max(4L, r$cpu_cores_logical)
    } else {
      # Integrated GPU → shared resources with CPU, be conservative
      # Don't exceed physical cores (hyperthreads won't help compute-bound work)
      max(2L, r$cpu_cores_physical)
    }
  } else {
    # No GPU → recommend based on CPU only
    # Leave 1-2 cores for OS + Shiny
    r$recommended_gpu_threads <- max(1L, r$cpu_cores_physical - 1L)
  }
  
  r
}


# ==============================================================================
# Internal: NVIDIA detection via nvidia-smi
# ==============================================================================
.try_nvidia_smi <- function() {
  out <- list(found = FALSE, name = "", memory_mb = 0L, raw = "")
  
  txt <- tryCatch(
    system2("nvidia-smi",
            args = c("--query-gpu=name,memory.total,compute_cap",
                     "--format=csv,noheader,nounits"),
            stdout = TRUE, stderr = FALSE, timeout = 5),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (is.null(txt) || length(txt) == 0 || !nzchar(txt[1])) return(out)
  
  parts <- trimws(strsplit(txt[1], ",")[[1]])
  if (length(parts) >= 2) {
    out$found     <- TRUE
    out$name      <- parts[1]
    out$memory_mb <- suppressWarnings(as.integer(parts[2]))
    if (is.na(out$memory_mb)) out$memory_mb <- 0L
    out$raw       <- txt[1]
  }
  out
}


# ==============================================================================
# Internal: AMD detection via rocm-smi
# ==============================================================================
.try_rocm_smi <- function() {
  out <- list(found = FALSE, name = "", memory_mb = 0L, raw = "")
  
  txt <- tryCatch(
    system2("rocm-smi", args = "--showproductname",
            stdout = TRUE, stderr = FALSE, timeout = 5),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (is.null(txt) || length(txt) == 0) return(out)
  
  # Look for "Card Series" or "Card Model" line
  name_line <- grep("Card|GPU|Series|Model", txt, ignore.case = TRUE, value = TRUE)
  if (length(name_line) > 0) {
    out$found <- TRUE
    out$name  <- trimws(sub(".*:\\s*", "", name_line[1]))
    out$raw   <- paste(txt, collapse = "\n")
    
    # Try to get memory
    mem_txt <- tryCatch(
      system2("rocm-smi", args = "--showmeminfo vram",
              stdout = TRUE, stderr = FALSE, timeout = 5),
      error = function(e) NULL
    )
    if (!is.null(mem_txt)) {
      total_line <- grep("Total", mem_txt, ignore.case = TRUE, value = TRUE)
      if (length(total_line) > 0) {
        mem_val <- suppressWarnings(as.numeric(gsub("[^0-9]", "", total_line[1])))
        if (!is.na(mem_val) && mem_val > 0) {
          # rocm-smi reports in bytes usually
          out$memory_mb <- as.integer(mem_val / (1024 * 1024))
          if (out$memory_mb < 1) out$memory_mb <- as.integer(mem_val) # might already be MB
        }
      }
    }
  }
  out
}


# ==============================================================================
# Internal: Check if a command exists on PATH
# ==============================================================================
.try_command_exists <- function(cmd) {
  res <- tryCatch(
    system2("which", args = cmd, stdout = TRUE, stderr = FALSE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  !is.null(res) && length(res) > 0 && nchar(res[1]) > 0
}


# ==============================================================================
# Internal: Windows GPU detection via PowerShell
# No runtime needed — queries WMI directly from the OS driver layer
# ==============================================================================
.detect_windows_gpu <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", raw = "")
  
  # PowerShell command that works on all Windows 10/11 without any GPU SDK
  # Get-CimInstance queries the OS device manager, not the GPU runtime
  ps_cmd <- paste0(
    "Get-CimInstance -ClassName Win32_VideoController | ",
    "Select-Object Name, AdapterRAM, VideoProcessor, DriverVersion | ",
    "ConvertTo-Json"
  )
  
  txt <- tryCatch(
    system2("powershell", args = c("-NoProfile", "-Command", ps_cmd),
            stdout = TRUE, stderr = FALSE, timeout = 10),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (is.null(txt) || length(txt) == 0) {
    # Fallback: try wmic (deprecated but still works on older Windows)
    return(.detect_windows_gpu_wmic())
  }
  
  json_str <- paste(txt, collapse = "\n")
  
  parsed <- tryCatch(jsonlite::fromJSON(json_str, simplifyDataFrame = FALSE),
                     error = function(e) NULL)
  
  if (is.null(parsed)) {
    return(.detect_windows_gpu_wmic())
  }
  
  # Could be a single object or a list of objects
  if (!is.null(parsed$Name)) parsed <- list(parsed)
  
  # Find the best GPU (prefer discrete over integrated)
  best_gpu <- NULL
  best_type <- "none"
  best_mem <- 0
  
  for (gpu in parsed) {
    name <- if (!is.null(gpu$Name)) gpu$Name else ""
    if (nchar(name) == 0) next
    
    # Skip Microsoft Basic Display Adapter (virtual/no driver)
    if (grepl("Microsoft Basic", name, ignore.case = TRUE)) next
    
    # Classify
    this_type <- if (grepl("NVIDIA|GeForce|Quadro|Tesla|RTX|GTX", name, ignore.case = TRUE)) {
      "discrete"
    } else if (grepl("AMD|Radeon RX|Radeon Pro", name, ignore.case = TRUE) &&
               !grepl("Vega.*Graphics|Radeon.*Graphics$", name, ignore.case = TRUE)) {
      "discrete"
    } else {
      "integrated"
    }
    
    # AdapterRAM is in bytes
    mem_bytes <- suppressWarnings(as.numeric(gpu$AdapterRAM))
    mem_mb <- if (!is.na(mem_bytes) && mem_bytes > 0) as.integer(mem_bytes / (1024 * 1024)) else 0L
    
    # Prefer discrete > integrated, then by memory
    if (this_type == "discrete" && best_type != "discrete") {
      best_gpu <- gpu; best_type <- this_type; best_mem <- mem_mb
    } else if (this_type == best_type && mem_mb > best_mem) {
      best_gpu <- gpu; best_type <- this_type; best_mem <- mem_mb
    } else if (is.null(best_gpu)) {
      best_gpu <- gpu; best_type <- this_type; best_mem <- mem_mb
    }
  }
  
  if (!is.null(best_gpu)) {
    out$found     <- TRUE
    out$name      <- best_gpu$Name
    out$memory_mb <- best_mem
    out$gpu_type  <- best_type
    out$platform  <- if (grepl("NVIDIA", best_gpu$Name, ignore.case = TRUE)) "NVIDIA CUDA"
    else if (grepl("AMD|Radeon", best_gpu$Name, ignore.case = TRUE)) "AMD"
    else if (grepl("Intel", best_gpu$Name, ignore.case = TRUE)) "Intel"
    else "Unknown"
    out$raw       <- json_str
  }
  
  out
}


# Windows WMIC fallback
.detect_windows_gpu_wmic <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", raw = "")
  
  txt <- tryCatch(
    system2("wmic", args = c("path", "win32_videocontroller", "get",
                             "Name,AdapterRAM", "/format:csv"),
            stdout = TRUE, stderr = FALSE, timeout = 10),
    error = function(e) NULL
  )
  
  if (is.null(txt)) return(out)
  
  # Parse CSV output
  data_lines <- txt[nchar(txt) > 5 & !grepl("^Node,", txt)]
  for (line in data_lines) {
    parts <- strsplit(line, ",")[[1]]
    if (length(parts) >= 3) {
      mem_str <- trimws(parts[2])
      name_str <- trimws(parts[3])
      
      if (grepl("Microsoft Basic", name_str, ignore.case = TRUE)) next
      if (nchar(name_str) < 3) next
      
      out$found    <- TRUE
      out$name     <- name_str
      out$gpu_type <- if (grepl("NVIDIA|GeForce|RTX|GTX|Quadro|AMD|Radeon RX", name_str, ignore.case = TRUE)) {
        "discrete"
      } else {
        "integrated"
      }
      out$platform <- if (grepl("NVIDIA", name_str, ignore.case = TRUE)) "NVIDIA CUDA"
      else if (grepl("AMD|Radeon", name_str, ignore.case = TRUE)) "AMD"
      else if (grepl("Intel", name_str, ignore.case = TRUE)) "Intel"
      else "Unknown"
      mem_val <- suppressWarnings(as.numeric(mem_str))
      if (!is.na(mem_val) && mem_val > 0) {
        out$memory_mb <- as.integer(mem_val / (1024 * 1024))
      }
      out$raw <- line
      break
    }
  }
  out
}


# ==============================================================================
# Internal: macOS GPU detection via system_profiler
# Always works — no runtime needed, built into macOS
# ==============================================================================
.detect_macos_gpu <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", raw = "")
  
  txt <- tryCatch(
    system2("system_profiler", args = "SPDisplaysDataType",
            stdout = TRUE, stderr = FALSE, timeout = 10),
    error = function(e) NULL
  )
  
  if (is.null(txt) || length(txt) == 0) return(out)
  
  raw_all <- paste(txt, collapse = "\n")
  
  # Chipset/Chip line
  chip_line <- grep("Chipset Model|Chip", txt, value = TRUE)
  if (length(chip_line) == 0) return(out)
  
  name <- trimws(sub(".*:\\s*", "", chip_line[1]))
  if (nchar(name) < 2) return(out)
  
  out$found <- TRUE
  out$name  <- name
  out$raw   <- raw_all
  
  # Detect type
  if (grepl("Apple M[0-9]", name, ignore.case = TRUE)) {
    out$platform <- "Apple Metal"
    out$gpu_type <- "integrated"  # Apple Silicon is unified memory
    
    # Try to get total unified memory
    mem_line <- grep("Memory|Unified", txt, ignore.case = TRUE, value = TRUE)
    if (length(mem_line) > 0) {
      mem_num <- as.numeric(gsub("[^0-9.]", "", mem_line[1]))
      if (!is.na(mem_num) && mem_num > 0) {
        # system_profiler reports in GB usually
        out$memory_mb <- as.integer(if (mem_num < 128) mem_num * 1024 else mem_num)
      }
    }
    
    # Estimate GPU cores from chip model
    out$gpu_compute_units <- .estimate_apple_gpu_cores(name)
    
  } else if (grepl("AMD|Radeon", name, ignore.case = TRUE)) {
    out$platform <- "AMD"
    out$gpu_type <- "discrete"
    vram_line <- grep("VRAM", txt, value = TRUE)
    if (length(vram_line) > 0) {
      vram_num <- as.numeric(gsub("[^0-9.]", "", vram_line[1]))
      if (!is.na(vram_num)) {
        out$memory_mb <- as.integer(if (vram_num < 128) vram_num * 1024 else vram_num)
      }
    }
  } else if (grepl("Intel", name, ignore.case = TRUE)) {
    out$platform <- "Intel"
    out$gpu_type <- "integrated"
  } else {
    out$platform <- "Unknown"
    out$gpu_type <- "integrated"
  }
  
  out
}


# Heuristic: estimate Apple Silicon GPU core count from chip name
.estimate_apple_gpu_cores <- function(chip_name) {
  # M1: 7-8, M1 Pro: 14-16, M1 Max: 24-32, M1 Ultra: 48-64
  # M2: 8-10, M2 Pro: 16-19, M2 Max: 30-38, M2 Ultra: 60-76
  # M3: 8-10, M3 Pro: 14-18, M3 Max: 30-40, M3 Ultra: 60-80
  # M4: 10, M4 Pro: 16-20, M4 Max: 32-40
  chip <- toupper(chip_name)
  if (grepl("ULTRA", chip)) return(60L)
  if (grepl("MAX", chip))   return(32L)
  if (grepl("PRO", chip))   return(16L)
  if (grepl("M[1-9]", chip)) return(8L)
  return(0L)
}


# ==============================================================================
# Internal: Linux GPU detection via kernel interfaces
# These work WITHOUT any GPU runtime — they read from the kernel's device tree
# ==============================================================================
.detect_linux_gpu <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", method = "", raw = "")
  
  # --- Method A: /sys/class/drm (DRM subsystem, always present if GPU has driver) ---
  drm_result <- .linux_drm_detect()
  if (drm_result$found) {
    out <- drm_result
    return(out)
  }
  
  # --- Method B: lspci (PCI bus scan, works for any PCI device) ---
  lspci_result <- .linux_lspci_detect()
  if (lspci_result$found) {
    out <- lspci_result
    return(out)
  }
  
  # --- Method C: /proc/driver/nvidia or /sys/module/amdgpu ---
  proc_result <- .linux_proc_detect()
  if (proc_result$found) {
    out <- proc_result
    return(out)
  }
  
  out
}


# Linux DRM subsystem detection
.linux_drm_detect <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", method = "DRM /sys/class/drm", raw = "")
  
  drm_cards <- list.files("/sys/class/drm", pattern = "^card[0-9]+$", full.names = TRUE)
  
  for (card_path in drm_cards) {
    # Read device name from different sources
    name <- ""
    
    # Try: /sys/class/drm/card0/device/label (some drivers provide this)
    label_file <- file.path(card_path, "device", "label")
    if (file.exists(label_file)) {
      name <- tryCatch(trimws(readLines(label_file, n = 1, warn = FALSE)),
                       error = function(e) "")
    }
    
    # Try: PCI device/vendor from sysfs
    if (nchar(name) == 0) {
      vendor_file <- file.path(card_path, "device", "vendor")
      device_file <- file.path(card_path, "device", "device")
      if (file.exists(vendor_file) && file.exists(device_file)) {
        vendor_id <- tryCatch(trimws(readLines(vendor_file, n = 1, warn = FALSE)),
                              error = function(e) "")
        device_id <- tryCatch(trimws(readLines(device_file, n = 1, warn = FALSE)),
                              error = function(e) "")
        
        # Classify by PCI vendor ID
        # 0x10de = NVIDIA, 0x1002 = AMD/ATI, 0x8086 = Intel
        platform <- ""
        gpu_type <- "none"
        
        if (grepl("0x10de", vendor_id, ignore.case = TRUE)) {
          platform <- "NVIDIA CUDA"
          gpu_type <- "discrete"
          name <- paste0("NVIDIA GPU [PCI:", device_id, "]")
        } else if (grepl("0x1002", vendor_id, ignore.case = TRUE)) {
          platform <- "AMD"
          # Check if it's an integrated APU GPU by looking at boot_vga
          boot_vga_file <- file.path(card_path, "device", "boot_vga")
          is_primary <- tryCatch(trimws(readLines(boot_vga_file, n = 1, warn = FALSE)) == "1",
                                 error = function(e) TRUE)
          gpu_type <- "discrete"  # Default; hard to tell APU from sysfs alone
          name <- paste0("AMD GPU [PCI:", device_id, "]")
        } else if (grepl("0x8086", vendor_id, ignore.case = TRUE)) {
          platform <- "Intel"
          gpu_type <- "integrated"
          name <- paste0("Intel GPU [PCI:", device_id, "]")
        } else {
          next  # Unknown vendor, skip
        }
        
        out$found    <- TRUE
        out$name     <- name
        out$platform <- platform
        out$gpu_type <- gpu_type
        out$raw      <- paste0("vendor=", vendor_id, " device=", device_id)
        
        # Try to read memory from sysfs (NVIDIA and AMD sometimes expose this)
        mem_file <- file.path(card_path, "device", "mem_info_vram_total")
        if (file.exists(mem_file)) {
          mem_bytes <- tryCatch(
            as.numeric(trimws(readLines(mem_file, n = 1, warn = FALSE))),
            error = function(e) NA
          )
          if (!is.na(mem_bytes) && mem_bytes > 0) {
            out$memory_mb <- as.integer(mem_bytes / (1024 * 1024))
          }
        }
        
        # Prefer discrete GPU, so if this is discrete, return immediately
        if (gpu_type == "discrete") return(out)
      }
    }
  }
  
  out
}


# Linux lspci detection
.linux_lspci_detect <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", method = "lspci", raw = "")
  
  # -nn gives both text name and PCI IDs
  txt <- tryCatch(
    system2("lspci", args = "-nn", stdout = TRUE, stderr = FALSE, timeout = 5),
    error = function(e) NULL
  )
  
  if (is.null(txt) || length(txt) == 0) return(out)
  
  # Filter for VGA/3D/Display controllers
  gpu_lines <- grep("VGA|3D|Display controller", txt, ignore.case = TRUE, value = TRUE)
  if (length(gpu_lines) == 0) return(out)
  
  # Prefer discrete GPU
  best <- NULL
  best_type <- "none"
  
  for (line in gpu_lines) {
    name <- trimws(sub("^[0-9a-f:.]+\\s+[^:]+:\\s*", "", line))
    
    this_type <- if (grepl("NVIDIA|GeForce|RTX|GTX|Quadro|Tesla", line, ignore.case = TRUE)) {
      "discrete"
    } else if (grepl("Radeon RX|Radeon Pro|\\[AMD/ATI\\].*Navi|Vega.*\\[", line, ignore.case = TRUE) &&
               !grepl("Renoir|Cezanne|Barcelo|Phoenix|Hawk|Raphael.*Radeon", line, ignore.case = TRUE)) {
      "discrete"
    } else {
      "integrated"
    }
    
    platform <- if (grepl("NVIDIA", line, ignore.case = TRUE)) "NVIDIA CUDA"
    else if (grepl("AMD|ATI|Radeon", line, ignore.case = TRUE)) "AMD"
    else if (grepl("Intel", line, ignore.case = TRUE)) "Intel"
    else "Unknown"
    
    if (this_type == "discrete" && best_type != "discrete") {
      best <- list(name = name, platform = platform, type = this_type, raw = line)
      best_type <- this_type
    } else if (is.null(best)) {
      best <- list(name = name, platform = platform, type = this_type, raw = line)
      best_type <- this_type
    }
  }
  
  if (!is.null(best)) {
    out$found    <- TRUE
    out$name     <- best$name
    out$platform <- best$platform
    out$gpu_type <- best$type
    out$raw      <- best$raw
    out$method   <- "lspci -nn"
  }
  
  out
}


# Linux /proc and /sys/module detection
.linux_proc_detect <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              gpu_type = "none", method = "/proc + /sys/module", raw = "")
  
  if (dir.exists("/proc/driver/nvidia")) {
    out$found    <- TRUE
    out$name     <- "NVIDIA GPU (kernel module loaded)"
    out$platform <- "NVIDIA CUDA"
    out$gpu_type <- "discrete"
    out$method   <- "/proc/driver/nvidia"
    
    # Try to get version info
    ver_file <- "/proc/driver/nvidia/version"
    if (file.exists(ver_file)) {
      out$raw <- tryCatch(
        paste(readLines(ver_file, warn = FALSE), collapse = " "),
        error = function(e) ""
      )
    }
    return(out)
  }
  
  if (file.exists("/sys/module/amdgpu/version") || dir.exists("/sys/module/amdgpu")) {
    out$found    <- TRUE
    out$name     <- "AMD GPU (amdgpu kernel module loaded)"
    out$platform <- "AMD"
    out$gpu_type <- "discrete"
    out$method   <- "/sys/module/amdgpu"
    return(out)
  }
  
  if (dir.exists("/sys/module/i915")) {
    out$found    <- TRUE
    out$name     <- "Intel GPU (i915 kernel module loaded)"
    out$platform <- "Intel"
    out$gpu_type <- "integrated"
    out$method   <- "/sys/module/i915"
    return(out)
  }
  
  if (dir.exists("/sys/module/xe")) {
    out$found    <- TRUE
    out$name     <- "Intel GPU (Xe kernel module loaded)"
    out$platform <- "Intel"
    out$gpu_type <- "integrated"
    out$method   <- "/sys/module/xe"
    return(out)
  }
  
  out
}


# ==============================================================================
# Internal: OpenCL detection via clinfo
# ==============================================================================
.try_clinfo <- function() {
  out <- list(found = FALSE, name = "", platform = "", memory_mb = 0L,
              compute_units = 0L, gpu_type = "none", raw = "")
  
  txt <- tryCatch(
    system2("clinfo", args = "--list", stdout = TRUE, stderr = FALSE, timeout = 5),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (is.null(txt) || length(txt) == 0) return(out)
  
  # clinfo --list outputs: "Platform #0: ...\n  Device #0: ..."
  device_lines <- grep("Device", txt, ignore.case = TRUE, value = TRUE)
  if (length(device_lines) == 0) return(out)
  
  # Take first non-CPU device
  for (line in device_lines) {
    if (grepl("CPU", line, ignore.case = TRUE)) next
    
    name <- trimws(sub(".*:\\s*", "", line))
    if (nchar(name) < 3) next
    
    out$found <- TRUE
    out$name  <- name
    out$raw   <- paste(txt, collapse = "\n")
    
    out$platform <- if (grepl("NVIDIA", name, ignore.case = TRUE)) "NVIDIA CUDA"
    else if (grepl("AMD|Radeon", name, ignore.case = TRUE)) "AMD"
    else if (grepl("Intel", name, ignore.case = TRUE)) "Intel"
    else "Unknown"
    
    out$gpu_type <- if (grepl("Intel.*UHD|Intel.*Iris|Intel.*HD", name, ignore.case = TRUE)) {
      "integrated"
    } else {
      "discrete"
    }
    
    break
  }
  
  out
}

# ==============================================================================
# Policy-combo parallel worker (backward-compatible name)
# ============================================================================== 
#' @export
run_whole_scenario_job_gpu <- function(task_info,
                                       scenarios_df,
                                       policy_combos_logic,
                                       all_params,
                                       out_dir_base,
                                       cpp_abs_path,
                                       gpu_threads = 4L) {
  if (is.null(all_params$params$execution)) all_params$params$execution <- list()
  all_params$params$execution$combo_threads <- max(0L, as.integer(gpu_threads))
  run_whole_scenario_job_shiny(
    task_info = task_info,
    scenarios_df = scenarios_df,
    policy_combos_logic = policy_combos_logic,
    all_params = all_params,
    out_dir_base = out_dir_base,
    cpp_abs_path = cpp_abs_path
  )
}


# ==============================================================================
# Hybrid CPU+GPU Worker Function
# ==============================================================================

#' @title Run Whole Scenario Job with Internal Policy-Combo Parallelism
#' @description Backward-compatible wrapper. The current C++ engines use CPU
#'   std::threads for policy-combo parallelism; no CUDA/OpenCL code is invoked.
#' @export
run_whole_scenario_job_hybrid <- function(task_info,
                                          scenarios_df,
                                          policy_combos_logic,
                                          all_params,
                                          out_dir_base,
                                          cpp_abs_path,
                                          gpu_threads = 4L,
                                          cpu_fraction = 0.5) {
  if (is.null(all_params$params$execution)) all_params$params$execution <- list()
  all_params$params$execution$combo_threads <- max(0L, as.integer(gpu_threads))
  run_whole_scenario_job_shiny(
    task_info = task_info,
    scenarios_df = scenarios_df,
    policy_combos_logic = policy_combos_logic,
    all_params = all_params,
    out_dir_base = out_dir_base,
    cpp_abs_path = cpp_abs_path
  )
}


# ==============================================================================
# compute_T_safe function
#
# Juvenile fast-forward safe month calculation.
# ==============================================================================


#' @title Compute Safe Fast-Forward Months for Juvenile Recruits
#' @description Calculates the maximum number of months that newly-recruited
#'   juveniles can be "fast-forwarded" (batch-simulated via expected survival)
#'   before any individual could either (a) reach fishery-entry size
#'   (juv_onlyM_len) or (b) reach adult age (min_adult_age).
#'
#'   Uses MAXIMUM growth (lowest density, PG = g1_a + g1_b) to be conservative:
#'   even the fastest-growing bootstrap fish must not exceed juv_onlyM_len
#'   before T_safe.
#'
#' @param theta_clean DataFrame with columns Linf, K, t0 (VBGF bootstrap).
#' @param juv_onlyM_len Fishery entry length threshold (mm).
#' @param min_adult_age Age at which fish become adults (density/mortality switch).
#' @param age_recruit Age of recruits at entry (usually 0; recruits enter at age 0).
#' @param g1_a,g1_b,g1_c,g1_d_avg Juvenile density-dependent growth params (growth_1).
#' @param use_dd_growth Logical. If FALSE, growth is not density-dependent
#'   (PG = 1.0 constant); if TRUE, max growth PG = g1_a + g1_b.
#' @param max_months Safety cap on iteration (default 240 = 20 years).
#' @return A list with:
#'   \describe{
#'     \item{T_safe}{Integer. Safe fast-forward months.}
#'     \item{T_length}{Months until fastest fish reaches juv_onlyM_len.}
#'     \item{T_age}{Months until recruit reaches min_adult_age.}
#'     \item{limiting_factor}{"length" or "age" — which bound is active.}
#'     \item{fastest_fish_idx}{Which bootstrap row grows fastest.}
#'   }
#' @export
compute_T_safe <- function(theta_clean,
                           juv_onlyM_len,
                           min_adult_age,
                           age_recruit = 0.0,
                           g1_a = 1.0, g1_b = 0.0, g1_c = 1.614, g1_d_avg = 210.29,
                           use_dd_growth = TRUE,
                           max_months = 240L) {
  
  if (is.null(theta_clean) || nrow(theta_clean) == 0) {
    stop("compute_T_safe: theta_clean is empty. Run VBGF bootstrap first.")
  }
  
  # Required columns
  if (!all(c("Linf", "K", "t0") %in% names(theta_clean))) {
    stop("compute_T_safe: theta_clean must have columns Linf, K, t0.")
  }
  
  Linf <- theta_clean$Linf
  K    <- theta_clean$K
  t0   <- theta_clean$t0
  n_boot <- length(Linf)
  
  dt <- 1.0 / 12.0
  
  # Maximum growth multiplier (lowest density → fastest growth)
  # In growthf_arma: PG_juv = g1_a + g1_b * exp(-g1_c * PD_juv)
  # At density → 0: PD_juv → 0, exp(0) = 1, so PG_max = g1_a + g1_b
  PG_max <- if (use_dd_growth) (g1_a + g1_b) else 1.0
  
  # ----------------------------------------------------------------
  # For each bootstrap fish, iterate month-by-month with MAX growth
  # until it reaches juv_onlyM_len. Track the fastest one.
  # ----------------------------------------------------------------
  T_length_per_fish <- rep(NA_integer_, n_boot)
  
  for (j in seq_len(n_boot)) {
    # Initial length at recruit entry (growthf0: age=0 VBGF length)
    # L0 = Linf * (1 - exp(-K * (0 - t0)))
    L   <- Linf[j] * (1.0 - exp(-K[j] * (0.0 - t0[j])))
    if (!is.finite(L) || L < 0) L <- 0.0
    age <- age_recruit
    
    # If a recruit is already vulnerable at entry, no fast-forward month is safe.
    if (L >= juv_onlyM_len) {
      T_length_per_fish[j] <- 0L
      next
    }
    
    reached <- FALSE
    for (m in seq_len(max_months)) {
      base <- Linf[j] * exp(-K[j] * (age - t0[j]))
      inc  <- 1.0 - exp(-K[j] * dt)
      L    <- L + PG_max * base * inc
      age  <- age + dt
      
      if (L >= juv_onlyM_len) {
        T_length_per_fish[j] <- m
        reached <- TRUE
        break
      }
    }
    if (!reached) T_length_per_fish[j] <- max_months  # never reached within cap
  }
  
  # Fastest fish = smallest month to reach threshold
  T_length_raw <- min(T_length_per_fish, na.rm = TRUE)
  fastest_idx  <- which.min(T_length_per_fish)
  
  # A batch fast-forwarded for T_safe months undergoes only T_safe - 1
  # growth steps because the recruit-entry month has survival but no growth.
  # Therefore, if the threshold is first reached on growth step m, T_safe = m
  # is the largest safe duration.
  T_length <- max(0L, as.integer(T_length_raw))
  
  # ----------------------------------------------------------------
  # T_age: months until recruit reaches adult age
  # Recruit enters at age = age_recruit, becomes adult at min_adult_age
  # ----------------------------------------------------------------
  T_age <- max(0L, as.integer(ceiling((min_adult_age - age_recruit) * 12.0)))
  
  # ----------------------------------------------------------------
  # T_safe = min of the two bounds
  # ----------------------------------------------------------------
  T_safe <- min(T_length, T_age)
  limiting <- if (T_length <= T_age) "length" else "age"
  
  list(
    T_safe          = as.integer(T_safe),
    T_length        = as.integer(T_length),
    T_age           = as.integer(T_age),
    limiting_factor = limiting,
    fastest_fish_idx = fastest_idx,
    PG_max          = PG_max,
    n_bootstrap     = n_boot
  )
}


# ==============================================================================
# Oversubscription & Memory Stress Test
# ==============================================================================

#' @title Get System Memory Information
#' @description Cross-platform total and available system RAM via the ps package.
#' @return A list with total_mb, available_mb, and used_percent. Falls back to a
#'   conservative 8 GB total / 4 GB available if ps is unavailable.
#' @export
get_system_memory_mb <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(list(
      total_mb = NA_real_,
      available_mb = NA_real_,
      used_percent = NA_real_
    ))
  }
  info <- tryCatch(ps::ps_system_memory(), error = function(e) NULL)
  if (is.null(info)) {
    return(list(
      total_mb = NA_real_,
      available_mb = NA_real_,
      used_percent = NA_real_
    ))
  }
  # Different ps versions may name this field "avail" or "available".
  available_bytes <- if ("available" %in% names(info)) {
    info[["available"]]
  } else {
    info[["avail"]]
  }
  
  list(
    total_mb     = as.numeric(info[["total"]]) / 1024^2,
    available_mb = as.numeric(available_bytes) / 1024^2,
    used_percent = as.numeric(info[["percent"]])
  )
}


#' @title Run Parallel Performance and Memory Test
#' @description Runs the user's current run-control configuration to estimate
#'   CPU contention and memory pressure. Memory is classified using three levels:
#'   safe when projected demand is below currently available RAM; warning when it
#'   exceeds currently available RAM but remains below the hard total-RAM limit;
#'   and blocked when it reaches the hard total-RAM limit. The function also
#'   estimates whether sequential replicate batching can make the plan runnable.
#' @param run_one_fn Function(rep_id) that runs one replicate with the user's
#'   current engine/omp/combo settings. Provided by the app.
#' @param n_cores Number of concurrent replicate workers to actually launch
#'   for the benchmark (the probe size, not necessarily the full setting).
#' @param requested_workers The full replicate-core setting, for reporting.
#' @param combo_threads Policy-combo threads per worker (reporting only).
#' @param omp_threads Individual OpenMP threads per model (reporting only).
#' @param cluster_setup_fn Function(cl) that library()+clusterExport so workers
#'   can run run_one_fn.
#' @param logical_cores Number of detected logical processors.
#' @param total_tasks Total number of independent scenario-by-replicate jobs in
#'   the planned full simulation.
#' @param mem_abort_frac Fraction of TOTAL physical RAM used as the hard memory
#'   limit. Default is 0.95.
#' @return A named list describing the benchmark result (see $status).
#' @export
run_oversubscription_test <- function(run_one_fn,
                                      n_cores,
                                      requested_workers,
                                      combo_threads,
                                      omp_threads,
                                      cluster_setup_fn,
                                      logical_cores,
                                      total_tasks = requested_workers,
                                      mem_abort_frac = 0.95) {
  
  have_ps <- requireNamespace("ps", quietly = TRUE)
  mem <- get_system_memory_mb()
  system_ram_mb <- mem$total_mb
  available_ram_mb <- mem$available_mb
  requested_workers <- max(
    1L,
    as.integer(requested_workers)
  )
  
  n_cores <- max(
    1L,
    as.integer(n_cores)
  )
  
  total_tasks <- max(
    1L,
    as.integer(total_tasks)
  )
  configured_workers <- as.integer(
    min(
      requested_workers,
      total_tasks
    )
  )
  
  # No batching: the actual full-run concurrency is simply the configured
  # workers (capped by total tasks).
  effective_workers <- configured_workers
  
  # The benchmark itself may still use a limited probe.
  probe_workers <- as.integer(
    max(
      1L,
      min(
        n_cores,
        effective_workers
      )
    )
  )
  # ---- Worker body: run one replicate, report maxrss, timing, and the
  #      SYSTEM available RAM sampled right after the run (used to derive the
  #      true system-level concurrent peak without maxrss over-counting). ----
  sys_avail_mb <- function() {
    if (!requireNamespace("ps", quietly = TRUE)) return(NA_real_)
    info <- tryCatch(ps::ps_system_memory(), error = function(e) NULL)
    if (is.null(info)) return(NA_real_)
    ab <- if ("available" %in% names(info)) info[["available"]] else info[["avail"]]
    as.numeric(ab) / 1024^2
  }
  environment(sys_avail_mb) <- baseenv()
  worker_run <- function(rep_id, .run_one_fn) {
    if (requireNamespace("ps", quietly = TRUE)) {
      h <- ps::ps_handle()
      t0 <- Sys.time()
      tryCatch({
        .run_one_fn(rep_id)
        after <- ps::ps_memory_full_info(h)
        peak_bytes <- if (
          "maxrss" %in% names(after) &&
          length(after[["maxrss"]]) == 1L &&
          is.finite(as.numeric(after[["maxrss"]]))
        ) {
          as.numeric(after[["maxrss"]])
        } else {
          # Fallback to current RSS when the operating system does not report maxrss.
          current_mem <- ps::ps_memory_info(h)
          as.numeric(current_mem[["rss"]])
        }
        
        list(
          ok = TRUE,
          msg = "",
          elapsed = as.numeric(
            difftime(Sys.time(), t0, units = "secs")
          ),
          maxrss_mb = peak_bytes / 1024^2,
          # System available RAM right after this worker's run, while its
          # own memory is still resident (before the worker exits).
          sys_avail_after_mb = sys_avail_mb()
        )
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e),
             elapsed = NA_real_, maxrss_mb = NA_real_,
             sys_avail_after_mb = NA_real_)
      })
    } else {
      t0 <- Sys.time()
      tryCatch({
        .run_one_fn(rep_id)
        list(ok = TRUE, msg = "",
             elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")),
             maxrss_mb = NA_real_, sys_avail_after_mb = NA_real_)
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e),
             elapsed = NA_real_, maxrss_mb = NA_real_,
             sys_avail_after_mb = NA_real_)
      })
    }
  }
  worker_run_env <- new.env(
    parent = baseenv()
  )
  
  worker_run_env$sys_avail_mb <- sys_avail_mb
  
  environment(worker_run) <- worker_run_env
  # ---- STAGE 1: solo baseline (one replicate, this process) ----
  gc(full = TRUE)
  
  solo_cl <- NULL
  
  solo <- tryCatch({
    
    solo_cl <- parallel::makeCluster(1L)
    
    cluster_setup_fn(solo_cl)
    
    parallel::clusterExport(
      solo_cl,
      varlist = c(
        "worker_run",
        "run_one_fn"
      ),
      envir = environment()
    )
    
    parallel::parLapply(
      solo_cl,
      1L,
      function(k) {
        worker_run(k, run_one_fn)
      }
    )[[1L]]
    
  }, error = function(e) {
    
    list(
      ok = FALSE,
      msg = conditionMessage(e),
      elapsed = NA_real_,
      maxrss_mb = NA_real_
    )
    
  }, finally = {
    
    if (!is.null(solo_cl)) {
      tryCatch(
        parallel::stopCluster(solo_cl),
        error = function(e) NULL
      )
    }
  })
  if (!isTRUE(solo$ok)) {
    mem_crash <- grepl("cannot allocate|bad_alloc|memory|out of memory",
                       solo$msg, ignore.case = TRUE)
    return(list(
      status = if (mem_crash) "memory_crash" else "worker_error",
      worker_error = solo$msg,
      n_cores = n_cores, requested_workers = requested_workers,
      combo_threads = combo_threads, omp_threads = omp_threads,
      logical_cores = logical_cores, have_ps = have_ps
    ))
  }
  solo_elapsed  <- solo$elapsed
  solo_proc_mem <- solo$maxrss_mb
  
  # ---- STAGE 2: baseline measurement + minimal crash guard ----------
  # We no longer abort based on maxrss * N (that over-counts shared memory
  # and produced false "out of memory" reports). Instead we measure the
  # REAL concurrent footprint in Stage 3. The only pre-run abort here is the
  # extreme case where a single worker alone already exceeds the hard limit.
  gc(full = TRUE)
  Sys.sleep(0.2)
  
  memory_now <- if (have_ps) {
    tryCatch(ps::ps_system_memory(), error = function(e) NULL)
  } else NULL
  
  if (!is.null(memory_now)) {
    system_ram_mb <- as.numeric(memory_now[["total"]]) / 1024^2
    available_bytes <- if ("available" %in% names(memory_now)) {
      memory_now[["available"]]
    } else {
      memory_now[["avail"]]
    }
    available_ram_mb <- as.numeric(available_bytes) / 1024^2
  } else {
    available_ram_mb <- NA_real_
  }
  
  # Baseline system available RAM right before launching concurrent workers.
  baseline_avail_mb <- available_ram_mb
  
  # Hard limit based on TOTAL physical RAM.
  ram_limit <- system_ram_mb * mem_abort_frac
  # ------------------------------------------------------------
  # Stage 2 memory projection
  #
  # maxrss may include some shared library memory and therefore is not a
  # perfect additive estimate. However, using it as a conservative pre-check
  # is safer than launching enough workers to crash the R session.
  # ------------------------------------------------------------
  projected_mem <- if (
    is.finite(solo_proc_mem)
  ) {
    solo_proc_mem * probe_workers
  } else {
    NA_real_
  }
  
  single_worker_too_large <- (
    is.finite(solo_proc_mem) &&
      solo_proc_mem >= ram_limit
  )
  
  projected_exceeds_available <- (
    is.finite(projected_mem) &&
      is.finite(available_ram_mb) &&
      projected_mem >= available_ram_mb
  )
  
  projected_exceeds_hard_limit <- (
    is.finite(projected_mem) &&
      is.finite(ram_limit) &&
      projected_mem >= ram_limit
  )
  
  if (
    isTRUE(single_worker_too_large) ||
    isTRUE(projected_exceeds_hard_limit)
  ) {
    return(
      list(
        status = "memory_abort",
        memory_precheck = "abort",
        
        solo_elapsed = solo_elapsed,
        solo_proc_mem = solo_proc_mem,
        projected_mem = projected_mem,
        
        system_ram_mb = system_ram_mb,
        available_ram_mb = available_ram_mb,
        ram_limit = ram_limit,
        
        single_worker_too_large = single_worker_too_large,
        projected_exceeds_available = projected_exceeds_available,
        projected_exceeds_hard_limit = projected_exceeds_hard_limit,
        
        requested_workers = requested_workers,
        configured_workers = configured_workers,
        effective_workers = effective_workers,
        probe_workers = probe_workers,
        
        combo_threads = combo_threads,
        omp_threads = omp_threads,
        logical_cores = logical_cores,
        have_ps = have_ps
      )
    )
  }
  
 
  
  
  # ---- STAGE 3: concurrent stress ----
  concurrent_elapsed <- NA_real_
  concurrent_peak_mem <- NA_real_
  worker_error <- NULL
  cl <- NULL
  concurrent_used_mb <- NA_real_
  tryCatch({
    cl <- parallel::makeCluster(probe_workers)
    cluster_setup_fn(cl)
    parallel::clusterExport(
      cl,
      varlist = c(
        "worker_run",
        "run_one_fn"
      ),
      envir = environment()
    )
    
    tc <- Sys.time()
    results <- parallel::parLapply(
      cl,
      seq_len(probe_workers), function(k) {
        worker_run(k, run_one_fn)
      })
    concurrent_elapsed <- as.numeric(difftime(Sys.time(), tc, units = "secs"))
    
    # System-level concurrent footprint estimate based on available-RAM
    # samples collected immediately after each worker completes its run.
    # This avoids summing shared process memory, but it may miss short-lived
    # peaks that occurred earlier inside the simulation.
    avail_during <- vapply(results, function(x) {
      v <- x$sys_avail_after_mb
      if (is.null(v) || is.na(v)) NA_real_ else v
    }, numeric(1))
    avail_during <- avail_during[!is.na(avail_during)]
    min_avail_during <- if (length(avail_during) == 0) NA_real_ else min(avail_during)
    
    # Estimated system-memory change associated with the concurrent probe.
    concurrent_used_mb <- if (is.na(baseline_avail_mb) || is.na(min_avail_during)) {
      NA_real_
    } else {
      max(0, baseline_avail_mb - min_avail_during)
    }
    # App-facing value: sampled concurrent memory footprint for the probe.
    concurrent_peak_mem <- concurrent_used_mb
    
    bad <- Filter(function(x) !isTRUE(x$ok), results)
    if (length(bad) > 0) {
      worker_error <- paste(vapply(bad, function(x) x$msg, character(1)),
                            collapse = "; ")
    }
  }, error = function(e) {
    worker_error <<- conditionMessage(e)
  }, finally = {
    if (!is.null(cl)) tryCatch(parallel::stopCluster(cl), error = function(e) NULL)
  })
  
  # ---- Real per-worker memory cost (from measured concurrent footprint) ----
  # This is the marginal RAM each additional worker actually needs, with shared
  # memory already excluded (system-level measurement).
  per_worker_mb <- if (
    is.na(concurrent_used_mb) ||
    concurrent_used_mb <= 0 ||
    probe_workers < 1L
  ) {
    # System-level sampling was unavailable or distorted by unrelated
    # memory changes. Fall back to the solo worker's process peak.
    solo_proc_mem
  } else {
    concurrent_used_mb / probe_workers
  }
  
  # Extrapolate to the FULL configured concurrency the user would actually run.
  projected_full_mem <- if (is.na(per_worker_mb)) {
    NA_real_
  } else {
    per_worker_mb * effective_workers
  }
  
  # Projected footprint represented by the benchmark probe itself.
  projected_probe_mem <- if (is.na(per_worker_mb)) {
    NA_real_
  } else {
    per_worker_mb * probe_workers
  }
  
  # Backward-compatible app-facing alias: the selected execution plan.
  projected_mem <- projected_full_mem
  
  # Max workers that fit under the hard limit, from the REAL per-worker cost.
  # This is the sole memory recommendation now: "lower cores to N".
  max_safe_workers_by_total <- if (is.na(per_worker_mb) || per_worker_mb <= 0 || is.na(ram_limit)) {
    NA_integer_
  } else {
    as.integer(floor((ram_limit * (1 - 1e-9)) / per_worker_mb))
  }
  if (!is.na(max_safe_workers_by_total)) {
    max_safe_workers_by_total <- max(0L, min(configured_workers, max_safe_workers_by_total))
  }
  
  # Three-level memory verdict, based on the REAL extrapolated footprint.
  memory_precheck <- if (is.na(projected_full_mem) || is.na(available_ram_mb) || is.na(system_ram_mb)) {
    "unknown"
  } else if (projected_full_mem < available_ram_mb) {
    "safe"
  } else if (projected_full_mem < ram_limit) {
    "warning"
  } else {
    "abort"
  }
  
  plan_info <- list(
    total_tasks                   = total_tasks,
    requested_workers             = requested_workers,
    configured_workers            = configured_workers,
    effective_workers             = effective_workers,
    probe_workers                 = probe_workers,
    n_cores                       = probe_workers,
    per_worker_mb                 = per_worker_mb,
    projected_probe_mem           = projected_probe_mem,
    projected_full_mem            = projected_full_mem,
    max_safe_workers_by_total     = max_safe_workers_by_total,
    single_worker_too_large       = single_worker_too_large
  )
  
  if (!is.null(worker_error)) {
    mem_crash <- grepl("cannot allocate|bad_alloc|memory|out of memory",
                       worker_error, ignore.case = TRUE)
    return(
      c(
        list(
          status = if (mem_crash) "memory_crash" else "worker_error",
          worker_error        = worker_error,
          memory_precheck     = memory_precheck,
          solo_elapsed        = solo_elapsed,
          solo_proc_mem       = solo_proc_mem,
          projected_mem       = projected_full_mem,
          concurrent_peak_mem = concurrent_peak_mem,
          system_ram_mb       = system_ram_mb,
          available_ram_mb    = available_ram_mb,
          ram_limit           = ram_limit,
          combo_threads       = combo_threads,
          omp_threads         = omp_threads,
          logical_cores       = logical_cores,
          have_ps             = have_ps
        ),
        plan_info
      )
    )
  }
  
  # If the REAL extrapolated footprint reaches the hard limit, abort the full run.
  if (identical(memory_precheck, "abort")) {
    return(
      c(
        list(
          status              = "memory_abort",
          memory_precheck     = "abort",
          solo_elapsed        = solo_elapsed,
          solo_proc_mem       = solo_proc_mem,
          projected_mem       = projected_full_mem,
          concurrent_peak_mem = concurrent_peak_mem,
          system_ram_mb       = system_ram_mb,
          available_ram_mb    = available_ram_mb,
          ram_limit           = ram_limit,
          combo_threads       = combo_threads,
          omp_threads         = omp_threads,
          logical_cores       = logical_cores,
          have_ps             = have_ps
        ),
        plan_info
      )
    )
  }
  
  oversub_factor <- if (is.na(concurrent_elapsed) || is.na(solo_elapsed) || solo_elapsed <= 0) {
    NA_real_
  } else {
    concurrent_elapsed / solo_elapsed
  }
  c(
    list(
      status               = "ok",
      memory_precheck      = memory_precheck,
      solo_elapsed         = solo_elapsed,
      concurrent_elapsed   = concurrent_elapsed,
      oversub_factor       = oversub_factor,
      solo_proc_mem        = solo_proc_mem,
      projected_mem        = projected_full_mem,
      concurrent_peak_mem  = concurrent_peak_mem,
      system_ram_mb        = system_ram_mb,
      available_ram_mb     = available_ram_mb,
      ram_limit            = ram_limit,
      combo_threads        = combo_threads,
      omp_threads          = omp_threads,
      logical_cores        = logical_cores,
      have_ps              = have_ps
    ),
    plan_info
  )
}

# ==============================================================================
# Age imputation with an age-length key (FSA)
# ==============================================================================

#' @title Estimate Missing Ages with an Age-Length Key
#' @description Fish with a known age are used to build an age-length key, which
#'   is then applied to fish whose age is missing. This follows the standard
#'   fisheries approach implemented in the FSA package: length categories are
#'   formed from the aged subsample, the observed age composition within each
#'   length category becomes the key, and each unaged fish is assigned an age
#'   drawn from the key row matching its length.
#' @param df A data frame with numeric 'Length' and 'Age' columns, where 'Age'
#'   may contain NA values.
#' @param bin_width Width of the length categories. When NULL, a width is chosen
#'   automatically so the length range is covered by roughly 18 categories.
#' @param seed Random seed for the age draws. Ages are assigned semi-randomly
#'   from the key, so the seed makes the result reproducible. When NULL, a seed
#'   is generated and returned so it can be recorded and reused.
#' @return A list with the completed data (\code{data}), the number of aged fish
#'   (\code{n_aged}), the number of fish whose age was estimated
#'   (\code{n_imputed}), the number of unaged fish that fell outside the key and
#'   had to be discarded (\code{n_dropped}), the length-category width used
#'   (\code{bin_width}), the seed used (\code{seed}), and the age-length key
#'   itself (\code{alk}).
#' @export
impute_ages_alk <- function(df, bin_width = NULL, seed = NULL) {
  
  if (!requireNamespace("FSA", quietly = TRUE)) {
    stop("The FSA package is required to estimate missing ages.")
  }
  
  # Ages are drawn semi-randomly from the key, so a seed is recorded to make
  # the assignment reproducible.
  if (is.null(seed) || !is.finite(suppressWarnings(as.numeric(seed)))) {
    seed <- sample.int(999999L, 1L)
  }
  seed <- as.integer(seed)
  
  if (!all(c("Length", "Age") %in% names(df))) {
    stop("The growth data must contain 'Length' and 'Age' columns.")
  }
  
  d <- data.frame(
    Length = suppressWarnings(as.numeric(df$Length)),
    Age    = suppressWarnings(as.numeric(df$Age))
  )
  d <- d[is.finite(d$Length), , drop = FALSE]
  
  aged   <- d[!is.na(d$Age), , drop = FALSE]
  unaged <- d[is.na(d$Age), , drop = FALSE]
  
  if (nrow(aged) < 10L) {
    stop(paste0(
      "At least 10 fish with a known age are needed to build an age-length key; ",
      "this file has ", nrow(aged), "."
    ))
  }
  
  if (nrow(unaged) == 0L) {
    return(list(
      data      = aged,
      n_aged    = nrow(aged),
      n_imputed = 0L,
      n_dropped = 0L,
      n_filled  = 0L,
      bin_width = NA_real_,
      seed      = seed,
      alk       = NULL
    ))
  }
  
  # Automatic length-category width: aim for roughly 18 categories.
  if (is.null(bin_width) || !is.finite(bin_width) || bin_width <= 0) {
    span <- diff(range(d$Length, na.rm = TRUE))
    bin_width <- max(1, round(span / 18))
  }
  bin_width <- as.numeric(bin_width)
  
  # Use one common origin so both subsets fall on the same category grid.
  start_len <- floor(min(d$Length, na.rm = TRUE) / bin_width) * bin_width
  
  aged$LCat   <- FSA::lencat(aged$Length,   w = bin_width, startcat = start_len)
  unaged$LCat <- FSA::lencat(unaged$Length, w = bin_width, startcat = start_len)
  
  # The key is built on a contiguous grid of length categories so that it has
  # no holes. Without this, a length class that happens to contain no aged
  # fish (say 120-139 mm when only 100-119 and 140-159 were aged) would leave
  # unaged fish of that size with no row to draw an age from.
  aged_cats <- sort(unique(aged$LCat))
  cats_all  <- seq(min(aged_cats), max(aged_cats), by = bin_width)
  
  freq <- table(factor(aged$LCat, levels = cats_all), aged$Age)
  
  if (ncol(freq) == 0L) {
    stop("The aged fish are not sufficient to build an age-length key.")
  }
  
  # A length class with no aged fish borrows the age composition of the
  # nearest class that does have some.
  row_tot  <- rowSums(freq)
  n_filled <- sum(row_tot == 0)
  
  if (n_filled > 0L) {
    donors <- which(row_tot > 0)
    if (length(donors) == 0L) {
      stop("The aged fish are not sufficient to build an age-length key.")
    }
    for (i in which(row_tot == 0)) {
      freq[i, ] <- freq[donors[which.min(abs(donors - i))], ]
    }
  }
  
  alk <- prop.table(freq, margin = 1)
  
  # Fish shorter than the smallest length category in the key cannot be
  # assigned an age. Fish longer than the largest category are handled by
  # the key's top row, so they are kept.
  key_cats  <- suppressWarnings(as.numeric(rownames(alk)))
  keep      <- unaged$Length >= min(key_cats, na.rm = TRUE)
  n_dropped <- sum(!keep)
  unaged_ok <- unaged[keep, , drop = FALSE]
  
  imputed <- NULL
  if (nrow(unaged_ok) > 0L) {
    # Pass only the columns the key needs; FSA builds its own length classes
    # from the key's row names.
    to_age   <- unaged_ok[, c("Length", "Age"), drop = FALSE]
    assigned <- FSA::alkIndivAge(alk, Age ~ Length, data = to_age, seed = seed)
    imputed  <- assigned[, c("Length", "Age"), drop = FALSE]
  }
  
  out <- rbind(aged[, c("Length", "Age"), drop = FALSE], imputed)
  rownames(out) <- NULL
  
  list(
    data      = out,
    n_aged    = nrow(aged),
    n_imputed = if (is.null(imputed)) 0L else nrow(imputed),
    n_dropped = as.integer(n_dropped),
    n_filled  = as.integer(n_filled),
    bin_width = bin_width,
    seed      = seed,
    alk       = alk
  )
}


#' @title Summarise Length at Age into Age-Length Key Data
#' @description Collapses individual length-age records into the four-column
#'   summary the application expects for age-length key data: 'Age', 'n',
#'   'Length' (mean length at that age) and 'Lengthsd' (standard deviation of
#'   length at that age). Ages represented by a single fish are reported with a
#'   standard deviation of zero.
#' @param df A data frame with numeric 'Length' and 'Age' columns.
#' @return A data frame with columns Age, n, Length and Lengthsd.
#' @export
build_alk_summary <- function(df) {
  
  if (!all(c("Length", "Age") %in% names(df))) {
    stop("The data must contain 'Length' and 'Age' columns.")
  }
  
  d <- data.frame(
    Length = suppressWarnings(as.numeric(df$Length)),
    Age    = suppressWarnings(as.numeric(df$Age))
  )
  d <- d[is.finite(d$Length) & is.finite(d$Age), , drop = FALSE]
  
  if (nrow(d) == 0L) {
    stop("There are no aged fish to summarise.")
  }
  
  ages <- sort(unique(d$Age))
  
  out <- data.frame(
    Age = ages,
    n = vapply(ages, function(a) sum(d$Age == a), numeric(1)),
    Length = vapply(ages, function(a) mean(d$Length[d$Age == a]), numeric(1)),
    Lengthsd = vapply(ages, function(a) {
      v <- d$Length[d$Age == a]
      if (length(v) < 2L) 0 else stats::sd(v)
    }, numeric(1))
  )
  
  out$Length   <- round(out$Length, 2)
  out$Lengthsd <- round(out$Lengthsd, 2)
  rownames(out) <- NULL
  out
}

# ==============================================================================
# Simulation engine dispatcher
# ==============================================================================

#' @title Run One Replicate with the Selected Simulation Engine
#' @description Chooses the appropriate compiled simulation routine for the
#'   current execution settings and runs a single replicate. Three routes are
#'   available: the large-population optimized engine (individual-level OpenMP
#'   plus policy threads), the standard engine with policy-combination
#'   parallelism, and the plain sequential engine.
#'
#'   The per-replicate seed is derived as \code{seed + rep_id - 1}, so a single
#'   base seed reproduces an entire set of replicates while giving each one a
#'   distinct random stream.
#'
#'   This function is deliberately self-contained: it depends only on its
#'   arguments and resolves the compiled routines through the package
#'   namespace. That makes it safe to call from parallel workers and from a
#'   cloud container, not just from the Shiny session.
#' @param all_params Packed parameter list produced by the application.
#' @param cpp_scen A one-row data frame describing the scenario to run.
#' @param cpp_pol_df Data frame of policy combinations.
#' @param rep_id Replicate index, starting at 1. Used to derive the seed.
#' @return The simulation result returned by the selected compiled routine.
#' @export
run_selected_cpp <- function(all_params, cpp_scen, cpp_pol_df, rep_id = 1L) {
  is_age_mode <- identical(all_params$other$f_age_mode, "age")
  engine <- if (!is.null(all_params$execution$engine)) all_params$execution$engine else "legacy"
  omp_threads <- if (!is.null(all_params$execution$omp_nthreads)) as.integer(all_params$execution$omp_nthreads) else 1L
  combo_threads <- if (!is.null(all_params$execution$combo_threads)) as.integer(all_params$execution$combo_threads) else 0L

  seed_base <- if (!is.null(all_params$seed)) as.integer(all_params$seed) else 1L
  rep_seed <- seed_base + as.integer(rep_id) - 1L

  args <- list(
    zr_w_dist            = all_params$z_vec,
    month_weights        = all_params$month_weights,
    W1_alk               = all_params$alk_mat,
    agedata              = all_params$agedata_mat,
    harvest_params_in    = all_params$harvest,
    growth_params_dd_in1 = all_params$growth_1,
    growth_params_dd_in2 = all_params$growth_2,
    survival_params      = all_params$survival,
    scenario_to_run      = cpp_scen,
    policy_combos        = cpp_pol_df,
    compliance_structure = all_params$compliance_struct,
    before_policy_years  = all_params$before_policy_years,
    policy_years         = all_params$policy_years,
    lake_area_ha         = all_params$other$lake_area_ha,
    initial_pop_size     = all_params$other$initial_pop_size,
    rec_a                = all_params$other$rec_a,
    rec_b                = all_params$other$rec_b,
    rec_v                = all_params$other$rec_v,
    F_over_Z_ratio       = all_params$other$F_over_Z_ratio,
    juv_onlyM_len        = all_params$other$juv_onlyM_len,
    spawn_month          = all_params$other$spawn_month,
    recruit_entry_month  = all_params$other$recruit_entry_month,
    rep                  = rep_seed,
    vmonthly_avg         = all_params$other$vmonthly_avg,
    min_adult_age        = all_params$other$min_adult_age,
    age_recruit          = all_params$other$age_recruit,
    age_spawn            = all_params$other$age_spawn,
    psd_stock            = all_params$other$psd_stock,
    psd_quality          = all_params$other$psd_quality,
    psd_preferred        = all_params$other$psd_preferred,
    psd_memorable        = all_params$other$psd_memorable,
    psd_trophy           = all_params$other$psd_trophy,
    Fagemode             = is_age_mode,
    use_ricker           = all_params$other$use_ricker,
    T_safe               = all_params$other$T_safe
  )

  craibm_ns <- asNamespace("craibm")

  get_craibm_fun <- function(name) {
    get0(
      name,
      envir = craibm_ns,
      mode = "function",
      inherits = FALSE,
      ifnotfound = NULL
    )
  }

  # ------------------------------------------------------------
  # Large-population optimized engine
  # Individual OpenMP threads and policy threads are both passed
  # to the V2 engine.
  # ------------------------------------------------------------
  if (identical(engine, "v2")) {

    sim_fun <- get_craibm_fun("run_simulation_v2_cpp")

    if (is.null(sim_fun)) {
      stop(
        paste0(
          "The large-population optimized method is selected, but ",
          "run_simulation_v2_cpp is unavailable in the installed craibm build."
        )
      )
    }

    args$omp_nthreads <- max(1L, omp_threads)
    args$gpu_threads <- max(0L, combo_threads)

    return(
      do.call(sim_fun, args)
    )
  }

  # ------------------------------------------------------------
  # Standard engine with policy-combination parallelism
  # ------------------------------------------------------------
  if (combo_threads > 0L) {

    sim_fun <- get_craibm_fun("run_simulation_gpu")

    if (is.null(sim_fun)) {
      stop(
        paste0(
          "Policy parallelism is enabled, but the policy-parallel ",
          "simulation function is unavailable in the installed craibm build."
        )
      )
    }

    args$gpu_threads <- max(1L, combo_threads)

    return(
      do.call(sim_fun, args)
    )
  }

  # ------------------------------------------------------------
  # Standard sequential engine
  # ------------------------------------------------------------
  sim_fun <- get_craibm_fun(
    "run_simulation_sizelimit_cpp"
  )

  if (is.null(sim_fun)) {
    stop(
      paste0(
        "The standard simulation function run_simulation_sizelimit_cpp ",
        "is unavailable in the installed craibm build."
      )
    )
  }

  do.call(sim_fun, args)
}
