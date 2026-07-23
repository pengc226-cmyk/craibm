#' @title Fish IBM Validation Functions
#' @description Input validation functions for the craibm package.
#' @keywords internal
#' @name validation_utils
NULL


# validation.R


#' @title Parse and Check Numeric Vector
#' @description Parses a string to numeric vector with bounds checking.
#' @export
parse_and_check_vec <- function(str_val, name, min_val = -Inf, max_val = Inf, len_must_be = NULL) {
  if (is.null(str_val) || !nzchar(str_val)) {
    return(list(valid = FALSE, msg = paste0("❌ Error: [", name, "] cannot be empty.\n")))
  }
  
  clean_str <- gsub(",", ",", str_val)
  parts <- trimws(unlist(strsplit(clean_str, ",")))
  parts <- parts[parts != ""]
  
  if (length(parts) == 0) {
    return(list(valid = FALSE, msg = paste0("❌ Error: [", name, "] is empty or invalid format.\n")))
  }
  
  nums <- suppressWarnings(as.numeric(parts))
  
  if (any(is.na(nums))) {
    bad_inputs <- parts[is.na(nums)]
    return(list(valid = FALSE, msg = paste0("❌ Error: [", name, "] contains non-numeric values: '", paste(bad_inputs, collapse="', '"), "'. (Did you type 'o' instead of '0'?)\n")))
  }
  
  if (!is.null(len_must_be) && length(nums) != len_must_be) {
    return(list(valid = FALSE, msg = paste0("❌ Error: [", name, "] must contain exactly ", len_must_be, " numbers. You provided ", length(nums), ".\n")))
  }
  
  if (any(nums < min_val) || any(nums > max_val)) {
    return(list(valid = FALSE, msg = paste0("❌ Error: [", name, "] values must be between ", min_val, " and ", max_val, ".\n")))
  }
  
  return(list(valid = TRUE, nums = nums, msg = ""))
}

#' @title Check Numeric Value
#' @description Validates a numeric input against bounds.
#' @export
check_num <- function(val, name, min_val = -Inf, max_val = Inf, is_int = FALSE) {
  if (is.null(val) || length(val) == 0 || is.na(val)) {
    return(paste0("❌ Error: [", name, "] is empty or not a number.\n"))
  }
  if (!is.numeric(val)) {
    return(paste0("❌ Error: [", name, "] must be numeric.\n"))
  }
  if (val < min_val || val > max_val) {
    return(paste0("❌ Error: [", name, "] must be between ", min_val, " and ", max_val, ".\n"))
  }
  if (is_int && val %% 1 != 0) {
    return(paste0("❌ Error: [", name, "] must be an integer.\n"))
  }
  return(NULL)
}

#' @title Check CSV Extension
#' @description Validates that the file has a .csv extension.
#' @export
check_csv_ext <- function(filename) {
  if (is.null(filename)) return("❌ Error: No file uploaded.\n")
  if (!grepl("\\.csv$", filename, ignore.case = TRUE)) {
    return(paste0("❌ Error: Invalid file format '", filename, "'.\nPlease upload a .csv file.\n"))
  }
  return(NULL)}


# 1. VBGF

#' @title Check VBGF Inputs
#' @description Validates growth data upload and bootstrap settings.
#' @export
check_vbgf_inputs <- function(file_obj, df, boot_b) {
  msgs <- c()
  
  
  ext_err <- check_csv_ext(file_obj$name)
  if (!is.null(ext_err)) return(list(pass = FALSE, msg = ext_err))
  
  
  if (is.null(df) || nrow(df) == 0) return(list(pass = FALSE, msg = "❌ Error: File is empty.\n"))
  
  
  # ： 2 , Age Length
  if (ncol(df) != 2) {
    return(list(pass = FALSE, msg = paste0("❌ Error: File must have exactly 2 columns (Age, Length).\nYour file has ", ncol(df), " columns.\n")))
  }
  
  req_cols <- c("Age", "Length")
  if (!all(req_cols %in% names(df))) {
    return(list(pass = FALSE, msg = paste0("❌ Error: Columns must be exactly: 'Age', 'Length'.\nFound: ", paste(names(df), collapse=", "), "\n")))
  }
  
  
  if (!is.numeric(df$Length)) msgs <- c(msgs, "❌ Error: 'Length' column contains non-numbers.\n")
  if (!is.numeric(df$Age))    msgs <- c(msgs, "❌ Error: 'Age' column contains non-numbers.\n")
  
  # 5. Bootstrap
  err <- check_num(boot_b, "Bootstrap Replicates", min_val = 100, is_int = TRUE)
  if (!is.null(err)) msgs <- c(msgs, err)
  
  pass <- length(msgs) == 0
  return(list(pass = pass, msg = ifelse(pass, "✅ VBGF Input Check Passed!\n", paste(msgs, collapse = ""))))
}


# 2. Mortality

#' @title Check Z Estimation Inputs
#' @description Validates mortality estimation parameters.
#' @export
check_z_inputs <- function(growth_df, full, last, bg2) {
  msgs <- c()
  
  if (is.null(growth_df)) return(list(pass = FALSE, msg = "❌ Error: Please submit ALK data first in Step 1: Parameters 1\n"))
  if (is.na(full)) {
    msgs <- c(msgs, "❌ Error: [Transition Age is missing.\n Please go to the 'Other' tab -> 'Life History' box to set it.\n")
  } else {
    msgs <- c(msgs, check_num(full, "Transition Age", min_val = 0))
  }
  
  msgs <- c(msgs, check_num(last, "Max Age", min_val = 1))
  msgs <- c(msgs, check_num(bg2, "Bootstrap Replicates", min_val = 100, is_int = TRUE))
  
  if (is.numeric(full) && is.numeric(last) && !is.na(full) && !is.na(last) && last <= full) {
    msgs <- c(msgs, paste0(
      "❌ Error: Max Age (", last, ") must be strictly greater than Transition Age(", full, ").\n",
      "Please adjust 'Transition Age' in the 'Other' tab or 'Max Age' here.\n"
    ))
  }
  
  pass <- length(msgs) == 0
  return(list(pass = pass, msg = ifelse(pass, "✅ Mortality (Z) Check Passed!\n", paste(msgs, collapse = ""))))
}


#' @title Check ALK Inputs
#' @description Validates age-length key data upload.
#' @export
check_alk_inputs <- function(file_obj, df) {
  
  ext_err <- check_csv_ext(file_obj$name)
  if (!is.null(ext_err)) return(list(pass = FALSE, msg = ext_err))
  
  if (is.null(df) || nrow(df) == 0) return(list(pass = FALSE, msg = "❌ Error: File is empty.\n"))
  
  
  # ： 4 , Age, n, Length, Lengthsd
  if (ncol(df) != 4) {
    return(list(pass = FALSE, msg = paste0("❌ Error: File must have exactly 4 columns.\nYour file has ", ncol(df), " columns.\n")))
  }
  
  req_cols <- c("Age", "n", "Length", "Lengthsd")
  if (!all(req_cols %in% names(df))) {
    return(list(pass = FALSE, msg = paste0("❌ Error: Columns must be: 'Age', 'n', 'Length', 'Lengthsd'.\nFound: ", paste(names(df), collapse=", "), "\n")))
  }
  
  msgs <- c()
  if (!is.numeric(df$Age)) msgs <- c(msgs, "❌ Error: 'Age' must be numeric.\n")
  if (!is.numeric(df$n))   msgs <- c(msgs, "❌ Error: 'n' must be numeric.\n")
  if (is.numeric(df$n) && any(df$n < 0, na.rm=TRUE)) msgs <- c(msgs, "❌ Error: 'n' cannot be negative.\n")
  
  pass <- length(msgs) == 0
  return(list(pass = pass, msg = ifelse(pass, "✅ ALK Input Check Passed!\n", paste(msgs, collapse = ""))))
}

#' @title Check Bootstrap Outcomes
#' @description Validates bootstrap results for sufficient clean runs.
#' @export
check_boot_outcomes <- function(theta_df, expected_runs) {
  msgs <- c()
  pass <- TRUE
  
  
  if (is.null(theta_df) || nrow(theta_df) == 0) {
    return(list(pass = FALSE, msg = "❌ Error: Model produced NO valid runs (Convergence Failure).\nPossible causes: Data sparse, bad starting values, or outliers.\n"))
  }
  
  # 2. NA / Inf
  # dataframe
  n_na <- sum(is.na(theta_df))
  n_inf <- sum(sapply(theta_df, function(x) sum(is.infinite(x))))
  
  if (n_na > 0) {
    pass <- FALSE
    msgs <- c(msgs, paste0("⚠️ Warning: Results contain ", n_na, " NA values.\n"))
  }
  if (n_inf > 0) {
    pass <- FALSE
    msgs <- c(msgs, paste0("❌ Error: Results contain ", n_inf, " Infinite values (Math Error).\n"))
  }
  
  # 3. (Success Rate)
  success_rate <- nrow(theta_df) / expected_runs
  if (success_rate < 0.1) {
    pass <- FALSE
    msgs <- c(msgs, paste0("❌ Error: Convergence rate too low (", round(success_rate*100, 1), "%).\nOnly ", nrow(theta_df), " out of ", expected_runs, " runs succeeded.\nData may be insufficient for Bootstrap.\n"))
  } else if (success_rate < 0.8) {
    msgs <- c(msgs, paste0("⚠️ Warning: Low convergence rate (", round(success_rate*100, 1), "%).\n"))
  }
  
  final_msg <- if(length(msgs) == 0) "✅ Model Run Successful & Healthy!\n" else paste(msgs, collapse = "")
  return(list(pass = pass, msg = final_msg))
}


# 3. Global Parameters (The BIG Check)

#' @title Check Global Parameters
#' @description Validates all global simulation parameters.
#' @export
check_global_inputs <- function(inputs) {
  msgs <- c()
  
  
  # --- 0. Pre-requisite Checks (Survival) ---
  if (isFALSE(inputs$survival_ok)) {
    msgs <- c(msgs, "❌ Error: Natural mortality parameters are not confirmed.\n   Please go to the 'Natural mortality' tab and click 'Confirm & Save Parameters'.\n")
  }
  # --- 1. Timeline ---
  # (Iterations and Seed moved to check_runcontrol_inputs / Step 2 Run control)
  err <- check_num(inputs$burn_in_years, "Burn-in Years", min_val = 0)
  if(!is.null(err)) msgs <- c(msgs, err)
  
  err <- check_num(inputs$stable_years, "Stable Years", min_val = 0)
  if(!is.null(err)) msgs <- c(msgs, err)
  
  err <- check_num(inputs$policy_years, "Policy Years", min_val = 1)
  if(!is.null(err)) msgs <- c(msgs, err)
  
  # --- 3. Density Dependent ---
  if (isTRUE(inputs$use_dd_survival)) {
    
    msgs <- c(msgs, check_num(inputs$surv_a, "Survival a"))
    msgs <- c(msgs, check_num(inputs$surv_b, "Survival b"))
    msgs <- c(msgs, check_num(inputs$surv_d1, "Survival d1 (density)", min_val = 0))
    msgs <- c(msgs, check_num(inputs$surv_d2, "Survival d2 (density)", min_val = 0))
  }
  
  # --- 4. Harvest ---
  if (isTRUE(inputs$flag_harvest_curve)) {
    
    msgs <- c(msgs, check_num(inputs$harv_L50, "Harvest L50", min_val = 0))
    msgs <- c(msgs, check_num(inputs$harv_pmax, "Harvest p_max ", min_val = 0, max_val = 1))
    msgs <- c(msgs, check_num(inputs$harv_slope, "Harvest Slope"))
  } else {
    # B: , p_max ( Fixed Probability)
    # L50 Slope,/
    msgs <- c(msgs, check_num(inputs$harv_pmax, "Fixed Harvest Probability", min_val = 0, max_val = 1))
  }
  
  # --- 5. Month Weights () ---
  # 12
  mw_chk <- parse_and_check_vec(inputs$month_weights, "Month Weights", min_val = 0, len_must_be = 12)
  if (!mw_chk$valid) {
    msgs <- c(msgs, mw_chk$msg)
  }
  
  # --- 6. Life History & Biology ---
  # Ages
  msgs <- c(msgs, check_num(inputs$age_spawn, "Maturity Age", min_val = 0.1))
  msgs <- c(msgs, check_num(inputs$min_adult_age, "Transition Age", min_val = 0.1))
  msgs <- c(msgs, check_num(inputs$age_recruit, "Recruit Age (Fishery)", min_val = 0))
  
  # R-S & Months
  msgs <- c(msgs, check_num(inputs$rec_a, "R-S alpha", min_val = 0))
  msgs <- c(msgs, check_num(inputs$rec_b, "R-S beta", min_val = 0))
  msgs <- c(msgs, check_num(inputs$spawn_month, "Spawning Month", min_val = 1, max_val = 12, is_int = TRUE))
  msgs <- c(msgs, check_num(inputs$recruit_entry_month, "Recruit Entry Month", min_val = 1, max_val = 12, is_int = TRUE))
  
  # Environment
  msgs <- c(msgs, check_num(inputs$lake_area_ha, "Lake Area (ha)", min_val = 0.0001))
  msgs <- c(msgs, check_num(inputs$initial_pop_size, "Initial Pop Size", min_val = 100))
  
  #PSD
  msgs <- c(msgs, check_num(inputs$psd_stock, "PSD Stock", min_val = 0))
  msgs <- c(msgs, check_num(inputs$psd_quality, "PSD Quality", min_val = 0))
  msgs <- c(msgs, check_num(inputs$psd_preferred, "PSD Preferred", min_val = 0))
  msgs <- c(msgs, check_num(inputs$psd_memorable, "PSD Memorable", min_val = 0))
  msgs <- c(msgs, check_num(inputs$psd_trophy, "PSD Trophy", min_val = 0))
  if (is.numeric(inputs$psd_stock) && is.numeric(inputs$psd_quality) &&
      is.numeric(inputs$psd_preferred) && is.numeric(inputs$psd_memorable) &&
      is.numeric(inputs$psd_trophy)) {
    
    
    if (inputs$psd_stock >= inputs$psd_quality) {
      msgs <- c(msgs, paste0("❌ Error: PSD Stock (", inputs$psd_stock, ") must be smaller than Quality (", inputs$psd_quality, ").\n"))
    }
    if (inputs$psd_quality >= inputs$psd_preferred) {
      msgs <- c(msgs, paste0("❌ Error: PSD Quality (", inputs$psd_quality, ") must be smaller than Preferred (", inputs$psd_preferred, ").\n"))
    }
    if (inputs$psd_preferred >= inputs$psd_memorable) {
      msgs <- c(msgs, paste0("❌ Error: PSD Preferred (", inputs$psd_preferred, ") must be smaller than Memorable (", inputs$psd_memorable, ").\n"))
    }
    if (inputs$psd_memorable >= inputs$psd_trophy) {
      msgs <- c(msgs, paste0("❌ Error: PSD Memorable (", inputs$psd_memorable, ") must be smaller than Trophy (", inputs$psd_trophy, ").\n"))
    }
  }
  
  
  
  pass <- length(msgs) == 0
  
  
  final_msg <- if(pass) {
    "✅ Global Parameters Verified!\nAll inputs are valid and ready for simulation."
  } else {
    paste0("❌ Validation Failed:\n", paste(msgs, collapse = ""))
  }
  
  return(list(pass = pass, msg = final_msg))
}


# 3b. Run Control (Step 2: iterations, seed, parallel acceleration, fast-forward)

#' @title Check Run Control Inputs
#' @description Validates Step 2 run-control inputs: number of iterations,
#'   random seed, the three parallel-acceleration layers, and juvenile
#'   fast-forward. Produces an errors/pass result plus an informational
#'   thread-usage message.
#'
#' @param inputs A named list with:
#'   n_iter, seed,
#'   n_cores (layer 1: replicate parallelism),
#'   use_policy_parallel (logical), policy_threads (layer 2),
#'   use_large_pop (logical, layer 3 engine on/off), omp_threads (layer 3),
#'   engine_available (logical: is run_simulation_v2_cpp compiled?),
#'   openmp_available (logical), openmp_max (int, may be NA),
#'   fast_forward_mode ("auto"/"off"/"manual"), t_safe_manual, t_safe_auto,
#'   logical_cores (int, from parallel::detectCores()).
#' @return list(pass, msg, info_msg) where info_msg describes thread usage
#'   (either a simple cores line, or an oversubscription warning).
#' @export
check_runcontrol_inputs <- function(inputs) {
  msgs <- c()
  
  # --- 1. Iterations & Seed (moved here from Global Parameters) ---
  err <- check_num(inputs$n_iter, "Iterations", min_val = 1, is_int = TRUE)
  if (!is.null(err)) msgs <- c(msgs, err)
  
  err <- check_num(inputs$seed, "Seed", min_val = 1, is_int = TRUE)
  if (!is.null(err)) msgs <- c(msgs, err)
  
  # --- 2. Layer 1: Replicate parallelism (cores) ---
  err <- check_num(inputs$n_cores, "Parallel cores", min_val = 1, is_int = TRUE)
  if (!is.null(err)) msgs <- c(msgs, err)
  
  # --- 3. Layer 2: Policy parallelism ---
  use_policy <- isTRUE(inputs$use_policy_parallel)
  policy_threads <- 1L
  if (use_policy) {
    err <- check_num(inputs$policy_threads, "Policy-combo threads", min_val = 1, is_int = TRUE)
    if (!is.null(err)) {
      msgs <- c(msgs, err)
    } else {
      policy_threads <- max(1L, as.integer(inputs$policy_threads))
    }
  }
  
  # --- 4. Layer 3: Individual parallelism (large-population engine) ---
  use_large_pop <- isTRUE(inputs$use_large_pop)
  omp_threads <- 1L
  if (use_large_pop) {
    # Engine must actually be compiled in
    if (!isTRUE(inputs$engine_available)) {
      msgs <- c(msgs, paste0(
        "\u274c The large-population optimized method is selected, but the compiled ",
        "engine (run_simulation_v2_cpp) is not available in this package build.\n",
        "   Rebuild the package with the v2 engine, or choose the Standard method.\n"))
    }
    err <- check_num(inputs$omp_threads, "Individual-level parallel threads", min_val = 1, is_int = TRUE)
    if (!is.null(err)) {
      msgs <- c(msgs, err)
    } else {
      omp_threads <- max(1L, as.integer(inputs$omp_threads))
    }
    # If OpenMP isn't available, more than 1 thread has no effect (warn, not error)
    if (omp_threads > 1 && !isTRUE(inputs$openmp_available)) {
      msgs <- c(msgs, paste0(
        "\u26a0\ufe0f OpenMP is not enabled in this build; individual-level threads > 1 ",
        "will fall back to a single thread (no error, just no speed-up).\n"))
    }
  }
  
  # --- 5. Juvenile fast-forward ---
  ff_mode <- if (is.null(inputs$fast_forward_mode)) "auto" else inputs$fast_forward_mode
  if (identical(ff_mode, "manual")) {
    err <- check_num(inputs$t_safe_manual, "Fast-forward months", min_val = 0, is_int = TRUE)
    if (!is.null(err)) {
      msgs <- c(msgs, err)
    } else {
      manual_v <- as.integer(inputs$t_safe_manual)
      auto_v <- if (!is.null(inputs$t_safe_auto)) as.integer(inputs$t_safe_auto) else NA_integer_
      # Manual value must not exceed the automatic safe upper bound
      if (!is.na(auto_v) && manual_v > auto_v) {
        msgs <- c(msgs, paste0(
          "\u274c Manual fast-forward months (", manual_v, ") exceed the automatic safe ",
          "upper bound (", auto_v, ").\n",
          "   Values above the safe bound would let juveniles enter the fishery or ",
          "reach adult age during fast-forward. Use ", auto_v, " or lower.\n"))
      }
    }
  }
  
  # --- 6. Thread-usage info (the key single-vs-multi logic) ---
  # In cloud mode the run happens on a rented machine, so comparing against the
  # cores of the laptop in front of the user would be misleading: someone with
  # an 8-core laptop who has rented a 32-core machine should not be told they
  # are four times over capacity. When a machine type is supplied its core
  # count is used instead.
  logical_cores <- inputs$logical_cores
  if (is.null(logical_cores) || is.na(logical_cores) || logical_cores < 1) logical_cores <- 4L

  cores_source <- "this machine"
  if (isTRUE(inputs$use_cloud)) {
    cloud_cores <- parse_machine_type_cores(inputs$cloud_machine_type)
    if (!is.na(cloud_cores) && cloud_cores >= 1L) {
      logical_cores <- cloud_cores
      cores_source <- paste0("the cloud machine (", inputs$cloud_machine_type, ")")
    } else {
      # Machine type not recognised: capacity cannot be judged, so say so
      # rather than comparing against the wrong number.
      cores_source <- NA_character_
    }
  }

  layer1 <- max(1L, as.integer(inputs$n_cores))
  layer2 <- policy_threads
  layer3 <- omp_threads
  total_threads <- layer1 * layer2 * layer3

  # How many layers are actually engaged (>1 thread)?
  active_layers <- sum(c(layer1 > 1, layer2 > 1, layer3 > 1))

  if (is.na(cores_source)) {
    info_msg <- paste0(
      "\u2139\ufe0f Cloud run: ", layer1, " replicate worker(s) \u00d7 ",
      layer2, " policy thread(s) \u00d7 ", layer3, " individual thread(s) = ",
      total_threads, " total threads.\n",
      "   The machine type could not be read, so capacity is not checked here. ",
      "Run the parallel performance check to measure the rented machine.")
    pass <- length(msgs) == 0
    final_msg <- if (pass) {
      paste0("\u2705 Run Control Verified!\n", info_msg)
    } else {
      paste0("\u274c Validation Failed:\n", paste(msgs, collapse = ""), "\n", info_msg)
    }
    return(list(pass = pass, msg = final_msg, info_msg = info_msg,
                total_threads = total_threads, logical_cores = NA_integer_,
                oversubscribed = NA))
  }

  over <- total_threads > logical_cores
  ratio <- total_threads / logical_cores
  
  if (active_layers <= 1) {
    # Only one acceleration path in use — no multiplication to show,
    # but still compare against the core count.
    used <- max(layer1, layer2, layer3)
    if (!over) {
      info_msg <- paste0(
        "\u2705 Using ", used, " thread(s) on a machine with ", logical_cores,
        " logical cores. Within capacity.")
    } else {
      info_msg <- paste0(
        "\u26a0\ufe0f Using ", used, " thread(s) on a machine with only ", logical_cores,
        " logical cores (", sprintf("%.1f", ratio), "x capacity).",
        "\n   The run is still allowed, but performance may drop once you exceed your ",
        "core count. Consider lowering it to ", logical_cores, " or fewer, then run the ",
        "Parallel performance check on the Test Simulation page.")
    }
  } else {
    # Multiple layers multiply — report the product and warn if over capacity.
    detail <- paste0(
      "Replicate cores (", layer1, ") \u00d7 Policy-combo threads (", layer2,
      ") \u00d7 Individual-level threads (", layer3, ") = ", total_threads,
      " total threads vs ", logical_cores, " logical cores.")
    if (!over) {
      info_msg <- paste0("\u2705 Thread usage within capacity. ", detail)
    } else {
      info_msg <- paste0(
        "\u26a0\ufe0f Oversubscription: ", sprintf("%.1f", ratio),
        "x your logical cores. ", detail,
        "\n   The run is still allowed, but performance may drop. Run the Test ",
        "Simulation page first to estimate speed; if unsatisfied, lower one of the three.")
    }
  }
  
  pass <- length(msgs) == 0
  final_msg <- if (pass) {
    paste0("\u2705 Run Control Verified!\n", info_msg)
  } else {
    paste0("\u274c Validation Failed:\n", paste(msgs, collapse = ""), "\n", info_msg)
  }
  
  return(list(pass = pass, msg = final_msg, info_msg = info_msg,
              total_threads = total_threads, logical_cores = logical_cores,
              oversubscribed = isTRUE(over)))
}



# 4. Design (Experiment Design)

#' @title Check Experiment Design Inputs
#' @description Validates experiment design CSV and vectors.
#' @export
check_design_inputs <- function(file_obj, df, esd_str, pae_str, rm_str, breaks_str, probs_str, comp_mode) {
  msgs <- c()
  
  # --- 1. CSV ---
  if (is.null(df) || nrow(df) == 0L) {
    
    msgs <- c(
      msgs,
      "❌ Error: Upload or restore a Size limit CSV.\n"
    )
    
  } else {
    
    if (
      !is.null(file_obj) &&
      !is.null(file_obj$name) &&
      nzchar(file_obj$name)
    ) {
      ext_err <- check_csv_ext(file_obj$name)
      
      if (!is.null(ext_err)) {
        msgs <- c(msgs, ext_err)
      }
    }
    
    req_cols <- c(
      "scenario_name",
      "min_len_mm",
      "max_len_mm"
    )
    
    missing <- setdiff(req_cols, names(df))
    
    if (length(missing) > 0L) {
      
      msgs <- c(
        msgs,
        paste0(
          "❌ Error: CSV missing columns: ",
          paste(missing, collapse = ", "),
          ".\n"
        )
      )
      
    } else {
      
      if (!is.numeric(df$min_len_mm)) {
        msgs <- c(
          msgs,
          "❌ Error: 'min_len_mm' must be numeric.\n"
        )
      }
      
      if (!is.numeric(df$max_len_mm)) {
        msgs <- c(
          msgs,
          "❌ Error: 'max_len_mm' must be numeric.\n"
        )
      }
      
      if (
        is.numeric(df$min_len_mm) &&
        any(df$min_len_mm < 0, na.rm = TRUE)
      ) {
        msgs <- c(
          msgs,
          "❌ Error: 'min_len_mm' cannot be negative.\n"
        )
      }
    }
  }
  
  # --- 2. (ESD, PAE, RM) ---
  # ESD (>=0)
  chk_esd <- parse_and_check_vec(esd_str, "ESD", min_val = 0)
  if (!chk_esd$valid) msgs <- c(msgs, chk_esd$msg)
  
  # PAE (0-1)
  chk_pae <- parse_and_check_vec(pae_str, "PAE", min_val = 0, max_val = 1)
  if (!chk_pae$valid) msgs <- c(msgs, chk_pae$msg)
  
  # RM (0-1)
  chk_rm <- parse_and_check_vec(rm_str, "Release Mortality", min_val = 0, max_val = 1)
  if (!chk_rm$valid) msgs <- c(msgs, chk_rm$msg)
  
  # --- 3. Compliance () ---
  if (is.null(comp_mode)) {
    msgs <- c(msgs, "❌ Error: Please select at least one 'Compliance Mode'.\n")
  }
  
  chk_brk <- parse_and_check_vec(breaks_str, "Breakpoints", min_val = 0)
  chk_prb <- parse_and_check_vec(probs_str, "Compliance Probs", min_val = 0, max_val = 1)
  
  if (!chk_brk$valid) msgs <- c(msgs, chk_brk$msg)
  if (!chk_prb$valid) msgs <- c(msgs, chk_prb$msg)
  
  # ：, Breakpoint 0
  if (chk_brk$valid && chk_prb$valid) {
    if (length(chk_brk$nums) != length(chk_prb$nums)) {
      msgs <- c(msgs, paste0("❌ Error: Breakpoints count (", length(chk_brk$nums), ") != Probs count (", length(chk_prb$nums), "). They must match.\n"))
    }
    if (chk_brk$nums[1] != 0) {
      msgs <- c(msgs, "❌ Error: Breakpoints must start with 0 (e.g., 0,254).\n")
    }
    
    if (is.unsorted(chk_brk$nums)) {
      msgs <- c(msgs, "❌ Error: Breakpoints must be strictly increasing.\n")
    }
  }
  
  pass <- length(msgs) == 0
  final_msg <- if(pass) "✅ Design Parameters Verified! Jumping to Preview..." else paste0("❌ Validation Failed:\n", paste(msgs, collapse = ""))
  
  return(list(pass = pass, msg = final_msg))
}


# 5.Results Data

#' @title Check Results Directory
#' @description Validates that a results directory contains expected files.
#' @export
check_results_data <- function(dir_path) {
  
  if (!dir.exists(dir_path)) {
    return(list(pass = FALSE, msg = "❌ Error: Directory does not exist.\n"))
  }
  
  
  subdirs <- list.dirs(dir_path, full.names = TRUE, recursive = FALSE)
  if (length(subdirs) == 0) {
    return(list(pass = FALSE, msg = "❌ Error: No subfolders found in this directory.\n"))
  }
  
  # 3. ( scenario_info.csv)
  valid_count <- 0
  for (d in subdirs) {
    if (file.exists(file.path(d, "scenario_info.csv"))) valid_count <- valid_count + 1
  }
  
  if (valid_count == 0) {
    return(list(pass = FALSE, msg = "❌ Error: No valid simulation folders (missing scenario_info.csv).\n"))
  }
  
  return(list(pass = TRUE, msg = paste0("✅ Results Loaded! Found ", valid_count, " valid scenarios.\n")))
}

#' @title Check Whether a Catch Curve Can Be Fitted
#' @description A catch curve regresses log abundance on age along the
#'   descending limb, so it needs several age classes that actually contain
#'   fish. Data with only one or two usable ages cannot support the regression
#'   or its bootstrap. The simulation itself can still run in that situation,
#'   but adult mortality has to be supplied directly instead of estimated.
#' @param alk_df Age-length key data, with 'Age' and 'n' columns.
#' @param full Transition age: the first age on the descending limb.
#' @param last Maximum age used by the catch curve.
#' @param min_ages Minimum number of usable age classes. Defaults to 3, which
#'   leaves one residual degree of freedom for the regression.
#' @return A list with pass, msg and n_ages.
#' @export
check_catch_curve_data <- function(alk_df, full, last, min_ages = 3L) {
  
  if (is.null(alk_df) || !is.data.frame(alk_df) || nrow(alk_df) == 0L) {
    return(list(pass = FALSE, n_ages = 0L,
                msg = "\u274c Error: No age-length key data is available yet.\n"))
  }
  
  if (!all(c("Age", "n") %in% names(alk_df))) {
    return(list(pass = FALSE, n_ages = 0L,
                msg = "\u274c Error: Age-length key data must contain 'Age' and 'n'.\n"))
  }
  
  age <- suppressWarnings(as.numeric(alk_df$Age))
  num <- suppressWarnings(as.numeric(alk_df$n))
  
  full_v <- suppressWarnings(as.numeric(full))
  last_v <- suppressWarnings(as.numeric(last))
  if (length(full_v) == 0L || is.na(full_v)) full_v <- -Inf
  if (length(last_v) == 0L || is.na(last_v)) last_v <- Inf
  
  usable <- is.finite(age) & is.finite(num) & num > 0 &
    age >= full_v & age <= last_v
  n_ages <- length(unique(age[usable]))
  
  if (n_ages < min_ages) {
    return(list(
      pass = FALSE,
      n_ages = n_ages,
      msg = paste0(
        "\u274c Not enough data to estimate Z from a catch curve.\n",
        "Only ", n_ages, " age class(es) with fish fall between the Transition Age (",
        if (is.finite(full_v)) full_v else "not set",
        ") and the Catch Curve Max Age (",
        if (is.finite(last_v)) last_v else "not set",
        "). At least ", min_ages,
        " are needed to fit the descending limb.\n",
        "The simulation can still run: switch off catch-curve estimation and ",
        "enter adult mortality directly with 'Fixed Adult Annual M'.\n"
      )
    ))
  }
  
  list(
    pass = TRUE,
    n_ages = n_ages,
    msg = paste0("\u2705 Catch curve data check passed (", n_ages, " age classes).\n")
  )
}

#' @title Read the Core Count from a Machine Type
#' @description Google machine type names end with the number of virtual CPUs,
#'   for example \code{n2-highmem-32}. Reading that number lets the run-control
#'   check judge capacity against the machine that will actually do the work.
#'   Shared-core types such as \code{e2-micro} carry no number and are treated
#'   as a single core.
#' @param machine_type A machine type string.
#' @return The number of virtual CPUs, or \code{NA_integer_} if it cannot be read.
#' @export
parse_machine_type_cores <- function(machine_type) {
  if (is.null(machine_type) || !nzchar(as.character(machine_type))) {
    return(NA_integer_)
  }
  mt <- trimws(tolower(as.character(machine_type)))

  if (grepl("^(e2|f1|g1)-(micro|small|medium)$", mt)) return(1L)

  # Custom machine types end in both vCPU and memory, for example
  # n2-custom-8-32768. The vCPU count is the number after "custom", not the
  # final memory value.
  if (grepl("-custom-[0-9]+-[0-9]+$", mt)) {
    n <- suppressWarnings(as.integer(
      sub("^.*-custom-([0-9]+)-[0-9]+$", "\\1", mt)
    ))
    if (!is.na(n) && n >= 1L) return(n)
  }

  m <- regmatches(mt, regexpr("[0-9]+$", mt))
  if (length(m) == 0L || !nzchar(m)) return(NA_integer_)

  n <- suppressWarnings(as.integer(m))
  if (is.na(n) || n < 1L) return(NA_integer_)
  n
}


#' @title Check the Cloud Settings Before Submitting a Job
#' @description Confirms that everything needed to reach the user's own Google
#'   Cloud project has been supplied, and that the entries look plausible, so
#'   mistakes are caught before a job is queued and charged for.
#' @param key_path Path to the uploaded service-account key.
#' @param project Project identifier.
#' @param region Region such as \code{us-central1}.
#' @param bucket Bucket name.
#' @param machine_type Machine type string.
#' @param image Public GHCR container image, including a tag or digest.
#' @return A list with \code{pass} and a message.
#' @export
check_cloud_inputs <- function(key_path, project, region, bucket, machine_type,
                               image) {

  msgs <- character(0)
  blank <- function(x) is.null(x) || !nzchar(trimws(as.character(x)))

  if (blank(key_path)) {
    msgs <- c(msgs, "\u274c Upload a Google Cloud service-account key (.json).\n")
  } else if (!file.exists(key_path)) {
    msgs <- c(msgs, "\u274c The service-account key file could not be found.\n")
  }

  if (blank(project)) {
    msgs <- c(msgs, "\u274c Enter your Google Cloud project ID.\n")
  } else if (!grepl("^[a-z][a-z0-9-]{4,28}[a-z0-9]$", trimws(tolower(project)))) {
    msgs <- c(msgs, paste0(
      "\u274c '", project, "' does not look like a project ID. ",
      "Project IDs are 6-30 characters, lower case, and may contain digits and hyphens.\n"))
  }

  if (blank(region)) {
    msgs <- c(msgs, "\u274c Enter the region to run in, for example us-central1.\n")
  } else if (!grepl("^[a-z]+-[a-z]+[0-9]$", trimws(tolower(region)))) {
    msgs <- c(msgs, paste0(
      "\u274c '", region, "' does not look like a region. ",
      "Regions look like us-central1 or europe-west4.\n"))
  }

  if (blank(bucket)) {
    msgs <- c(msgs, "\u274c Enter the Cloud Storage bucket to use.\n")
  } else {
    b <- trimws(tolower(bucket))
    if (grepl("^gs://", b)) {
      msgs <- c(msgs, "\u274c Enter the bucket name only, without the gs:// prefix.\n")
    } else if (!grepl("^[a-z0-9][a-z0-9._-]{1,61}[a-z0-9]$", b)) {
      msgs <- c(msgs, paste0("\u274c '", bucket, "' is not a valid bucket name.\n"))
    }
  }

  if (blank(machine_type)) {
    msgs <- c(msgs, paste0(
      "\u274c Enter the machine type to rent, for example n2-highmem-8. ",
      "Choose one in the Google Cloud console to match your memory needs.\n"))
  } else if (is.na(parse_machine_type_cores(machine_type))) {
    msgs <- c(msgs, paste0(
      "\u26a0\ufe0f '", machine_type, "' was not recognised. ",
      "The run can still be submitted, but capacity cannot be checked beforehand.\n"))
  }

  if (blank(image)) {
    msgs <- c(msgs, paste0(
      "\u274c Enter the public GHCR image produced by this package's GitHub ",
      "workflow, for example ghcr.io/your-name/craibm:latest.\n"))
  } else if (!grepl(
    "^ghcr\\.io/[a-z0-9._-]+/[a-z0-9._-]+(:[A-Za-z0-9._-]+|@sha256:[a-f0-9]{64})$",
    trimws(image),
    perl = TRUE
  )) {
    msgs <- c(msgs, paste0(
      "\u274c '", image, "' is not a valid tagged GHCR image. ",
      "Use ghcr.io/owner/repository:tag and keep owner/repository lower case.\n"))
  }

  hard_errors <- grep("^\u274c", msgs, value = TRUE)
  pass <- length(hard_errors) == 0

  if (pass && length(msgs) == 0) {
    cores <- parse_machine_type_cores(machine_type)
    return(list(pass = TRUE, msg = paste0(
      "\u2705 Cloud settings look complete (", machine_type,
      if (!is.na(cores)) paste0(", ", cores, " vCPU") else "",
      ", region ", region, ").\n",
      "Container: ", image, "\n")))
  }

  list(pass = pass, msg = paste(msgs, collapse = ""))
}
