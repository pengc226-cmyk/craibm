#!/usr/bin/env Rscript
# ==============================================================================
# craibm cloud entry point
#
# Runs inside the container started by Cloud Batch. It collects its
# instructions from environment variables, fetches the payload the application
# uploaded, runs the requested work, and reports back through the same bucket.
#
# Two habits matter here. Progress is published regularly so the application
# can show how far the run has got, and finished results are copied to the
# bucket as they appear rather than only at the end. A machine that is lost
# part way through therefore still leaves its completed replicates behind.
# ==============================================================================

suppressWarnings(suppressMessages({
  library(craibm)
  library(jsonlite)
}))

BUCKET    <- Sys.getenv("CRAIBM_BUCKET")
JOB_ID    <- Sys.getenv("CRAIBM_JOB_ID")
TASK_TYPE <- Sys.getenv("CRAIBM_TASK_TYPE", "full")

if (!nzchar(BUCKET) || !nzchar(JOB_ID)) {
  stop("CRAIBM_BUCKET and CRAIBM_JOB_ID must be set.")
}

ROOT      <- paste0("gs://", BUCKET, "/jobs/", JOB_ID)
WORK_DIR  <- "/workspace/run"
OUT_DIR   <- file.path(WORK_DIR, "out")
PROG_LOG  <- file.path(WORK_DIR, "progress.log")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

t_container_start <- Sys.time()

# ---- Small wrappers around the storage tool ---------------------------------

gs_copy <- function(from, to, recursive = FALSE) {
  args <- c("-q", "cp")
  if (recursive) args <- c(args, "-r")
  status <- system2("gsutil", c(args, from, to),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(status), 0L)) {
    stop(sprintf("gsutil copy failed (%s -> %s; status %s).",
                 from, to, status), call. = FALSE)
  }
  invisible(TRUE)
}

gs_sync <- function(from, to) {
  status <- system2("gsutil", c("-q", "-m", "rsync", "-r", from, to),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(status), 0L)) {
    stop(sprintf("gsutil sync failed (%s -> %s; status %s).",
                 from, to, status), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Progress reporting ------------------------------------------------------

publish_progress <- function(status, done = NA, total = NA, phase = NA,
                             message = NA, report = NA, error = NA,
                             startup_sec = NA, compute_sec = NA) {
  payload <- list(
    status      = status,
    phase       = phase,
    done        = if (is.na(done)) NULL else as.integer(done),
    total       = if (is.na(total)) NULL else as.integer(total),
    message     = if (is.na(message)) NULL else message,
    report      = if (is.na(report)) NULL else report,
    error       = if (is.na(error)) NULL else error,
    startup_sec = if (is.na(startup_sec)) NULL else round(startup_sec, 2),
    compute_sec = if (is.na(compute_sec)) NULL else round(compute_sec, 2),
    updated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  tmp <- file.path(WORK_DIR, "progress.json")
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"), tmp)
  gs_copy(tmp, paste0(ROOT, "/progress.json"))
}

fail <- function(msg) {
  publish_progress("failed", error = msg)
  # Keep whatever was produced before the failure.
  try(gs_sync(OUT_DIR, paste0(ROOT, "/partial")), silent = TRUE)
  quit(status = 1L)
}

publish_progress("starting", phase = "preparing")

# ---- Fetch the payload -------------------------------------------------------

payload_local <- file.path(WORK_DIR, "payload.rds")
download_error <- tryCatch({
  gs_copy(paste0(ROOT, "/payload.rds"), payload_local)
  NULL
}, error = function(e) conditionMessage(e))

if (!is.null(download_error) || !file.exists(payload_local)) {
  fail(paste0(
    "The payload could not be downloaded from Cloud Storage.",
    if (!is.null(download_error)) paste0(" ", download_error) else ""
  ))
}

payload <- tryCatch(readRDS(payload_local),
                    error = function(e) NULL)
if (is.null(payload)) fail("The payload could not be read.")

startup_sec <- as.numeric(difftime(Sys.time(), t_container_start, units = "secs"))

all_params <- payload$all_params
cpp_scen   <- payload$cpp_scen
cpp_pol    <- payload$cpp_pol_df

# ==============================================================================
# Model validation: a single replicate, reported with the setup cost separated
# from the computation itself so the machine can be judged fairly.
# ==============================================================================

if (identical(TASK_TYPE, "validation")) {

  publish_progress("running", phase = "validation", done = 0L, total = 1L,
                   startup_sec = startup_sec)

  t0 <- Sys.time()
  result <- tryCatch(
    run_selected_cpp(all_params, cpp_scen, cpp_pol, rep_id = 1L),
    error = function(e) e
  )
  if (inherits(result, "error")) fail(paste("Validation run failed:", conditionMessage(result)))

  compute_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  saveRDS(result, file.path(OUT_DIR, "validation_result.rds"))

  zip_path <- file.path(WORK_DIR, "results.zip")
  old <- setwd(OUT_DIR); on.exit(setwd(old), add = TRUE)
  utils::zip(zip_path, files = list.files(".", recursive = TRUE), flags = "-r9Xq")
  setwd(old)
  gs_copy(zip_path, paste0(ROOT, "/results.zip"))

  publish_progress("done", phase = "validation", done = 1L, total = 1L,
                   startup_sec = startup_sec, compute_sec = compute_sec,
                   message = "Model validation finished on the cloud machine.")
  quit(status = 0L)
}

# ==============================================================================
# Parallel performance check: measures contention and memory on this machine,
# which is the only place those numbers mean anything for a cloud run.
# ==============================================================================

if (identical(TASK_TYPE, "perfcheck")) {

  publish_progress("running", phase = "perfcheck", startup_sec = startup_sec)

  requested <- as.integer(payload$requested_workers)
  if (is.na(requested) || requested < 1L) requested <- 1L
  probe <- as.integer(payload$probe_workers)
  if (is.na(probe) || probe < 1L) probe <- min(requested, 4L)

  logical_cores <- parallel::detectCores(logical = TRUE)
  if (is.na(logical_cores)) logical_cores <- 1L

  run_one <- function(rep_id) {
    run_selected_cpp(all_params, cpp_scen, cpp_pol, rep_id = rep_id)
    invisible(NULL)
  }

  setup_cluster <- function(cl) {
    parallel::clusterEvalQ(cl, suppressMessages(library(craibm)))
    parallel::clusterExport(cl, varlist = c("all_params", "cpp_scen", "cpp_pol"),
                            envir = environment())
  }

  t0 <- Sys.time()
  res <- tryCatch(
    run_oversubscription_test(
      run_one_fn        = run_one,
      n_cores           = probe,
      requested_workers = requested,
      combo_threads     = all_params$execution$combo_threads,
      omp_threads       = all_params$execution$omp_nthreads,
      cluster_setup_fn  = setup_cluster,
      logical_cores     = logical_cores,
      total_tasks       = as.integer(payload$total_tasks),
      mem_abort_frac    = 0.95
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) fail(paste("Performance check failed:", conditionMessage(res)))

  compute_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  saveRDS(res, file.path(OUT_DIR, "perfcheck_result.rds"))

  gb <- function(mb) if (is.null(mb) || is.na(mb)) "n/a" else sprintf("%.2f GB", mb / 1024)
  report <- paste0(
    "Machine: ", logical_cores, " logical cores\n",
    "Status: ", res$status, "\n",
    "Memory per active worker: ", gb(res$per_worker_mb), "\n",
    "Projected peak: ", gb(res$projected_mem), "\n",
    "Total memory: ", gb(res$system_ram_mb), "\n",
    "Oversubscription factor: ",
    if (is.null(res$oversub_factor) || is.na(res$oversub_factor)) "n/a"
    else sprintf("%.2fx", res$oversub_factor)
  )

  zip_path <- file.path(WORK_DIR, "results.zip")
  old <- setwd(OUT_DIR); on.exit(setwd(old), add = TRUE)
  utils::zip(zip_path, files = list.files(".", recursive = TRUE), flags = "-r9Xq")
  setwd(old)
  gs_copy(zip_path, paste0(ROOT, "/results.zip"))

  publish_progress("done", phase = "perfcheck",
                   startup_sec = startup_sec, compute_sec = compute_sec,
                   report = report,
                   message = "Performance check finished on the cloud machine.")
  quit(status = 0L)
}

# ==============================================================================
# Full simulation
# ==============================================================================

worker_packets <- payload$worker_packets
total_tasks    <- as.integer(payload$total_tasks_count)
n_workers      <- as.integer(payload$actual_cores)
scenarios_df   <- payload$scenarios_df
policy_logic   <- payload$policy_logic
burnin_rm      <- payload$burnin_rm

if (is.null(worker_packets) || length(worker_packets) == 0L) {
  fail("The payload contained no work to do.")
}
if (is.na(n_workers) || n_workers < 1L) n_workers <- 1L
n_workers <- min(n_workers, length(worker_packets))

if (!isTRUE(file.create(PROG_LOG))) {
  fail("The container could not create its progress log.")
}
publish_progress("running", phase = "simulation", done = 0L, total = total_tasks,
                 startup_sec = startup_sec)

t_compute_start <- Sys.time()

# The watchdog runs beside the simulation. It counts the lines the workers have
# appended, republishes progress, and copies newly finished results to the
# bucket so an interrupted run still leaves its completed work behind.
watchdog <- parallel::mcparallel({
  repeat {
    Sys.sleep(90)
    lines <- tryCatch(readLines(PROG_LOG, warn = FALSE),
                      error = function(e) character(0))
    done <- sum(startsWith(lines, "done "))
    failed <- sum(startsWith(lines, "failed "))
    payload_json <- list(
      status      = "running",
      phase       = "simulation",
      done        = as.integer(done),
      total       = as.integer(total_tasks),
      message     = if (failed > 0L) {
        paste(failed, "task(s) have failed; completed outputs are being preserved.")
      } else {
        NULL
      },
      startup_sec = round(startup_sec, 2),
      compute_sec = round(as.numeric(difftime(Sys.time(), t_compute_start,
                                              units = "secs")), 2),
      updated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    tmp <- file.path(WORK_DIR, "progress_wd.json")
    writeLines(jsonlite::toJSON(payload_json, auto_unbox = TRUE, null = "null"), tmp)
    system2("gsutil", c("-q", "cp", tmp, paste0(ROOT, "/progress.json")),
            stdout = FALSE, stderr = FALSE)
    system2("gsutil", c("-q", "-m", "rsync", "-r", OUT_DIR,
                        paste0(ROOT, "/partial")),
            stdout = FALSE, stderr = FALSE)
  }
})

cl <- parallel::makeCluster(n_workers)

ok <- tryCatch({
  parallel::clusterEvalQ(cl, suppressMessages({
    library(craibm)
    library(dplyr)
    library(data.table)
  }))
  parallel::clusterExport(
    cl,
    varlist = c("scenarios_df", "policy_logic", "all_params",
                "burnin_rm", "OUT_DIR", "PROG_LOG"),
    envir = environment()
  )

  worker_failures <- parallel::parLapply(cl, worker_packets, function(packet) {
    packet_failures <- character(0)
    for (task in packet) {
      task_info <- list(
        sidx = task$sidx,
        iter_i = task$iter_i,
        burnin_rm_val = burnin_rm
      )
      task_error <- tryCatch({
        run_whole_scenario_job_shiny(
          task_info           = task_info,
          scenarios_df        = scenarios_df,
          policy_combos_logic = policy_logic,
          all_params          = all_params,
          out_dir_base        = OUT_DIR,
          cpp_abs_path        = NULL
        )
        NULL
      },
        error = function(e) {
          conditionMessage(e)
        }
      )

      if (is.null(task_error)) {
        # Only successful tasks count as done.
        cat(sprintf("done %s %s\n", task$sidx, task$iter_i),
            file = PROG_LOG, append = TRUE)
      } else {
        clean_error <- gsub("[\r\n]+", " ", task_error)
        failure <- sprintf("scenario=%s iter=%s: %s",
                           task$sidx, task$iter_i, clean_error)
        packet_failures <- c(packet_failures, failure)
        cat(sprintf("failed %s %s %s\n",
                    task$sidx, task$iter_i, clean_error),
            file = PROG_LOG, append = TRUE)
      }
    }
    packet_failures
  })

  worker_failures <- unlist(worker_failures, use.names = FALSE)
  if (length(worker_failures) > 0L) {
    preview <- paste(utils::head(worker_failures, 10L), collapse = "; ")
    if (length(worker_failures) > 10L) {
      preview <- paste0(preview, "; ...")
    }
    stop(sprintf("%d simulation task(s) failed: %s",
                 length(worker_failures), preview), call. = FALSE)
  }
  TRUE
}, error = function(e) {
  conditionMessage(e)
})

try(parallel::stopCluster(cl), silent = TRUE)
try(tools::pskill(watchdog$pid), silent = TRUE)

if (!isTRUE(ok)) fail(paste("The simulation failed:", ok))

compute_sec <- as.numeric(difftime(Sys.time(), t_compute_start, units = "secs"))
done <- tryCatch({
  lines <- readLines(PROG_LOG, warn = FALSE)
  sum(startsWith(lines, "done "))
}, error = function(e) NA_integer_)

# ---- Package everything up ---------------------------------------------------

publish_progress("packaging", phase = "packaging", done = done, total = total_tasks,
                 startup_sec = startup_sec, compute_sec = compute_sec)

if (!is.null(payload$settings_rds)) {
  saveRDS(payload$settings_rds,
          file.path(OUT_DIR, paste0("work data saved on ",
                                    format(Sys.time(), "%Y%m%d_%H%M"), ".rds")))
}

zip_path <- file.path(WORK_DIR, "results.zip")
old <- setwd(OUT_DIR)
utils::zip(zip_path, files = list.files(".", recursive = TRUE), flags = "-r9Xq")
setwd(old)

gs_copy(zip_path, paste0(ROOT, "/results.zip"))
gs_sync(OUT_DIR, paste0(ROOT, "/partial"))

publish_progress("done", phase = "finished", done = done, total = total_tasks,
                 startup_sec = startup_sec, compute_sec = compute_sec,
                 message = paste0("Simulation finished: ", done, " of ",
                                  total_tasks, " runs completed."))
quit(status = 0L)
