# ==============================================================================
# Google Cloud execution
#
# The Shiny application always runs locally. These helpers push a prepared
# simulation payload to the user's own Google Cloud project, start a Cloud
# Batch job that runs it inside a prebuilt container, follow its progress, and
# bring the results back.
#
# Authentication uses a service-account key supplied by the user. Requests go
# straight to the Google Cloud REST APIs so the package does not depend on the
# heavier Google client libraries.
# ==============================================================================


# ---- Internal helpers --------------------------------------------------------

#' @keywords internal
.cloud_gcs_base <- "https://storage.googleapis.com"

#' @keywords internal
.cloud_batch_base <- "https://batch.googleapis.com/v1"

#' @title Build a Job Identifier
#' @description Creates an identifier that is also used as the folder prefix
#'   inside the bucket, so every run keeps its payload, progress file, partial
#'   results and final archive together.
#' @param prefix Short label placed at the front of the identifier.
#' @return A character identifier such as \code{craibm-20260722-1430-8412}.
#' @export
cloud_make_job_id <- function(prefix = "craibm") {
  sprintf(
    "%s-%s-%04d",
    prefix,
    format(Sys.time(), "%Y%m%d-%H%M"),
    sample.int(9999L, 1L)
  )
}


#' @title Paths Used Inside the Bucket
#' @description Central definition of where each artefact lives, so the R side
#'   and the container entry point cannot drift apart.
#' @param job_id Job identifier.
#' @return A named list of object paths.
#' @export
cloud_paths <- function(job_id) {
  root <- paste0("jobs/", job_id)
  list(
    root     = root,
    payload  = paste0(root, "/payload.rds"),
    progress = paste0(root, "/progress.json"),
    results  = paste0(root, "/results.zip"),
    partial  = paste0(root, "/partial"),
    logs     = paste0(root, "/logs")
  )
}


#' @title Human-Readable Result Location
#' @description The address shown to the user so they can retrieve results
#'   themselves if they close the application while a job is still running.
#' @param bucket Bucket name.
#' @param job_id Job identifier.
#' @return A \code{gs://} address.
#' @export
cloud_result_uri <- function(bucket, job_id) {
  paste0("gs://", bucket, "/", cloud_paths(job_id)$results)
}


# ---- Authentication ----------------------------------------------------------

#' @title Authenticate with a Service-Account Key
#' @description Exchanges a service-account key for a short-lived access token
#'   using the standard JWT bearer flow. The returned object records when the
#'   token expires so long-running jobs can refresh it.
#' @param json_path Path to the service-account key file.
#' @param lifetime_seconds Requested token lifetime. Google caps this at one hour.
#' @return A list with the token, its expiry time and the key contents.
#' @export
cloud_auth <- function(json_path, lifetime_seconds = 3600L) {
  
  for (pkg in c("httr", "jsonlite", "openssl")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required for cloud execution.", pkg))
    }
  }
  
  if (!file.exists(json_path)) {
    stop("The service-account key file could not be found.")
  }
  
  key <- tryCatch(
    jsonlite::fromJSON(json_path),
    error = function(e) stop("The service-account key is not valid JSON.")
  )
  
  needed <- c("client_email", "private_key", "token_uri")
  if (!all(needed %in% names(key))) {
    stop("This file does not look like a service-account key.")
  }
  
  now <- as.integer(Sys.time())
  scope <- paste(
    "https://www.googleapis.com/auth/cloud-platform",
    "https://www.googleapis.com/auth/devstorage.read_write"
  )
  
  b64url <- function(x) {
    s <- openssl::base64_encode(x)
    s <- gsub("\\+", "-", s)
    s <- gsub("/", "_", s)
    gsub("=+$", "", s)
  }
  
  header  <- b64url(charToRaw(jsonlite::toJSON(
    list(alg = "RS256", typ = "JWT"), auto_unbox = TRUE)))
  claim   <- b64url(charToRaw(jsonlite::toJSON(list(
    iss   = key$client_email,
    scope = scope,
    aud   = key$token_uri,
    exp   = now + lifetime_seconds,
    iat   = now
  ), auto_unbox = TRUE)))
  
  signing_input <- paste0(header, ".", claim)
  pk <- openssl::read_key(
    as.character(key$private_key),
    der = FALSE
  )
  sig <- b64url(openssl::signature_create(
    charToRaw(signing_input), hash = openssl::sha256, key = pk))
  
  resp <- httr::POST(
    key$token_uri,
    body = list(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion  = paste0(signing_input, ".", sig)
    ),
    encode = "form",
    httr::timeout(30)
  )
  
  if (httr::status_code(resp) != 200L) {
    stop(paste0(
      "Google rejected the service-account key (HTTP ",
      httr::status_code(resp), "). Check that the key is valid and that the ",
      "Batch and Cloud Storage APIs are enabled."
    ))
  }
  
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  
  list(
    token      = parsed$access_token,
    expires_at = Sys.time() + as.numeric(parsed$expires_in) - 60,
    json_path  = json_path,
    email      = key$client_email
  )
}


#' @title Refresh an Access Token When It Is About to Expire
#' @description Long jobs outlive a single token. Polling calls this first so a
#'   temporary network outage followed by an expired token does not look like a
#'   permanent failure.
#' @param auth An object from \code{cloud_auth()}.
#' @return A valid auth object, refreshed if necessary.
#' @export
cloud_refresh_auth <- function(auth) {
  if (is.null(auth) || is.null(auth$expires_at)) return(auth)
  if (Sys.time() < auth$expires_at) return(auth)
  cloud_auth(auth$json_path)
}


# ---- Cloud Storage -----------------------------------------------------------

#' @keywords internal
.cloud_gcs_upload <- function(auth, bucket, object, file_path,
                              content_type = "application/octet-stream") {
  url <- paste0(.cloud_gcs_base, "/upload/storage/v1/b/", bucket, "/o")
  resp <- httr::POST(
    url,
    query = list(uploadType = "media", name = object),
    httr::add_headers(Authorization = paste("Bearer", auth$token)),
    httr::content_type(content_type),
    body = httr::upload_file(file_path),
    httr::timeout(600)
  )
  if (!httr::status_code(resp) %in% c(200L, 201L)) {
    stop(paste0("Upload to Cloud Storage failed (HTTP ",
                httr::status_code(resp), ")."))
  }
  invisible(TRUE)
}


#' @keywords internal
.cloud_gcs_download <- function(auth, bucket, object, dest_path) {
  url <- paste0(.cloud_gcs_base, "/storage/v1/b/", bucket, "/o/",
                utils::URLencode(object, reserved = TRUE))
  resp <- httr::GET(
    url,
    query = list(alt = "media"),
    httr::add_headers(Authorization = paste("Bearer", auth$token)),
    httr::write_disk(dest_path, overwrite = TRUE),
    httr::timeout(600)
  )
  httr::status_code(resp)
}


#' @keywords internal
.cloud_gcs_read_text <- function(auth, bucket, object) {
  url <- paste0(.cloud_gcs_base, "/storage/v1/b/", bucket, "/o/",
                utils::URLencode(object, reserved = TRUE))
  resp <- httr::GET(
    url,
    query = list(alt = "media"),
    httr::add_headers(Authorization = paste("Bearer", auth$token)),
    httr::timeout(15)
  )
  if (httr::status_code(resp) != 200L) return(NULL)
  httr::content(resp, as = "text", encoding = "UTF-8")
}


#' @title Verify the Cloud Setup Before Submitting Work
#' @description Confirms that the credentials work and that the bucket is
#'   reachable, so problems surface immediately rather than after a job has been
#'   queued.
#' @param auth An object from \code{cloud_auth()}.
#' @param project Google Cloud project identifier.
#' @param region Batch region such as \code{us-central1}.
#' @param bucket Cloud Storage bucket name.
#' @return A list with \code{pass} and a message describing the outcome.
#' @export
cloud_check_setup <- function(auth, project, region, bucket) {
  
  out <- function(pass, msg, service_account = NULL) {
    list(pass = pass, msg = msg, service_account = service_account)
  }
  
  if (is.null(auth) || is.null(auth$token)) {
    return(out(FALSE, "Not authenticated. Upload a service-account key first."))
  }
  
  # List one object instead of reading bucket metadata. Storage Object Admin
  # includes storage.objects.list, but it does not include storage.buckets.get.
  # Using the object endpoint therefore checks exactly the permission the app
  # asks the user to grant.
  resp <- tryCatch(
    httr::GET(
      paste0(.cloud_gcs_base, "/storage/v1/b/",
             utils::URLencode(bucket, reserved = TRUE), "/o"),
      httr::add_headers(Authorization = paste("Bearer", auth$token)),
      query = list(maxResults = 1),
      httr::timeout(30)
    ),
    error = function(e) NULL
  )
  
  if (is.null(resp)) {
    return(out(FALSE, "Could not reach Cloud Storage. Check your connection."))
  }
  
  code <- httr::status_code(resp)
  if (code == 404L) {
    return(out(FALSE, paste0("Bucket '", bucket, "' does not exist.")))
  }
  if (code == 403L) {
    return(out(FALSE, paste0(
      "The service account (", auth$email, ") cannot access bucket '", bucket,
      "'. Grant it the Storage Object Admin role.")))
  }
  if (code != 200L) {
    return(out(FALSE, paste0("Cloud Storage returned HTTP ", code, ".")))
  }
  
  # A harmless list call confirms the Batch API is enabled and permitted.
  bresp <- tryCatch(
    httr::GET(
      paste0(.cloud_batch_base, "/projects/", project,
             "/locations/", region, "/jobs"),
      httr::add_headers(Authorization = paste("Bearer", auth$token)),
      query = list(pageSize = 1),
      httr::timeout(30)
    ),
    error = function(e) NULL
  )
  
  if (is.null(bresp)) {
    return(out(FALSE, "Could not reach the Batch API."))
  }
  bcode <- httr::status_code(bresp)
  if (bcode == 403L) {
    return(out(FALSE, paste0(
      "The Batch API is not enabled for project '", project,
      "', or the service account lacks the Batch Jobs Editor role.")))
  }
  if (bcode == 404L) {
    return(out(FALSE, paste0(
      "The Batch project or region was not found (project '", project,
      "', region '", region, "').")))
  }
  if (bcode != 200L) {
    return(out(FALSE, paste0("The Batch API returned HTTP ", bcode, ".")))
  }
  
  out(TRUE, paste0(
    "Connected. Bucket '", bucket, "' is reachable and the Batch API is ",
    "available for project '", project, "' in region '", region, "'."),
    service_account = auth$email
  )
}


#' @title Upload the Simulation Payload
#' @description Writes the payload to a temporary file and stores it in the
#'   bucket where the container will look for it.
#' @param auth An object from \code{cloud_auth()}.
#' @param bucket Bucket name.
#' @param job_id Job identifier.
#' @param payload A list holding everything the run needs.
#' @return Invisibly \code{TRUE}.
#' @export
cloud_upload_payload <- function(auth, bucket, job_id, payload) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(payload, tmp)
  .cloud_gcs_upload(auth, bucket, cloud_paths(job_id)$payload, tmp)
  invisible(TRUE)
}

# ---- Cloud Batch ------------------------------------------------------------

# Internal helper: convert an E2 machine type into Batch task resources.
# This function is not exported.
.cloud_task_resources <- function(machine_type) {
  
  mt <- trimws(tolower(as.character(machine_type)))
  
  # Memory per vCPU for the predefined machine families, in GiB. Batch performs
  # automatic task sizing only for a narrow set of types, so the resources are
  # stated explicitly instead, which leaves every family available.
  ratios <- c(
    "e2-standard"   = 4,    "e2-highcpu"   = 1,    "e2-highmem"   = 8,
    "n1-standard"   = 3.75, "n1-highcpu"   = 0.9,  "n1-highmem"   = 6.5,
    "n2-standard"   = 4,    "n2-highcpu"   = 1,    "n2-highmem"   = 8,
    "n2d-standard"  = 4,    "n2d-highcpu"  = 1,    "n2d-highmem"  = 8,
    "n4-standard"   = 4,    "n4-highcpu"   = 2,    "n4-highmem"   = 8,
    "c2-standard"   = 4,
    "c2d-standard"  = 4,    "c2d-highcpu"  = 2,    "c2d-highmem"  = 8,
    "c3-standard"   = 4,    "c3-highcpu"   = 2,    "c3-highmem"   = 8,
    "c3d-standard"  = 4,    "c3d-highcpu"  = 2,    "c3d-highmem"  = 8,
    "c4-standard"   = 4,    "c4-highcpu"   = 2,    "c4-highmem"   = 8,
    "t2d-standard"  = 4,    "t2a-standard" = 4,
    "m1-megamem"    = 14.9, "m1-ultramem"  = 24.1
  )
  
  parts <- regmatches(mt, regexec("^([a-z0-9]+)-([a-z]+)-([0-9]+)$", mt))[[1L]]
  
  # Shared-core and unrecognised names carry no size in the name. The job is
  # still submitted; Batch is simply left to place it.
  if (length(parts) != 4L) {
    return(NULL)
  }
  
  family <- paste0(parts[[2L]], "-", parts[[3L]])
  vcpus  <- as.integer(parts[[4L]])
  
  if (is.na(vcpus) || vcpus < 1L) {
    return(NULL)
  }
  
  per_cpu <- ratios[[family]]
  if (is.null(per_cpu) || is.na(per_cpu)) {
    # Family not in the table: still state the CPU request, which is what
    # takes the job off the automatic-sizing path.
    return(list(cpuMilli = as.character(vcpus * 1000L)))
  }
  
  # Batch reserves a little of the machine for its own agent, so asking for
  # every last mebibyte can leave a job permanently unscheduled.
  memory_mib <- floor(vcpus * per_cpu * 1024 * 0.90)
  
  list(
    cpuMilli  = as.character(vcpus * 1000L),
    memoryMib = as.character(as.integer(memory_mib))
  )
}

# Retained so that older calls keep working.
.cloud_e2_task_resources <- .cloud_task_resources

# ---- Cloud Batch -------------------------------------------------------------

#' @title Submit a Cloud Batch Job
#' @description Starts a single-task Batch job that runs the craibm container.
#'   The container reads its instructions from environment variables and takes
#'   everything else from the payload already in the bucket.
#' @param auth An object from \code{cloud_auth()}.
#' @param project Project identifier.
#' @param region Region such as \code{us-central1}.
#' @param bucket Bucket name.
#' @param job_id Job identifier.
#' @param machine_type Machine type string chosen by the user.
#' @param task_type One of \code{validation}, \code{perfcheck} or \code{full}.
#' @param image Container image to run.
#' @param worker_service_account Service account attached to the Batch VM.
#'   Defaults to the account represented by the uploaded JSON key so the
#'   container and the submitting application use the same explicitly
#'   configured identity rather than the Compute Engine default account.
#' @param max_run_seconds Upper bound on run time, after which Batch stops the job.
#' @return A list with the submitted job name.
#' @export
cloud_submit_batch <- function(
    auth,
    project,
    region,
    bucket,
    job_id,
    machine_type,
    task_type,
    image = "ghcr.io/craibm/craibm:latest",
    worker_service_account = NULL,
    max_run_seconds = 86400L
) {
  
  if (
    is.null(worker_service_account) ||
    length(worker_service_account) != 1L ||
    is.na(worker_service_account) ||
    !nzchar(trimws(as.character(worker_service_account)))
  ) {
    stop(
      "A Batch worker service account must be supplied.",
      call. = FALSE
    )
  }
  
  worker_service_account <- trimws(
    as.character(worker_service_account)
  )
  task_resources <- .cloud_task_resources(machine_type)
  
  task_spec <- list(
    runnables = list(list(
      container = list(
        imageUri = image,
        entrypoint = "/usr/local/bin/Rscript",
        commands = list("/opt/craibm/entrypoint.R")
      )
    )),
    environment = list(
      variables = list(
        CRAIBM_BUCKET    = bucket,
        CRAIBM_JOB_ID    = job_id,
        CRAIBM_TASK_TYPE = task_type
      )
    ),
    maxRunDuration = paste0(as.integer(max_run_seconds), "s")
  )
  
  # Only sent when the machine type could be sized. An empty field would be
  # serialised as a JSON null, which Batch rejects.
  if (!is.null(task_resources)) {
    task_spec$computeResource <- task_resources
  }
  
  body <- list(
    taskGroups = list(list(
      taskCount = 1L,
      taskSpec = task_spec
    )),
    allocationPolicy = list(
      instances = list(list(
        policy = list(machineType = machine_type)
      )),
      serviceAccount = list(
        email = worker_service_account,
        scopes = list("https://www.googleapis.com/auth/cloud-platform")
      )
    ),
    logsPolicy = list(destination = "CLOUD_LOGGING")
  )
  
  url <- paste0(.cloud_batch_base, "/projects/", project,
                "/locations/", region, "/jobs")
  
  resp <- httr::POST(
    url,
    query = list(jobId = job_id),
    httr::add_headers(Authorization = paste("Bearer", auth$token)),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    httr::content_type_json(),
    httr::timeout(60)
  )
  
  code <- httr::status_code(resp)
  if (!code %in% c(200L, 201L)) {
    detail <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                       error = function(e) "")
    stop(paste0("Could not start the Cloud Batch job (HTTP ", code, ").\n", detail))
  }
  
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  list(name = parsed$name, job_id = job_id)
}


#' @title Read the State of a Cloud Batch Job
#' @description Reports what Batch itself believes about the job. This is the
#'   authoritative signal for failure, as distinct from a job that is merely
#'   quiet.
#' @param auth An object from \code{cloud_auth()}.
#' @param project Project identifier.
#' @param region Region.
#' @param job_id Job identifier.
#' @return A list with \code{ok} and, when reachable, \code{state}.
#' @export
cloud_job_state <- function(auth, project, region, job_id) {
  url <- paste0(.cloud_batch_base, "/projects/", project,
                "/locations/", region, "/jobs/", job_id)
  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers(Authorization = paste("Bearer", auth$token)),
              httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp)) return(list(ok = FALSE, state = NA_character_))
  if (httr::status_code(resp) != 200L) {
    return(list(ok = FALSE, state = NA_character_,
                http = httr::status_code(resp)))
  }
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  state <- tryCatch(parsed$status$state, error = function(e) NA_character_)
  list(ok = TRUE, state = if (is.null(state)) NA_character_ else state)
}


#' @title Stop a Running Cloud Batch Job
#' @description Deletes the job, which stops the machines and therefore the
#'   charges. Results already synchronised to the bucket remain available.
#' @param auth An object from \code{cloud_auth()}.
#' @param project Project identifier.
#' @param region Region.
#' @param job_id Job identifier.
#' @return Invisibly \code{TRUE} when the request was accepted.
#' @export
cloud_cancel_job <- function(auth, project, region, job_id) {
  url <- paste0(.cloud_batch_base, "/projects/", project,
                "/locations/", region, "/jobs/", job_id)
  resp <- tryCatch(
    httr::DELETE(url,
                 httr::add_headers(Authorization = paste("Bearer", auth$token)),
                 httr::timeout(60)),
    error = function(e) NULL
  )
  if (is.null(resp)) stop("Could not reach the Batch API to stop the job.")
  code <- httr::status_code(resp)
  if (!code %in% c(200L, 202L, 204L, 404L)) {
    stop(paste0("The job could not be stopped (HTTP ", code, ")."))
  }
  invisible(TRUE)
}


# ---- Progress and results ----------------------------------------------------

#' @title Read the Progress File Written by the Container
#' @description Progress is reported through a small file in the bucket. The
#'   file carries a timestamp so a job that has stopped writing can be told
#'   apart from one that is simply between updates.
#' @param auth An object from \code{cloud_auth()}.
#' @param bucket Bucket name.
#' @param job_id Job identifier.
#' @param stale_after_seconds How long without an update counts as stalled.
#' @return A list describing progress, including \code{available} and \code{stale}.
#' @export
cloud_poll_progress <- function(auth, bucket, job_id,
                                stale_after_seconds = 360) {
  
  txt <- tryCatch(
    .cloud_gcs_read_text(auth, bucket, cloud_paths(job_id)$progress),
    error = function(e) NULL
  )
  
  if (is.null(txt) || !nzchar(txt)) {
    return(list(available = FALSE, status = "unknown",
                done = NA_integer_, total = NA_integer_, stale = FALSE))
  }
  
  parsed <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(parsed)) {
    return(list(available = FALSE, status = "unknown",
                done = NA_integer_, total = NA_integer_, stale = FALSE))
  }
  
  as_int <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
  
  updated <- tryCatch(
    as.POSIXct(parsed$updated_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    error = function(e) NA
  )
  stale <- FALSE
  if (!is.null(parsed$status) &&
      parsed$status %in% c("starting", "running", "packaging") &&
      !is.na(updated)) {
    stale <- as.numeric(difftime(Sys.time(), updated, units = "secs")) >
      stale_after_seconds
  }
  
  list(
    available   = TRUE,
    status      = if (is.null(parsed$status)) "unknown" else parsed$status,
    phase       = if (is.null(parsed$phase)) NA_character_ else parsed$phase,
    done        = as_int(parsed$done),
    total       = as_int(parsed$total),
    startup_sec = if (is.null(parsed$startup_sec)) NA_real_ else as.numeric(parsed$startup_sec),
    compute_sec = if (is.null(parsed$compute_sec)) NA_real_ else as.numeric(parsed$compute_sec),
    message     = if (is.null(parsed$message)) NA_character_ else parsed$message,
    report      = if (is.null(parsed$report)) NA_character_ else parsed$report,
    error       = if (is.null(parsed$error)) NA_character_ else parsed$error,
    updated_at  = updated,
    stale       = isTRUE(stale)
  )
}


#' @title Download and Unpack the Results
#' @description Retrieves the archive produced by the run and expands it into a
#'   local folder, from which the usual loading and plotting steps proceed.
#' @param auth An object from \code{cloud_auth()}.
#' @param bucket Bucket name.
#' @param job_id Job identifier.
#' @param dest_dir Folder to unpack into.
#' @return A list with \code{pass}, a message and the destination folder.
#' @export
cloud_download_results <- function(auth, bucket, job_id, dest_dir) {
  
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  zip_path <- file.path(dest_dir, paste0(job_id, "_results.zip"))
  code <- .cloud_gcs_download(auth, bucket, cloud_paths(job_id)$results, zip_path)
  
  if (code == 404L) {
    return(list(pass = FALSE, dir = dest_dir, msg = paste0(
      "No results archive was found for this job yet. If the run was stopped ",
      "early, partial results may still be available under ",
      "gs://", bucket, "/", cloud_paths(job_id)$partial)))
  }
  if (code != 200L) {
    return(list(pass = FALSE, dir = dest_dir,
                msg = paste0("Download failed (HTTP ", code, ").")))
  }
  
  ok <- tryCatch({
    utils::unzip(zip_path, exdir = dest_dir)
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok) {
    return(list(pass = FALSE, dir = dest_dir,
                msg = "The archive was downloaded but could not be unpacked."))
  }
  
  list(pass = TRUE, dir = dest_dir, msg = paste0(
    "Results downloaded and unpacked into: ", dest_dir))
}


#' @title Download Partial Results After an Interrupted Run
#' @description When a run is stopped or a machine is lost, the results that had
#'   already been synchronised remain in the bucket. This collects them.
#' @param auth An object from \code{cloud_auth()}.
#' @param bucket Bucket name.
#' @param job_id Job identifier.
#' @param dest_dir Folder to place the files in.
#' @return A list with \code{pass}, a message and the number of files retrieved.
#' @export
cloud_download_partial <- function(auth, bucket, job_id, dest_dir) {
  
  prefix <- paste0(cloud_paths(job_id)$partial, "/")
  url <- paste0(.cloud_gcs_base, "/storage/v1/b/", bucket, "/o")
  
  items <- list()
  page_token <- NULL
  repeat {
    query <- list(prefix = prefix)
    if (!is.null(page_token) && nzchar(page_token)) {
      query$pageToken <- page_token
    }
    
    resp <- tryCatch(
      httr::GET(url,
                query = query,
                httr::add_headers(Authorization = paste("Bearer", auth$token)),
                httr::timeout(60)),
      error = function(e) NULL
    )
    
    if (is.null(resp) || httr::status_code(resp) != 200L) {
      return(list(pass = FALSE, n = 0L,
                  msg = "Could not list the partial results."))
    }
    
    parsed <- httr::content(resp, as = "parsed", type = "application/json")
    if (!is.null(parsed$items)) items <- c(items, parsed$items)
    page_token <- parsed$nextPageToken
    if (is.null(page_token) || !nzchar(page_token)) break
  }
  
  if (is.null(items) || length(items) == 0L) {
    return(list(pass = FALSE, n = 0L,
                msg = "No partial results have been saved for this job."))
  }
  
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  n <- 0L
  for (it in items) {
    obj <- it$name
    rel <- sub(prefix, "", obj, fixed = TRUE)
    if (!nzchar(rel)) next
    dest <- file.path(dest_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (.cloud_gcs_download(auth, bucket, obj, dest) == 200L) n <- n + 1L
  }
  
  list(pass = n > 0L, n = n, msg = paste0(
    n, " partial result file(s) downloaded into: ", dest_dir))
}