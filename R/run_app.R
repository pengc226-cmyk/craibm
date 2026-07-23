#' @title Launch the Fish IBM Shiny Application
#' @description Starts the interactive Shiny application for parameterizing,
#'   running, and visualizing sportfish IBM simulations.
#' @param ... Additional arguments passed to \code{shiny::runApp}.
#' @param launch.browser Logical or function controlling whether and how the
#'   application is opened in a browser. Defaults to \code{TRUE}. Set to
#'   \code{FALSE} for server or headless use.
#'
#' @export
run_app <- function(
    ...,
    launch.browser = TRUE
) {
  
  app_dir <- system.file(
    "app",
    package = "craibm"
  )
  
  if (!nzchar(app_dir)) {
    stop(
      paste0(
        "Could not find the CraIBM application directory. ",
        "Please reinstall the craibm package."
      ),
      call. = FALSE
    )
  }
  
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    ...
  )
}

#' @export run_simulation_sizelimit_cpp
#' @export run_simulation_gpu
#' @export run_simulation_hybrid
#' @export run_simulation_v2_cpp
#' @export detect_gpu_info
#' @export detect_openmp_info
NULL

