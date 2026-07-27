
.check_craibm_runtime_dependencies <- function() {
  
  # Read the dependency requirements directly from the installed
  # craibm DESCRIPTION file.
  description <- utils::packageDescription("craibm")
  
  dependency_fields <- unlist(
    description[c("Depends", "Imports")],
    use.names = FALSE
  )
  
  dependency_fields <- dependency_fields[
    !is.na(dependency_fields) &
      nzchar(dependency_fields)
  ]
  
  dependency_entries <- trimws(
    unlist(
      strsplit(
        dependency_fields,
        split = ",",
        fixed = TRUE
      )
    )
  )
  
  dependency_entries <- dependency_entries[
    nzchar(dependency_entries)
  ]
  
  # Extract package names.
  package_names <- trimws(
    sub(
      "\\s*\\(.*$",
      "",
      dependency_entries
    )
  )
  
  # Extract version requirements such as ">= 1.8.1".
  version_requirements <- rep(
    NA_character_,
    length(dependency_entries)
  )
  
  has_version_requirement <- grepl(
    "\\(",
    dependency_entries
  )
  
  version_requirements[has_version_requirement] <- trimws(
    sub(
      "^.*\\(([^()]*)\\).*$",
      "\\1",
      dependency_entries[has_version_requirement]
    )
  )
  
  # R itself is handled separately by the package installer and cannot
  # be updated using install.packages().
  keep <- package_names != "R"
  
  package_names <- package_names[keep]
  version_requirements <- version_requirements[keep]
  
  problems <- character()
  packages_to_install <- character()
  
  for (i in seq_along(package_names)) {
    
    pkg <- package_names[[i]]
    requirement <- version_requirements[[i]]
    
    package_path <- find.package(
      pkg,
      quiet = TRUE
    )
    
    # ---------------------------------------------------------------
    # Missing dependency
    # ---------------------------------------------------------------
    if (
      length(package_path) == 0L ||
      !nzchar(package_path)
    ) {
      
      problems <- c(
        problems,
        sprintf(
          "  - %s is not installed.",
          pkg
        )
      )
      
      packages_to_install <- c(
        packages_to_install,
        pkg
      )
      
      next
    }
    
    # Packages without a version requirement only need to exist.
    if (is.na(requirement) || !nzchar(requirement)) {
      next
    }
    
    # ---------------------------------------------------------------
    # Installed dependency with a version requirement
    # ---------------------------------------------------------------
    requirement_parts <- strsplit(
      requirement,
      "\\s+"
    )[[1]]
    
    if (length(requirement_parts) < 2L) {
      next
    }
    
    operator <- requirement_parts[[1]]
    required_version <- requirement_parts[[2]]
    
    installed_version <- as.character(
      utils::packageVersion(
        pkg,
        lib.loc = dirname(package_path)
      )
    )
    
    version_comparison <- utils::compareVersion(
      installed_version,
      required_version
    )
    
    version_is_valid <- switch(
      operator,
      ">=" = version_comparison >= 0L,
      ">"  = version_comparison > 0L,
      "<=" = version_comparison <= 0L,
      "<"  = version_comparison < 0L,
      "==" = version_comparison == 0L,
      "="  = version_comparison == 0L,
      TRUE
    )
    
    if (!version_is_valid) {
      
      problems <- c(
        problems,
        sprintf(
          "  - %s %s is installed, but %s %s is required.",
          pkg,
          installed_version,
          operator,
          required_version
        )
      )
      
      packages_to_install <- c(
        packages_to_install,
        pkg
      )
    }
  }
  
  # ---------------------------------------------------------------
  # Refuse to launch and show a targeted repair command
  # ---------------------------------------------------------------
  if (length(problems) > 0L) {
    
    packages_to_install <- unique(
      packages_to_install
    )
    
    install_command <- sprintf(
      "install.packages(c(%s))",
      paste(
        sprintf(
          '"%s"',
          packages_to_install
        ),
        collapse = ", "
      )
    )
    
    stop(
      paste(
        c(
          "CraIBM cannot start because required dependencies are missing or outdated:",
          "",
          problems,
          "",
          "Please run the following command:",
          "",
          paste0("  ", install_command),
          "",
          "After installation, restart R and run craibm::run_app() again."
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

#' @title Launch the Fish IBM Shiny Application
#' @description Starts the interactive Shiny application for parameterizing,
#'   running, and visualizing sportfish IBM simulations. This is the only
#'   function the package exports; everything else is internal and is reached
#'   through the application itself.
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
  .check_craibm_runtime_dependencies()
  
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
  
  # The application is launched as a directory, which is what lets Shiny
  # serve inst/app/www automatically. It does NOT rely on this function to
  # reach the package's internals: app.R binds them from the namespace
  # itself, so it behaves the same however it is started.
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    ...
  )
}

