local({
  # Set repository and renv options before activating renv so restore() can
  # prefer prebuilt binaries instead of compiling packages from source.
  options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
  options(renv.consent = TRUE)
  options(renv.config.repos.override = "CRAN=https://packagemanager.posit.co/cran/latest")

  if (.Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")) {
    options(pkgType = "binary")
  }
})

source("renv/activate.R")

local({
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (!requireNamespace("renv", quietly = TRUE)) {
    utils::install.packages("renv", repos = getOption("repos"), type = getOption("pkgType"))
  }

  if (!requireNamespace("renv", quietly = TRUE)) {
    stop("Package 'renv' could not be installed. Please install it manually and retry.", call. = FALSE)
  }

  renv::activate(project = project_dir)

  required_packages <- c(
    "shiny",
    "leaflet",
    "terra",
    "ggplot2",
    "glue",
    "sf",
    "viridisLite",
    "later"
  )

  project_library <- renv::paths$library(project = project_dir)
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (!dir.exists(project_library) || length(missing_packages) > 0) {
    renv::restore(
      project = project_dir,
      prompt = FALSE,
      repos = getOption("repos")
    )
  }
})
