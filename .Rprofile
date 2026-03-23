source("renv/activate.R")
local({
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  options(renv.consent = TRUE)

  # Prefer prebuilt binaries when the platform supports them so fresh installs
  # do not need to compile packages like png from source.
  if (.Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")) {
    options(pkgType = "binary")
  }

  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (!requireNamespace("renv", quietly = TRUE)) {
    utils::install.packages("renv", repos = getOption("repos"))
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
    renv::restore(project = project_dir, prompt = FALSE)
  }
})
