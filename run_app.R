clear_conflicting_geospatial_env <- function() {
  for (var in c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA")) {
    value <- Sys.getenv(var, unset = "")
    if (nzchar(value) && grepl("(anaconda|miniconda|conda)", value, ignore.case = TRUE)) {
      Sys.unsetenv(var)
    }
  }
}

ensure_user_library <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER", unset = "")

  if (!nzchar(user_lib)) {
    return(invisible(NULL))
  }

  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(normalizePath(user_lib, winslash = "/", mustWork = TRUE), .libPaths())))
}

install_missing_packages <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (!length(missing_packages)) {
    return(invisible(NULL))
  }

  options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

  if (.Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")) {
    options(pkgType = "binary")
  }

  utils::install.packages(
    missing_packages,
    repos = getOption("repos"),
    dependencies = TRUE,
    type = getOption("pkgType")
  )
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)

project_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(project_dir)
clear_conflicting_geospatial_env()
ensure_user_library()

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

install_missing_packages(required_packages)

shiny::runApp(appDir = project_dir, launch.browser = TRUE)
