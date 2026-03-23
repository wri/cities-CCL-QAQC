args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)

project_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(project_dir)

# Load the project bootstrap so renv is activated before the app starts.
source(".Rprofile", local = TRUE)

shiny::runApp(appDir = project_dir, launch.browser = TRUE)
