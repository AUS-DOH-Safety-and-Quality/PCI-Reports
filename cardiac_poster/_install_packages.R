# _install_packages.R
# Installs missing dependencies for the Cardiac Poster project

required_packages <- c("dplyr", "officer", "svDialogs", "scales", "readxl", "here", "lubridate", "writexl")

# helper to check and install
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message(paste("Installing package:", p))
    install.packages(p, repos = "https://cran.csiro.au/")
  } else {
    message(paste("Package already installed:", p))
  }
}

invisible(lapply(required_packages, install_if_missing))
message("All dependencies checked.")
