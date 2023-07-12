# This script sets up a renv for the project and installs required packages

# We are using the daily CRAN snapshots from RStudio Package Manager: 
# https://packagemanager.rstudio.com/client/#/repos/1/overview
# Currently, we are using the snapshot from July 6, 2023:
# https://packagemanager.posit.co/cran/2023-07-06

# Select the repo snapshot:
options(repos = c(
  REPO_NAME = "https://packagemanager.posit.co/cran/2023-07-06"
  ))

# Install renv
install.packages("renv")

# Initialize renv for the project
# bare = TRUE: instead of installing dependencies automatically, we install packages manually
renv::init(bare = TRUE)

# Install the packages
install.packages(c(
  "tidyverse", "dplyr", "stringr", "tidyr", "rvest", "pdftools", "rlang"
  ))

# Take a snapshot of the renv
renv::snapshot()
