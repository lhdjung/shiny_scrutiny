
# All packages required by the app should be entered here:
packages_needed <- c(
  "shiny",
  "bslib",
  "rlang",
  "ggplot2",
  "dplyr",
  "corrr",
  "readr",
  "stringr",
  "janitor",
  "scrutiny",
  "forecast"
)

# Which packages are missing (i.e., not installed)? How many?
packages_installed <- installed.packages()[, "Package"]
packages_missing <- packages_needed[!packages_needed %in% packages_installed]
n_missing <- length(packages_missing)

# If any packages are missing, inform the user that they need to be installed,
# provide the code to do so, and throw an error:
if (n_missing > 0) {
  exactly_one_missing <- n_missing == 1
  msg_package_s <- if (exactly_one_missing) "package" else "packages"
  msg_it_them <- if (exactly_one_missing) "it" else "them"
  msg_its_their <- if (exactly_one_missing) "its" else "their"
  msg_open <- if (exactly_one_missing) "" else "c("
  msg_close <- if (exactly_one_missing) "" else ")"
  packages_missing <- paste0("\"", packages_missing, "\"", collapse = ", ")
  message(paste(n_missing, "required", msg_package_s, "missing."))
  message(paste(
    "Run this code to install", msg_it_them, "along with",
    msg_its_their, "dependencies:"
  ))
  cat(paste0(
    "  install.packages(", msg_open, packages_missing, msg_close,
    ", dependencies = TRUE)\n"
  ))
  stop("Can't run app without required packages installed. See fix above.")
}

# Without an error, all packages are installed, so just attach them:
for (i in seq_along(packages_needed)) {
  library(packages_needed[i], character.only = TRUE)
}

# Remove large objects; they won't be needed later:
rm(list = ls())

