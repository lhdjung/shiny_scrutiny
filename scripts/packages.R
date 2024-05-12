
# All packages that the app needs should be entered here:
packages_required <- c(
  "shiny",
  "bslib",
  "rlang",
  "ggplot2",
  "dplyr",
  "corrr",
  "readr",
  "stringr",
  "janitor",
  "scrutiny"
)

# Which packages are missing (i.e., not installed)? How many?
packages_installed <- installed.packages()[, "Package"]
packages_missing <- packages_required[!packages_required %in% packages_installed]
n_missing <- length(packages_missing)

# If any packages are missing, inform the user that they need to be installed,
# provide the code to do so, and throw an error. The wording of the error
# message is different if exactly one package is missing, which is conveyed by
# the `missing1` flag.
if (n_missing > 0L) {
  missing1 <- n_missing == 1
  msg_package_s <- if (missing1) "package" else "packages"
  msg_it_them   <- if (missing1) "it"      else "them"
  msg_its_their <- if (missing1) "its"     else "their"
  msg_c_open    <- if (missing1) ""        else "c("
  msg_c_close   <- if (missing1) ""        else ")"
  msg_package_names <- paste0("\"", packages_missing, "\"", collapse = ", ")
  message(paste("Note:", n_missing, "required", msg_package_s, "missing."))
  message(paste(
    "Run this code to install", msg_it_them, "along with",
    msg_its_their, "dependencies:"
  ))
  cat(paste0(
    "  install.packages(", msg_c_open, msg_package_names, msg_c_close,
    ", dependencies = TRUE)\n"
  ))
  stop("Can't run app without required packages installed. See fix above.")
}

# If the app gets here, all packages are already installed, so just attach them.
# The option `character.only = TRUE` is needed because the package name is not
# entered as a literal expression, as in `library(dplyr)`, but via a variable.
for (package in packages_required) {
  library(package, character.only = TRUE)
}

# Remove large objects; they won't be needed later:
rm(list = ls())
