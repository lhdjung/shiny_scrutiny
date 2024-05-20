
# NOTE: run this script to generate the code for renaming columns after calling
# `audit_seq()`. It's called "devtime" because it's meant to be done by the
# developer, not at runtime; even though the code is not compiled. The script
# requires the constructive package to be installed.

# Follow these steps:

# (1) Enter the name of each supported consistency test in the `test_df_all`
# list under "Preparation" below. Next to it, call the corresponding sequence
# mapper with a suitable data frame as an argument.

# (2) Run the script.

# (3) Copy the column names -- printed at the end of the console output -- into
# the switch statement within `rename_after_audit_seq()` in scripts/functions.R.


# Preparation -------------------------------------------------------------

# Source packages.R because the code here relies on some of those packages.
# Also, functions.R is needed because it contains `rename_key_vars()`, which is
# called within `devtime_rename_after_audit_seq()`.
source("scripts/packages.R")
source("scripts/functions.R")

test_df_all <- list(
  GRIM = grim_map_seq(pigs1),
  GRIMMER = grimmer_map_seq(pigs5),
  DEBIT = debit_map_seq(pigs3)
)


# Functions ---------------------------------------------------------------

devtime_generate_colnames_audit_seq <- function(name_test) {
  message(paste0(
    "\"", name_test, "\"", " column names in `rename_after_audit_seq()`:"
  ))
  test_df_all[[name_test]] |>
    audit_seq() |>
    devtime_rename_after_audit_seq() |>
    colnames() |>
    constructive::construct() |>
    print()
  message("\n")
}

# Helper for `devtime_generate_colnames_audit_seq()`:
devtime_rename_after_audit_seq <- function(df) {
  regex_key_var_names <- paste0(
    "(?<=(^(hits_|diff_)))(",
    paste0(names(devtime_select_key_cols(df)), collapse = "|"),
    ")(?=(_up|_down|))"
  )
  names_all <- names(df)
  for (i in seq_along(names_all)) {
    if (str_starts(names_all[i], "(hits_|diff_)")) {
      names_all[i] <- names_all[i] |>
        str_replace(regex_key_var_names, rename_key_vars) |>
        str_replace("^hits_", "Hits for ") |>
        str_replace("^diff_", "Least step difference in ") |>
        str_replace("_up$", " (upward)") |>
        str_replace("_down$", " (downward)")
    } else {
      names_all[i] <- rename_key_vars(names_all[i])
    }
  }
  names_all[names_all == "Hits for total"] <- "Total number of hits"
  `names<-`(df, value = names_all)
}

# Helper for the other helper:
devtime_select_key_cols <- function(df) {
  df[1L:(match("consistency", names(df)) - 1L)]
}


# Printing column names ---------------------------------------------------

for (name in names(test_df_all)) {
  devtime_generate_colnames_audit_seq(name)
}
