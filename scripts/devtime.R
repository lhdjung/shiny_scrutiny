
# Devtime: renaming columns after `audit_seq()` ---------------------------

# Use the outcommented function `devtime_generate_colnames_audit_seq()` below,
# for example with "GRIMMER", to generate the code for renaming columns after
# calling `audit_seq()`. Copy it from the console, then paste it into
# `rename_after_audit_seq()`. This requires the constructive package to be
# installed, as well as sourcing all of the usually-outcommented functions in
# this section. (It's called "devtime" because it's meant to be done by the
# developer, not at runtime; even though the code is not compiled.)

devtime_generate_colnames_audit_seq <- function(name_test = c("GRIM", "GRIMMER", "DEBIT")) {
  name_test <- arg_match(name_test)
  df_tested_seq <- switch(
    name_test,
    "GRIM" = grim_map_seq(pigs1),
    "GRIMMER" = grimmer_map_seq(pigs5),
    "DEBIT" = debit_map_seq(pigs3)
  )
  df_tested_seq |>
    audit_seq() |>
    devtime_rename_after_audit_seq() |>
    colnames() |>
    constructive::construct()
}

# Helper for `devtime_generate_colnames_audit_seq()`:
devtime_rename_after_audit_seq <- function(df) {
  regex_key_var_names <- paste0(
    "(?<=(^(hits_|diff_)))(",
    paste0(names(select_key_cols(df)), collapse = "|"),
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
select_key_cols <- function(df) {
  df[1L:(match("consistency", names(df)) - 1L)]
}
