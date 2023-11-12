
# Consistency testing -----------------------------------------------------

rename_after_testing <- function(df, name_test, percent) {
  names(df) <- str_to_title(names(df))
  if (name_test == "GRIM") {
    mean_or_percent <- if (percent) "Percentage (deflated)" else "Mean"
    rename(
      df,
      "{mean_or_percent}" := X,
      `GRIM ratio` = Ratio
    )
  } else if (name_test == "GRIMMER") {
    rename(
      df,
      Mean = X,
      SD = Sd
    )
  } else if (name_test == "DEBIT") {
    rename(
      df,
      Mean = X,
      SD = Sd,
      `Lower SD` = Sd_lower,
      `Include lower SD` = Sd_incl_lower,
      `Upper SD` = Sd_upper,
      `Include upper SD` = Sd_incl_upper,
      `Lower mean` = X_lower,
      `Upper mean` = X_upper
    )
  }
}


rename_after_audit <- function(df, name_test) {
  if (name_test == "GRIM") {
    rename(
      df,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Mean GRIM ratio` = mean_grim_ratio,
      `Inconsistencies / ratio` = incons_to_ratio,
      `Testable cases` = testable_cases,
      `Testable cases rate` = testable_rate
    )
  } else if (name_test == "GRIMMER") {
    rename(
      df,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Failed GRIM` = fail_grim,
      `Failed GRIMMER (test 1)` = fail_test1,
      `Failed GRIMMER (test 2)` = fail_test2,
      `Failed GRIMMER (test 3)` = fail_test3
    )
  } else if (name_test == "DEBIT") {
    rename(
      df,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Mean of means` = mean_x,
      `Mean of SDs` = mean_sd,
      `Distinct sample sizes` = distinct_n
    )
  }
}


# This function MUST contain renaming instructions for all key variables of all
# consistency tests currently implemented! All column names are set to title
# case first, which already takes care of `n` --> `N`, but also of `consistency`
# --> `Consistency` (not a key variable, but convenient to cover here).
rename_key_vars <- function(name) {
  name <- str_to_title(name)
    switch(
      name,
      "X"  = "Mean",
      "Sd" = "SD",
      name
    )
}


rename_after_testing_seq <- function(df, name_test, percent) {
  names(df) <- str_to_title(names(df))
  if (name_test == "GRIM") {
    mean_or_percent <- if (percent) "Percentage (deflated)" else "Mean"
    df <- rename(
      df,
      "{mean_or_percent}" := X,
      `GRIM ratio` = Ratio,
      `Step difference to reported` = Diff_var,
      Variable = Var
    )
  } else if (name_test == "GRIMMER") {
    df <- rename(
      df,
      Mean = X,
      SD = Sd,
      `Step difference to reported` = Diff_var,
      Variable = Var
    )
  } else if (name_test == "DEBIT") {
    df <- rename(
      df,
      Mean = X,
      SD = Sd,
      `Lower SD` = Sd_lower,
      `Include lower SD` = Sd_incl_lower,
      `Upper SD` = Sd_upper,
      `Include upper SD` = Sd_incl_upper,
      `Lower mean` = X_lower,
      `Upper mean` = X_upper,
      `Step difference to reported` = Diff_var,
      Variable = Var
    )
  }
  df$Variable <- vapply(df$Variable, rename_key_vars, character(1L))
  df
}


select_key_cols <- function(df) {
  df[1L:(match("consistency", names(df)) - 1L)]
}


rename_after_audit_seq <- function(df) {
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


plot_test_results <- function(df, name_test, size_text) {
  if (any(name_test == c("GRIM", "GRIMMER"))) {
    grim_plot(df) +
      theme(text = element_text(size = size_text))
  } else if (name_test == "DEBIT") {
    debit_plot(df, label_size = size_text * 0.285) +
      theme_minimal(base_size = size_text)
  } else {
    stop("No visualization defined")
  }
}


# Duplicate analysis ------------------------------------------------------

# This transformation is adapted from `broom:::tidy.acf()`:
tidy_acf <- function(acf_object) {
  tibble(
    Lag = as.numeric(acf_object$lag),
    `Autocorrelation function` = as.numeric(acf_object$acf)
  )
}

rename_duplicate_count_df <- function(df) {
  `names<-`(df, c(
    "Value", "Duplicate count", "Locations", "Number of locations"
  ))
}

special_colnames_count_colpair <- c(
  "Duplicate count (values in both columns)",
  "Total number of column-1 values",
  "Total number of column-2 values",
  "Proportion of column-1 values also in column 2",
  "Proportion of column-2 values also in column 1"
)

rename_duplicate_count_colpair_df <- function(df) {
  `names<-`(df, c(
      "Original column 1", "Original column 2",
      special_colnames_count_colpair
    ))
}

# rename_duplicate_tally_df <- function(df) {
#   is_even <- function(x) x %% 2 == 0
#   df[is_even(seq_len(ncol(df)))]
# }

rename_duplicate_summary <- function(df, function_ending) {
  df$term <- switch(
    function_ending,
    "count" = c("Duplicate count", "Number of locations"),
    "count_colpair" = special_colnames_count_colpair,
    "tally" = c(df$term[seq_along(df$term) - 1L], "Total")
  )
  `names<-`(df, c(
    "Term", "Mean", "SD", "Median", "Minimum", "Maximum",
    "Number of missing values", "Proportion of missing values"
  ))
}


# Other -------------------------------------------------------------------

format_download_file_name <- function(name_input_file, name_technique,
                                      addendum = NULL) {
  name_input_file |>
    str_remove("\\.[^.]+$") |>
    paste0("_", name_technique, addendum, ".csv")
}

