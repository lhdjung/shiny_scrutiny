
# Consistency testing -----------------------------------------------------

select_rounding_method <- function(rounding) {
  switch(
    rounding,
    "Up or down" = "up_or_down",
    "Up" = "up",
    "Down" = "down",
    "Up from..." = "up_from",
    "Down from..." = "down_from",
    "Ceiling or floor" = "ceiling_or_floor",
    "Ceiling" = "ceiling",
    "Floor" = "floor",
    "Truncate" = "trunc",
    "Anti-truncate" = "anti_trunc"
  )
}

plot_test_results <- function(df, name_test, size_text) {
  if (any(name_test == c("GRIM", "GRIMMER"))) {
    grim_plot(df, rounding = rounding) +
      theme(text = element_text(size = size_text))
  } else if (name_test == "DEBIT") {
    debit_plot(df, label_size = size_text * 0.285) +
      theme_minimal(base_size = size_text)
  } else {
    stop("No visualization defined")
  }
}

parse_dispersion <- function(string) {
  string <- paste0("c(", string, ")")
  tryCatch(
    eval(parse_expr(string)),
    error = function(cond) {
      stop(safeError("Dispersion sequence couldn't be parsed."))
    }
  )
}

check_dispersion_audit_seq <- function(dispersion) {
  if (!is_seq_ascending(parse_dispersion(dispersion), test_linear = TRUE)) {
    stop(safeError(paste(
      "Summaries of dispersed sequences are only supported",
      "if \"Dispersion\" is a linearly increasing sequence."
    )))
  }
}

# The if-tree can't be replaced by `switch()` here because this wouldn't work
# with the assignment to `mean_or_percent`.
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
  `names<-`(
    df, switch(
      name_test,
      "GRIM" = c(
        "Inconsistent cases",
        "All cases",
        "Inconsistency rate",
        "Mean GRIM ratio",
        "Inconsistencies / ratio",
        "Testable cases",
        "Testable cases rate"
      ),
      "GRIMMER" = c(
        "Inconsistent cases",
        "All cases",
        "Inconsistency rate",
        "Failed GRIM",
        "Failed GRIMMER (test 1)",
        "Failed GRIMMER (test 2)",
        "Failed GRIMMER (test 3)"
      ),
      "DEBIT" = c(
        "Inconsistent cases",
        "All cases",
        "Inconsistency rate",
        "Mean of means",
        "Mean of SDs",
        "Distinct sample sizes"
      )
    )
  )
}

# This function MUST contain renaming instructions for all key variables of all
# consistency tests currently supported! However, this doesn't mean they
# necessarily need to contain explicit and specific instructions as key-value
# pairs, such as `"X" = "Mean"`. All column names are set to title case first,
# which already takes care of `n` --> `N`, but also of `consistency`
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

# The if-tree is necessary here; see the comment on `rename_after_testing()`.
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


rename_after_audit_seq <- function(df, name_test) {
  `names<-`(
    df,
    switch(
      name_test,
      "GRIM" = c(
        "Mean", "N", "Consistency", "Total number of hits", "Hits for Mean",
        "Hits for N", "Least step difference in Mean",
        "Least step difference in Mean (upward)",
        "Least step difference in Mean (downward)", "Least step difference in N",
        "Least step difference in N (upward)",
        "Least step difference in N (downward)"
      ),
      "GRIMMER" = c(
        "Mean", "SD", "N", "Consistency", "Total number of hits", "Hits for Mean",
        "Hits for SD", "Hits for N", "Least step difference in Mean",
        "Least step difference in Mean (upward)",
        "Least step difference in Mean (downward)", "Least step difference in SD",
        "Least step difference in SD (upward)",
        "Least step difference in SD (downward)", "Least step difference in N",
        "Least step difference in N (upward)",
        "Least step difference in N (downward)"
      ),
      "DEBIT" = c(
        "Mean", "SD", "N", "Consistency", "Total number of hits", "Hits for Mean",
        "Hits for SD", "Hits for N", "Least step difference in Mean",
        "Least step difference in Mean (upward)",
        "Least step difference in Mean (downward)", "Least step difference in SD",
        "Least step difference in SD (upward)",
        "Least step difference in SD (downward)", "Least step difference in N",
        "Least step difference in N (upward)",
        "Least step difference in N (downward)"
      )
    )
  )
}



# Devtime: renaming columns after `audit_seq()` ---------------------------

# Use the outcommented function `devtime_generate_colnames_audit_seq()` below,
# for example with "GRIMMER", to generate the code for renaming columns after
# calling `audit_seq()`. Copy it from the console, then paste it into
# `rename_after_audit_seq()`. This requires the constructive package to be
# installed, as well as sourcing all of the usually-outcommented functions in
# this section. (It's called "devtime" because it's meant to be done by the
# developer, not at runtime; even though the code is not compiled.)

# devtime_generate_colnames_audit_seq <- function(name_test = c("GRIM", "GRIMMER", "DEBIT")) {
#   name_test <- arg_match(name_test)
#   df_tested_seq <- switch(
#     name_test,
#     "GRIM" = grim_map_seq(pigs1),
#     "GRIMMER" = grimmer_map_seq(pigs5),
#     "DEBIT" = debit_map_seq(pigs3)
#   )
#   df_tested_seq |>
#     audit_seq() |>
#     devtime_rename_after_audit_seq() |>
#     colnames() |>
#     constructive::construct()
# }
#
# devtime_rename_after_audit_seq <- function(df) {
#   regex_key_var_names <- paste0(
#     "(?<=(^(hits_|diff_)))(",
#     paste0(names(select_key_cols(df)), collapse = "|"),
#     ")(?=(_up|_down|))"
#   )
#   names_all <- names(df)
#   for (i in seq_along(names_all)) {
#     if (str_starts(names_all[i], "(hits_|diff_)")) {
#       names_all[i] <- names_all[i] |>
#         str_replace(regex_key_var_names, rename_key_vars) |>
#         str_replace("^hits_", "Hits for ") |>
#         str_replace("^diff_", "Least step difference in ") |>
#         str_replace("_up$", " (upward)") |>
#         str_replace("_down$", " (downward)")
#     } else {
#       names_all[i] <- rename_key_vars(names_all[i])
#     }
#   }
#   names_all[names_all == "Hits for total"] <- "Total number of hits"
#   `names<-`(df, value = names_all)
# }
#
# select_key_cols <- function(df) {
#   df[1L:(match("consistency", names(df)) - 1L)]
# }



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

# # Renaming the columns of `duplicate_tally()`'s output is not necessary:
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

# Predicate to test if all elements of a vector are whole numbers. This function
# uses some code from the `?integer` documentation.
is_whole_number <- function (x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}

format_after_upload <- function(df, digits) {
  # Which columns can be coerced to numeric? The key question is whether
  # coercion generates new `NA`s, which happens whenever a string can't be
  # parsed as a number. See `scrutiny::is_numeric_like()`.
  indices_numeric_like_cols <- which(vapply(
    df, is_numeric_like, logical(1L), USE.NAMES = FALSE
  ))
  # Determine the maximum number of decimal places from among the numeric-like
  # values in `df` (see `scrutiny::is_numeric_like()`) and `digits`:
  width_max <- df[indices_numeric_like_cols] |>
    unlist(use.names = FALSE) |>
    decimal_places() |>
    max(digits)
  # Select the columns that only store whole numbers. Convert them to integer so
  # that they are displayed better. (They don't need to be padded with trailing
  # zeros because they presumably never had any decimal numbers to begin with.)
  # Decimal numbers are padded to `width_max` with trailing zeros. Disclaimer:
  # The anonymous function uses some code from the `?integer` documentation.
  mutate(df, across(
    .cols = all_of(indices_numeric_like_cols),
    .fns  = function(x) {
      if (all(is_whole_number(as.numeric(x)))) {
        as.integer(x)
      } else {
        restore_zeros(x, width = width_max)
      }
    }
  ))
}

format_download_file_name <- function(name_input_file, name_technique,
                                      addendum = NULL) {
  name_input_file |>
    str_remove("\\.[^.]+$") |>
    paste0("_", name_technique, addendum, ".csv")
}

