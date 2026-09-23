# General helpers ---------------------------------------------------------

# Consistency testing -----------------------------------------------------

select_rounding_method <- function(rounding) {
  switch(
    rounding,
    "Up or down" = "up_or_down",
    "Up" = "up",
    "Down" = "down",
    "Ceiling or floor" = "ceiling_or_floor",
    "Ceiling" = "ceiling",
    "Floor" = "floor",
    "Truncate" = "trunc",
    "Anti-truncate" = "anti_trunc"
  )
}

plot_test_results <- function(df, name_test, size_text) {
  if (any(name_test == c("GRIM", "GRIMMER"))) {
    suppressWarnings(
      grim_plot(df) +
        theme(text = element_text(size = size_text), aspect.ratio = 1)
    )
  } else if (name_test == "DEBIT") {
    suppressWarnings(
      debit_plot(df, label_size = size_text * 0.285, show_outer_boxes = FALSE) +
        theme_minimal(base_size = size_text) +
        theme(aspect.ratio = 1)
    )
  } else {
    stop(paste("No visualization defined for", name_test))
  }
}

# The if-tree can't be replaced by `switch()` here because this wouldn't work
# with the assignment to `mean_or_percent`.
rename_after_testing <- function(df, name_test, percent) {
  names(df) <- str_to_title(names(df))

  # Make sure any `sd` column is always displayed as `SD`, not `Sd`. This is
  # needed for GRIM, which doesn't test SDs, so `rename_key_vars()` takes no
  # effect if the data still include an `sd` column -- as does, notably, the
  # example dataset `pigs5`.
  names(df)[names(df) == "sd" | names(df) == "Sd"] <- "SD"
  df <- rename(df, any_of(digits_labels))

  # Rename by consistency test. Since scrutiny 1.0.0, percentages are deflated
  # internally only; the output still shows them as reported.
  if (name_test == "GRIM") {
    mean_or_percent <- if (percent) "Percentage" else "Mean"
    ratio_header <- if (percent) "percentages" else "means"
    ratio_header <- paste(
      "Probability of inconsistency for random",
      ratio_header
    )
    rename(
      df,
      "{mean_or_percent}" := X,
      # # Not tested by GRIM but can still occur in the data
      # SD = Sd,
      "{ratio_header}" := Probability
    )
  } else if (name_test == "GRIMMER") {
    rename(
      df,
      Mean = X #,
      # SD = Sd
    )
  } else if (name_test == "DEBIT") {
    rename(
      df,
      Mean = X,
      # SD = Sd,
      `Lower SD` = Sd_lower,
      `Include lower SD` = Sd_incl_lower,
      `Upper SD` = Sd_upper,
      `Include upper SD` = Sd_incl_upper,
      `Lower mean` = X_lower,
      `Upper mean` = X_upper
    )
  }
}

# scrutiny 1.0.0 reports the decimal places it tested at, per row.
digits_labels <- c(
  "Decimal places (mean)" = "Digits_x",
  "Decimal places (SD)" = "Digits_sd"
)

# Renamed by name, not by position: a column added or moved upstream then keeps
# its raw name instead of silently inheriting the label of a different column.
rename_after_audit <- function(df, percent) {
  ratio_header <- paste(
    "Mean probability of inconsistency for random",
    if (isTRUE(percent)) "percentages" else "means"
  )
  cols <- c(
    "Inconsistent cases" = "incons_cases",
    "All cases" = "all_cases",
    "Inconsistency rate" = "incons_rate",
    "Inconsistencies / probability" = "incons_to_prob",
    "Testable cases" = "testable_cases",
    "Testable cases rate" = "testable_rate",
    "Failed GRIM" = "fail_grim",
    "Failed GRIMMER (test 1)" = "fail_test1",
    "Failed GRIMMER (test 2)" = "fail_test2",
    "Failed GRIMMER (test 3)" = "fail_test3",
    "Failed scale bounds" = "fail_scale",
    "Mean of means" = "mean_x",
    "Mean of SDs" = "mean_sd",
    "Distinct sample sizes" = "distinct_n",
    "Rows excluded (missing values)" = "excluded_rows"
  )
  cols[ratio_header] <- "mean_grim_prob"
  rename(df, any_of(cols))
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
    "X" = "Mean",
    "Sd" = "SD",
    "sd" = "SD",
    name
  )
}

# The if-tree is necessary here; see the comment on `rename_after_testing()`.
rename_after_testing_seq <- function(df, name_test, percent) {
  names(df) <- str_to_title(names(df))
  df <- rename(df, any_of(digits_labels))
  if (name_test == "GRIM") {
    mean_or_percent <- if (percent) "Percentage" else "Mean"
    df <- rename(
      df,
      "{mean_or_percent}" := X,
      `Probability of inconsistency for random values` = Probability,
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
  df |>
    set_names(switch(
      name_test,
      "GRIM" = c(
        "Mean",
        "N",
        "Consistency",
        "Total number of hits",
        "Hits for Mean",
        "Hits for N",
        "Least step difference in Mean",
        "Least step difference in Mean (upward)",
        "Least step difference in Mean (downward)",
        "Least step difference in N",
        "Least step difference in N (upward)",
        "Least step difference in N (downward)"
      ),
      "GRIMMER" = c(
        "Mean",
        "SD",
        "N",
        "Consistency",
        "Total number of hits",
        "Hits for Mean",
        "Hits for SD",
        "Hits for N",
        "Least step difference in Mean",
        "Least step difference in Mean (upward)",
        "Least step difference in Mean (downward)",
        "Least step difference in SD",
        "Least step difference in SD (upward)",
        "Least step difference in SD (downward)",
        "Least step difference in N",
        "Least step difference in N (upward)",
        "Least step difference in N (downward)"
      ),
      "DEBIT" = c(
        "Mean",
        "SD",
        "N",
        "Consistency",
        "Total number of hits",
        "Hits for Mean",
        "Hits for SD",
        "Hits for N",
        "Least step difference in Mean",
        "Least step difference in Mean (upward)",
        "Least step difference in Mean (downward)",
        "Least step difference in SD",
        "Least step difference in SD (upward)",
        "Least step difference in SD (downward)",
        "Least step difference in N",
        "Least step difference in N (upward)",
        "Least step difference in N (downward)"
      )
    ))
}


# Duplicate analysis ------------------------------------------------------

rename_duplicate_count_df <- function(df) {
  df |>
    set_names(c(
      "Value",
      "Frequency",
      "Locations",
      "Number of locations"
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
  df |>
    set_names(c(
      "Original column 1",
      "Original column 2",
      special_colnames_count_colpair
    ))
}

rename_duplicate_summary <- function(df, function_ending) {
  df$term <- switch(
    function_ending,
    "count" = c("Frequency", "Number of locations"),
    "count_colpair" = special_colnames_count_colpair,
    "tally" = c(head(df$term, -1L), "Total")
  )

  df |>
    set_names(c(
      "Term",
      "Mean",
      "SD",
      "Median",
      "Minimum",
      "Maximum",
      "Number of missing values",
      "Proportion of missing values"
    ))
}


# UI helpers --------------------------------------------------------------

# Help sits on an icon in the header rather than on the whole card, where it
# popped up over the table whenever the pointer was on it. A button, so that it
# can be reached by keyboard and by tapping.
card_header_help <- function(title, help) {
  card_header(
    title,
    tooltip(
      tags$button(
        type = "button",
        class = "btn btn-link p-0 ms-1 align-baseline",
        `aria-label` = paste("About:", title),
        "\u24d8"
      ),
      help
    )
  )
}

# On screen, a verdict reads better as a word than as TRUE / FALSE. Downloads
# keep the logical column for analysis.
label_consistency <- function(df, untestable = FALSE) {
  df$Consistency <- if_else(
    df$Consistency,
    "Consistent",
    "Inconsistent",
    missing = "Undecidable"
  )
  df$Consistency[which(untestable)] <- "Not testable (sample too large)"
  df
}

# Once `n` reaches 10 ^ digits, every mean is attainable: GRIM passes the row
# without having tested anything. Its probability of inconsistency is then 0.
grim_untestable <- function(df) {
  if (!"probability" %in% names(df)) {
    return(FALSE)
  }
  df$probability %in% 0
}

# Downloads keep the verdict logical, where an untestable row is NA: TRUE would
# read as a pass.
mark_untestable <- function(df) {
  df$consistency[grim_untestable(df)] <- NA
  df
}

# Offending values for an error message, so they can be found in the preview.
quote_values <- function(values) {
  values <- unique(values)
  more <- if (length(values) > 5L) ", ..." else ""
  paste0("\"", head(values, 5L), "\"", collapse = ", ") |> paste0(more)
}

# A download that hits a validate() message would otherwise just fail in the
# browser, without saying why. `df` is forced here so that its error is caught.
write_download <- function(df, file) {
  df <- tryCatch(df, error = function(e) {
    msg <- conditionMessage(e)
    showNotification(
      if (nzchar(msg)) msg else "Nothing to download yet.",
      type = "error"
    )
    stop(e)
  })
  write_csv(clean_names(df), file)
}

# Create a centered, scrollable table div with custom styling
styled_table_div <- function(output_id) {
  div(
    style = "overflow-x: auto; min-width: 0;",
    tags$style(paste(
      # fmt: skip
      paste0(
        "#", output_id, " table { width: auto !important; margin: 0 auto; }"
      ),
      paste0("#", output_id, " th, #", output_id, " td { padding: 8px 30px; }")
    )),
    tableOutput(output_id)
  )
}


# Other -------------------------------------------------------------------

# Predicate to test if all elements of a vector are whole numbers. This function
# uses some code from the `?integer` documentation.
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}

# Double columns holding only whole numbers display better as integers. Only
# doubles: a character column such as IDs "001" must keep its zeros, and `x` /
# `sd` are kept as uploaded because their decimal places carry the precision
# that GRIM and friends test against. Values beyond the integer range would
# silently become NA, so such columns are left alone too.
format_after_upload <- function(df) {
  is_integer_like_col <- function(col) {
    is.double(col) &&
      all(
        is_whole_number(col) & abs(col) <= .Machine$integer.max,
        na.rm = TRUE
      )
  }

  cols_integer_like <- names(df)[
    vapply(df, is_integer_like_col, logical(1L), USE.NAMES = FALSE)
  ]

  mutate(
    df,
    across(
      .cols = all_of(setdiff(cols_integer_like, c("x", "sd"))),
      .fns = as.integer
    )
  )
}

# Decimal places to declare to scrutiny, one per row: since 1.0.0 it takes them
# as an explicit argument rather than inferring them from trailing zeros in
# strings. Each value is tested at the precision it still shows, or at the
# user's "Restore decimal zeros" value, whichever is greater.
digits_declared <- function(col, digits_min) {
  floor_digits <- if (isTRUE(digits_min >= 0)) as.integer(digits_min) else 0L
  pmax(floor_digits, decimal_places(col), na.rm = TRUE)
}

# A double can't carry trailing zeros, so a mean tested as 4.10 would display
# and download as 4.1 -- a value that reads as consistent. Print the key values
# at the precision they were tested at.
format_tested_values <- function(df) {
  if ("digits_x" %in% names(df)) {
    df$x <- sprintf("%.*f", as.integer(df$digits_x), df$x)
  }
  if ("digits_sd" %in% names(df)) {
    df$sd <- sprintf("%.*f", as.integer(df$digits_sd), df$sd)
  }
  df
}

format_download_file_name <- function(
  name_input_file,
  name_technique,
  addendum = NULL
) {
  name_input_file |>
    str_remove("\\.[^.]+$") |>
    paste0("_", name_technique, addendum, ".csv")
}
