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
      grim_plot_grid(df) + theme(text = element_text(size = size_text))
    )
  } else if (name_test == "DEBIT") {
    suppressWarnings(
      debit_plot(df) +
        theme_minimal(base_size = size_text) +
        theme(aspect.ratio = 1)
    )
  } else {
    stop(paste("No visualization defined for", name_test))
  }
}

# Display labels for result columns, by name after title-casing. A column that
# scrutiny adds or drops upstream then keeps its raw name instead of silently
# taking the label of a different column.
result_labels <- function(percent) {
  mean_label <- if (isTRUE(percent)) "Percentage" else "Mean"
  labels <- c(
    X = mean_label,
    Sd = "SD",
    Items_averaged = "Items averaged",
    Digits_x = paste0("Decimal places (", tolower(mean_label), ")"),
    Digits_sd = "Decimal places (SD)",
    Exp_incon = paste0(
      "Expected inconsistency rate for random ",
      tolower(mean_label),
      "s"
    ),
    Diff_var = "Step difference to reported",
    Var = "Variable"
  )
  set_names(names(labels), labels)
}

rename_after_testing <- function(df, percent) {
  names(df) <- str_to_title(names(df))
  rename(df, any_of(result_labels(percent)))
}

rename_after_testing_seq <- function(df, percent) {
  df <- rename_after_testing(df, percent)
  df$Variable <- unname(c(
    x = if (isTRUE(percent)) "Percentage" else "Mean",
    sd = "SD",
    n = "N"
  )[df$Variable])
  df
}

rename_after_audit <- function(df, percent) {
  values <- if (isTRUE(percent)) "percentages" else "means"
  cols <- c(
    "All cases" = "all_cases",
    "Testable cases" = "testable_cases",
    "Inconsistent cases" = "incon_cases",
    "Inconsistency rate" = "emp_incon",
    "Failed GRIM" = "fail_grim",
    "Failed GRIMMER (test 1)" = "fail_test1",
    "Failed GRIMMER (test 3)" = "fail_test3",
    "Mean of means" = "mean_x",
    "Mean of SDs" = "mean_sd",
    "Distinct sample sizes" = "distinct_n",
    "Rows excluded (missing values)" = "excluded_rows"
  )
  cols[paste("Expected inconsistency rate for random", values)] <- "exp_incon"
  cols["Inconsistency rate / expected rate"] <- "ratio"
  rename(df, any_of(cols))
}

# For GRIM on percentages, "Mean" becomes "Percentage", as in the tables above.
rename_after_audit_seq <- function(df, percent = FALSE) {
  vars <- c(x = if (isTRUE(percent)) "Percentage" else "Mean", sd = "SD", n = "N")
  labels <- c(
    vars,
    items_averaged = "Items averaged",
    consistency = "Consistency",
    hits_total = "Total number of hits"
  )
  for (v in names(vars)) {
    least <- paste("Least step difference in", vars[[v]])
    labels[paste0(c("hits_", "diff_", "diff_", "diff_"), v, c("", "", "_up", "_down"))] <-
      c(paste("Hits for", vars[[v]]), least, paste(least, c("(upward)", "(downward)")))
  }
  rename(df, any_of(set_names(names(labels), labels)))
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
# without having tested anything. Its expected inconsistency rate is then 0.
grim_untestable <- function(df) {
  if (!"exp_incon" %in% names(df)) {
    return(FALSE)
  }
  df$exp_incon %in% 0
}

# Downloads keep the verdict logical, where an untestable row is NA: TRUE would
# read as a pass.
mark_untestable <- function(df) {
  df$consistency[grim_untestable(df)] <- NA
  df
}

# grim_plot_grid() draws one precision per raster and errors on a mix. DEBIT's plot
# has no raster.
validate_plot_precision <- function(df, name_test) {
  digits_seen <- sort(unique(df$digits_x))
  validate(need(
    name_test == "DEBIT" || length(digits_seen) == 1L,
    paste0(
      "The plot needs means reported at a single number of decimal ",
      "places; these have ",
      paste(digits_seen, collapse = ", "),
      ". The test results are unaffected."
    )
  ))
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

# Tables that grow with the upload are paged on the server, so the browser only
# holds one page: 10,000 rows took 1.7 s to show as a plain table, 0.2 s paged.
# Short ones look as before, without search box and pager. Rows keep their
# order, which case numbers refer to.
long_table <- function(df) {
  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      pageLength = 25L,
      order = list(),
      scrollX = TRUE,
      dom = if (nrow(df) > 25L) "ftip" else "t"
    )
  )
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

# No paper reports more decimal places than this. A value showing more, like
# 0.30000000000000004, is a floating-point artefact of the software that wrote
# the file, and testing it at that precision flags it falsely.
max_digits <- 9L

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

# scrutiny refuses to bind the output of different mapper calls, because the
# rows would be read under the first call's settings. The sequence parts share
# their settings and differ only in precision, which travels per row in
# `digits_*`, so they are bound as plain tibbles.
bind_plain <- function(parts) {
  bind_rows(lapply(parts, \(part) {
    attr(part, "scrutiny") <- NULL
    class(part) <- c("tbl_df", "tbl", "data.frame")
    part
  }))
}
