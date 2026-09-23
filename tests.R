# Run with: Rscript tests.R
#
# Guards the decimal-place handling. Losing a trailing zero is invisible by
# inspection -- it does not error, it just makes GRIM more permissive -- so it
# needs an assertion rather than a code review.

suppressMessages({
  library(dplyr)
  library(rlang)
  library(stringr)
  library(scrutiny)
  library(ggplot2)
})
source("scripts/functions.R")

check <- function(label, expr) {
  stopifnot(isTRUE(expr))
  cat("ok:", label, "\n")
}

# The headline case. A mean of 4.10 at n = 25 is GRIM-inconsistent at the two
# decimal places the paper printed, and consistent at one. If this flips, the
# app is clearing real inconsistencies.
grim1 <- function(x, n, digits) {
  grim_map(tibble(x = x, n = n), digits_x = digits)$consistency
}

check("4.10 at n=25 is inconsistent at 2 dp", !grim1(4.10, 25L, 2))
check("4.10 at n=25 looks consistent at 1 dp", grim1(4.10, 25L, 1))

# Precision survives the upload pipeline when the file still carries it.
raw <- tibble(x = c("4.10", "5.00"), n = c(25, 40))
fmt <- format_after_upload(raw)
check("`n` becomes integer", is.integer(fmt$n))
check("`x` is not collapsed to integer", !is.integer(fmt$x))
check(
  "declared digits read 2 from the strings",
  all(digits_declared(fmt$x, 0) == 2)
)
check(
  "a whole-number mean keeps its zeros",
  !grim_map(
    mutate(fmt, x = as.numeric(x)),
    digits_x = digits_declared(fmt$x, 0)
  )$consistency[1]
)

# `input$digits` is the floor, used when readr already dropped the zeros.
check("user-declared digits win", all(digits_declared(c(4.1, 5.3), 2) == 2))
check(
  "digits are declared per row, floored by the user's value",
  identical(digits_declared(c(4.123, 5.3), 2), c(3L, 2L))
)
check(
  "a blank digits field means no floor",
  all(digits_declared(c(4.1), NA) == 1)
)
check(
  "all-NA column does not error",
  all(digits_declared(c(NA_real_, NA_real_), 0) == 0)
)

# Integer display conversion must never lose data.
wide <- format_after_upload(tibble(
  id = c(1e10, 2e10),
  code = c("001", "002"),
  n = c(3, 4)
))
check(
  "values beyond the integer range are kept",
  identical(wide$id, c(1e10, 2e10))
)
check(
  "character IDs keep their leading zeros",
  identical(wide$code, c("001", "002"))
)
check("`n` still becomes integer", is.integer(wide$n))

# Audit renaming is by name, so an extra upstream column keeps its own name
# instead of taking another column's label -- and nothing ends up unlabelled.
d <- tibble(
  x = c(5.30, 7.22, 4.10),
  sd = c(1.20, 0.75, 1.00),
  n = c(40L, 40L, 25L)
)
audits <- list(
  GRIM = audit(grim_map(d, digits_x = 2)),
  GRIMMER = audit(grimmer_map(d, digits_x = 2, digits_sd = 2)),
  DEBIT = audit(debit_map(
    tibble(x = c(0.35, 0.47), sd = c(0.48, 0.50), n = c(20L, 40L)),
    digits_x = 2,
    digits_sd = 2
  ))
)
for (name_test in names(audits)) {
  renamed <- rename_after_audit(audits[[name_test]], percent = FALSE)
  check(
    paste(name_test, "audit has no blank or NA column names"),
    ncol(renamed) == ncol(audits[[name_test]]) &&
      !anyNA(names(renamed)) &&
      all(nzchar(names(renamed)))
  )
  check(
    paste(name_test, "audit columns are actually relabelled"),
    "Inconsistent cases" %in% names(renamed)
  )
}
check(
  "GRIMMER's added `fail_scale` column is labelled",
  "Failed scale bounds" %in% names(rename_after_audit(audits$GRIMMER, FALSE))
)

# Positional renames of the dispersed-sequence summaries must match scrutiny's
# current column counts; `rlang::set_names()` errors loudly otherwise.
seqs <- list(
  GRIM = grim_map_seq(d, digits_x = 2, dispersion = 1:2),
  GRIMMER = grimmer_map_seq(d, digits_x = 2, digits_sd = 2, dispersion = 1:2),
  DEBIT = debit_map_seq(
    tibble(x = c(0.35, 0.47), sd = c(0.48, 0.50), n = c(20L, 40L)),
    digits_x = 2,
    digits_sd = 2,
    dispersion = 1:2
  )
)
for (name_test in names(seqs)) {
  check(
    paste(name_test, "sequence summary renames cleanly"),
    is.data.frame(rename_after_audit_seq(
      audit_seq(seqs[[name_test]]),
      name_test
    ))
  )
  check(
    paste(name_test, "sequence results rename cleanly"),
    is.data.frame(rename_after_testing_seq(seqs[[name_test]], name_test, FALSE))
  )
}

# Every rounding method the UI offers must map to something scrutiny accepts.
methods_ui <- c(
  "Up or down",
  "Up",
  "Down",
  "Ceiling or floor",
  "Ceiling",
  "Floor",
  "Truncate",
  "Anti-truncate"
)
for (m in methods_ui) {
  check(
    paste("rounding:", m),
    nrow(grim_map(
      d[, c("x", "n")],
      digits_x = 2,
      rounding = select_rounding_method(m)
    )) ==
      3
  )
}

# --- Items handling, driven through the real reactive graph ----------------
# The items column is folded into `n` by the app, and scrutiny will silently
# fold a leftover `items` column in a second time. Both flow into `n`, and an
# inflated `n` only ever makes GRIM more permissive, so a mistake here clears
# real inconsistencies without erroring.

suppressMessages(library(shiny))

csv <- function(text) {
  path <- tempfile(fileext = ".csv")
  writeLines(text, path)
  path
}
upload <- function(path) {
  data.frame(
    name = basename(path),
    size = file.size(path),
    type = "text/csv",
    datapath = path
  )
}
# `n_items` is the column to merge; `items` is an unrelated leftover.
clash <- csv(c("x,n,n_items,items", "5.30,40,3,7", "4.10,25,3,7"))

base <- list(
  use_example_data_pigs5 = FALSE,
  x = "x",
  sd = "sd",
  n = "n",
  items_col = "",
  digits = 0,
  name_test = "GRIM",
  mean_percent = "Mean",
  items = 1,
  rounding = "Up or down",
  dispersion = 5,
  plot_size_text = 14
)

testServer(shinyAppFile("app.R"), {
  # Readable before any file is loaded: the old reactiveVal was only set as a
  # side effect of `user_data()`, so this read FALSE until data happened to
  # load.
  do.call(
    session$setInputs,
    modifyList(base, list(items_col = "n_items", items = 4))
  )
  check("items column is active before any upload", items_col_active())
  check(
    "scalar items is bypassed while a column is active",
    effective_items() == 1L
  )

  do.call(
    session$setInputs,
    modifyList(
      base,
      list(
        input_df = upload(clash),
        items_col = "n_items",
        items = 1,
        digits = 2
      )
    )
  )
  check(
    "items column is folded into n exactly once",
    all(testable_data()$n == c(120L, 75L))
  )
  check(
    "a leftover `items` column never reaches scrutiny",
    !"items" %in% names(testable_data())
  )
  # Fails at n = 525, the sample size the leftover column used to produce.
  check(
    "4.10 at the merged n = 75 stays inconsistent",
    !tested_df()$consistency[2]
  )

  # Turning the column off must revert immediately, with no stale flag.
  session$setInputs(items_col = "", items = 4)
  check(
    "clearing the column restores the scalar input",
    effective_items() == 4L
  )
  check("clearing the column restores n", all(testable_data()$n == c(40L, 25L)))

  # Trailing zeros survive the upload, and each row is tested at the
  # precision it was reported with: "4.10" is inconsistent at n = 25, "4.1"
  # is not. Before, readr had already turned both into 4.1.
  mixed <- csv(c("x,n,id", "4.1,25,001", "4.10,25,002", "5.00,40,003"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(mixed))))
  check("upload keeps the mean as text", is.character(user_data()$x))
  check("upload keeps trailing zeros", identical(user_data()$x[2], "4.10"))
  check("non-key columns are typed", is.integer(user_data()$n))
  check("IDs keep leading zeros", identical(user_data()$id[1], "001"))
  check(
    "digits are declared per row",
    identical(test_input()$digits_x, c(1L, 2L, 2L))
  )
  check(
    "4.1 cleared, 4.10 flagged, 5.00 (= 200/40) cleared",
    identical(tested_df()$consistency, c(TRUE, FALSE, TRUE))
  )
  check(
    "sequences disperse only the flagged row, numbered as in the results",
    identical(unique(tested_df_seq()$case), 2L)
  )
  check(
    "sequence summary works on the remapped cases",
    nrow(df_audit_seq()) == 1L
  )
  check("the user can raise the floor", {
    session$setInputs(digits = 2)
    identical(tested_df()$consistency, c(FALSE, FALSE, TRUE))
  })

  # European format: semicolons and comma decimal marks.
  euro <- csv(c("x;sd;n", "4,10;1,20;25", "5,30;0,50;40"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(euro))))
  check(
    "European decimal marks are normalised",
    identical(user_data()$x, c("4.10", "5.30"))
  )
  check(
    "European sample sizes are numbers",
    identical(user_data()$n, c(25L, 40L))
  )
  check("European 4,10 at n = 25 is inconsistent", !tested_df()$consistency[1])

  # Displayed and downloaded at the tested precision: as a double, "4.10"
  # prints as 4.1, which reads as consistent next to a FALSE verdict.
  do.call(session$setInputs, modifyList(base, list(input_df = upload(mixed))))
  check(
    "results table shows 4.10, not 4.1",
    grepl("> 4.10 <", output$output_df)
  )
  check("results table shows 5.00 in full", grepl("> 5.00 <", output$output_df))
  check(
    "download keeps the tested precision",
    identical(
      read.csv(output$download_consistency_test, colClasses = "character")$mean,
      c("4.1", "4.10", "5.00")
    )
  )
  # grim_plot() errors on mixed precision; the app must say why instead.
  check(
    "plot on mixed precision is a message, not a crash",
    inherits(
      tryCatch(output$output_plot, error = identity),
      "shiny.silent.error"
    )
  )

  # A European file with only two columns has one ";" and no ",".
  euro2 <- csv(c("x;n", "4,10;25", "5,30;40"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(euro2))))
  check(
    "two-column European file is detected",
    identical(user_data()$x, c("4.10", "5.30"))
  )

  # Semicolons with dot decimals: "." must not be read as a grouping mark.
  semi <- csv(c("x;n;other", "4.10;25;4.10", "5.30;40;5.3"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(semi))))
  check(
    "semicolon file keeps dot decimals",
    !any(user_data()$other %in% c(410, 53))
  )

  # Items only apply where the sidebar shows them. A value left in the hidden
  # field must not inflate `n` for percentages.
  pct <- csv(c("x,n,k", "10.4,23,5"))
  do.call(
    session$setInputs,
    modifyList(
      base,
      list(input_df = upload(pct), mean_percent = "Percentage", items = 5)
    )
  )
  check(
    "hidden items field is ignored for percentages",
    effective_items() == 1L
  )
  check("10.4% at n = 23 stays inconsistent", !tested_df()$consistency)
  session$setInputs(items_col = "k")
  check("items column is not merged for percentages", testable_data()$n == 23L)
  session$setInputs(name_test = "DEBIT", items = NA, items_col = "")
  check(
    "DEBIT is not blocked by the hidden items field",
    effective_items() == 1L
  )

  # Duplicate analysis sees the data as uploaded, not with items merged into
  # `n` -- that fabricated a duplicate between `n` and `other` (75, 80).
  dup <- csv(c("x,n,k,other", "4.10,25,3,75", "5.30,40,2,80"))
  do.call(
    session$setInputs,
    modifyList(base, list(input_df = upload(dup), items_col = "k"))
  )
  check(
    "upload keeps n and the items column",
    identical(user_data()$n, c(25L, 40L)) && "k" %in% names(user_data())
  )
  check(
    "the test still sees merged n",
    identical(testable_data()$n, c(75L, 80L))
  )

  # --- UX ------------------------------------------------------------------
  silent <- function(expr) {
    inherits(tryCatch(expr, error = identity), "shiny.silent.error")
  }
  fail_msg <- function(expr) tryCatch({expr; ""}, error = conditionMessage)

  # Mapping onto a name the data already uses is explained, not a vctrs error.
  coll <- csv(c("x,mean,n", "1,4.10,25"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(coll), x = "mean")))
  check("column-name collision is a validation message", silent(user_data()))

  # A printed placeholder is a missing value: the row is dropped and counted.
  ph <- csv(c("x,n", "4.10,25", "-,40", "NR,40", "5.30,40"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(ph))))
  check("placeholders are dropped, not fatal", nrow(testable_data()) == 2L && n_dropped() == 2L)
  check(
    "the summary carries the excluded rows",
    df_audit()$excluded_rows == 2L &&
      "Rows excluded (missing values)" %in% names(rename_after_audit(df_audit(), FALSE))
  )
  bad <- csv(c("x,n", "4.10,25", "abc,40"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(bad))))
  check("a non-number is named in the message", grepl('"abc"', fail_msg(testable_data())))

  # Mixed precision is pointed out until the user declares one.
  do.call(session$setInputs, modifyList(base, list(input_df = upload(mixed))))
  check("mixed precision gets a note", grepl("between 1 and 2", output$precision_note$html))
  session$setInputs(digits = 2)
  check("the note goes once precision is declared", !nzchar(output$precision_note$html))

  # Verdicts read as words on screen, logical in downloads.
  check("results table labels verdicts", grepl("Inconsistent", output$output_df))

  # At n = 1000, every one-decimal mean is attainable: GRIM
  # passes 4.2 without testing it, and must not display that as a pass.
  big <- csv(c("x,n", "4.2,1000", "4.10,25"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(big))))
  check("an untestable row is labelled as such", grepl("Not testable", output$output_df))
  check("a tested row is not", grepl("Inconsistent", output$output_df))

  # Sequences step at each row's own precision, not the finest in the data.
  steps <- csv(c("x,n", "4.10,25", "4.5,3"))
  do.call(session$setInputs, modifyList(base, list(input_df = upload(steps))))
  check("both rows are flagged", identical(tested_df()$consistency, c(FALSE, FALSE)))
  seq_digits <- distinct(tested_df_seq(), case, digits_x)
  check("each row is dispersed at its own precision", identical(seq_digits$digits_x, c(2L, 1L)))
  check("sequence summary has one row per case, in order", identical(df_audit_seq()$x, c(4.1, 4.5)))
  check("sequence plot on mixed precision is a message", silent(output$output_plot_seq))

  session$setInputs(dispersion = 150)
  check("dispersion above 100 is refused, not clamped", silent(tested_df_seq()))

  # The example data gets its two decimal places back.
  do.call(
    session$setInputs,
    modifyList(base, list(use_example_data_pigs5 = TRUE, name_test = "GRIMMER"))
  )
  check(
    "example data is declared at 2 decimal places",
    all(test_input()$digits_x == 2L, test_input()$digits_sd == 2L)
  )
  check("GRIMMER on the example data runs", is.logical(tested_df()$consistency))
})

cat("\nAll checks passed.\n")
