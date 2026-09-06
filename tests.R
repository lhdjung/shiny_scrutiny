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
raw <- tibble(x = c("4.10", "5.00"), n = c("25", "40"))
fmt <- format_after_upload(raw)
check("`n` becomes integer", is.integer(fmt$n))
check("`x` is not collapsed to integer", !is.integer(fmt$x))
check("declared digits read 2 from the strings", digits_declared(fmt$x, 0) == 2)
check(
  "a whole-number mean keeps its zeros",
  !grim_map(
    mutate(fmt, x = as.numeric(x)),
    digits_x = digits_declared(fmt$x, 0)
  )$consistency[1]
)

# `input$digits` is the floor, used when readr already dropped the zeros.
check("user-declared digits win", digits_declared(c(4.1, 5.3), 2) == 2)
check("observed digits win when higher", digits_declared(c(4.123, 5.3), 2) == 3)
check(
  "all-NA column does not error",
  digits_declared(c(NA_real_, NA_real_), 0) == 0
)

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
})

cat("\nAll checks passed.\n")
