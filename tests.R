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

cat("\nAll checks passed.\n")
