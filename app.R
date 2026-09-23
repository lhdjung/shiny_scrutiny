library(shiny)
library(bslib)
library(rlang)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(scrutiny)

# scrutiny 1.0.0 takes decimal places as `digits_x` / `digits_sd`; with an older
# version, every test fails on those arguments.
if (packageVersion("scrutiny") < "1.0.0") {
  stop("This app needs scrutiny 1.0.0 or later.")
}


# Load helper functions:
source("scripts/functions.R")

# # Deploy like this:
# rsconnect::deployApp(
#   appName = "scrutiny",
#   account = "errors"
# )

# Define UI ---------------------------------------------------------------

ui <- page_navbar(
  title = "Error detection (beta 0.3.0)",
  id = "nav",
  header = tags$style(".card-header { text-align: center; }"),

  # Sidebar ---------------------------------------------------------------

  sidebar = sidebar(
    # Sidebar: data upload ------------------------------------------------

    conditionalPanel(
      "input.nav === 'Data upload'",
      fileInput(
        "input_df",
        "Upload your data:",
        # Now fixed to accept CSV and TSV files
        accept = c("text/csv", ".csv", ".tsv")
      ),
      checkboxInput(
        "use_example_data_pigs5",
        label = "Use example data",
        value = FALSE
      ),
      # Identifying `x` and `n` columns:
      textInput("x", "Mean / percentage column:", "x"),
      textInput("sd", "Standard deviation column:", "sd"),
      textInput("n", "Sample size column:", "n"),
      textInput("items_col", "Items column:", "") |>
        tooltip(
          "If the data has a column with the number of scale items per mean, \
          enter its name here. Its integer values will be multiplied with the \
          sample size column to form the effective sample size when testing \
          means with GRIM or GRIMMER. Leave blank \
          to ignore."
        )
    ),

    # Sidebar: consistency testing ----------------------------------------

    conditionalPanel(
      "input.nav === 'Consistency testing'",
      selectInput(
        "name_test",
        "Consistency test:",
        choices = c("GRIM", "GRIMMER", "DEBIT")
      ) |>
        tooltip("See \"About\" for more information."),
      # Mean / percentage selection:
      conditionalPanel(
        "input.name_test === 'GRIM'",
        selectInput(
          "mean_percent",
          label = "Mean or percentage?",
          choices = c("Mean", "Percentage")
        ) |>
          tooltip(
            "For GRIM, if the \"x\" column in your data contains percentages, \
            they will be deflated (that is, divided by 100) before testing."
          )
      ),
      # Number of items:
      conditionalPanel(
        "(input.name_test === 'GRIM' && input.mean_percent === 'Mean') || input.name_test === 'GRIMMER'",
        numericInput(
          "items",
          label = "Number of scale items",
          value = 1,
          min = 1,
          step = 1
        ) |>
          tooltip(
            "If the scale from which the means are derived is composed \
            of multiple items, enter the number of those items here."
          ),
        uiOutput("items_conflict_warning")
      ),
      numericInput(
        "digits",
        label = "Restore decimal zeros:",
        value = 0L,
        min = 0
      ) |>
        tooltip(
          "Decimal numbers may have lost trailing zeros, but these are \
          important when testing for consistency. They are padded with \
          zeros to match the number chosen here or the greatest number \
          of decimal places from among them, whichever is greater."
        ),
      selectInput(
        "rounding",
        label = "Rounding method:",
        choices = c(
          "Up or down",
          "Up",
          "Down",
          "Ceiling or floor",
          "Ceiling",
          "Floor",
          "Truncate",
          "Anti-truncate"
        )
      ) |>
        tooltip(
          "The consistency of summary data is determined on the basis of \
          reconstructing numbers rounded in the chosen way (or in either \
          of two ways, as with the permissive default \"Up or down\")."
        ),
      numericInput(
        "dispersion",
        label = "Dispersion:",
        value = 5,
        min = 1,
        max = 100,
        step = 1
      ) |>
        tooltip(
          "How far should the dispersed sequences be spread out?
          You can define the number of steps. For example, the default
          \"5\" goes five steps up and down from the reported values."
        ),
      numericInput(
        "plot_size_text",
        label = "Plot text size:",
        value = 14,
        min = 1
      ),
      downloadButton("download_consistency_test", "Download results by case"),
      downloadButton(
        "download_consistency_test_summary",
        "Download summary of results by case"
      ),
      downloadButton(
        "download_consistency_test_seq",
        "Download dispersed sequences"
      ),
      downloadButton(
        "download_consistency_test_audit_seq",
        "Download summary (dispersed sequences)"
      )
    ),

    # Sidebar: duplicate analysis ----------------------------------------

    conditionalPanel(
      "input.nav === 'Duplicate analysis'",
      downloadButton("download_duplicate_count", "Download frequency table"),
      downloadButton(
        "download_duplicate_count_audit",
        "Download summary (frequency table)"
      ),
      downloadButton(
        "download_duplicate_count_colpair",
        "Download duplicates across columns"
      ),
      downloadButton(
        "download_duplicate_count_colpair_audit",
        "Download summary (duplicates across columns)"
      ),
      downloadButton(
        "download_duplicate_tally",
        "Download value tally at original location"
      ),
      downloadButton(
        "download_duplicate_tally_audit",
        "Download summary (value tally at original location)"
      )
    ),
    conditionalPanel("input.nav === 'About'")
  ),

  # Nav panel -----------------------------------------------------------

  # Nav panel: data upload ----------------------------------------------

  nav_panel(
    "Data upload",
    div(
      style = "max-width: 75%; margin: 0 auto;",
      card(
        card_header("Information"),
        uiOutput("text_info_upload")
      ),
      card(
        card_header_help(
          "Data preview",
          "Your data. Rename columns in the sidebar on the left \
          if they don't already have the names shown there."
        ),
        styled_table_div("uploaded_data")
      )
    )
  ),

  # Nav panel: consistency testing -----------------------------------------

  nav_panel(
    "Consistency testing",
    uiOutput("dropped_rows_note"),
    uiOutput("precision_note"),

    # Basic analyses -- two long cards side by side:
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header_help(
          "Results by case",
          "Your data, tested for consistency."
        ),
        styled_table_div("output_df"),
        max_height = "500px",
        full_screen = TRUE
      ),
      card(
        card_header_help(
          "Visualization",
          "Blue: consistent, red: inconsistent. The grey background \
          flags all possible inconsistent combinations, whether \
          present in the data or not."
        ),
        plotOutput("output_plot"),
        max_height = "500px",
        full_screen = TRUE
      )
    ),
    # Temporary note about partially incorrect info in the plot tooltips for
    # DEBIT -- they are simply the same as for GRIM and GRIMMER.
    conditionalPanel(
      "input.name_test === 'DEBIT'",
      card(
        card_header("Please note"),
        textOutput("debit_plot_tooltip_note"),
        max_height = "500px",
        full_screen = TRUE
      )
    ),
    # Basic analyses -- one wide card below:
    card(
      card_header_help(
        "Summary of results by case",
        "Simple summaries of testing your data."
      ),
      styled_table_div("output_df_audit"),
      full_screen = TRUE
    ),
    # Further analyses -- two long cards side by side:
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header_help(
          "Results of dispersed sequences",
          "Checking the numeric neighborhood of inconsistent value sets \
          for consistent ones. Variables to the left of \"consistency\" \
          are marginally varied up and down, holding the other one(s) \
          constant each time."
        ),
        styled_table_div("output_df_seq"),
        max_height = "500px",
        full_screen = TRUE
      ),
      card(
        card_header_help(
          "Visualization of dispersed sequences",
          "Blue: consistent, red: inconsistent. The cross pattern emerges \
          because values are varied up and down along both axes.
          The grey background flags all inconsistent combinations, \
          whether present in the data or not."
        ),
        plotOutput("output_plot_seq"),
        max_height = "500px",
        full_screen = TRUE
      )
    ),
    # Further analyses -- one wide card below:
    card(
      card_header_help(
        "Summary of dispersed sequences",
        "A \"hit\" is a consistent value set found by varying the \
        inconsistent numbers above. \"Hits for\" a variable \
        are those found by varying that variable. \"Least step difference\" \
        is the minimum number of steps between the reported values \
        of a variable and the nearby consistent ones. \
        They are split up by the direction of variation: upward and downward.
        \"NA\" indicates that no hits could be found in the respective way."
      ),
      styled_table_div("output_df_audit_seq"),
      full_screen = TRUE
    )
  ),

  # Nav panel: duplicate analysis --------------------------------------

  nav_panel(
    "Duplicate analysis",
    card(
      card_header_help(
        "Frequency table",
        "Ranked by the frequency. Locations are
        the names of the columns in your data where a given value appears."
      ),
      styled_table_div("output_duplicate_count"),
      full_screen = TRUE
    ),
    card(
      card_header_help(
        "Summary (frequency table)",
        "Summary statistics of the two numeric columns
        from the frequency table."
      ),
      styled_table_div("output_duplicate_count_summary"),
      full_screen = TRUE
    ),
    card(
      card_header_help(
        "Duplicates across columns",
        "This checks each pair of columns in your data for duplicates:
        values that appear in both columns. Shown on the right are
        the proportion of values in the original column 1 that are also
        found in column 2, and vice versa. These two \"Proportion\" columns
        are equal unless some values are missing. The same is true
        of the \"Total number\" columns in the center;
        they also exclude missing values."
      ),
      styled_table_div("output_duplicate_count_colpair"),
      full_screen = TRUE
    ),
    card(
      card_header_help(
        "Summary (duplicates across columns)",
        "Summary statistics of all columns from the cross-column table
        (except for those that list the original columns from your data)."
      ),
      styled_table_div("output_duplicate_count_colpair_summary"),
      full_screen = TRUE
    ),
    card(
      card_header_help(
        "Value tally at original location",
        "Next to each column from your data, an \"_n\" column shows
        how often its values appear in the data overall.
        Note that the frequency of each value appears a number
        of times equal to the frequency itself."
      ),
      styled_table_div("output_duplicate_tally"),
      full_screen = TRUE
    ),
    card(
      card_header_help(
        "Summary (value tally at original location)",
        "Summary statistics of the \"_n\" columns.
        Because the frequencies appear as often as their own value says,
        these statistics should be interpreted with caution."
      ),
      styled_table_div("output_duplicate_tally_summary"),
      full_screen = TRUE
    )
  ),

  # Nav panel: other elements ---------------------------------------------

  nav_panel(
    "About",
    uiOutput("text_about")
  ),
  nav_spacer(),
  nav_item(a(
    href = "https://error.reviews/",
    img(src = "uni_bern_funding.drawio.svg", height = "40px")
  )),
  fillable = FALSE,
  theme = bs_theme(version = 5)
)


# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  # An items column supersedes the scalar "Number of scale items" input. Derived
  # from the input rather than stored: `user_data()` folds the column into `n`
  # and drops it, so a separate flag would only be a second copy of this fact,
  # written as a side effect and read before it was set.
  items_col_active <- reactive(isTruthy(input$items_col))

  # Server: data upload ---------------------------------------------------

  output$text_info_upload <- renderUI({
    htmltools::tagList(
      p(
        "Please upload a file in a tabular format such as CSV
        (or check \"Use example data\" on the left)."
      ),
      p(
        "For GRIM and other consistency tests, it should have
        columns with specific types of summary data:
        All tests require mean and sample size columns.
        GRIMMER and DEBIT additionally require a standard deviation
        column. You may need to specify the columns (see sidebar left).
        They will be shown renamed below. Duplicate analysis
        doesn't require any specific columns."
      ),
      p(
        "Select \u24d8 next to a panel's title for information about it."
      )
    )
  })

  # Capture the user-uploaded dataframe and, if necessary, rename some columns:
  user_data <- reactive({
    # Optionally, use the example `pigs5` data instead of user-uploaded data:
    if (input$use_example_data_pigs5) {
      # scrutiny 1.0.0 stores `pigs5` as numbers; its values were reported to
      # two decimal places.
      out <- mutate(
        pigs5,
        across(c(x, sd), \(col) restore_zeros(col, width = 2))
      )
      is_european <- FALSE
    } else {
      validate(need(input$input_df, "Upload data first."))
      # Detect European CSV format (semicolon-delimited, comma decimal mark).
      # Counting both separators beats testing for a bare ";", which misreads a
      # comma-delimited file whose header merely contains one.
      first_line <- readLines(
        input$input_df$datapath,
        n = 1L,
        encoding = "UTF-8",
        warn = FALSE
      )
      validate(need(length(first_line) == 1L, "ERROR: The file is empty."))
      count_char <- function(char) {
        nchar(first_line) - nchar(gsub(char, "", first_line, fixed = TRUE))
      }
      is_european <- count_char(";") > count_char(",")
      loc <- if (is_european) {
        # Not ".": a semicolon file with dot decimals would read 4.10 as 410.
        # Such values stay text instead.
        locale(decimal_mark = ",", grouping_mark = "'")
      } else {
        default_locale()
      }
      # Read everything as text: a number parsed as a double loses its
      # trailing zeros, and with them the precision the tests need. The
      # non-key columns are converted below, once the key columns are known.
      out <- read_delim(
        input$input_df$datapath,
        delim = if (is_european) ";" else NULL,
        col_types = cols(.default = col_character()),
        locale = loc,
        # Placeholders that papers print for a missing value. Read as missing,
        # the row is dropped and counted; as text, it blocked the whole test.
        na = c("", "NA", "N/A", "n/a", "NR", "-", "\u2013", "\u2014"),
        trim_ws = TRUE,
        show_col_types = FALSE
      )
    }

    # Rename the key columns if their names are not "x" and "n" etc.:
    for (key in c("x", "sd", "n")) {
      name_given <- input[[key]]
      if (!nzchar(name_given) || name_given == key) {
        next
      }
      validate(need(
        name_given %in% names(out),
        paste0("ERROR: Column \"", name_given, "\" not found in the data.")
      ))
      validate(need(
        !key %in% names(out),
        paste0(
          "ERROR: Column \"", name_given, "\" can't be used as \"", key,
          "\" because the data already has a column named \"", key,
          "\". Rename that one in your file first."
        )
      ))
      out <- rename(out, !!key := !!name_given)
    }

    # `x` and `sd` stay as uploaded; everything else gets its natural type.
    if (!input$use_example_data_pigs5) {
      cols_precise <- intersect(c("x", "sd"), names(out))
      cols_other <- setdiff(names(out), cols_precise)
      if (length(cols_other) > 0L) {
        out[cols_other] <- suppressMessages(type_convert(
          out[cols_other],
          locale = loc
        ))
      }
      if (is_european) {
        out <- mutate(
          out,
          across(all_of(cols_precise), \(col) str_replace(col, ",", "."))
        )
      }
    }

    format_after_upload(out)
  })

  # A new upload means the user wants to see it, not the example data, which
  # would otherwise keep silently taking precedence.
  observeEvent(input$input_df, {
    updateCheckboxInput(session, "use_example_data_pigs5", value = FALSE)
  })

  name_input_file <- reactive({
    if (input$use_example_data_pigs5) {
      "example"
    } else {
      input$input_df$name %||% "data"
    }
  })

  percent <- reactive({
    if (input$name_test == "GRIM") {
      input$mean_percent == "Percentage"
    } else {
      NULL
    }
  })

  # Items apply only where the sidebar shows them. A value left in the hidden
  # field would otherwise multiply `n` for percentages, clearing real
  # inconsistencies.
  uses_items <- reactive({
    input$name_test == "GRIMMER" ||
      (input$name_test == "GRIM" && input$mean_percent == "Mean")
  })

  # The merge has already folded the items into `n`, so scrutiny must not
  # multiply by them a second time.
  effective_items <- reactive({
    if (!uses_items() || items_col_active()) {
      return(1L)
    }
    validate(need(
      isTruthy(input$items) &&
        input$items >= 1 &&
        is_whole_number(input$items),
      "ERROR: The number of scale items must be a positive whole number."
    ))
    as.integer(input$items)
  })

  plot_size_text <- reactive({
    if (isTruthy(input$plot_size_text)) input$plot_size_text else 14
  })

  output$items_conflict_warning <- renderUI({
    if (items_col_active() && isTruthy(input$items) && input$items > 1) {
      tags$p(
        style = "color: orange; font-size: 0.85em;",
        "\u26a0 \u201cNumber of scale items\u201d is ignored because an items column is active."
      )
    }
  })

  # Display uploaded data:
  output$uploaded_data <- renderTable({
    user_data()
  })

  # Server: consistency testing -------------------------------------------

  rounding_method <- reactive({
    select_rounding_method(input$rounding)
  })

  # Filter user data to rows with complete required columns for the selected

  # test. GRIM needs x and n; GRIMMER and DEBIT also need sd.
  testable_data <- reactive({
    required_cols <- if (input$name_test == "GRIM") {
      c("x", "n")
    } else {
      c("x", "sd", "n")
    }
    df <- user_data()
    validate(need(
      all(required_cols %in% names(df)),
      paste0(
        "ERROR: This test needs a \"",
        paste(setdiff(required_cols, names(df)), collapse = "\" and a \""),
        "\" column. Name it in the sidebar."
      )
    ))

    # Merge the items column into `n` here, not at upload: duplicate analysis
    # must see the data as uploaded, and DEBIT and percentages take no items.
    items_col_name <- input$items_col
    if (uses_items() && items_col_active()) {
      validate(need(
        items_col_name %in% names(df),
        paste0(
          "ERROR: Items column \"",
          items_col_name,
          "\" not found in the data."
        )
      ))
      items_vals <- df[[items_col_name]]
      validate(need(
        is.numeric(items_vals),
        paste0(
          "ERROR: The items column (\"",
          items_col_name,
          "\") must be a numeric ",
          "column, not strings."
        )
      ))
      validate(need(
        is.numeric(df[["n"]]),
        "ERROR: The sample size column must be numeric to merge with the items column."
      ))
      validate(need(
        all(is_whole_number(items_vals), na.rm = TRUE),
        paste0(
          "ERROR: The items column (\"",
          items_col_name,
          "\") must contain ",
          "whole numbers only."
        )
      ))
      df$n <- df$n * as.integer(items_vals)
      df[[items_col_name]] <- NULL
    }

    df <- df[complete.cases(df[, required_cols]), ]
    # Drop any surviving "items" column. scrutiny consumes one silently and
    # multiplies `n` by it, overriding the `items` argument. A column consumed
    # by the merge is already gone, so this only ever hits an unrelated
    # leftover.
    df[["items"]] <- NULL
    validate(need(
      nrow(df) > 0,
      "ERROR: No rows have all of the required columns."
    ))
    n_num <- suppressWarnings(as.numeric(df$n))
    bad_n <- df$n[is.na(n_num) | !is_whole_number(n_num) | n_num <= 0]
    validate(need(
      length(bad_n) == 0L,
      paste(
        "ERROR: The sample size column must contain positive whole numbers",
        "only. Found:",
        quote_values(bad_n)
      )
    ))
    for (key in setdiff(required_cols, "n")) {
      bad <- df[[key]][is.na(suppressWarnings(as.numeric(df[[key]])))]
      validate(need(
        length(bad) == 0L,
        paste0(
          "ERROR: The \"", key, "\" column must contain numbers only. Found: ",
          quote_values(bad)
        )
      ))
    }
    df
  })

  # What the mappers receive: key columns as numbers, with the precision they
  # were reported at travelling separately, one value per row, in `digits_x` /
  # `digits_sd`. Taken from the same rows so that the two line up.
  test_input <- reactive({
    df <- testable_data()
    list(
      df = mutate(df, across(any_of(c("x", "sd")), as.numeric)),
      digits_x = digits_declared(df$x, input$digits),
      digits_sd = if (input$name_test == "GRIM") {
        NULL
      } else {
        digits_declared(df$sd, input$digits)
      }
    )
  })

  # How many rows the tests never saw. Silence here would understate the
  # denominator of every rate the app reports.
  n_dropped <- reactive(nrow(user_data()) - nrow(testable_data()))

  output$dropped_rows_note <- renderUI({
    n_dropped <- n_dropped()
    if (n_dropped < 1) {
      return(NULL)
    }
    tags$p(
      style = "color: orange;",
      sprintf(
        "\u26a0 %d of %d row(s) were excluded from testing because a required
        value was missing. Reported counts and rates below cover the remaining
        %d row(s) only.",
        n_dropped,
        nrow(user_data()),
        nrow(testable_data())
      )
    )
  })

  # Lost trailing zeros can't be detected, only suspected: a column whose
  # values show different numbers of decimal places. Excel drops such zeros,
  # and a lost zero only ever clears inconsistencies.
  output$precision_note <- renderUI({
    ti <- test_input()
    notes <- lapply(c("x", "sd"), function(key) {
      digits <- ti[[paste0("digits_", key)]]
      if (length(unique(digits)) < 2L) {
        return(NULL)
      }
      name_col <- if (nzchar(input[[key]])) input[[key]] else key
      tags$p(
        style = "color: orange;",
        sprintf(
          "\u26a0 Values in the \"%s\" column show between %d and %d decimal
          places, and each is tested at its own precision. If the source
          reported them all with %d, trailing zeros were lost (e.g., by
          Excel): set \"Restore decimal zeros\" in the sidebar to %d.",
          name_col,
          min(digits),
          max(digits),
          max(digits),
          max(digits)
        )
      )
    })
    htmltools::tagList(notes)
  })

  # `numericInput` yields NA while the field is empty, and its bounds don't
  # stop typed values.
  dispersion_steps <- reactive({
    validate(need(
      isTruthy(input$dispersion) &&
        input$dispersion >= 1 &&
        input$dispersion <= 100,
      "ERROR: Dispersion must be a whole number from 1 to 100."
    ))
    as.integer(input$dispersion)
  })

  # Basic analyses:
  tested_df <- reactive({
    # Forced here, not left as lazy arguments: evaluated inside scrutiny, a
    # validate() message is rethrown as a raw error instead of rendering.
    method <- rounding_method()
    ti <- test_input()
    df <- ti$df
    items <- effective_items()
    is_percent <- percent()

    if (input$name_test == "DEBIT") {
      msg_error <- "ERROR: DEBIT only works with means and SDs of binary data."
      validate(
        need(all(between(df$x, 0, 1)), msg_error),
        need(all(between(df$sd, 0, 1)), msg_error)
      )
    }

    # Test for consistency using a mapping function
    out <- switch(
      input$name_test,
      "GRIM" = grim_map(
        df,
        digits_x = ti$digits_x,
        items = items,
        percent = is_percent,
        rounding = method
      ),
      "GRIMMER" = grimmer_map(
        df,
        digits_x = ti$digits_x,
        digits_sd = ti$digits_sd,
        items = items,
        rounding = method
      ),
      "DEBIT" = debit_map(
        df,
        digits_x = ti$digits_x,
        digits_sd = ti$digits_sd,
        rounding = method
      )
    )

    # Many consistency tests have a key argument / column corresponding to the
    # sample size ("n"). It should be integer because, as a double, the app
    # would misleadingly display it with decimal zeros, like, e.g., "5.00".
    if (any(names(out) == "n")) {
      out <- mutate(out, n = as.integer(n))
    }
    out
  })

  output$output_df <- renderTable({
    df <- tested_df()
    df |>
      format_tested_values() |>
      rename_after_testing(
        input$name_test,
        percent = percent()
      ) |>
      label_consistency(untestable = grim_untestable(df))
  })

  # The excluded rows travel with the summary, so that a downloaded rate
  # doesn't lose its denominator.
  df_audit <- reactive({
    mutate(audit(tested_df()), excluded_rows = n_dropped())
  })

  output$output_df_audit <- renderTable({
    df_audit() |>
      rename_after_audit(percent())
  })

  output$output_plot <- renderPlot({
    df <- tested_df()
    # grim_plot() draws one precision per raster and errors on a mix.
    digits_seen <- sort(unique(df$digits_x))
    validate(need(
      input$name_test == "DEBIT" || length(digits_seen) == 1L,
      paste0(
        "The plot needs means reported at a single number of decimal ",
        "places; these have ",
        paste(digits_seen, collapse = ", "),
        ". The test results are unaffected."
      )
    ))
    plot_test_results(df, input$name_test, plot_size_text())
  })

  # Results of dispersed sequences:

  # The sequence mappers take a single `digits_x` / `digits_sd` per call, so
  # the rows flagged above are dispersed in groups of equal precision: each in
  # steps of the decimal places it was reported with.
  tested_seq_parts <- reactive({
    method <- rounding_method()
    ti <- test_input()
    items <- effective_items()
    is_percent <- percent()
    steps <- seq_len(dispersion_steps())
    msg_none <- "No inconsistent cases to disperse from. All tested values are consistent."
    inconsistent <- which(!tested_df()$consistency)
    validate(need(length(inconsistent) > 0L, msg_none))

    groups <- split(
      inconsistent,
      paste(ti$digits_x[inconsistent], ti$digits_sd[inconsistent])
    )
    parts <- lapply(unname(groups), function(rows) {
      df <- ti$df[rows, ]
      dp_x <- ti$digits_x[rows[1L]]
      dp_sd <- ti$digits_sd[rows[1L]] # NULL for GRIM
      out <- suppressWarnings(switch(
        input$name_test,
        "GRIM" = grim_map_seq(
          df,
          digits_x = dp_x,
          dispersion = steps,
          items = items,
          percent = is_percent,
          rounding = method
        ),
        "GRIMMER" = grimmer_map_seq(
          df,
          digits_x = dp_x,
          digits_sd = dp_sd,
          dispersion = steps,
          items = items,
          rounding = method
        ),
        "DEBIT" = debit_map_seq(
          df,
          digits_x = dp_x,
          digits_sd = dp_sd,
          dispersion = steps,
          rounding = method
        )
      ))
      # `case` counts rows of what the mapper saw; point it back at the rows
      # of the results table above.
      out$case <- rows[out$case]
      out
    })
    parts <- Filter(\(part) nrow(part) > 0L, parts)
    validate(need(length(parts) > 0L, msg_none))
    parts
  })

  # Binding drops scrutiny's classes, so the bound table is for display and
  # download only; the summary and plot work on the parts.
  tested_df_seq <- reactive({
    arrange(bind_rows(tested_seq_parts()), case)
  })

  df_audit_seq <- reactive({
    parts <- tested_seq_parts()
    cases <- unlist(lapply(parts, \(part) unique(part$case)))
    bind_rows(lapply(parts, audit_seq))[order(cases), ]
  })

  output$output_df_seq <- renderTable({
    df <- tested_df_seq()
    df |>
      format_tested_values() |>
      rename_after_testing_seq(
        input$name_test,
        percent = percent()
      ) |>
      label_consistency(untestable = grim_untestable(df))
  })

  output$output_df_audit_seq <- renderTable({
    df_audit_seq() |>
      mutate(across(
        .cols = starts_with("hits") | starts_with("diff"),
        .fns = as.integer
      )) |>
      rename_after_audit_seq(input$name_test) |>
      label_consistency()
  })

  output$output_plot_seq <- renderPlot({
    parts <- tested_seq_parts()
    validate(need(
      length(parts) == 1L,
      paste(
        "The plot needs inconsistent values reported at a single number of",
        "decimal places. The sequences are unaffected."
      )
    ))
    plot_test_results(parts[[1L]], input$name_test, plot_size_text())
  })
  # Server: duplicate analysis -------------------------------------------

  # Conduct the duplicate analyses:
  duplicate_count_df <- reactive({
    user_data() |>
      duplicate_count()
  })
  duplicate_count_colpair_df <- reactive({
    user_data() |>
      duplicate_count_colpair()
  })
  duplicate_tally_df <- reactive({
    user_data() |>
      duplicate_tally()
  })

  # Display the duplicate analyses:
  output$output_duplicate_count <- renderTable({
    duplicate_count_df() |>
      rename_duplicate_count_df()
  })
  output$output_duplicate_count_colpair <- renderTable({
    duplicate_count_colpair_df() |>
      rename_duplicate_count_colpair_df()
  })
  output$output_duplicate_tally <- renderTable({
    duplicate_tally_df()
  })

  # Summarize the duplicate analyses:
  output$output_duplicate_count_summary <- renderTable({
    duplicate_count_df() |>
      audit() |>
      rename_duplicate_summary("count")
  })
  output$output_duplicate_count_colpair_summary <- renderTable({
    duplicate_count_colpair_df() |>
      audit() |>
      rename_duplicate_summary("count_colpair")
  })
  output$output_duplicate_tally_summary <- renderTable({
    duplicate_tally_df() |>
      audit() |>
      rename_duplicate_summary("tally")
  })

  # Server: download handlers -----------------------------------------------

  # Server: download handlers: consistency testing --------------------------

  # The name of a downloaded file will be "<input file name (without
  # extension)>_<selected consistency test>.csv". For example, after
  # GRIM-testing "pigs1.csv", the downloaded file will be called
  # "pigs1_GRIM.csv". When preparing the file itself, `rename_after_testing()`
  # is called again because it can't be part of the definition of `tested_df()`
  # itself without breaking compatibility with `audit()` etc.

  # Results by case:
  output$download_consistency_test <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), input$name_test)
    },
    content = function(file) {
      write_download(
        tested_df() |>
          format_tested_values() |>
          rename_after_testing(
            name_test = input$name_test,
            percent = percent()
          ),
        file
      )
    }
  )
  # Summary of results by case:
  output$download_consistency_test_summary <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(),
        name_technique = input$name_test,
        addendum = "_summary"
      )
    },
    content = function(file) {
      write_download(
        df_audit() |>
          rename_after_audit(percent()),
        file
      )
    }
  )

  # Results of dispersed sequences:
  output$download_consistency_test_seq <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(),
        name_technique = input$name_test,
        addendum = "_sequences"
      )
    },
    content = function(file) {
      write_download(
        tested_df_seq() |>
          format_tested_values() |>
          rename_after_testing_seq(
            name_test = input$name_test,
            percent = percent()
          ),
        file
      )
    }
  )
  # Summary (dispersed sequences):
  output$download_consistency_test_audit_seq <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(),
        name_technique = input$name_test,
        addendum = "_sequences_summary"
      )
    },
    content = function(file) {
      write_download(
        df_audit_seq() |>
          rename_after_audit_seq(input$name_test),
        file
      )
    }
  )

  # Server: download handlers: duplicate analysis --------------------------

  # Frequency table:
  output$download_duplicate_count <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), "duplicate_count")
    },
    content = function(file) {
      write_download(
        duplicate_count_df() |>
          rename_duplicate_count_df(),
        file
      )
    }
  )
  # Summary (frequency table):
  output$download_duplicate_count_audit <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(),
        name_technique = "duplicate_count",
        addendum = "_summary"
      )
    },
    content = function(file) {
      write_download(
        duplicate_count_df() |>
          audit() |>
          rename_duplicate_summary("count"),
        file
      )
    }
  )

  # Duplicates across columns:
  output$download_duplicate_count_colpair <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), "duplicate_count_colpair")
    },
    content = function(file) {
      write_download(
        duplicate_count_colpair_df() |>
          rename_duplicate_count_colpair_df(),
        file
      )
    }
  )
  # Summary (duplicates across columns):
  output$download_duplicate_count_colpair_audit <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(),
        name_technique = "duplicate_count_colpair",
        addendum = "_summary"
      )
    },
    content = function(file) {
      write_download(
        duplicate_count_colpair_df() |>
          audit() |>
          rename_duplicate_summary("count_colpair"),
        file
      )
    }
  )

  # Value tally at original location:
  output$download_duplicate_tally <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), "duplicate_tally")
    },
    content = function(file) {
      write_download(
        duplicate_tally_df(),
        file
      )
    }
  )
  # Summary (value tally at original location):
  output$download_duplicate_tally_audit <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(),
        name_technique = "duplicate_tally",
        addendum = "_summary"
      )
    },
    content = function(file) {
      write_download(
        duplicate_tally_df() |>
          audit() |>
          rename_duplicate_summary("tally"),
        file
      )
    }
  )

  # Server: Misc text -----------------------------------------------------

  output$debit_plot_tooltip_note <- renderText({
    "If you hover over DEBIT plots, not all of the information
    currently displayed is correct. Blue and red do stand for
    consistent and inconsistent value sets. All consistent value sets
    lie on the parabola."
  })

  output$text_about <- renderUI({
    htmltools::tagList(
      "This webapp was made by",
      a("Lukas Jung", href = "https://github.com/lhdjung"),
      "in R, using",
      a("shiny", href = "https://shiny.posit.co/"),
      "with",
      a(
        "bslib",
        href = "https://rstudio.github.io/bslib/index.html",
        .noWS = "after"
      ),
      ".",
      br(),
      br(), # Newlines
      "It applies tools from the",
      a("scrutiny", href = "https://lhdjung.github.io/scrutiny/"),
      "package for error detection in science. See",
      a(
        "Brown and Heathers (2017)",
        href = "https://journals.sagepub.com/doi/abs/10.1177/1948550616673876"
      ),
      "on GRIM,",
      a(
        "Allard (2018)",
        href = "https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/"
      ),
      "on GRIMMER, and",
      a("Heathers and Brown (2019)", href = "https://osf.io/5vb3u"),
      "on DEBIT.",
      br(),
      br(), # Newlines
      "Hosting provided by",
      a(
        "ERROR: a bug bounty program for science",
        href = "https://error.reviews",
        .noWS = "after"
      ),
      ", which is funded by the University of Bern \"Humans in Digital Transformation\" fund.",
      br(),
      br(), # Newlines
      "Source code is",
      a(
        "on Github",
        href = "https://github.com/lhdjung/shiny_scrutiny",
        .noWS = "after"
      ),
      ". For feedback, open an issue there or write an email to: jung-lukas@gmx.net"
    )
  })
}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
