library(shiny)
library(bslib)
library(rlang)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(scrutiny)


# Load helper functions:
source("scripts/functions.R")

# # Deploy like this:
# rsconnect::deployApp(
#   appName = "scrutiny",
#   account = "errors"
# )

# Define UI ---------------------------------------------------------------

ui <- page_navbar(
  title = "Error detection (beta 0.2.3)",
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
          sample size column to form the effective sample size. Leave blank \
          to ignore."
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
      # # TODO: implement item column merging
      # conditionalPanel(
      #   "input.merge_items != '' && (input.name_test === 'GRIM' || input.name_test === 'GRIMMER')",
      #   checkboxInput("merge_items", label = "Merge items column", value = TRUE)
      # ),
      # Rounding:
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
      downloadButton("download_duplicate_count", "Download\nfrequency table"),
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
        card_header("Data preview"),
        styled_table_div("uploaded_data")
      ) |>
        tooltip(
          "Your data. Rename columns in the sidebar on the left \
          if they don't already have the names shown there."
        )
    )
  ),

  # Nav panel: consistency testing -----------------------------------------

  nav_panel(
    "Consistency testing",
    uiOutput("dropped_rows_note"),

    # Warning note for GRIMMER test 3 reliability
    conditionalPanel(
      "input.name_test === 'GRIMMER'",
      card(
        card_header(tags$strong("Warning")),
        uiOutput("grimmer_test3_warning"),
        full_screen = TRUE
      )
    ),

    # Basic analyses -- two long cards side by side:
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Results by case"),
        styled_table_div("output_df"),
        max_height = "500px",
        full_screen = TRUE
      ) |>
        tooltip("Your data, tested for consistency."),
      card(
        card_header("Visualization"),
        plotOutput("output_plot"),
        max_height = "500px",
        full_screen = TRUE
      ) |>
        tooltip(
          "Blue: consistent, red: inconsistent. The grey background \
          flags all possible inconsistent combinations, whether \
          present in the data or not."
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
      card_header("Summary of results by case"),
      styled_table_div("output_df_audit"),
      full_screen = TRUE
    ) |>
      tooltip("Simple summaries of testing your data."),
    # Further analyses -- two long cards side by side:
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Results of dispersed sequences"),
        styled_table_div("output_df_seq"),
        max_height = "500px",
        full_screen = TRUE
      ) |>
        tooltip(
          "Checking the numeric neighborhood of inconsistent value sets \
          for consistent ones. Variables to the left of \"consistency\" \
          are marginally varied up and down, holding the other one(s) \
          constant each time."
        ),
      card(
        card_header("Visualization of dispersed sequences"),
        plotOutput("output_plot_seq"),
        max_height = "500px",
        full_screen = TRUE
      ) |>
        tooltip(
          "Blue: consistent, red: inconsistent. The cross pattern emerges \
          because values are varied up and down along both axes.
          The grey background flags all inconsistent combinations, \
          whether present in the data or not."
        )
    ),
    # Further analyses -- one wide card below:
    card(
      card_header("Summary of dispersed sequences"),
      styled_table_div("output_df_audit_seq"),
      full_screen = TRUE
    ) |>
      tooltip(
        "A \"hit\" is a consistent value set found by varying the \
        inconsistent numbers above. \"Hits for\" a variable \
        are those found by varying that variable. \"Least step difference\" \
        is the minimum number of steps between the reported values \
        of a variable and the nearby consistent ones. \
        They are split up by the direction of variation: upward and downward.
        \"NA\" indicates that no hits could be found in the respective way."
      )
  ),

  # Nav panel: duplicate analysis --------------------------------------

  nav_panel(
    "Duplicate analysis",
    card(
      card_header("Frequency table"),
      styled_table_div("output_duplicate_count"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Ranked by the frequency. Locations are
        the names of the columns in your data where a given value appears."
      ),
    card(
      card_header("Summary (frequency table)"),
      styled_table_div("output_duplicate_count_summary"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Summary statistics of the two numeric columns
        from the frequency table."
      ),
    card(
      card_header("Duplicates across columns"),
      styled_table_div("output_duplicate_count_colpair"),
      full_screen = TRUE
    ) |>
      tooltip(
        "This checks each pair of columns in your data for duplicates:
        values that appear in both columns. Shown on the right are
        the proportion of values in the original column 1 that are also
        found in column 2, and vice versa. These two \"Proportion\" columns
        are equal unless some values are missing. The same is true
        of the \"Total number\" columns in the center;
        they also exclude missing values."
      ),
    card(
      card_header("Summary (duplicates across columns)"),
      styled_table_div("output_duplicate_count_colpair_summary"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Summary statistics of all columns from the cross-column table
        (except for those that list the original columns from your data)."
      ),
    card(
      card_header("Value tally at original location"),
      styled_table_div("output_duplicate_tally"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Next to each column from your data, an \"_n\" column shows
        how often its values appear in the data overall.
        Note that the frequency of each value appears a number
        of times equal to the frequency itself."
      ),
    card(
      card_header("Summary (value tally at original location)"),
      styled_table_div("output_duplicate_tally_summary"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Summary statistics of the \"_n\" columns.
        Because the frequencies appear as often as their own value says,
        these statistics should be interpreted with caution."
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
  items_merged <- reactiveVal(FALSE)

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
        "Hover over a panel for information about it."
      )
    )
  })

  # Capture the user-uploaded dataframe and, if necessary, rename some columns:
  user_data <- reactive({
    # Optionally, use the example `pigs5` data instead of user-uploaded data:
    if (input$use_example_data_pigs5) {
      out <- pigs5
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
        lengths(gregexpr(char, first_line, fixed = TRUE))
      }
      if (count_char(";") > count_char(",")) {
        out <- read_delim(
          input$input_df$datapath,
          delim = ";",
          locale = locale(decimal_mark = ",", grouping_mark = "."),
          show_col_types = FALSE
        )
      } else {
        out <- read_delim(input$input_df$datapath, show_col_types = FALSE)
      }
    }

    # Rename the key columns if their names are not "x" and "n" etc.:
    for (key in c("x", "sd", "n")) {
      name_given <- input[[key]]
      if (name_given == key) {
        next
      }
      validate(need(
        name_given %in% names(out),
        paste0("ERROR: Column \"", name_given, "\" not found in the data.")
      ))
      out <- rename(out, !!key := !!name_given)
    }

    # Merge items column into n if specified and present:
    items_col_name <- input$items_col
    if (nzchar(items_col_name)) {
      validate(need(
        items_col_name %in% names(out),
        paste0(
          "ERROR: Items column \"",
          items_col_name,
          "\" not found in the data."
        )
      ))
      items_vals <- out[[items_col_name]]
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
        is.numeric(out[["n"]]),
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
      out$n <- out$n * as.integer(items_vals)
      out[[items_col_name]] <- NULL
      items_merged(TRUE)
    } else {
      items_merged(FALSE)
    }

    format_after_upload(out)
  })

  name_input_file <- reactive({
    if (input$use_example_data_pigs5) {
      "example"
    } else {
      input$input_df$name
    }
  })

  percent <- reactive({
    if (input$name_test == "GRIM") {
      input$mean_percent == "Percentage"
    } else {
      NULL
    }
  })

  # When an items column was merged into n, the scalar items input is bypassed:
  effective_items <- reactive({
    if (items_merged()) 1L else input$items
  })

  output$items_conflict_warning <- renderUI({
    if (items_merged() && input$items > 1) {
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
    df <- df[complete.cases(df[, intersect(required_cols, names(df))]), ]
    # Drop any "items" column not configured for merging; passing it through
    # would conflict with scrutiny's internal items handling.
    if (!items_merged() && "items" %in% names(df)) {
      df[["items"]] <- NULL
    }
    validate(need(
      all(required_cols %in% names(df)),
      paste0(
        "ERROR: This test needs a \"",
        paste(setdiff(required_cols, names(df)), collapse = "\" and a \""),
        "\" column. Name it in the sidebar."
      )
    ))
    validate(need(
      nrow(df) > 0,
      "ERROR: No rows have all of the required columns."
    ))
    validate(need(
      is_numeric_like(df$n) &&
        all(
          is_whole_number(as.numeric(df$n)) & as.numeric(df$n) > 0,
          na.rm = TRUE
        ),
      "ERROR: The sample size column must contain positive whole numbers only."
    ))
    # Key columns go to scrutiny as numbers; precision travels separately, in
    # `digits_x` / `digits_sd`.
    mutate(df, across(any_of(c("x", "sd")), as.numeric))
  })

  # How many rows the tests never saw. Silence here would understate the
  # denominator of every rate the app reports.
  output$dropped_rows_note <- renderUI({
    n_dropped <- nrow(user_data()) - nrow(testable_data())
    if (n_dropped < 1) {
      return(NULL)
    }
    tags$p(
      style = "color: orange;",
      sprintf(
        "\u26a0 %d of %d row(s) were excluded from testing because a required
        column was missing. Reported counts and rates below cover the remaining
        %d row(s) only.",
        n_dropped,
        nrow(user_data()),
        nrow(testable_data())
      )
    )
  })

  # Decimal places declared to scrutiny, taken before the key columns are
  # reduced to numbers. `input$digits` is the floor, for zeros already lost.
  digits_of <- function(key) {
    reactive({
      df <- user_data()
      validate(need(
        key %in% names(df),
        paste0(
          "ERROR: No \"",
          key,
          "\" column in the data. Name it in the sidebar."
        )
      ))
      digits_declared(df[[key]], input$digits)
    })
  }
  digits_x <- digits_of("x")
  digits_sd <- digits_of("sd")

  # `numericInput` yields NA while the field is empty or out of bounds.
  dispersion_steps <- reactive({
    validate(need(
      isTruthy(input$dispersion) && input$dispersion >= 1,
      "ERROR: Dispersion must be a whole number of at least 1."
    ))
    min(as.integer(input$dispersion), 100L)
  })

  # Basic analyses:
  tested_df <- reactive({
    if (input$name_test == "DEBIT") {
      msg_error <- "ERROR: DEBIT only works with means and SDs of binary data."
      validate(
        need(all(between(as.numeric(testable_data()$x), 0, 1)), msg_error),
        need(all(between(as.numeric(testable_data()$sd), 0, 1)), msg_error)
      )
    }

    # Forced here, not left as lazy arguments: evaluated inside scrutiny, a
    # validate() message is rethrown as a raw error instead of rendering.
    method <- rounding_method()
    df <- testable_data()
    items <- effective_items()
    is_percent <- percent()
    dp_x <- digits_x()
    dp_sd <- if (input$name_test == "GRIM") NULL else digits_sd()

    # Test for consistency using a mapping function
    out <- switch(
      input$name_test,
      "GRIM" = grim_map(
        df,
        digits_x = dp_x,
        items = items,
        percent = is_percent,
        rounding = method
      ),
      "GRIMMER" = grimmer_map(
        df,
        digits_x = dp_x,
        digits_sd = dp_sd,
        items = items,
        rounding = method
      ),
      "DEBIT" = debit_map(
        df,
        digits_x = dp_x,
        digits_sd = dp_sd,
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
    tested_df() |>
      rename_after_testing(
        input$name_test,
        percent = percent()
      )
  })

  df_audit <- reactive({
    audit(tested_df())
  })

  output$output_df_audit <- renderTable({
    df_audit() |>
      rename_after_audit(input$mean_percent == "Percentage")
  })

  output$output_plot <- renderPlot(
    tested_df() |>
      plot_test_results(
        input$name_test,
        input$plot_size_text
      )
  )

  # Results of dispersed sequences:

  tested_df_seq <- reactive({
    method <- rounding_method()
    df <- testable_data()
    items <- effective_items()
    is_percent <- percent()
    steps <- seq_len(dispersion_steps())
    dp_x <- digits_x()
    dp_sd <- if (input$name_test == "GRIM") NULL else digits_sd()

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

    validate(need(
      nrow(out) > 0,
      "No inconsistent cases to disperse from. All tested values are consistent."
    ))

    out
  })

  output$output_df_seq <- renderTable({
    tested_df_seq() |>
      rename_after_testing_seq(
        input$name_test,
        percent = percent()
      )
  })

  output$output_df_audit_seq <- renderTable({
    tested_df_seq() |>
      audit_seq() |>
      mutate(across(
        .cols = starts_with("hits") | starts_with("diff"),
        .fns = as.integer
      )) |>
      rename_after_audit_seq(input$name_test)
  })

  output$output_plot_seq <- renderPlot(
    tested_df_seq() |>
      plot_test_results(
        input$name_test,
        input$plot_size_text
      )
  )

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
      tested_df() |>
        rename_after_testing(
          name_test = input$name_test,
          percent = percent()
        ) |>
        clean_names() |>
        write_csv(file)
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
      df_audit() |>
        rename_after_audit(percent()) |>
        clean_names() |>
        write_csv(file)
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
      tested_df_seq() |>
        rename_after_testing_seq(
          name_test = input$name_test,
          percent = percent()
        ) |>
        clean_names() |>
        write_csv(file)
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
      tested_df_seq() |>
        audit_seq() |>
        rename_after_audit_seq(input$name_test) |>
        clean_names() |>
        write_csv(file)
    }
  )

  # Server: download handlers: duplicate analysis --------------------------

  # Frequency table:
  output$download_duplicate_count <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), "duplicate_count")
    },
    content = function(file) {
      duplicate_count_df() |>
        rename_duplicate_count_df() |>
        clean_names() |>
        write_csv(file)
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
      duplicate_count_df() |>
        audit() |>
        rename_duplicate_summary("count") |>
        clean_names() |>
        write_csv(file)
    }
  )

  # Duplicates across columns:
  output$download_duplicate_count_colpair <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), "duplicate_count_colpair")
    },
    content = function(file) {
      duplicate_count_colpair_df() |>
        rename_duplicate_count_colpair_df() |>
        clean_names() |>
        write_csv(file)
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
      duplicate_count_colpair_df() |>
        audit() |>
        rename_duplicate_summary("count_colpair") |>
        clean_names() |>
        write_csv(file)
    }
  )

  # Value tally at original location:
  output$download_duplicate_tally <- downloadHandler(
    filename = function() {
      format_download_file_name(name_input_file(), "duplicate_tally")
    },
    content = function(file) {
      duplicate_tally_df() |>
        clean_names() |>
        write_csv(file)
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
      duplicate_tally_df() |>
        audit() |>
        rename_duplicate_summary("tally") |>
        clean_names() |>
        write_csv(file)
    }
  )

  # Server: Misc text -----------------------------------------------------

  output$debit_plot_tooltip_note <- renderText({
    "If you hover over DEBIT plots, not all of the information
    currently displyayed is correct. Blue and red do stand for
    consistent and inconsistent value sets. All consistent value sets
    lie on the parabola."
  })

  output$grimmer_test3_warning <- renderUI({
    HTML(paste(
      "The results of GRIMMER's test 3 are currently not reliable.",
      "This will be fixed in the future. (The first two tests and GRIM",
      "are not affected.) For more information, see:",
      "<a href='https://github.com/lhdjung/scrutiny/issues/80'>",
      "https://github.com/lhdjung/scrutiny/issues/80</a>"
    ))
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
