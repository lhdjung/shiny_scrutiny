library(shiny)
library(bslib)
library(rlang)
library(ggplot2)
library(dplyr)
library(corrr)
library(readr)
library(stringr)
library(janitor)
library(scrutiny)
library(forecast)

# Load helper functions:
source("scripts/functions.R")


# Define UI ---------------------------------------------------------------

ui <- page_navbar(
  title = "Scrutiny webapp (beta)",
  id = "nav",
  sidebar = sidebar(
    conditionalPanel(
      "input.nav === 'Data upload'",
      fileInput("input_df", "Upload your data:", accept = "text/plain"),
      checkboxInput(
        "use_example_data_pigs5", label = "Use example data", value = FALSE
      ),
      # Identifying `x` and `n` columns:
      textInput("x", "Mean / percentage column:", "x"),
      textInput("sd", "Standard deviation column:", "sd"),
      textInput("n", "Sample size column:", "n"),
      numericInput(
        "digits", label = "Restore decimal zeros:", value = 0L, min = 0
      ) |>
        tooltip(
          "Decimal numbers may have lost trailing zeros, but these are \
          important when testing for consistency. They are padded with \
          zeros to match the number chosen here or the greatest number \
          of decimal places from among them, whichever is greater."
        )
    ),
    conditionalPanel(
      "input.nav === 'Consistency testing'",
      selectInput(
        "name_test", "Consistency test:",
        choices = c("GRIM", "GRIMMER", "DEBIT")
      ) |>
        tooltip("See \"About\" for more information."),
      # Mean / percentage selection:
      conditionalPanel(
        "input.name_test === 'GRIM'",
        selectInput(
          "mean_percent", label = "Mean or percentage?",
          choices = c("Mean", "Percentage")
        ) |>
          tooltip(
            "For GRIM, if the \"x\" column in your data contains percentages, \
            they will be deflated (that is, divided by 100) before testing."
          )
      ),
      # Number of items:
      conditionalPanel(
        "input.mean_percent === 'Mean'",
        numericInput(
          "items", label = "Number of scale items", value = 1, min = 1, step = 1
        ) |>
          tooltip(
            "If the scale from which the means are derived is composed \
            of multiple items, enter the number of those items here."
          )
      ),
      # # Item column merging:
      # conditionalPanel(
      #   "input.merge_items != '' && (input.name_test === 'GRIM' || input.name_test === 'GRIMMER')",
      #   checkboxInput("merge_items", label = "Merge items column", value = TRUE)
      # ),
      # Rounding:
      selectInput(
        "rounding", label = "Rounding method:",
        choices = c(
          "Up or down", "Up", "Down", "Up from...", "Down from...",
          "Ceiling or floor", "Ceiling", "Floor", "Truncate", "Anti-truncate"
        )
      ) |>
        tooltip(
          "The consistency of summary data is determined on the basis of \
          reconstructing numbers rounded in the chosen way (or either \
          of two ways, as with the permissive default \"Up or down\")."
        ),
      conditionalPanel(
        "input.rounding === 'Up from...'",
        numericInput(
          "rounding_up_from", label = "Round up from:", value = 5,
          min = 0, max = 9, step = 1
        )
      ),
      conditionalPanel(
        "input.rounding === 'Down from...'",
        numericInput(
          "rounding_down_from", label = "Round down from:", value = 5,
          min = 0, max = 9, step = 1
        )
      ),
      textInput("dispersion", label = "Dispersion:", value = "1:5") |>
        tooltip(
          "How far should the dispersed sequences be spread out?
          You can define a sequence of steps: for example, the default
          \"1:5\" goes five steps up and down from the reported values.
          Alternatively, choose specific steps separated by commas,
          like \"2, 5, 7\"."
        ),
      numericInput(
        "plot_size_text", label = "Plot text size:", value = 14, min = 1
      ),
      downloadButton("download_consistency_test", "Download results by case"),
      downloadButton("download_consistency_test_summary", "Download summary (results by case)"),
      downloadButton("download_consistency_test_seq", "Download dispersed sequences"),
      downloadButton("download_consistency_test_audit_seq", "Download summary (dispersed sequences)"),
    ),
    conditionalPanel(
      "input.nav === 'Duplicate analysis'",
      numericInput(
        "plot_size_text_acf", label = "Plot text size:", value = 16, min = 1
      ),
      numericInput(
        "acf_ci", label = "Confidence interval (autocorrelation):",
        value = 0.95, min = 0, max = 1, step = 0.05
      ) |>
        tooltip(
          "Coverage probability for the autocorrelation plot's \
          confidence interval"
        ),
      downloadButton("download_duplicate_count", "Download\nfrequency table"),
      downloadButton("download_duplicate_count_audit", "Download summary (frequency table)"),
      downloadButton("download_duplicate_count_colpair", "Download duplicates across columns"),
      downloadButton("download_duplicate_count_colpair_audit", "Download summary (duplicates across columns)"),
      downloadButton("download_duplicate_tally", "Download value tally at original location"),
      downloadButton("download_duplicate_tally_audit", "Download summary (value tally at original location)"),
    ),
    conditionalPanel(
      "input.nav === 'About'"
    )
  ),
  nav_panel(
    "Data upload",
    card(
      card_header("Information"),
      textOutput("text_info_upload")
    ),
    card(
      card_header("Data preview"),
      tableOutput("uploaded_data")
    ) |>
      tooltip(
        "Your data. Rename columns in the sidebar on the left \
        if they don't already have the names shown there."
      )
  ),
  nav_panel(
    "Consistency testing",
    # Basic analyses -- two long cards side by side:
    layout_column_wrap(
      width = 0.5,
      card(
        card_header("Results by case"),
        tableOutput("output_df"),
        # card_body(max_height = "10px"),
        max_height = "500px",
        full_screen = TRUE
      ) |>
        tooltip("Your data, tested for consistency."),
      card(
        card_header("Visualization"),
        plotOutput("output_plot"),
        # card_body(max_height = "10px"),
        max_height = "500px",
        full_screen = TRUE
      ) |>
        tooltip(
          "Blue: consistent, red: inconsistent. The grey background flags \
          all inconsistent combinations, whether present in the data or not."
        )
    ),
    # Basic analyses -- one wide card below:
    card(
      card_header("Summary (results by case)"),
      tableOutput("output_df_audit"),
      # card_body(max_height = "10px"),
      full_screen = TRUE
    ) |>
      tooltip("Simple summaries of testing your data."),
    # Further analyses -- two long cards side by side:
    layout_column_wrap(
      width = 0.5,
      card(
        card_header("Dispersed sequences"),
        tableOutput("output_df_seq"),
        # card_body(max_height = "10px"),
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
        card_header("Visualization (dispersed sequences)"),
        plotOutput("output_plot_seq"),
        # card_body(max_height = "10px"),
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
      card_header("Summary (dispersed sequences)"),
      tableOutput("output_df_audit_seq"),
      # card_body(max_height = "10px"),
      full_screen = TRUE
    ) |>
      tooltip(
        "A \"hit\" is a consistent value set found by varying the \
        inconsistent numbers above. \"Hits for\" a variable \
        are those found by varying that variable. \"Least step difference\" \
        is the minimum number of steps between the reported values \
        of a variable and the nearby consistent ones. \
        They are split up by the direction of variation: upward and downward."
      )
  ),
  nav_panel(
    "Duplicate analysis",
    card(
      card_header("Frequency table"),
      tableOutput("output_duplicate_count"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Ranked by the duplicate count (or frequency). Locations are
        the names of the columns in your data where a given value appears."
      ),
    card(
      card_header("Summary (frequency table)"),
      tableOutput("output_duplicate_count_summary"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Summary statistics of the two numeric columns
        from the frequency table."
      ),
    card(
      card_header("Duplicates across columns"),
      tableOutput("output_duplicate_count_colpair"),
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
      tableOutput("output_duplicate_count_colpair_summary"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Summary statistics of all columns from the cross-column table
        (except for those that list the original columns from your data)."
      ),
    card(
      card_header("Value tally at original location"),
      tableOutput("output_duplicate_tally"),
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
      tableOutput("output_duplicate_tally_summary"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Summary statistics of the \"_n\" columns.
        Because the frequencies appear as often as their own value says,
        these statistics should be interpreted with caution."
      ),
    # card(
    #   card_header("Autocorrelation results"),
    #   tableOutput("output_acf_df"),
    #   full_screen = FALSE
    # ),
    card(
      card_header("Autocorrelation plot"),
      plotOutput("output_acf_plot"),
      full_screen = TRUE
    ) |>
      tooltip(
        "Autocorrelation function (ACF) plot. The dashed blue line
        is the confidence interval."
      )
  ),
  nav_panel(
    "About",
    uiOutput("about_text")
  ),
  fillable = FALSE, theme = bs_theme(version = 5)
)



# Define server logic -----------------------------------------------------

server <- function(input, output) {

  output$text_info_upload <- renderText({
    c(
      "Please upload a file in a tabular format such as CSV
      (or check \"Use example data\" on the left).
      For GRIM and other consistency tests, it should have
      columns with specific types of summary data:
      All tests require mean and sample size columns.
      GRIMMER and DEBIT additionally require a standard deviation
      column. You may need to specify the columns (see sidebar left).
      They will be shown renamed below. Duplicate analysis doesn't
      require any specific columns.
      Hover over a panel for information about it."
    )
  })

  # Capture the user-uploaded dataframe and, if necessary, rename some columns:
  user_data <- reactive({

    # Optionally, use the example `pigs5` data instead of user-uploaded data:
    if (input$use_example_data_pigs5) {
      out <- pigs5
    } else {
      validate(need(input$input_df, "Upload data first."))
      out <- read_delim(input$input_df$datapath)
    }

    # Rename the key columns if their names are not "x" and "n" etc.:
    if (input$x != "x") {
      out <- rename(out, x = !!input$x)
    }
    if (input$sd != "sd") {
      out <- rename(out, sd = !!input$sd)
    }
    if (input$n != "n") {
      out <- rename(out, n = !!input$n)
    }

    format_after_upload(out, digits = input$digits)
  })

  name_input_file <- reactive({
    if (input$use_example_data_pigs5) {
      "example"
    } else {
      input$input_df$name
    }
  })

  # Display uploaded data:
  output$uploaded_data <- renderTable({
    user_data()
  })

  rounding_method <- reactive({
    select_rounding_method(input$rounding)
  })

  rounding_threshold <- reactive({
    switch(
      rounding_method(),
      "up_from" = input$rounding_up_from,
      "down_from" = input$rounding_down_from,
      5
    )
  })

  # Basic analyses:
  tested_df <- reactive({
    if (input$name_test == "DEBIT") {
      msg_error <- "Error: DEBIT only works with means and SDs of binary data."
      validate(
        need(all(between(as.numeric(user_data()$x), 0, 1)), msg_error),
        need(all(between(as.numeric(user_data()$sd), 0, 1)), msg_error)
      )
    }
    # Test for consistency using a mapping function, then return the data frame:
    out <- switch(
      input$name_test,
      "GRIM" = grim_map(
        user_data(), items = input$items,
        percent = input$mean_percent == "Percentage",
        rounding = rounding_method(), threshold = rounding_threshold()
      ),
      "GRIMMER" = grimmer_map(
        user_data(), items = input$items,
        rounding = rounding_method(), threshold = rounding_threshold()
      ),
      "DEBIT" = debit_map(
        user_data(), rounding = rounding_method(),
        threshold = rounding_threshold()
      )
    )
    # Many consistency tests have a key argument / column corresponding to the
    # sample size ("n"). It should be integer because, as a double, the app
    # would misleadingly display it with decimal zeros, like, e.g., "5.00".
    if (any(names(out) == "n")) {
      mutate(out, n = as.integer(n))
    } else {
      out
    }
  })

  output$output_df <- renderTable({
    tested_df() |>
      rename_after_testing(
        input$name_test, percent = input$mean_percent == "Percentage"
      )
  })

  df_audit <- reactive({
    audit(tested_df())
  })

  output$output_df_audit <- renderTable({
    df_audit() |>
      rename_after_audit(input$name_test)
  })

  output$output_plot <- renderPlot(
    tested_df() |>
      plot_test_results(input$name_test, input$plot_size_text)
  )

  # Dispersed sequences:

  tested_df_seq <- reactive({
    switch(
      input$name_test,
      "GRIM" = grim_map_seq(
        user_data(), dispersion = parse_dispersion(input$dispersion),
        items = input$items,
        percent = input$mean_percent == "Percentage",
        rounding = rounding_method(), threshold = rounding_threshold()
      ),
      "GRIMMER" = grimmer_map_seq(
        user_data(), dispersion = parse_dispersion(input$dispersion),
        items = input$items,
        rounding = rounding_method(), threshold = rounding_threshold()
      ),
      "DEBIT" = debit_map_seq(
        user_data(), dispersion = parse_dispersion(input$dispersion),
        rounding = rounding_method(), threshold = rounding_threshold()
      )
    )
  })

  output$output_df_seq <- renderTable({
    tested_df_seq() |>
      rename_after_testing_seq(
        input$name_test, percent = input$mean_percent == "Percentage"
      )
  })

  output$output_df_audit_seq <- renderTable({
    tested_df_seq() |>
      audit_seq() |>
      mutate(across(
        .cols = starts_with("hits") | starts_with("diff"),
        .fns  = as.integer
      )) |>
      rename_after_audit_seq(input$name_test)
  })

  output$output_plot_seq <- renderPlot(
    tested_df_seq() |>
      plot_test_results(input$name_test, input$plot_size_text)
  )

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
  user_data_numeric <- reactive({
    user_data() |>
      select(where(is_numeric_like)) |>
      mutate(across(everything(), as.numeric))
  })
  # acf_df <- reactive({
  #   user_data_numeric() |>
  #     acf(plot = FALSE) |>
  #     tidy_acf()
  # })
  acf_plot <- reactive({
    user_data_numeric() |>
      ggAcf(ci = input$acf_ci) +
      labs(title = NULL, y = "Autocorrelation function") +
      theme_minimal(base_size = input$plot_size_text_acf) +
      theme(panel.grid = element_blank())
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
    duplicate_tally_df() #|>
    # rename_duplicate_tally_df()
  })
  # output$output_acf_df <- renderTable({
  #   acf_df()
  # })
  output$output_acf_plot <- renderPlot({
    acf_plot()
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


  # Download handlers (consistency testing) ---------------------------------

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
          input$name_test, percent = input$mean_percent == "Percentage"
        ) |>
        clean_names() |>
        write_csv(file)
    }
  )
  # Summary (results by case):
  output$download_consistency_test_summary <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(), input$name_test, addendum = "_summary"
      )
    },
    content = function(file) {
      df_audit() |>
        rename_after_audit(input$name_test) |>
        clean_names() |>
        write_csv(file)
    }
  )

  # Dispersed sequences:
  output$download_consistency_test_seq <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(), input$name_test, "_sequences"
      )
    },
    content = function(file) {
      tested_df_seq() |>
        rename_after_testing_seq(
          input$name_test, percent = input$mean_percent == "Percentage"
        ) |>
        clean_names() |>
        write_csv(file)
    }
  )
  # Summary (dispersed sequences):
  output$download_consistency_test_audit_seq <- downloadHandler(
    filename = function() {
      format_download_file_name(
        name_input_file(), input$name_test, "_sequences_summary"
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


  # Download handlers (duplication analysis) --------------------------------

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
      format_download_file_name(name_input_file(), "duplicate_count", "_summary")
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
        name_input_file(), "duplicate_count_colpair", "_summary"
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
        name_input_file(), "duplicate_tally", "_summary"
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

  output$about_text <- renderUI({
    htmltools::tagList(
      "This webapp was made by",
      a("Lukas Jung", href = "https://github.com/lhdjung"),
      "in R, using shiny with bslib. It applies tools from the",
      a("scrutiny", href = "https://lhdjung.github.io/scrutiny/"),
      "package for error detection in science. The",
      a("forecast", href = "https://pkg.robjhyndman.com/forecast/index.html"),
      "package is used to create the ACF plot. See",
      a("Brown and Heathers 2017", href = "https://journals.sagepub.com/doi/abs/10.1177/1948550616673876"),
      "on GRIM,",
      a("Allard 2018", href = "https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/"),
      "on GRIMMER, and",
      a("Heathers and Brown 2019", href = "https://osf.io/5vb3u"),
      "on DEBIT."
    )
  })
}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
