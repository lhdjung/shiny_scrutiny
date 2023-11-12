library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(scrutiny)
library(forecast)

# Load helper functions:
source("scripts/functions.R")


# Define UI ---------------------------------------------------------------

# # Prepare optional cards for duplicate analysis:
# cards_duplicate_analysis <- list(
#   card(
#     card_header("Frequency table"),
#     tableOutput("output_duplicate_count")
#   ),
#   card(
#     card_header("")
#   )
# )

ui <- page_navbar(
  title = "Scrutiny webapp",
  id = "nav",
  sidebar = sidebar(
    conditionalPanel(
      "input.nav === 'Data upload'",
      fileInput("input_df", "Upload your data:", accept = "text/plain"),
      # Identifying `x` and `n` columns:
      textInput("x", "Mean / percentage column:", "x"),
      textInput("sd", "Standard deviation column:", "sd"),
      textInput("n", "Sample size column:", "n"),
      numericInput("digits", label = "Restore decimal zeros:", value = 0L, min = 0)
    ),
    conditionalPanel(
      "input.nav === 'Consistency testing'",
      selectInput(
        "name_test", "Consistency test:",
        choices = c("GRIM", "GRIMMER", "DEBIT")
      ),
      # Mean / percentage selection:
      selectInput(
        "mean_percent", label = "Mean or percentage?",
        choices = c("Mean", "Percentage")
      ),
      numericInput(
        "plot_size_text", label = "Plot text size:", value = 14, min = 1
      ),
      downloadButton("download_consistency_test", "Download results"),
      downloadButton("download_consistency_test_summary", "Download summary")
    ),
    conditionalPanel(
      "input.nav === 'Duplicate analysis'",
      numericInput(
        "plot_size_text_acf", label = "Plot text size:", value = 16, min = 1
      ),
      numericInput(
        "acf_ci", label = "Confidence interval (autocorrelation):", value = 0.95,
        min = 0, max = 1, step = 0.05
      ),
      downloadButton("download_duplicate_count", "Download\nfrequency table"),
      downloadButton("download_duplicate_count_colpair", "Download duplicates across columns"),
      downloadButton("download_duplicate_tally", "Download value tally at original location")
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
    )
  ),
  nav_panel(
    "Consistency testing",
    # Basic analyses -- two long cards side by side:
    layout_column_wrap(
      0.5,
      card(
        card_header("Results by case"),
        tableOutput("output_df"),
        # card_body(max_height = "10px"),
        max_height = "500px",
        full_screen = TRUE
      ),
      card(
        card_header("Visualization"),
        plotOutput("output_plot"),
        # card_body(max_height = "10px"),
        max_height = "500px",
        full_screen = TRUE
      ),
    ),
    # Basic analyses -- one wide card below:
    card(
      card_header("Summary"),
      tableOutput("output_df_audit"),
      # card_body(max_height = "10px"),
      full_screen = TRUE
    ),
    # Further analyses -- two long cards side by side:
    layout_column_wrap(
      0.5,
      card(
        card_header("Dispersed sequences"),
        tableOutput("output_df_seq"),
        # card_body(max_height = "10px"),
        max_height = "500px",
        full_screen = TRUE
      ),
      card(
        card_header("Visualization (dispersed sequences)"),
        plotOutput("output_plot_seq"),
        # card_body(max_height = "10px"),
        max_height = "500px",
        full_screen = TRUE
      ),
    ),
    # Further analyses -- one wide card below:
    card(
      card_header("Summary (dispersed sequences)"),
      tableOutput("output_df_audit_seq"),
      # card_body(max_height = "10px"),
      full_screen = TRUE
    )
  ),
  nav_panel(
    "Duplicate analysis",
    card(
      card_header("Frequency table"),
      tableOutput("output_duplicate_count"),
      full_screen = TRUE
    ),
    card(
      card_header("Summary (frequency table)"),
      tableOutput("output_duplicate_count_summary"),
      full_screen = TRUE
    ),
    card(
      card_header("Duplicates across columns"),
      tableOutput("output_duplicate_count_colpair"),
      full_screen = TRUE
    ),
    card(
      card_header("Summary (duplicates across columns)"),
      tableOutput("output_duplicate_count_colpair_summary"),
      full_screen = TRUE
    ),
    card(
      card_header("Value tally at original location"),
      tableOutput("output_duplicate_tally"),
      full_screen = TRUE
    ),
    card(
      card_header("Summary (value tally at original location)"),
      tableOutput("output_duplicate_tally_summary"),
      full_screen = TRUE
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
    )
  ),
  nav_panel(
    "About",
    textOutput("about_text")
  ),
  fillable = FALSE
)



# Define server logic -----------------------------------------------------

server <- function(input, output) {

  output$text_info_upload <- renderText({c(
    "Please upload a CSV file or a file in another tabular format.\n",
    "For GRIM and other consistency tests, you may need to specify
    the columns to be tested (see sidebar left). They will be shown
    renamed below.\n",
    "Duplicate analysis doesn't require doing so."
  )})

  # Capture the user-uploaded dataframe and, if necessary, rename some columns:
  user_data <- reactive({
    validate(need(input$input_df, "Go to \"Data upload\" first."))
    out <- read_delim(input$input_df$datapath)

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

    # Convert the columns to string vectors. Also, restore decimal zeros in the
    # input to the maximum of those already present -- or, if the user-specified
    # number is greater, to that number:
    if (any(names(out) == "n")) {
      restore_zeros_df(out, !n, width = max(input$digits, max(decimal_places(out$x))))
    } else {
      restore_zeros_df(out, width = max(input$digits, max(decimal_places(out$x))))
    }

  })

  # Display uploaded data:
  output$uploaded_data <- renderTable({
    user_data()
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
      "GRIM"    = grim_map(user_data(), percent = input$mean_percent == "Percentage"),
      "GRIMMER" = grimmer_map(user_data()),
      "DEBIT"   = debit_map(user_data())
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
      "GRIM"    = grim_map_seq(user_data(), percent = input$mean_percent == "Percentage"),
      "GRIMMER" = grimmer_map_seq(user_data()),
      "DEBIT"   = debit_map_seq(user_data())
    )
  })

  output$output_df_seq <- renderTable({
    tested_df_seq() |>
      rename_after_testing_seq(input$name_test, percent = input$mean_percent == "Percentage")
  })

  output$output_df_audit_seq <- renderTable({
    tested_df_seq() |>
      audit_seq() |>
      mutate(across(
        .cols = starts_with("hits") | starts_with("diff"),
        .fns  = as.integer
      )) |>
      rename_after_audit_seq()
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

  # Consistency testing download handlers. The name of a downloaded file will be
  # "<input file name (without extension)>_<selected consistency test>.csv". For
  # example, after GRIM-testing "pigs1.csv", the downloaded file will be called
  # "pigs1_GRIM.csv". When preparing the file itself, `rename_after_testing()`
  # is called again because it can't be part of the definition of `tested_df()`
  # itself without breaking compatibility with `audit()` etc.
  output$download_consistency_test <- downloadHandler(
    filename = function() {
      format_download_file_name(input$input_df$name, input$name_test)
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
  output$download_consistency_test_summary <- downloadHandler(
    filename = function() {
      format_download_file_name(
        input$input_df$name, input$name_test, addendum = "_summary"
      )
    },
    content = function(file) {
      df_audit() |>
        clean_names() |>
        write_csv(file)
    }
  )

  # Duplication analysis download handlers:
  output$download_duplicate_count <- downloadHandler(
    filename = function() {
      format_download_file_name(input$input_df$name, "duplicate_count")
    },
    content = function(file) {
      duplicate_count_df() |>
        clean_names() |>
        write_csv(file)
    }
  )
  output$download_duplicate_count_colpair <- downloadHandler(
    filename = function() {
      format_download_file_name(input$input_df$name, "duplicate_count_colpair")
    },
    content = function(file) {
      duplicate_count_colpair_df() |>
        clean_names() |>
        write_csv(file)
    }
  )
  output$download_duplicate_tally <- downloadHandler(
    filename = function() {
      format_download_file_name(input$input_df$name, "duplicate_tally")
    },
    content = function(file) {
      duplicate_tally_df() |>
        clean_names() |>
        write_csv(file)
    }
  )

  # TODO: FIX THIS
  # output$about_text <- renderUI({
  #   paste(
  #     "This webapp was made by Lukas Jung in R, using shiny with bslib. \
  #     It applies tools from the scrutiny package for error detection \
  #     in science.
  #     - For more about GRIM, see",
  #     a("Brown and Heathers 2017", href="https://journals.sagepub.com/doi/abs/10.1177/1948550616673876"),
  #     "- For more about GRIMMER, see ",
  #     a("Allard 2018", href="https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/"),
  #   )
  # })

  # Like this?
  url_grim <- a("Brown and Heathers 2017", href="https://journals.sagepub.com/doi/abs/10.1177/1948550616673876")
  url_grimmer <- a("Allard 2018", href="https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/")
  url_debit <- a("Heathers and Brown 2019", href="https://osf.io/5vb3u")

  output$about_text <- renderUI({
    htmltools::tagList("URL link: ", url_grimmer)
  })

}



# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)

