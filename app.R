library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(scrutiny)

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
    # # Data upload:
    # fileInput("input_df", "Upload your data:", accept = "text/plain"),
    conditionalPanel(
      "input.nav === 'Data upload'",
      fileInput("input_df", "Upload your data:", accept = "text/plain"),
      # Identifying `x` and `n` columns:
      textInput("x", "Mean / percentage column:", "x"),
      textInput("sd", "Standard deviation column:", "sd"),
      textInput("n", "Sample size column:", "n"),
      numericInput("digits", label = "Restore decimal zeros:", value = 0L)
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
      numericInput("plot_size_text", label = "Plot text size:", value = 14),
      downloadButton("download_consistency_test", "Download results"),
      downloadButton("download_consistency_test_summary", "Download summary")
    ),
    conditionalPanel(
      "input.nav === 'Duplicate analysis'"
    ),
    conditionalPanel(
      "input.nav === 'About'"
    )
  ),
  nav_panel(
    "Data upload",
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
    "Watch this space!"
  ),
  nav_panel(
    "About",
    textOutput("about_text")
  ),
  fillable = FALSE
)



# Define server logic -----------------------------------------------------

server <- function(input, output) {

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
    # # TODO: FIX THIS
    # if (input$name_test == "GRIM") {
    #   validate(need(all(c("x", "n") %in% names(user_data()))))
    # } else if (input$name_test == "GRIMMER") {
    #   validate(need(all(c("x", "sd", "n") %in% names(user_date()))))
    # }

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
    # would misleadingly display it like, e.g., "25.00".
    if (any(names(out) == "n")) {
      mutate(out, n = as.integer(n))
    } else {
      out
    }
    # rename(!!input$x := x, !!input$n := n)
  })

  output$output_df <- renderTable({
    tested_df() |>
      rename_after_testing(input$name_test, percent = input$mean_percent == "Percentage")
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

  # The name of a downloaded file will be "<input file name (without
  # extension)>_<selected consistency test>.csv". For example, after
  # GRIM-testing "pigs1.csv", the downloaded file will be called
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

  # TODO: FIX THIS
  output$about_text <- renderText({
    paste(
      "This webapp was made by Lukas Jung in R, using shiny with bslib. \
      It applies tools from the scrutiny package for error detection \
      in science.
      - For more about GRIM, see" #,
      # a("Brown and Heathers 2017", href="https://journals.sagepub.com/doi/abs/10.1177/1948550616673876"),
      # "- For more about GRIMMER, see ",
      # a("Allard 2018", href="https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/"),
    )
  })

}



# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)

