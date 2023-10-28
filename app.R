library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(scrutiny)

# Load helper functions:
source("scripts/functions.R")

# TODO: USE
# https://rstudio.github.io/bslib/articles/sidebars/index.html#conditional-contents
# TO CREATE A NEW PAGE WITH ITS OWN SIDEBAR FOR DUPLICATE ANALYSIS!


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
  title = "Your data",
  id = "nav",
  sidebar = sidebar(
    # Data upload:
    fileInput("input_df", "Summary data file:", accept = "text/plain"),
    conditionalPanel(
      "input.nav === 'Consistency testing'",
      selectInput(
        "name_test", "Consistency test:",
        choices = c("GRIM", "GRIMMER", "DEBIT")
      ),
      # Identifying `x` and `n` columns:
      textInput("x", "Mean / percentage column:", "x"),
      textInput("n", "Sample size column:", "n"),
      numericInput("digits", label = "Restore decimal zeros:", value = 0L),
      # Mean / percentage selection:
      selectInput(
        "mean_percent", label = "Mean or percentage?",
        choices = c("Mean", "Percentage")
      ),
      numericInput("plot_size_text", label = "Plot text size:", value = 14)
    ),
    conditionalPanel(
      "input.nav === 'Duplicate analysis'"
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
    "Page 2 contents"
  ),
  fillable = FALSE
)



# Define server logic -----------------------------------------------------

server <- function(input, output) {

  # Basic analyses:

  user_data <- reactive({
    validate(need(input$input_df, "Please upload tabular data."))
    # Decimal zeros in the input are restored to the maximum of those already
    # present -- or, if the user-specified number is greater, to that number:
    input$input_df$datapath |>
      read_delim() |>
      restore_zeros_df(!n)
    # Should this be the width?: max(input$digits, max(decimal_places(df$x)))
  })

  tested_df <- reactive({
    # # Decimal zeros in the input are restored to the maximum of those already
    # # present -- or, if the user-specified number is greater, to that number:
    # df <- input$input_df$datapath |>
    #   read_delim() |>
    #   restore_zeros_df(!n)
    # # Should this be the width?: max(input$digits, max(decimal_places(df$x)))

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

  output$output_df_audit <- renderTable({
    tested_df() |>
      audit() |>
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

}



# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)

