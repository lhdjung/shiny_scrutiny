library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(scrutiny)


# Helper functions ----

names_to_title_case <- function(x) {
  `names<-`(x, value = str_to_title(names(x)))
}

names_to_lower_case <- function(x) {
  `names<-`(x, value = tolower(names(x)))
}

rename_after_audit <- function(x, name_test) {
  if (name_test == "GRIM") {
    rename(
      x,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Mean GRIM ratio` = mean_grim_ratio,
      `Inconsistencies / ratio` = incons_to_ratio,
      `Testable cases` = testable_cases,
      `Testable cases rate` = testable_rate
    )
  } else if (name_test == "GRIMMER") {
    rename(
      x,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Failed GRIM` = fail_grim,
      `Failed GRIMMER (test 1)` = fail_test1,
      `Failed GRIMMER (test 2)` = fail_test2,
      `Failed GRIMMER (test 3)` = fail_test3
    )
  } else if (name_test == "DEBIT") {
    rename(
      x,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Mean of means` = mean_x,
      `Mean of SDs` = mean_sd,
      `Distinct sample sizes` = `distinct_n`
    )
  }
}


# Define UI ----
ui <- page_sidebar(
  title = "scrutiny webapp",
  sidebar = sidebar(
    # Data upload:
    fileInput("input_df", "Summary data file:", accept = "text/plain"),
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
    numericInput("plot_text_size", label = "Plot text size:", value = 14)
  ),
  # Two long cards side by side:
  layout_columns(
    card(
      card_header("Results by case"),
      tableOutput("output_df"),
      full_screen = TRUE
    ),
    card(
      card_header("Visualization"),
      plotOutput("output_plot"),
      full_screen = TRUE
    ),
  ),
  # One wide card below:
  card(
    card_header("Summary"),
    tableOutput("output_df_audit"),
    full_screen = TRUE
  )
)


# Define server logic ----
server <- function(input, output) {
  tested_df <- reactive({
    df <- read_delim(input$input_df$datapath)
    if (input$x != "x") {
      df <- rename(df, x = input$x)
    }
    if (input$n != "n") {
      df <- rename(df, n = input$n)
    }
    # Decimal zeros are restored to the maximum of those already present -- or,
    # if the user-specified number is greater, to that number:
    df <- restore_zeros_df(df, !n)
    # Should this be the width?: max(input$digits, max(decimal_places(df$x)))
    args_list <- list(
      df, percent = input$mean_percent == "Percentage",
      # TODO: GET KEY ARGS BELOW RIGHT!
      x = input$x, n = input$n
    )
    if (input$name_test != "GRIM") {
      args_list$percent <- NULL
    }
    # Return data frame after testing for consistency using a mapping function,
    # such as `grim_map()`:
    switch(
      input$name_test,
      "GRIM"    = grim_map(df),
      "GRIMMER" = grimmer_map(df),
      "DEBIT"   = debit_map(df)
    ) |>
      names_to_title_case()
    # rename(!!input$x := x, !!input$n := n)
  })

  output$output_df <- renderTable({
    tested_df()
  })

  # TODO: GENERALIZE `audit()` RENAMING SO THAT `audit()` WORKS FOR GRIMMER AND
  # DEBIT, AS WELL!
  output$output_df_audit <- renderTable({
    tested_df() |>
      names_to_lower_case() |>
      audit() |>
      rename_after_audit(input$name_test)
  })

  output$output_plot <- renderPlot(
    if (any(input$name_test == c("GRIM", "GRIMMER"))) {
      tested_df() |>
        names_to_lower_case() |>
        grim_plot() +
        theme(text = element_text(size = input$plot_text_size))
    } else if (input$name_test == "DEBIT") {
      tested_df() |>
        names_to_lower_case() |>
        debit_plot(label_size = input$plot_text_size * 0.3) +
        theme_minimal(base_size = input$plot_text_size)
    }
  )

}


# Run the app ----
shinyApp(ui = ui, server = server)


