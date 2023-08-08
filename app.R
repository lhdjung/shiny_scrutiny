library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(scrutiny)


# Helper functions ----

names_to_title_case <- function(x) {
  `names<-`(x, value = casefold(names(x)))
}

names_to_lower_case <- function(x) {
  `names<-`(x, value = tolower(names(x)))
}


# Define UI ----
ui <- page_sidebar(
  title = "scrutiny webapp",
  sidebar = sidebar(
    title = "Controls",
    # Data upload:
    fileInput("input_df", "Summary data file:", accept = "text/plain"),
    selectInput(
      "name_test", "Concistency test:",
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
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Case-wise results"),
      tableOutput("output_df")
    ),
    card(
      full_screen = TRUE,
      card_header("Visualization"),
      plotOutput("output_plot")
    ),
  ),
  card(
    full_screen = TRUE,
    card_header("Summary"),
    tableOutput("output_df_audit")
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
    df$x <- restore_zeros(
      df$x,
      width = max(input$digits, max(decimal_places(df$x)))
    )
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
    do.call(
      what = paste0(tolower(input$name_test), "_map"),
      args = args_list
    ) |>
      names_to_title_case()
    # rename(!!input$x := x, !!input$n := n)
  })

  output$output_df <- renderTable({
    tested_df()
  })

  output$output_df_audit <- renderTable({
    tested_df() |>
      names_to_lower_case() |>
      audit() |>
      rename(
        `Inconsistent cases` = incons_cases,
        `All cases` = all_cases,
        `Inconsistency rate` = incons_rate,
        `Mean GRIM ratio` = mean_grim_ratio,
        `Inconsistencies / ratio` = incons_to_ratio,
        `Testable cases` = testable_cases,
        `Testable cases rate` = testable_rate
      )
  })

  output$output_plot <- renderPlot(
    if (any(input$name_test == c("GRIM", "GRIMMER"))) {
      grim_plot(tested_df()) +
        ggplot2::theme(text = element_text(size = 12.25))
    }
  )

}


# Run the app ----
shinyApp(ui = ui, server = server)


