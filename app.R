library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(scrutiny)
data(penguins, package = "palmerpenguins")


# Define UI ----
ui <- page_sidebar(
  title = "scrutiny webapp",
  sidebar = sidebar(
    title = "Controls",
    # varSelectInput(
    #   "var", "Select variable",
    #   select(penguins, where(is.numeric))
    # ),

    # Data upload:
    fileInput("input_df", "Summary data file:", accept = "text/plain"),
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
  card(
    full_screen = TRUE,
    card_header("Consistency testing"),
    tableOutput("output_df")
  )
)


# Define server logic ----
server <- function(input, output) {
  output$output_df <- renderTable({
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
    grim_map(df, percent = input$mean_percent == "Percentage")
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)


