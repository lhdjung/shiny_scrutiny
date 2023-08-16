library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(scrutiny)


# Helper functions --------------------------------------------------------

rename_after_testing <- function(df, name_test) {
  names(df) <- str_to_title(names(df))
  if (name_test == "GRIM") {
    rename(
      df,
      Mean = X,
      `GRIM ratio` = Ratio
    )
  } else if (name_test == "GRIMMER") {
    rename(
      df,
      Mean = X,
      SD = Sd
    )
  } else if (name_test == "DEBIT") {
    rename(
      df,
      `Mean` = X,
      `SD` = Sd,
      `Lower SD` = Sd_lower,
      `Include lower SD` = Sd_incl_lower,
      `Upper SD` = Sd_upper,
      `Include upper SD` = Sd_incl_upper,
      `Lower mean` = X_lower,
      `Upper mean` = X_upper
    )
  }
}


rename_after_audit <- function(df, name_test) {
  if (name_test == "GRIM") {
    rename(
      df,
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
      df,
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
      df,
      `Inconsistent cases` = incons_cases,
      `All cases` = all_cases,
      `Inconsistency rate` = incons_rate,
      `Mean of means` = mean_x,
      `Mean of SDs` = mean_sd,
      `Distinct sample sizes` = `distinct_n`
    )
  }
}


# This function MUST contain renaming instructions for all key variables of all
# consistency tests currently implemented! Also, "consistency" is pragmatically
# included here because it's always present, although it's not a key variable.
rename_key_vars <- function(var) {
  switch(
    var,
    "x"  = "Mean",
    "sd" = "SD",
    "n"  = "N",
    "consistency" = "Consistency",
    var
  )
}


rename_after_testing_seq <- function(df, name_test) {
  names(df) <- str_to_title(names(df))
  df <- if (name_test == "GRIM") {
    rename(
      df,
      Mean = X,
      `GRIM ratio` = Ratio,
      Variable = Var
    )
  } else if (name_test == "GRIMMER") {
    rename(
      df,
      Mean = X,
      SD = Sd,
      Variable = Var
    )
  } else if (name_test == "DEBIT") {
    rename(
      df,
      Mean = X,
      SD = Sd,
      `Lower SD` = Sd_lower,
      `Include lower SD` = Sd_incl_lower,
      `Upper SD` = Sd_upper,
      `Include upper SD` = Sd_incl_upper,
      `Lower mean` = X_lower,
      `Upper mean` = X_upper,
      Variable = Var
    )
  }
  df$Variable <- vapply(df$Variable, rename_key_vars, character(1L))
  df
}


key_cols <- function(df) {
  df[1L:(match("consistency", names(df)) - 1L)]
}


rename_after_audit_seq <- function(df) {
  regex_key_var_names <- paste0(
    "(?<=(^(hits_|diff_)))(",
    paste0(names(key_cols(df)), collapse = "|"),
    ")(?=(_up|_down|))"
  )
  names_all <- names(df)
  for (i in seq_along(names_all)) {
    if (str_starts(names_all[i], "(hits_|diff_)")) {
      names_all[i] <- names_all[i] |>
        str_replace(regex_key_var_names, rename_key_vars) |>
        str_replace("^hits_", "Hits for ") |>
        str_replace("^diff_", "Difference in ") |>
        str_replace("_up$", " (upward)") |>
        str_replace("_down$", " (downward)")
    } else {
      names_all[i] <- rename_key_vars(names_all[i])
    }
  }
  names_all[names_all == "Hits for total"] <- "Total number of hits"
  `names<-`(df, value = names_all)
}


plot_test_results <- function(df, name_test, size_text) {
  if (any(name_test == c("GRIM", "GRIMMER"))) {
    grim_plot(df) +
      theme(text = element_text(size = size_text))
  } else if (name_test == "DEBIT") {
    debit_plot(df, label_size = size_text * 0.285) +
      theme_minimal(base_size = size_text)
  }
}



# Define UI ---------------------------------------------------------------

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
    numericInput("plot_size_text", label = "Plot text size:", value = 14)
  ),
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
      card_header("Visualization"),
      plotOutput("output_plot_seq"),
      # card_body(max_height = "10px"),
      max_height = "500px",
      full_screen = TRUE
    ),
  ),
  # Further analyses -- one wide card below:
  card(
    card_header("Summary"),
    tableOutput("output_df_audit_seq"),
    # card_body(max_height = "10px"),
    full_screen = TRUE
  ),
  fillable = FALSE
)



# Define server logic -----------------------------------------------------

server <- function(input, output) {

  # Basic analyses:

  user_data <- reactive({
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

    # Test for consistency using a mapping function, then return the data frame:
    switch(
      input$name_test,
      "GRIM"    = grim_map(user_data(), percent = input$mean_percent == "Percentage"),
      "GRIMMER" = grimmer_map(user_data()),
      "DEBIT"   = debit_map(user_data())
    )
    # rename(!!input$x := x, !!input$n := n)
  })

  output$output_df <- renderTable({
    tested_df() |>
      rename_after_testing(input$name_test)
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
      rename_after_testing_seq(input$name_test)
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

