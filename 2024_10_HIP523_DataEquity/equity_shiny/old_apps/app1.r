library(shiny)
library(tidyverse)
library(gtsummary)
library(bslib)
library(shinyAce)

# UI
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Header
  h1("Equity Analysis Interactive Explorer", class = "text-center my-4"),
  p("Explore different approaches to regression analyses involving equity", 
    class = "text-center mb-4"),
  
  # Main content
  navset_tab(
    # Example 1: Post-attribute Bias
    nav_panel(
      title = "Post-attribute Bias",
      layout_columns(
        fill = FALSE,
        value_box(
          title = "What is Post-attribute Bias?",
          value = "",
          p("Post-attribute bias occurs when we control for variables that may be affected by the attribute we're studying. In this case, earnings might be affected by women-owned status."),
          showcase = bsicons::bs_icon("info-circle")
        ),
        value_box(
          title = "Key Question",
          value = "",
          p("Should we control for earnings when studying the relationship between women-owned status and funding?"),
          showcase = bsicons::bs_icon("question-circle")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Data Explorer"),
          tableOutput("data1_table")
        ),
        card(
          card_header("Model Results"),
          tabsetPanel(
            tabPanel("Full Model", 
                    gt::gt_output("model1_full")),
            tabPanel("Bivariate Model", 
                    gt::gt_output("model1_bivariate")),
            tabPanel("Explanation",
                     value_box(
                       value = "",
                       p("In the full model, we falsely infer there is no inequity betewen WOB and non-WOB. Whether or not we should use the full or bivariate model in the above example depends on whether we want to understand inequity in access for any reason or for reasons unrelated to profitability.")))
          )
        )
      ),
      card(
        card_header("Interactive Code"),
        aceEditor("code1", 
                 value = 'data1 <- tibble(
  business_id = 1:6,
  women_owned = c(0, 0, 0, 1, 1, 1),
  earnings = c(50, 50, 50, 10, 10, 50),
  funded = c(1, 1, 1, 0, 0, 1)
)

# Try modifying the data or models!
full_model1 <- lm(funded ~ women_owned + earnings, data = data1)
bivariate_model1 <- lm(funded ~ women_owned, data = data1)',
                 mode = "r",
                 theme = "monokai"),
        actionButton("run1", "Run Code", class = "btn-primary mt-3"),
        verbatimTextOutput("code1_output")
      )
    ),
    
    # Example 2: Omitted Variable Bias
    nav_panel(
      title = "Omitted Variable Bias",
      layout_columns(
        fill = FALSE,
        value_box(
          title = "What is Omitted Variable Bias?",
          value = "",
          p("Omitted variable bias occurs when we fail to control for important variables that affect both the predictor and outcome."),
          showcase = bsicons::bs_icon("info-circle")
        ),
        value_box(
          title = "Key Question",
          value = "",
          p("Should we control for business structure when studying the relationship between black-owned status and funding?"),
          showcase = bsicons::bs_icon("question-circle")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Data Explorer"),
          tableOutput("data2_table")
        ),
        card(
          card_header("Model Results"),
          tabsetPanel(
            tabPanel("Full Model", 
                    gt::gt_output("model2_full")),
            tabPanel("Bivariate Model", 
                    gt::gt_output("model2_bivariate")),
            tabPanel("Explanation",
                     p("Black-owned business are more likely to be sole proprietorships, and sole proprietors are more likely to get funded. However, the only unfunded sole proprietorship is Black-owned. Thus, if we don't account for the fact that BOBs are more like sole proprietors, we are overly optimistic about their rates of funding compared to non-BOBs."),
                     p(""),
                     p("If our question were instead more similar to the first question, such as 'did Black-owned businesses receive less funding than non Black-owned businesses (for whatever reason)?', the answer we get from the bivariate model would be, correctly, no. However, if we are concerned about whether similarly situated BOBs and non-BOBs are funded at the same or similar rates, then the bivariate model will mask potential discrimination or other barriers that Black sole proprietors face compared to non-Black sole proprietors.")
                     )
          )
        )
      ),
      card(
        card_header("Interactive Code"),
        aceEditor("code2", 
                 value = 'data2 <- tibble(
  business_id = 1:8,
  black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
  sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
  funded = c(0, 0, 1, 1, 0, 1, 0, 1)
)

# Try modifying the data or models!
full_model2 <- lm(funded ~ black_owned + sole_prop, data = data2)
bivariate_model2 <- lm(funded ~ black_owned, data = data2)',
                 mode = "r",
                 theme = "monokai"),
        actionButton("run2", "Run Code", class = "btn-primary mt-3"),
        verbatimTextOutput("code2_output")
      )
    ),
    
    # Example 3: Uncorrelated Variables
    nav_panel(
      title = "Uncorrelated Variables",
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Why Consider Uncorrelated Variables?",
          value = "",
          p("Including uncorrelated controls may affect precision but shouldn't bias estimates if they're truly uncorrelated."),
          showcase = bsicons::bs_icon("info-circle")
        ),
        value_box(
          title = "Key Question",
          value = "",
          p("How does including priority zone status affect our analysis of black-owned businesses and funding?"),
          showcase = bsicons::bs_icon("question-circle")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Data Explorer"),
          tableOutput("data3_table")
        ),
        card(
          card_header("Model Results"),
          tabsetPanel(
            tabPanel("With Priority Zone", 
                    gt::gt_output("model3_uncorrelated")),
            tabPanel("All Variables", 
                    gt::gt_output("model3_all")),
            tabPanel("Explanation",
                     p("")
            )
          )
        )
      ),
      card(
        card_header("Interactive Code"),
        aceEditor("code3", 
                 value = 'data3 <- tibble(
  business_id = 1:8,
  black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
  sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
  priority_zone = c(1, 0, 1, 0, 1, 0, 1, 0),
  funded = c(0, 0, 1, 1, 0, 1, 0, 1)
)

# Try modifying the data or models!
model_uncorrelated <- lm(funded ~ black_owned + priority_zone, data = data3)
model_all <- lm(funded ~ black_owned + sole_prop + priority_zone, data = data3)',
                 mode = "r",
                 theme = "monokai"),
        actionButton("run3", "Run Code", class = "btn-primary mt-3"),
        verbatimTextOutput("code3_output")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Example 1: Post-attribute Bias
  data1 <- reactive({
    tibble(
      business_id = 1:6,
      women_owned = c(0, 0, 0, 1, 1, 1),
      earnings = c(50, 50, 50, 10, 10, 50),
      funded = c(1, 1, 1, 0, 0, 1)
    )
  })
  
  output$data1_table <- renderTable({
    data1()
  })
  
  output$model1_full <- gt::render_gt({
    full_model1 <- lm(funded ~ women_owned + earnings, data = data1())
    tbl_regression(full_model1) %>%
      as_gt()
  })
  
  output$model1_bivariate <- gt::render_gt({
    bivariate_model1 <- lm(funded ~ women_owned, data = data1())
    tbl_regression(bivariate_model1) %>%
      as_gt()
  })
  
  observeEvent(input$run1, {
    output$code1_output <- renderPrint({
      eval(parse(text = input$code1))
    })
  })
  
  # Example 2: Omitted Variable Bias
  data2 <- reactive({
    tibble(
      business_id = 1:8,
      black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
      sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
      funded = c(0, 0, 1, 1, 0, 1, 0, 1)
    )
  })
  
  output$data2_table <- renderTable({
    data2()
  })
  
  output$model2_full <- gt::render_gt({
    full_model2 <- lm(funded ~ black_owned + sole_prop, data = data2())
    tbl_regression(full_model2) %>%
      as_gt()
  })
  
  output$model2_bivariate <- gt::render_gt({
    bivariate_model2 <- lm(funded ~ black_owned, data = data2())
    tbl_regression(bivariate_model2) %>%
      as_gt()
  })
  
  observeEvent(input$run2, {
    output$code2_output <- renderPrint({
      eval(parse(text = input$code2))
    })
  })
  
  # Example 3: Uncorrelated Variables
  data3 <- reactive({
    tibble(
      business_id = 1:8,
      black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
      sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
      priority_zone = c(1, 0, 1, 0, 1, 0, 1, 0),
      funded = c(0, 0, 1, 1, 0, 1, 0, 1)
    )
  })
  
  output$data3_table <- renderTable({
    data3()
  })
  
  output$model3_uncorrelated <- gt::render_gt({
    model_uncorrelated <- lm(funded ~ black_owned + priority_zone, data = data3())
    tbl_regression(model_uncorrelated) %>%
      as_gt()
  })
  
  output$model3_all <- gt::render_gt({
    model_all <- lm(funded ~ black_owned + sole_prop + priority_zone, data = data3())
    tbl_regression(model_all) %>%
      as_gt()
  })
  
  observeEvent(input$run3, {
    output$code3_output <- renderPrint({
      eval(parse(text = input$code3))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
