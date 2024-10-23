library(shiny)
library(tidyverse)
library(gtsummary)
library(broom.helpers)
library(bslib)
library(shinyAce)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Header
  h1("Equity Analysis Interactive Explorer", class = "text-center my-4"),
  p("Explore different approaches to regression analyses involving equity.", class = "text-center mb-4"),
  p(a(href="https://oes.gsa.gov/assets/files/choosing-controls-in-regression-analyses-involving-equity.pdf",
      "Examples from OES Equity Evaluation Series"), class = "text-center mb-4"), 
  
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
          tableOutput("data1_table"),
          textOutput("error1")
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
        tableOutput("data2_table"),
        textOutput("error2")
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
      tableOutput("data3_table"),
      textOutput("error3")
    ),
    card(
      card_header("Model Results"),
      tabsetPanel(
        tabPanel("With Priority Zone", 
                 gt::gt_output("model3_uncorrelated")),
        tabPanel("All Variables", 
                 gt::gt_output("model3_all")),
        tabPanel("Explanation",
                 p("In this example, being in a priority zone is correlated with the outcome but not owner's race. 
                   When including priority zone as an adjustment variable, the coefficient of Black-owned is unchanged 
                   and inferences are more precise. However, sole proprietorship is a confounder correlated with funding and Block-owndership
                   and when adding to teh model the coefficient of Black-owned changes."),
                 p(""),
                 p("Relationships with multiple variables can be complex.")
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
), 
nav_panel(
  title = "Summary",
  
  p("From OES: When you are interested in the question of whether a demographic group receives access to a program at different rates compared to other demographic groups, irrespective of the reason, then a bivariate model will answer that question. However, if you are interested in whether otherwise similarly situated individuals of different demographics access a benefit at the same rate, then including pre-treatment controls that are correlated with the demographic characteristic and the outcome is typically appropriate, depending on the functional form of
the model."),
  p(""),
  p("Note: The Equity Evaluation Memo Series is intended to guide OES’ commitment to equity in our evaluation process and efforts toward understanding and reducing barriers to equitable access to federal programs. This series is intended to be an internal guidance document for OES team members.")
),
p("Developed for OHSU HIP523 October 2024 by Jessica Minnier. Most coding performed by Claude.AI (3.5 Sonnet).", class = "text-center mb-4"),
  )
)


# Updated server logic
server <- function(input, output, session) {
  
  # Example 1: Post-attribute Bias
  example1_default_data <- tibble(
    business_id = 1:6,
    women_owned = c(0, 0, 0, 1, 1, 1),
    earnings = c(50, 50, 50, 10, 10, 50),
    funded = c(1, 1, 1, 0, 0, 1)
  )
  
  example1_data <- reactiveVal(example1_default_data)
  example1_models <- reactive({
    if (!is.null(example1_error())) return(NULL)
    
    # Create new environment and evaluate code
    env <- new.env()
    eval(parse(text = input$code1), envir = env)
    
    # Return the models from the environment
    list(
      full = env$full_model1,
      bivariate = env$bivariate_model1
    )
  })
  example1_error <- reactiveVal(NULL)
  
  observeEvent(input$run1, {
    tryCatch({
      env <- new.env()
      eval(parse(text = input$code1), envir = env)
      
      if (!exists("data1", envir = env)) {
        example1_error("Error: data1 object not found. Please make sure to create a data1 data frame.")
        return()
      }
      if (!exists("full_model1", envir = env) || !exists("bivariate_model1", envir = env)) {
        example1_error("Error: model objects not found. Please make sure to create both full_model1 and bivariate_model1.")
        return()
      }
      
      example1_data(env$data1)
      example1_error(NULL)
      
    }, error = function(e) {
      example1_error(paste("Error:", e$message))
    })
  })
  
  # Example 2: Omitted Variable Bias
  example2_default_data <- tibble(
    business_id = 1:8,
    black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
    sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
    funded = c(0, 0, 1, 1, 0, 1, 0, 1)
  )
  
  example2_data <- reactiveVal(example2_default_data)
  example2_models <- reactive({
    if (!is.null(example2_error())) return(NULL)
    
    # Create new environment and evaluate code
    env <- new.env()
    eval(parse(text = input$code2), envir = env)
    
    # Return the models from the environment
    list(
      full = env$full_model2,
      bivariate = env$bivariate_model2
    )
  })
  example2_error <- reactiveVal(NULL)
  
  observeEvent(input$run2, {
    tryCatch({
      env <- new.env()
      eval(parse(text = input$code2), envir = env)
      
      if (!exists("data2", envir = env)) {
        example2_error("Error: data2 object not found. Please make sure to create a data2 data frame.")
        return()
      }
      if (!exists("full_model2", envir = env) || !exists("bivariate_model2", envir = env)) {
        example2_error("Error: model objects not found. Please make sure to create both full_model2 and bivariate_model2.")
        return()
      }
      
      example2_data(env$data2)
      example2_error(NULL)
      
    }, error = function(e) {
      example2_error(paste("Error:", e$message))
    })
  })
  
  # Example 3: Uncorrelated Variables
  example3_default_data <- tibble(
    business_id = 1:8,
    black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
    sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
    priority_zone = c(1, 0, 1, 0, 1, 0, 1, 0),
    funded = c(0, 0, 1, 1, 0, 1, 0, 1)
  )
  
  example3_data <- reactiveVal(example3_default_data)
  example3_models <- reactive({
    if (!is.null(example3_error())) return(NULL)
    
    # Create new environment and evaluate code
    env <- new.env()
    eval(parse(text = input$code3), envir = env)
    
    # Return the models from the environment
    list(
      uncorrelated = env$model_uncorrelated,
      all = env$model_all
    )
  })
  example3_error <- reactiveVal(NULL)
  
  observeEvent(input$run3, {
    tryCatch({
      env <- new.env()
      eval(parse(text = input$code3), envir = env)
      
      if (!exists("data3", envir = env)) {
        example3_error("Error: data3 object not found. Please make sure to create a data3 data frame.")
        return()
      }
      if (!exists("model_uncorrelated", envir = env) || !exists("model_all", envir = env)) {
        example3_error("Error: model objects not found. Please make sure to create both model_uncorrelated and model_all.")
        return()
      }
      
      example3_data(env$data3)
      example3_error(NULL)
      
    }, error = function(e) {
      example3_error(paste("Error:", e$message))
    })
  })
  
  # Outputs for all examples
  # Example 1 outputs
  output$data1_table <- renderTable({
    if (!is.null(example1_error())) return(NULL)
    example1_data()
  })
  
  output$error1 <- renderText({
    example1_error()
  })
  
  output$model1_full <- gt::render_gt({
    req(is.null(example1_error()))
    req(example1_models()$full)
    tbl_regression(example1_models()$full) %>%
      as_gt()
  })
  
  output$model1_bivariate <- gt::render_gt({
    req(is.null(example1_error()))
    req(example1_models()$bivariate)
    tbl_regression(example1_models()$bivariate) %>%
      as_gt()
  })
  
  # Example 2 outputs
  output$data2_table <- renderTable({
    if (!is.null(example2_error())) return(NULL)
    example2_data()
  })
  
  output$error2 <- renderText({
    example2_error()
  })
  
  output$model2_full <- gt::render_gt({
    req(is.null(example2_error()))
    req(example2_models()$full)
    tbl_regression(example2_models()$full) %>%
      as_gt()
  })
  
  output$model2_bivariate <- gt::render_gt({
    req(is.null(example2_error()))
    req(example2_models()$bivariate)
    tbl_regression(example2_models()$bivariate) %>%
      as_gt()
  })
  
  # Example 3 outputs
  output$data3_table <- renderTable({
    if (!is.null(example3_error())) return(NULL)
    example3_data()
  })
  
  output$error3 <- renderText({
    example3_error()
  })
  
  output$model3_uncorrelated <- gt::render_gt({
    req(is.null(example3_error()))
    req(example3_models()$uncorrelated)
    tbl_regression(example3_models()$uncorrelated) %>%
      as_gt()
  })
  
  output$model3_all <- gt::render_gt({
    req(is.null(example3_error()))
    req(example3_models()$all)
    tbl_regression(example3_models()$all) %>%
      as_gt()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
