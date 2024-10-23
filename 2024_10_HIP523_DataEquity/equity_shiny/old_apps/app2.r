library(shiny)
library(tidyverse)
library(gtsummary)
library(DT)

# Data preparation
data1 <- tibble(
  business_id = 1:6,
  women_owned = c(0, 0, 0, 1, 1, 1),
  earnings = c(50, 50, 50, 10, 10, 50),
  funded = c(1, 1, 1, 0, 0, 1)
)

data2 <- tibble(
  business_id = 1:8,
  black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
  sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
  funded = c(0, 0, 1, 1, 0, 1, 0, 1)
)

data3 <- tibble(
  business_id = 1:8,
  black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
  sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
  priority_zone = c(1, 0, 1, 0, 1, 0, 1, 0),
  funded = c(0, 0, 1, 1, 0, 1, 0, 1)
)

ui <- fluidPage(
  titlePanel("Business Equity Analysis Examples"),
  
  tabsetPanel(
    # Example 1: Post-attribute bias
    tabPanel("Post-attribute Bias",
      fluidRow(
        column(4,
          wellPanel(
            h4("About this example"),
            p("This example demonstrates post-attribute bias in analyzing funding decisions for women-owned businesses. Including earnings as a control might mask discrimination if earnings are themselves affected by discrimination."),
            checkboxInput("show_earnings", "Include Earnings as Control", FALSE),
            h4("Dataset"),
            DTOutput("data1_table")
          )
        ),
        column(8,
          h4("Regression Results"),
          verbatimTextOutput("model1_summary"),
          h4("Visual Representation"),
          plotOutput("plot1")
        )
      )
    ),
    
    # Example 2: Omitted variable bias
    tabPanel("Omitted Variable Bias",
      fluidRow(
        column(4,
          wellPanel(
            h4("About this example"),
            p("This example shows how omitting relevant variables (sole proprietorship status) can bias our understanding of funding disparities for Black-owned businesses."),
            checkboxInput("show_sole_prop", "Include Sole Proprietorship Status", FALSE),
            h4("Dataset"),
            DTOutput("data2_table")
          )
        ),
        column(8,
          h4("Regression Results"),
          verbatimTextOutput("model2_summary"),
          h4("Visual Representation"),
          plotOutput("plot2")
        )
      )
    ),
    
    # Example 3: Uncorrelated variables
    tabPanel("Control Variable Selection",
      fluidRow(
        column(4,
          wellPanel(
            h4("About this example"),
            p("This example explores the impact of including different control variables on our analysis of funding disparities."),
            checkboxGroupInput("controls", "Select Control Variables:",
              choices = c("Sole Proprietorship" = "sole_prop", 
                         "Priority Zone" = "priority_zone"),
              selected = NULL
            ),
            h4("Dataset"),
            DTOutput("data3_table")
          )
        ),
        column(8,
          h4("Regression Results"),
          verbatimTextOutput("model3_summary"),
          h4("Visual Representation"),
          plotOutput("plot3")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Example 1 outputs
  output$data1_table <- renderDT({
    datatable(data1, options = list(pageLength = 6))
  })
  
  output$model1_summary <- renderPrint({
    if(input$show_earnings) {
      model <- lm(funded ~ women_owned + earnings, data = data1)
    } else {
      model <- lm(funded ~ women_owned, data = data1)
    }
    summary(model)
  })
  
  output$plot1 <- renderPlot({
    if(input$show_earnings) {
      ggplot(data1, aes(x = earnings, y = funded, color = factor(women_owned))) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(color = "Women-owned",
             x = "Earnings",
             y = "Funded") +
        theme_minimal()
    } else {
      ggplot(data1, aes(x = factor(women_owned), y = funded)) +
        geom_boxplot(fill = "lightblue") +
        geom_jitter(width = 0.2) +
        labs(x = "Women-owned",
             y = "Funded") +
        theme_minimal()
    }
  })
  
  # Example 2 outputs
  output$data2_table <- renderDT({
    datatable(data2, options = list(pageLength = 8))
  })
  
  output$model2_summary <- renderPrint({
    if(input$show_sole_prop) {
      model <- lm(funded ~ black_owned + sole_prop, data = data2)
    } else {
      model <- lm(funded ~ black_owned, data = data2)
    }
    summary(model)
  })
  
  output$plot2 <- renderPlot({
    if(input$show_sole_prop) {
      ggplot(data2, aes(x = factor(black_owned), y = funded, fill = factor(sole_prop))) +
        geom_boxplot() +
        geom_jitter(width = 0.2) +
        labs(x = "Black-owned",
             y = "Funded",
             fill = "Sole Proprietorship") +
        theme_minimal()
    } else {
      ggplot(data2, aes(x = factor(black_owned), y = funded)) +
        geom_boxplot(fill = "lightblue") +
        geom_jitter(width = 0.2) +
        labs(x = "Black-owned",
             y = "Funded") +
        theme_minimal()
    }
  })
  
  # Example 3 outputs
  output$data3_table <- renderDT({
    datatable(data3, options = list(pageLength = 8))
  })
  
  output$model3_summary <- renderPrint({
    formula_str <- "funded ~ black_owned"
    if("sole_prop" %in% input$controls) {
      formula_str <- paste(formula_str, "+ sole_prop")
    }
    if("priority_zone" %in% input$controls) {
      formula_str <- paste(formula_str, "+ priority_zone")
    }
    model <- lm(as.formula(formula_str), data = data3)
    summary(model)
  })
  
  output$plot3 <- renderPlot({
    plot_data <- data3
    if(length(input$controls) > 0) {
      facet_var <- input$controls[1]
      ggplot(plot_data, aes(x = factor(black_owned), y = funded)) +
        geom_boxplot(fill = "lightblue") +
        geom_jitter(width = 0.2) +
        facet_wrap(as.formula(paste("~", facet_var))) +
        labs(x = "Black-owned",
             y = "Funded") +
        theme_minimal()
    } else {
      ggplot(plot_data, aes(x = factor(black_owned), y = funded)) +
        geom_boxplot(fill = "lightblue") +
        geom_jitter(width = 0.2) +
        labs(x = "Black-owned",
             y = "Funded") +
        theme_minimal()
    }
  })
}

shinyApp(ui = ui, server = server)
