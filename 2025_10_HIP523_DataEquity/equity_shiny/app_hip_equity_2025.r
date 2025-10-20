library(shiny)
library(tidyverse)
library(gtsummary)
library(broom.helpers)
library(bslib)
library(shinyAce)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Header
  h1("Equity Analysis in Clinical Research", class = "text-center my-4"),
  p("Explore different regression approaches for equity analysis in health data.", class = "text-center mb-4"),
  p("Examples adapted from the 'Beyond \"Adjusting for Race\": Data Equity in Clinical Research' talk (HIP523, Oct 2025).", class = "text-center mb-4"), 
  
  # Main content
  navset_tab(
    # Example 1: Post-attribute Bias (Opioid Prescribing)
    nav_panel(
      title = "Post-attribute Bias",
      layout_columns(
        fill = FALSE,
        value_box(
          title = "What is Post-attribute Bias?",
          value = "",
          p("Occurs when we control for a variable that is affected by the attribute of interest (e.g., race). This variable might be on the causal pathway (a mediator)."),
          showcase = bsicons::bs_icon("info-circle")
        ),
        value_box(
          title = "Key Question",
          value = "",
          p("Should we control for in-hospital pain when studying racial disparities in opioid prescribing?"),
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
            tabPanel("Bivariate Model (Unadjusted)", 
                     gt::gt_output("model1_bivariate")),
            tabPanel("Full Model (Adjusted)", 
                     gt::gt_output("model1_full")),
            tabPanel("Explanation",
                     value_box(
                       value = "",
                       p("The bivariate model shows a disparity: Black patients receive lower MME. The full model adjusts for in-hospital pain. However, if provider bias (linked to race) leads to under-treatment, Black patients might report higher pain. Controlling for this pain score (a post-attribute variable) could falsely make the lower prescription seem 'justified', masking the bias.")
                     ))
          )
        )
      ),
      card(
        card_header("Interactive Code"),
        aceEditor("code1", 
                  value = '# Data: Opioid prescriptions (MME) after surgery
# Post-attribute bias example
surgical_data <- tibble(
  patient_id = 1:6,
  patient_race = c("White", "White", "White", "Black", "Black", "Black"),
  # Assume bias -> undertreatment -> Black patients report higher pain
  in_hospital_pain = c(5, 5, 5, 8, 8, 5),
  # Black patients receive lower MME
  MME = c(60, 60, 60, 40, 40, 60)
)

# Bivariate model asks: "Is there a disparity?"
bivariate_model1 <- lm(MME ~ patient_race, data = surgical_data)

# Full model asks: "Is there a disparity, holding pain constant?"
full_model1 <- lm(MME ~ patient_race + in_hospital_pain, data = surgical_data)

# Try adding: print(summary(full_model1))',
        mode = "r",
        theme = "monokai"),
      actionButton("run1", "Run Code", class = "btn-primary mt-3"),
      # --- ADDED CONSOLE OUTPUT ---
      verbatimTextOutput("code1_output")
      )
    ),
  
  # Example 2: Omitted Variable Bias (Regional Anesthesia)
  nav_panel(
    title = "Omitted Variable Bias",
    layout_columns(
      fill = FALSE,
      value_box(
        title = "What is Omitted Variable Bias?",
        value = "",
        p("Occurs when we fail to control for a confounder—a variable that affects both the attribute of interest and the outcome."),
        showcase = bsicons::bs_icon("info-circle")
      ),
      value_box(
        title = "Key Question",
        value = "",
        p("Should we control for patient health (ASA score) when studying insurance type and receipt of a nerve block?"),
        showcase = bsicons::bs_icon("question-circle")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Explorer (n=40)"),
        tableOutput("data2_table"),
        textOutput("error2")
      ),
      card(
        card_header("Model Results"),
        tabsetPanel(
          tabPanel("Bivariate Model (Unadjusted)", 
                   gt::gt_output("model2_bivariate")),
          tabPanel("Full Model (Adjusted)", 
                   gt::gt_output("model2_full")),
          tabPanel("Explanation",
                   p("The bivariate model shows a small, non-significant difference (OR near 1). It looks like there is no disparity."),
                   p("However, ASA score is a confounder: Medicaid patients are more likely to have a High ASA score, and High ASA patients are (in this example) *more* likely to get a block. This masks the true disparity."),
                   p("The full model controls for ASA score. It reveals that for patients with the *same* ASA score, those with Medicaid are significantly *less* likely to receive a nerve block. This is the correct, unconfounded result.")
          )
        )
      )
    ),
    card(
      card_header("Interactive Code"),
      aceEditor("code2", 
                value = '# Data: Receipt of nerve block for shoulder surgery (n=40)
# Omitted Variable Bias (Confounding) example
# NEW data to prevent model separation and show confounding
set.seed(42)
ortho_data <- bind_rows(
  # Group 1: Private, Low ASA (n=12) - Low block rate
  tibble(insurance_type = "Private", patient_asa_score = "Low", nerve_block = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
  # Group 2: Private, High ASA (n=8) - High block rate
  tibble(insurance_type = "Private", patient_asa_score = "High", nerve_block = c(1, 1, 1, 1, 1, 1, 0, 0)),
  # Group 3: Medicaid, Low ASA (n=8) - VERY low block rate
  tibble(insurance_type = "Medicaid", patient_asa_score = "Low", nerve_block = c(1, 0, 0, 0, 0, 0, 0, 0)),
  # Group 4: Medicaid, High ASA (n=12) - Medium block rate
  tibble(insurance_type = "Medicaid", patient_asa_score = "High", nerve_block = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0))
) %>%
  mutate(
    patient_id = 1:n(),
    insurance_type = factor(insurance_type, levels = c("Private", "Medicaid")),
    patient_asa_score = factor(patient_asa_score, levels = c("Low", "High"))
  ) %>%
  select(patient_id, insurance_type, patient_asa_score, nerve_block)


# Bivariate model: Is there an overall difference?
bivariate_model2 <- glm(nerve_block ~ insurance_type, data = ortho_data, family = "binomial")

# Full model: Is there a difference, holding ASA score constant?
full_model2 <- glm(nerve_block ~ insurance_type + patient_asa_score, data = ortho_data, family = "binomial")

# Try adding: print(summary(full_model2))',
                mode = "r",
                theme = "monokai"),
      actionButton("run2", "Run Code", class = "btn-primary mt-3"),
      # --- ADDED CONSOLE OUTPUT ---
      verbatimTextOutput("code2_output")
    )
  ),
  
  # Example 3: Disparities vs. Mechanisms (LEP Readmission)
  nav_panel(
    title = "Disparities vs. Mechanisms",
    layout_columns(
      fill = FALSE,
      value_box(
        title = "What Question Are We Asking?",
        value = "",
        p("The model choice depends on the question: 'Are there disparities?' (bivariate) vs. 'What are the mechanisms?' (multivariable)."),
        showcase = bsicons::bs_icon("info-circle")
      ),
      value_box(
        title = "Key Question",
        value = "",
        p("Does LEP status predict readmission? Or is that relationship *explained by* surgical complexity and length of stay?"),
        showcase = bsicons::bs_icon("question-circle")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Explorer (n=100)"),
        tableOutput("data3_table"),
        textOutput("error3")
      ),
      card(
        card_header("Model Results"),
        tabsetPanel(
          tabPanel("Bivariate Model", 
                   gt::gt_output("model3_bivariate")),
          tabPanel("Full Model (Adjusted)", 
                   gt::gt_output("model3_full")),
          tabPanel("Explanation",
                   p("The Bivariate Model asks: 'Do patients with LEP have a higher 30-day readmission rate?' The answer here is YES. The OR is high and significant, identifying a clear disparity."),
                   p(""),
                   p("The Full Model asks: 'After accounting for how sick the patient was (surgical complexity, length of stay), does LEP status remain a predictor?' In this model, the OR for LEP status drops to ~1.0 and is *not significant*. The confounders are *significant*. This shows the disparity is *explained by* the fact that LEP patients in this sample had more complex surgeries.")
          )
        )
      )
    ),
    card(
      card_header("Interactive Code"),
      aceEditor("code3", 
                value = '# Data: 30-day readmission after major surgery (n=100)
# Disparity vs. Mechanism example
# Data updated to show strong confounding
set.seed(123)
n <- 100
surgery_discharge_data <- tibble(
  patient_id = 1:n,
  lep_status = factor(rep(c("Proficient", "LEP"), each = n/2)),
  # LEP patients have higher surgical complexity on average
  surgical_complexity = rnorm(n, mean = 5, sd = 1.5) + (lep_status == "LEP") * 2.5,
  # Length of stay is correlated with complexity
  length_of_stay = surgical_complexity + rnorm(n, mean = 1, sd = 0.5),
  
  # Log-odds of readmission driven *only* by complexity/LOS
  # The 0.0 coefficient for LEP status means its true adjusted OR is 1.0
  log_odds = -8 + 1.0 * surgical_complexity + 0.5 * length_of_stay + 0.0 * (lep_status == "LEP"),
  prob = plogis(log_odds),
  readmitted = rbinom(n, 1, prob)
) %>%
  select(patient_id, lep_status, surgical_complexity, length_of_stay, readmitted)


# Bivariate model asks: "Is there a disparity?"
bivariate_model3 <- glm(readmitted ~ lep_status, data = surgery_discharge_data, family = "binomial")

# Full model asks: "Is the disparity explained by these confounders?"
full_model3 <- glm(readmitted ~ lep_status + surgical_complexity + length_of_stay, 
                   data = surgery_discharge_data, family = "binomial")

# Try adding: print(summary(full_model3))',
                mode = "r",
                theme = "monokai"),
      actionButton("run3", "Run Code", class = "btn-primary mt-3"),
      # --- ADDED CONSOLE OUTPUT ---
      verbatimTextOutput("code3_output")
    )
  ), 
  
  # Summary Tab
  nav_panel(
    title = "Summary",
    p("When you are interested in the question of whether a demographic group receives access to a program at different rates compared to other demographic groups, irrespective of the reason, then a bivariate model will answer that question."),
    p(""),
    p("However, if you are interested in whether otherwise similarly situated individuals of different demographics access a benefit at the same rate, then including pre-treatment controls (confounders) that are correlated with the demographic characteristic and the outcome is typically appropriate."),
    p(""),
    p("Be cautious about controlling for 'post-attribute' variables (mediators), as this can mask the very inequity you are trying to study.")
  ),
  p("Developed for OHSU HIP523 October 2024 by Jessica Minnier. Most coding performed by Claude.AI (3.5 Sonnet). App content updated by Gemini.", class = "text-center mb-4")
  )
)


# Updated server logic
server <- function(input, output, session) {
  
  # Example 1: Post-attribute Bias (Opioid)
  example1_default_data <- tibble(
    patient_id = 1:6,
    patient_race = c("White", "White", "White", "Black", "Black", "Black"),
    in_hospital_pain = c(5, 5, 5, 8, 8, 5),
    MME = c(60, 60, 60, 40, 40, 60)
  )
  
  example1_data <- reactiveVal(example1_default_data)
  example1_models <- reactive({
    if (!is.null(example1_error())) return(NULL)
    env <- new.env(parent = .GlobalEnv) # Ensure packages are found
    eval(parse(text = input$code1), envir = env)
    list(
      full = env$full_model1,
      bivariate = env$bivariate_model1
    )
  })
  example1_error <- reactiveVal(NULL)
  # --- ADDED CONSOLE OUTPUT REACTIVEVAL ---
  example1_console_output <- reactiveVal(NULL)
  
  observeEvent(input$run1, {
    # Clear previous outputs
    example1_error(NULL)
    example1_console_output(NULL)
    
    tryCatch({
      # Capture console output
      output_text <- capture.output({
        env <- new.env(parent = .GlobalEnv)
        eval(parse(text = input$code1), envir = env)
      }, type = c("output", "message"))
      
      # Check for necessary objects
      if (!exists("surgical_data", envir = env)) {
        example1_error("Error: surgical_data object not found.")
        return()
      }
      if (!exists("full_model1", envir = env) || !exists("bivariate_model1", envir = env)) {
        example1_error("Error: model objects not found. Please create full_model1 and bivariate_model1.")
        return()
      }
      
      # Success: Update data and models
      example1_data(env$surgical_data)
      example1_error(NULL)
      
      # Display console output
      if (length(output_text) > 0) {
        example1_console_output(paste(output_text, collapse = "\n"))
      } else {
        example1_console_output("Code ran successfully. (Add print() commands to see variable output).")
      }
      
    }, error = function(e) {
      # Display error in both error box and console
      err_msg <- paste("Error:", e$message)
      example1_error(err_msg)
      example1_console_output(err_msg)
    })
  })
  
  # Example 2: Omitted Variable Bias (Nerve Block)
  example2_default_data <- {
    set.seed(42)
    bind_rows(
      tibble(insurance_type = "Private", patient_asa_score = "Low", nerve_block = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
      tibble(insurance_type = "Private", patient_asa_score = "High", nerve_block = c(1, 1, 1, 1, 1, 1, 0, 0)),
      tibble(insurance_type = "Medicaid", patient_asa_score = "Low", nerve_block = c(1, 0, 0, 0, 0, 0, 0, 0)),
      tibble(insurance_type = "Medicaid", patient_asa_score = "High", nerve_block = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0))
    ) %>%
      mutate(
        patient_id = 1:n(),
        insurance_type = factor(insurance_type, levels = c("Private", "Medicaid")),
        patient_asa_score = factor(patient_asa_score, levels = c("Low", "High"))
      ) %>%
      select(patient_id, insurance_type, patient_asa_score, nerve_block)
  }
  
  example2_data <- reactiveVal(example2_default_data)
  example2_models <- reactive({
    if (!is.null(example2_error())) return(NULL)
    env <- new.env(parent = .GlobalEnv)
    eval(parse(text = input$code2), envir = env)
    list(
      full = env$full_model2,
      bivariate = env$bivariate_model2
    )
  })
  example2_error <- reactiveVal(NULL)
  # --- ADDED CONSOLE OUTPUT REACTIVEVAL ---
  example2_console_output <- reactiveVal(NULL)
  
  observeEvent(input$run2, {
    # Clear previous outputs
    example2_error(NULL)
    example2_console_output(NULL)
    
    tryCatch({
      # Capture console output
      output_text <- capture.output({
        env <- new.env(parent = .GlobalEnv)
        eval(parse(text = input$code2), envir = env)
      }, type = c("output", "message"))
      
      # Check for necessary objects
      if (!exists("ortho_data", envir = env)) {
        example2_error("Error: ortho_data object not found.")
        return()
      }
      if (!exists("full_model2", envir = env) || !exists("bivariate_model2", envir = env)) {
        example2_error("Error: model objects not found. Please create full_model2 and bivariate_model2.")
        return()
      }
      
      # Success: Update data and models
      example2_data(env$ortho_data)
      example2_error(NULL)
      
      # Display console output
      if (length(output_text) > 0) {
        example2_console_output(paste(output_text, collapse = "\n"))
      } else {
        example2_console_output("Code ran successfully. (Add print() commands to see variable output).")
      }
      
    }, error = function(e) {
      # Display error in both error box and console
      err_msg <- paste("Error:", e$message)
      example2_error(err_msg)
      example2_console_output(err_msg)
    })
  })
  
  # Example 3: Disparities vs. Mechanisms (LEP Readmission)
  example3_default_data <- {
    set.seed(123)
    n <- 100
    tibble(
      patient_id = 1:n,
      lep_status = factor(rep(c("Proficient", "LEP"), each = n/2)),
      surgical_complexity = rnorm(n, mean = 5, sd = 1.5) + (lep_status == "LEP") * 2.5,
      length_of_stay = surgical_complexity + rnorm(n, mean = 1, sd = 0.5),
      log_odds = -8 + 1.0 * surgical_complexity + 0.5 * length_of_stay + 0.0 * (lep_status == "LEP"),
      prob = plogis(log_odds),
      readmitted = rbinom(n, 1, prob)
    ) %>%
      select(patient_id, lep_status, surgical_complexity, length_of_stay, readmitted)
  }
  
  example3_data <- reactiveVal(example3_default_data)
  example3_models <- reactive({
    if (!is.null(example3_error())) return(NULL)
    env <- new.env(parent = .GlobalEnv)
    eval(parse(text = input$code3), envir = env)
    list(
      bivariate = env$bivariate_model3,
      full = env$full_model3
    )
  })
  example3_error <- reactiveVal(NULL)
  # --- ADDED CONSOLE OUTPUT REACTIVEVAL ---
  example3_console_output <- reactiveVal(NULL)
  
  observeEvent(input$run3, {
    # Clear previous outputs
    example3_error(NULL)
    example3_console_output(NULL)
    
    tryCatch({
      # Capture console output
      output_text <- capture.output({
        env <- new.env(parent = .GlobalEnv)
        eval(parse(text = input$code3), envir = env)
      }, type = c("output", "message"))
      
      # Check for necessary objects
      if (!exists("surgery_discharge_data", envir = env)) {
        example3_error("Error: surgery_discharge_data object not found.")
        return()
      }
      if (!exists("bivariate_model3", envir = env) || !exists("full_model3", envir = env)) {
        example3_error("Error: model objects not found. Please create bivariate_model3 and full_model3.")
        return()
      }
      
      # Success: Update data and models
      example3_data(env$surgery_discharge_data)
      example3_error(NULL)
      
      # Display console output
      if (length(output_text) > 0) {
        example3_console_output(paste(output_text, collapse = "\n"))
      } else {
        example3_console_output("Code ran successfully. (Add print() commands to see variable output).")
      }
      
    }, error = function(e) {
      # Display error in both error box and console
      err_msg <- paste("Error:", e$message)
      example3_error(err_msg)
      example3_console_output(err_msg)
    })
  })
  
  # --- Outputs for all examples ---
  
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
    tbl_regression(example1_models()$full, exponentiate = FALSE) %>%
      as_gt()
  })
  
  output$model1_bivariate <- gt::render_gt({
    req(is.null(example1_error()))
    req(example1_models()$bivariate)
    tbl_regression(example1_models()$bivariate, exponentiate = FALSE) %>%
      as_gt()
  })
  
  # --- ADDED CONSOLE OUTPUT RENDERER ---
  output$code1_output <- renderPrint({
    req(example1_console_output())
    cat(example1_console_output())
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
    tbl_regression(example2_models()$full, exponentiate = TRUE) %>%
      as_gt()
  })
  
  output$model2_bivariate <- gt::render_gt({
    req(is.null(example2_error()))
    req(example2_models()$bivariate)
    tbl_regression(example2_models()$bivariate, exponentiate = TRUE) %>%
      as_gt()
  })
  
  # --- ADDED CONSOLE OUTPUT RENDERER ---
  output$code2_output <- renderPrint({
    req(example2_console_output())
    cat(example2_console_output())
  })
  
  # Example 3 outputs
  output$data3_table <- renderTable({
    if (!is.null(example3_error())) return(NULL)
    example3_data()
  })
  
  output$error3 <- renderText({
    example3_error()
  })
  
  output$model3_bivariate <- gt::render_gt({
    req(is.null(example3_error()))
    req(example3_models()$bivariate)
    tbl_regression(example3_models()$bivariate, exponentiate = TRUE) %>%
      as_gt()
  })
  
  output$model3_full <- gt::render_gt({
    req(is.null(example3_error()))
    req(example3_models()$full)
    tbl_regression(example3_models()$full, exponentiate = TRUE) %>%
      as_gt()
  })
  
  # --- ADDED CONSOLE OUTPUT RENDERER ---
  output$code3_output <- renderPrint({
    req(example3_console_output())
    cat(example3_console_output())
  })
}

# Run the app
shinyApp(ui = ui, server = server)

