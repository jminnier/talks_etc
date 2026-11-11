library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(gtsummary)

source("sims.R", keep.source = TRUE)

col_unadj <- "#b8002e"
col_adj <- "#0b3d5c"
role_cols <- c(Attribute = "#b8002e", Exposure = "#b8002e", Outcome = "#0b3d5c",
               Mediator = "#d08c00", Confounder = "#1a7f37", Modifier = "#6f42c1")
theme_set(theme_minimal(base_size = 15))

# ---- helpers ----------------------------------------------------------------

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
pct <- function(x) paste0(fmt(100 * x, 0), "%")
fmt_or <- function(r) paste0(fmt(r$est), " (95% CI ", fmt(r$lo), " to ", fmt(r$hi), ")")
fmt_pts <- function(rd) paste0(ifelse(rd > 0, "+", ""), fmt(100 * rd, 1), " percentage points")
ci_excludes_1 <- function(r) r$lo > 1 | r$hi < 1

or_row <- function(model, term, label) {
  b <- coef(model)[[term]]
  se <- sqrt(vcov(model)[term, term])
  tibble(model = label, est = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))
}

# risk difference by standardization: everyone as group 2 vs everyone as group 1
rd_gcomp <- function(model, data, x) {
  lv <- levels(data[[x]])
  d0 <- data; d0[[x]] <- factor(lv[1], levels = lv)
  d1 <- data; d1[[x]] <- factor(lv[2], levels = lv)
  mean(predict(model, d1, type = "response")) - mean(predict(model, d0, type = "response"))
}

compare_models <- function(data, y, x, z, adj_label) {
  m0 <- glm(reformulate(x, y), family = binomial, data = data)
  m1 <- glm(reformulate(c(x, z), y), family = binomial, data = data)
  term <- paste0(x, levels(data[[x]])[2])
  est <- bind_rows(or_row(m0, term, "Unadjusted"), or_row(m1, term, adj_label))
  est$rd <- c(rd_gcomp(m0, data, x), rd_gcomp(m1, data, x))
  est$model <- factor(est$model, levels = rev(est$model))
  list(m0 = m0, m1 = m1, est = est,
       u = est[est$model == "Unadjusted", ], a = est[est$model != "Unadjusted", ])
}

or_plot <- function(est) {
  ggplot(est, aes(est, model, colour = model)) +
    geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.3) +
    geom_point(size = 5) +
    scale_x_log10() +
    scale_colour_manual(values = setNames(c(col_adj, col_unadj), levels(est$model))) +
    labs(x = "Odds ratio with 95% CI (log scale). 1 = no difference", y = NULL) +
    theme(legend.position = "none")
}

est_table <- function(est) {
  est |>
    arrange(desc(model)) |>
    transmute(Model = as.character(model),
              `Odds ratio (95% CI)` = paste0(fmt(est), " (", fmt(lo), " to ", fmt(hi), ")"),
              `Risk difference` = fmt_pts(rd))
}

# nodes: name, label, x, y, role; edges: from, to, size (0 = no effect).
# `to` can be "mid:a:b" to point at the middle of the a -> b arrow (effect modification).
dag_plot <- function(nodes, edges) {
  xy <- select(nodes, name, x, y)
  get_xy <- function(n) {
    if (startsWith(n, "mid:")) {
      p <- strsplit(n, ":")[[1]]
      colMeans(rbind(unlist(xy[xy$name == p[2], c("x", "y")]), unlist(xy[xy$name == p[3], c("x", "y")])))
    } else unlist(xy[xy$name == n, c("x", "y")])
  }
  e <- edges
  s <- t(sapply(e$from, get_xy)); t_ <- t(sapply(e$to, get_xy))
  e$x0 <- s[, 1]; e$y0 <- s[, 2]; e$x1 <- t_[, 1]; e$y1 <- t_[, 2]
  e <- e |>
    mutate(len = sqrt((x1 - x0)^2 + (y1 - y0)^2),
           end_gap = ifelse(startsWith(to, "mid:"), 0.04, 0.24),
           xs = x0 + (x1 - x0) * 0.24 / len, ys = y0 + (y1 - y0) * 0.24 / len,
           xe = x1 - (x1 - x0) * end_gap / len, ye = y1 - (y1 - y0) * end_gap / len,
           lw = ifelse(size == 0, 0.6, 0.8 + 1.2 * pmin(abs(size), 2)),
           lt = ifelse(size == 0, "dashed", "solid"))
  ggplot() +
    geom_segment(data = e, aes(xs, ys, xend = xe, yend = ye, linewidth = lw, linetype = lt),
                 colour = "grey35", arrow = arrow(length = unit(4, "mm"), type = "closed")) +
    scale_linewidth_identity() +
    scale_linetype_identity() +
    geom_label(data = nodes, aes(x, y, label = label, fill = role), colour = "white",
               fontface = "bold", size = 4.6, label.padding = unit(0.45, "lines")) +
    scale_fill_manual(values = role_cols) +
    coord_cartesian(xlim = c(-0.3, 1.3), ylim = c(-0.25, 1.1)) +
    theme_void(base_size = 14) +
    theme(legend.position = "bottom", legend.title = element_blank())
}

dag_note <- p(class = "small text-muted mb-0",
              "Thicker arrow = bigger effect; dashed = no effect (set by the sliders). ",
              "Attribute = the characteristic we're asking about disparities by. ",
              "Mediator = on the path between the attribute and the outcome. ",
              "Confounder = affects both the attribute and the outcome.")

show_code <- function(fn_name, model_lines) {
  src <- paste(as.character(attr(get(fn_name), "srcref")), collapse = "\n")
  paste0(fn_name, " <- ", src, "\n\n", model_lines)
}

# one layout for examples 1-3
example_tab <- function(id, title, question, how, inputs) {
  nav_panel(
    title,
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(width = 320, inputs,
                        uiOutput(paste0(id, "_rates")),
                        actionButton(paste0(id, "_resample"), "New sample",
                                     class = "btn-outline-secondary")),
      p(class = "lead mb-1", question),
      p(class = "text-muted", how),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Assumed causal diagram"), plotOutput(paste0(id, "_dag"), height = 280), dag_note),
        card(card_header("What the two models say"),
             plotOutput(paste0(id, "_est"), height = 190),
             tableOutput(paste0(id, "_esttab")))
      ),
      card(card_header("What's going on"), uiOutput(paste0(id, "_explain"))),
      accordion(
        open = "Group summary",
        accordion_panel("Group summary", tableOutput(paste0(id, "_summary"))),
        accordion_panel("Model output (gtsummary)",
                        layout_columns(gt::gt_output(paste0(id, "_tbl0")),
                                       gt::gt_output(paste0(id, "_tbl1")))),
        accordion_panel("R code", verbatimTextOutput(paste0(id, "_code")))
      )
    )
  )
}

rate_box <- function(...) div(class = "small mb-3 p-2 bg-light rounded", ...)

# ---- UI ---------------------------------------------------------------------

ui <- page_navbar(
  title = "Equity analysis in clinical research",
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = col_adj),
  header = p(class = "text-muted small mt-2",
             "HIP 523, November 2026. All data are simulated, so we know the true answer.",
             "Move the sliders and watch how the two models respond. 'New sample' redraws the data with the same settings, so you can see how much results bounce around by chance."),

  example_tab(
    "ex1", "1. Post-attribute bias",
    "ED visits for a long-bone fracture: are Black patients less likely to get opioid analgesia? Should we adjust for triage acuity?",
    "Triage acuity is the Emergency Severity Index (ESI) a triage nurse assigns, from 1 (most urgent) to 5. Move the triage-bias slider and watch the two odds ratios. Then compare with tab 3, which has the same diagram but asks a different question.",
    tagList(
      sliderInput("ex1_triage", "Bias at triage: Black patients less likely to be assigned high acuity (0 = none)", 0, 2.5, 1.5, 0.25),
      sliderInput("ex1_direct", "Bias at the prescribing step (0 = none)", 0, 2, 0, 0.25),
      sliderInput("ex1_n", "Number of ED visits", 300, 5000, 1500, 100)
    )
  ),

  example_tab(
    "ex2", "2. Omitted variable bias",
    "Adults with diabetes: are Medicaid patients less likely to be prescribed a statin? Should we adjust for age?",
    "Age is a confounder here: it affects which insurance people have and whether a statin is recommended. Move the age-gap slider and watch the unadjusted estimate drift away from the truth.",
    tagList(
      sliderInput("ex2_agegap", "How much younger Medicaid patients are, on average (years)", 0, 20, 10, 1),
      sliderInput("ex2_effect", "True effect of Medicaid on statin prescribing (log odds ratio; 0 = none, negative = fewer statins)", -1.5, 0.5, -0.4, 0.1),
      sliderInput("ex2_n", "Number of patients", 300, 5000, 1500, 100)
    )
  ),

  example_tab(
    "ex3", "3. Disparity vs. mechanism",
    "Heart failure discharges: are patients with limited English proficiency (LEP) readmitted more often? What happens if we adjust for a 7-day follow-up visit?",
    "Same diagram as tab 1, but here we use the adjusted model on purpose, to see how much of the gap runs through missed follow-up. Move the follow-up slider and watch the adjusted odds ratio.",
    tagList(
      sliderInput("ex3_gap", "How much less likely LEP patients are to get 7-day follow-up (0 = no difference)", 0, 3, 2, 0.25),
      sliderInput("ex3_direct", "Effect of LEP on readmission through other pathways (0 = none)", 0, 1.5, 0, 0.25),
      sliderInput("ex3_n", "Number of discharges", 300, 5000, 1500, 100)
    )
  ),

  nav_panel(
    "4. Effect modification",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 320,
        sliderInput("ex4_low", "True program effect, low-ADI neighborhoods (mmHg; negative = BP goes down)", -15, 5, -8, 1),
        sliderInput("ex4_high", "True program effect, high-ADI neighborhoods (mmHg)", -15, 5, -2, 1),
        sliderInput("ex4_n", "Patients per ADI × arm group (4 groups)", 50, 500, 150, 25),
        actionButton("ex4_resample", "New sample", class = "btn-outline-secondary")
      ),
      p(class = "lead mb-1", "A remote blood pressure monitoring program: does it work as well for patients in high-deprivation neighborhoods?"),
      p(class = "text-muted", "ADI is the Area Deprivation Index, a neighborhood measure built from census data (income, education, housing). Set the two true effects and compare the single average effect with the group-specific ones. Red x = the true value."),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Assumed causal diagram"), plotOutput("ex4_dag", height = 280),
             p(class = "small text-muted mb-0", "ADI points at the arrow, not at the outcome: it changes how well the program works (effect modification).")),
        card(card_header("Estimated program effect"), plotOutput("ex4_est", height = 300))
      ),
      card(card_header("What's going on"), uiOutput("ex4_explain")),
      accordion(
        open = FALSE,
        accordion_panel("Model output (gtsummary)",
                        layout_columns(gt::gt_output("ex4_tbl0"), gt::gt_output("ex4_tbl1"))),
        accordion_panel("R code", verbatimTextOutput("ex4_code"))
      )
    )
  ),

  nav_panel(
    "5. Prediction model fairness",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 320,
        sliderInput("ex5_pct", "Share of group B in the training data", 0.02, 0.5, 0.1, 0.02),
        sliderInput("ex5_base", "How much higher group B's baseline risk is (log odds; 0 = same)", 0, 1.5, 0.8, 0.1),
        sliderInput("ex5_noise", "Measurement error in group B's lab value (group A is fixed at 0.2, so 0.2 = same as A)", 0.2, 2, 0.2, 0.1),
        checkboxInput("ex5_group", "Include group in the model", FALSE),
        actionButton("ex5_resample", "New sample", class = "btn-outline-secondary")
      ),
      p(class = "lead mb-1", "A 30-day readmission risk model built from EHR data. It is tested on new patients, half from group A and half from group B. Does it work equally well for both?"),
      p(class = "text-muted", "This ties back to the algorithmic fairness session. Try one problem at a time: first the baseline-risk slider, then the measurement-error slider (think pulse oximetry)."),
      card(
        class = "bg-light",
        card_body(
          p(class = "small mb-0",
            strong("AUC"), " (area under the ROC curve): how well the model ranks patients who were readmitted above those who weren't. 0.5 = coin flip, 1 = perfect. ",
            strong("Calibration"), ": when the model says 20% risk, do about 20% of those patients get readmitted? On the plot, points on the dashed line are well calibrated.")
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Discrimination and calibration by group (test data)"), tableOutput("ex5_tab")),
        card(card_header("Calibration plot (test data)"), plotOutput("ex5_cal", height = 320))
      ),
      card(card_header("What's going on"), uiOutput("ex5_explain")),
      accordion(open = FALSE, accordion_panel("R code", verbatimTextOutput("ex5_code")))
    )
  ),

  nav_panel(
    "Summary",
    card(
      card_body(
        tags$ul(
          tags$li("If the question is whether a group gets care at different rates, for whatever reason, the unadjusted comparison answers it. This is the total disparity."),
          tags$li("If the question is whether similar patients are treated the same, adjust for confounders: variables that come before the attribute and affect the outcome, like age in tab 2."),
          tags$li("Be careful adjusting for variables that are affected by the attribute (mediators, or post-attribute variables), like triage acuity in tab 1. Adjusting for them can hide the disparity you're trying to measure."),
          tags$li("Adjusting for a mediator on purpose can show where a disparity comes from, like follow-up visits in tab 3. It doesn't make the disparity go away."),
          tags$li("Look for effect modification. One average effect can hide a group the intervention doesn't help (tab 4)."),
          tags$li("Report absolute differences (percentage points, mmHg) alongside odds ratios. Equity questions are usually about how many people are affected."),
          tags$li("For prediction models, check AUC and calibration in each group (tab 5). A different baseline risk can be fixed in the model; measurement error in the inputs can't.")
        )
      )
    )
  ),

  footer = p(class = "text-center text-muted small my-3",
             "Developed for OHSU HIP 523 by Jessica Minnier (2024; updated November 2026).",
             "Code written with help from Claude.")
)

# ---- server -----------------------------------------------------------------

server <- function(input, output, session) {

  binary_outputs <- function(id, fit, data, dag, explain, code, summary_vars, rates) {
    output[[paste0(id, "_dag")]] <- renderPlot(dag())
    output[[paste0(id, "_est")]] <- renderPlot(or_plot(fit()$est))
    output[[paste0(id, "_esttab")]] <- renderTable(est_table(fit()$est), striped = TRUE)
    output[[paste0(id, "_explain")]] <- renderUI(explain())
    output[[paste0(id, "_rates")]] <- renderUI(rates())
    output[[paste0(id, "_summary")]] <- renderTable({
      d <- data()
      d |>
        group_by(across(all_of(summary_vars))) |>
        summarise(n = n(), `Outcome rate` = pct(mean(.data[[names(d)[ncol(d)]]])),
                  .groups = "drop")
    })
    output[[paste0(id, "_tbl0")]] <- gt::render_gt(
      tbl_regression(fit()$m0, exponentiate = TRUE) |> modify_caption("**Unadjusted**") |> as_gt())
    output[[paste0(id, "_tbl1")]] <- gt::render_gt(
      tbl_regression(fit()$m1, exponentiate = TRUE) |> modify_caption("**Adjusted**") |> as_gt())
    output[[paste0(id, "_code")]] <- renderText(code())
  }

  # ---- Example 1 ----
  d1 <- reactive(simulate_ed(input$ex1_n, input$ex1_triage, input$ex1_direct,
                             seed = 100 + input$ex1_resample))
  f1 <- reactive(compare_models(d1(), "analgesia", "race", "triage_acuity", "Adjusted for triage acuity"))
  binary_outputs(
    "ex1", f1, d1,
    dag = reactive(dag_plot(
      tibble(name = c("race", "triage", "y"), label = c("Race", "Triage acuity", "Opioid\nanalgesia"),
             x = c(0, 0.5, 1), y = c(0, 0.85, 0), role = c("Attribute", "Mediator", "Outcome")),
      tibble(from = c("race", "triage", "race"), to = c("triage", "y", "y"),
             size = c(input$ex1_triage, 1.5, input$ex1_direct)))),
    rates = reactive({
      d <- d1()
      r <- tapply(d$triage_acuity == "High (ESI 1-2)", d$race, mean)
      rate_box("In this sample, assigned high acuity (ESI 1-2): White ", strong(pct(r[["White"]])),
               ", Black ", strong(pct(r[["Black"]])), ".")
    }),
    explain = reactive({
      f <- f1(); u <- f$u; a <- f$a
      tri <- input$ex1_triage; dir <- input$ex1_direct
      tagList(
        p("Unadjusted, Black patients have an odds ratio of ", strong(fmt_or(u)),
          " for getting opioid analgesia, or ", strong(fmt_pts(u$rd)), "."),
        p("Adjusted for triage acuity, the odds ratio is ", strong(fmt_or(a)),
          ". In this simulation the true effect at the prescribing step is an odds ratio of ", strong(fmt(exp(-dir))),
          ", which is what the adjusted model estimates."),
        if (tri > 0 && dir == 0)
          p("All of the bias here happens at triage. Triage acuity sits between race and the outcome, so adjusting for it removes the part of the disparity that runs through biased triage. The adjusted model makes prescribing look fair when patients were not treated fairly. The unadjusted model is the one to report."),
        if (tri > 0 && dir > 0)
          p("Now there's bias at both steps. The adjusted model picks up the prescribing part, but it misses the bias at triage. The unadjusted model captures both."),
        if (tri == 0 && dir > 0)
          p("With no bias at triage, acuity is just a clinical factor, and the two models roughly agree."),
        if (tri == 0 && dir == 0)
          p("With no bias anywhere, both odds ratios should be close to 1. Any difference is chance."),
        p(class = "text-muted", "Try: set triage bias to 0 and prescribing bias above 0. Then try both above 0.")
      )
    }),
    code = reactive(show_code("simulate_ed", paste0(
      "d <- simulate_ed(n = ", input$ex1_n, ", triage_bias = ", input$ex1_triage,
      ", direct_bias = ", input$ex1_direct, ", seed = ", 100 + input$ex1_resample, ")\n",
      "glm(analgesia ~ race, family = binomial, data = d)\n",
      "glm(analgesia ~ race + triage_acuity, family = binomial, data = d)"))),
    summary_vars = c("race", "triage_acuity")
  )

  # ---- Example 2 ----
  d2 <- reactive(simulate_statin(input$ex2_n, input$ex2_agegap, input$ex2_effect,
                                 seed = 200 + input$ex2_resample))
  f2 <- reactive(compare_models(d2(), "statin", "insurance", "age", "Adjusted for age"))
  binary_outputs(
    "ex2", f2, d2,
    dag = reactive(dag_plot(
      tibble(name = c("ins", "age", "y"), label = c("Insurance", "Age", "Statin\nprescribed"),
             x = c(0, 0.5, 1), y = c(0, 0.85, 0), role = c("Attribute", "Confounder", "Outcome")),
      tibble(from = c("age", "age", "ins"), to = c("ins", "y", "y"),
             size = c(input$ex2_agegap / 10, 1.5, input$ex2_effect)))),
    rates = reactive({
      d <- d2()
      a <- tapply(d$age, d$insurance, mean)
      rate_box("In this sample, mean age: Private ", strong(fmt(a[["Private"]], 0)),
               ", Medicaid ", strong(fmt(a[["Medicaid"]], 0)), ".")
    }),
    explain = reactive({
      f <- f2(); u <- f$u; a <- f$a
      gap <- input$ex2_agegap; eff <- input$ex2_effect
      tagList(
        p("Unadjusted odds ratio for Medicaid: ", strong(fmt_or(u)), " (", fmt_pts(u$rd), "). ",
          "Adjusted for age: ", strong(fmt_or(a)), " (", fmt_pts(a$rd), "). ",
          "The true odds ratio set by the slider is ", strong(fmt(exp(eff))), "."),
        p("Age affects which insurance people have and whether a statin is recommended, and insurance doesn't change anyone's age. So age is a confounder and belongs in the model."),
        if (gap > 0)
          p("Medicaid patients here are younger, and younger patients get fewer statins, so leaving age out makes Medicaid look worse than it is",
            if (eff < 0) "." else if (eff == 0) ", even though insurance itself has no effect."
            else ". In this simulation Medicaid patients are actually more likely to get a statin at the same age, and the unadjusted model can't see it.")
        else
          p("With no age gap, age can't confound the comparison, and the two models agree up to chance."),
        if (eff < 0 && ci_excludes_1(a))
          p("Even after adjusting for age, a gap remains. That's the part worth asking questions about."),
        if (eff == 0)
          p("The true effect is zero, so any gap in the unadjusted model is coming from age."),
        p(class = "text-muted", "Try: set the age gap to 0, then set the true effect to 0.")
      )
    }),
    code = reactive(show_code("simulate_statin", paste0(
      "d <- simulate_statin(n = ", input$ex2_n, ", age_gap = ", input$ex2_agegap,
      ", insurance_effect = ", input$ex2_effect, ", seed = ", 200 + input$ex2_resample, ")\n",
      "glm(statin ~ insurance, family = binomial, data = d)\n",
      "glm(statin ~ insurance + age, family = binomial, data = d)"))),
    summary_vars = "insurance"
  )

  # ---- Example 3 ----
  d3 <- reactive(simulate_hf(input$ex3_n, input$ex3_gap, input$ex3_direct,
                             seed = 300 + input$ex3_resample))
  f3 <- reactive(compare_models(d3(), "readmit_30d", "lep_status", "followup_7d",
                                "Adjusted for 7-day follow-up"))
  binary_outputs(
    "ex3", f3, d3,
    dag = reactive(dag_plot(
      tibble(name = c("lep", "fu", "y"), label = c("LEP status", "7-day follow-up", "30-day\nreadmission"),
             x = c(0, 0.5, 1), y = c(0, 0.85, 0), role = c("Attribute", "Mediator", "Outcome")),
      tibble(from = c("lep", "fu", "lep"), to = c("fu", "y", "y"),
             size = c(input$ex3_gap, 0.9, input$ex3_direct)))),
    rates = reactive({
      d <- d3()
      r <- tapply(d$followup_7d == "Yes", d$lep_status, mean)
      rate_box("In this sample, made it to 7-day follow-up: English proficient ", strong(pct(r[[1]])),
               ", LEP ", strong(pct(r[[2]])), ".")
    }),
    explain = reactive({
      f <- f3(); u <- f$u; a <- f$a
      gap <- input$ex3_gap; dir <- input$ex3_direct
      tagList(
        p("Unadjusted, LEP patients have an odds ratio of ", strong(fmt_or(u)),
          " for readmission (", fmt_pts(u$rd), "). This answers \"is there a disparity?\""),
        p("Adjusted for a completed 7-day follow-up visit: ", strong(fmt_or(a)),
          ". The true effect of LEP through other pathways is an odds ratio of ", strong(fmt(exp(dir))), "."),
        if (gap > 0 && dir == 0)
          p("In this simulation the whole gap runs through missed follow-up. LEP patients are still readmitted more often. The adjusted odds ratio near 1 shows where the gap comes from, and follow-up is something we can work on: interpreters at discharge, and instructions in the patient's language."),
        if (gap > 0 && dir > 0)
          p("Part of the gap runs through follow-up and part runs through other pathways. The adjusted model estimates the part that's left after follow-up."),
        if (gap == 0 && dir > 0)
          p("Follow-up is the same in both groups, so it explains none of the gap, and the two models roughly agree."),
        if (gap == 0 && dir == 0)
          p("With no difference in follow-up and no other pathway, there's no true disparity. Both odds ratios should be close to 1."),
        p(class = "text-muted", "Compare with tab 1: same diagram. There, adjusting hid a disparity we wanted to measure. Here, we adjust on purpose to find where the disparity comes from. Which model to report depends on the question.")
      )
    }),
    code = reactive(show_code("simulate_hf", paste0(
      "d <- simulate_hf(n = ", input$ex3_n, ", followup_gap = ", input$ex3_gap,
      ", direct_effect = ", input$ex3_direct, ", seed = ", 300 + input$ex3_resample, ")\n",
      "glm(readmit_30d ~ lep_status, family = binomial, data = d)\n",
      "glm(readmit_30d ~ lep_status + followup_7d, family = binomial, data = d)"))),
    summary_vars = c("lep_status", "followup_7d")
  )

  # ---- Example 4 ----
  d4 <- reactive(simulate_rpm(input$ex4_n, input$ex4_low, input$ex4_high,
                              seed = 400 + input$ex4_resample))
  f4 <- reactive({
    m0 <- lm(sbp_change ~ adi + program, data = d4())
    m1 <- lm(sbp_change ~ adi * program, data = d4())
    b <- coef(m1); V <- vcov(m1)
    t1 <- "programRemote monitoring"; t2 <- "adiHigh ADI:programRemote monitoring"
    est <- tibble(
      model = c("Main effects model\n(one average effect)", "Interaction model:\nlow ADI", "Interaction model:\nhigh ADI"),
      est = c(coef(m0)[[t1]], b[[t1]], b[[t1]] + b[[t2]]),
      se = c(sqrt(vcov(m0)[t1, t1]), sqrt(V[t1, t1]), sqrt(V[t1, t1] + V[t2, t2] + 2 * V[t1, t2])),
      truth = c(mean(c(input$ex4_low, input$ex4_high)), input$ex4_low, input$ex4_high)
    ) |> mutate(lo = est - 1.96 * se, hi = est + 1.96 * se, model = factor(model, levels = rev(model)))
    inter <- c(est = b[[t2]], lo = b[[t2]] - 1.96 * sqrt(V[t2, t2]), hi = b[[t2]] + 1.96 * sqrt(V[t2, t2]))
    list(m0 = m0, m1 = m1, est = est, inter = inter)
  })
  output$ex4_dag <- renderPlot(dag_plot(
    tibble(name = c("p", "adi", "y"), label = c("Remote\nmonitoring", "Neighborhood\nADI", "Change in\nsystolic BP"),
           x = c(0, 0.5, 1), y = c(0, 0.85, 0), role = c("Exposure", "Modifier", "Outcome")),
    tibble(from = c("p", "adi"), to = c("y", "mid:p:y"),
           size = c(1, abs(input$ex4_high - input$ex4_low) / 5))))
  output$ex4_est <- renderPlot({
    e <- f4()$est
    ggplot(e, aes(est, model)) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.3, colour = col_adj) +
      geom_point(size = 5, colour = col_adj) +
      geom_point(aes(truth), shape = 4, size = 5, stroke = 1.5, colour = col_unadj) +
      labs(x = "Change in systolic BP vs. usual care (mmHg), 95% CI\nred x = true effect", y = NULL)
  })
  output$ex4_explain <- renderUI({
    f <- f4(); e <- f$est; it <- f$inter
    diff_true <- input$ex4_high - input$ex4_low
    detected <- it[["lo"]] > 0 | it[["hi"]] < 0
    tagList(
      p("The main effects model gives one average program effect: ", strong(fmt(e$est[1], 1), " mmHg"), "."),
      p("The interaction model estimates ", strong(fmt(e$est[2], 1), " mmHg"), " in low-ADI neighborhoods and ",
        strong(fmt(e$est[3], 1), " mmHg"), " in high-ADI neighborhoods. The difference between them (the interaction) is ",
        strong(fmt(it[["est"]], 1), " mmHg (95% CI ", fmt(it[["lo"]], 1), " to ", fmt(it[["hi"]], 1), ")"), "."),
      if (diff_true > 0)
        p("The program works less well in high-deprivation neighborhoods. If we only reported the average, we'd miss that. The gap could come from things like broadband access, or the time and money to keep up with home monitoring. Rolling the program out as-is could widen the disparity in blood pressure control.")
      else if (diff_true < 0)
        p("Here the program works better in high-deprivation neighborhoods. Reporting only the average would undersell it for the patients who benefit most.")
      else
        p("The true effect is the same in both groups, so the average is a fair summary. Any difference between the two estimates is chance."),
      if (diff_true != 0 && !detected)
        p("At this sample size the interaction's confidence interval includes 0. Interactions need much more data to detect than main effects. Try raising the sample size."),
      p(class = "text-muted", "Try: set the two effects equal. Then lower the sample size and see how hard the interaction is to detect.")
    )
  })
  output$ex4_tbl0 <- gt::render_gt(tbl_regression(f4()$m0) |> modify_caption("**Main effects**") |> as_gt())
  output$ex4_tbl1 <- gt::render_gt(tbl_regression(f4()$m1) |> modify_caption("**Interaction**") |> as_gt())
  output$ex4_code <- renderText(show_code("simulate_rpm", paste0(
    "d <- simulate_rpm(n_per_group = ", input$ex4_n, ", effect_low_adi = ", input$ex4_low,
    ", effect_high_adi = ", input$ex4_high, ", seed = ", 400 + input$ex4_resample, ")\n",
    "lm(sbp_change ~ adi + program, data = d)\n",
    "lm(sbp_change ~ adi * program, data = d)")))

  # ---- Example 5 ----
  ex5_seed <- reactive(500 + input$ex5_resample)
  f5 <- reactive({
    s <- ex5_seed()
    train <- simulate_risk(3000, input$ex5_pct, input$ex5_base, input$ex5_noise, seed = s)
    test <- simulate_risk(3000, 0.5, input$ex5_base, input$ex5_noise, seed = s + 1000)
    f <- if (input$ex5_group) readmit ~ prior_admits + lab_recorded + group else readmit ~ prior_admits + lab_recorded
    m <- glm(f, family = binomial, data = train)
    test$pred <- predict(m, test, type = "response")
    perf <- test |>
      group_by(group) |>
      summarise(auc = as.numeric(pROC::auc(readmit, pred, quiet = TRUE)),
                observed = mean(readmit), predicted = mean(pred), .groups = "drop")
    cal <- test |>
      group_by(group) |>
      mutate(bin = ntile(pred, 10)) |>
      group_by(group, bin) |>
      summarise(predicted = mean(pred), observed = mean(readmit), .groups = "drop")
    list(perf = perf, cal = cal, n_b = sum(train$group == "B"))
  })
  output$ex5_tab <- renderTable({
    f5()$perf |>
      transmute(Group = group, AUC = fmt(auc), `Observed readmission` = pct(observed),
                `Average predicted` = pct(predicted))
  }, striped = TRUE)
  output$ex5_cal <- renderPlot({
    ggplot(f5()$cal, aes(predicted, observed, colour = group)) +
      geom_abline(linetype = 2, colour = "grey50") +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_colour_manual(values = c(A = col_adj, B = col_unadj)) +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(x = "Mean predicted risk in each decile", y = "Observed readmission rate", colour = "Group")
  })
  output$ex5_explain <- renderUI({
    pf <- f5()$perf; a <- pf[pf$group == "A", ]; b <- pf[pf$group == "B", ]
    base <- input$ex5_base; noise <- input$ex5_noise; grp <- input$ex5_group
    off <- function(r) abs(r$predicted - r$observed) > 0.03
    tagList(
      p("The model was trained on 3,000 patients, ", strong(f5()$n_b), " of them from group B. In the test data the AUC is ",
        strong(fmt(a$auc)), " for group A and ", strong(fmt(b$auc)), " for group B. ",
        "Average predicted vs. observed readmission: group A ", strong(pct(a$predicted)), " vs. ", strong(pct(a$observed)),
        ", group B ", strong(pct(b$predicted)), " vs. ", strong(pct(b$observed)), "."),
      if (base > 0 && !grp)
        p("Group B has a higher baseline risk, and the model doesn't know who is in group B. It predicts something in between, so ",
          if (off(b)) "group B's risk is under-predicted and fewer of them would get flagged for help. " else "",
          if (input$ex5_pct < 0.3)
            "Adding more group B patients to the training data doesn't fix this. It moves the error around: try raising the share of B and watch group A become over-predicted. Including group in the model does fix it."
          else
            "With this many group B patients in training, the model's average moves toward B, and now group A is over-predicted. More data moved the error around without fixing it. Including group in the model does fix it."),
      if (base > 0 && grp)
        p("With group in the model, it can learn the difference in baseline risk, and both groups are close to calibrated. Whether to use race or similar group variables in clinical models is itself debated, because it can bake in differences that come from unequal care."),
      if (noise > 0.2)
        p("Group B's lab value is measured with more error (like pulse oximetry in patients with darker skin). That lowers B's AUC, and B's calibration curve flattens: the model is too confident for B in both directions. Neither more data nor including group fixes this. The measurement has to get better."),
      if (base == 0 && noise <= 0.2)
        p("Right now the two groups are the same in every way, so the model works about equally well for both. Any difference is chance."),
      p(class = "text-muted", "Small differences in AUC (a few hundredths) can be chance. Try 'New sample'.")
    )
  })
  output$ex5_code <- renderText(show_code("simulate_risk", paste0(
    "train <- simulate_risk(n = 3000, pct_b = ", input$ex5_pct, ", baseline_diff = ", input$ex5_base,
    ", noise_b = ", input$ex5_noise, ", seed = ", ex5_seed(), ")\n",
    "test  <- simulate_risk(n = 3000, pct_b = 0.5, baseline_diff = ", input$ex5_base,
    ", noise_b = ", input$ex5_noise, ", seed = ", ex5_seed() + 1000, ")\n",
    "m <- glm(readmit ~ prior_admits + lab_recorded", if (input$ex5_group) " + group", "",
    ", family = binomial, data = train)\n",
    "test$pred <- predict(m, test, type = \"response\")\n",
    "# AUC and calibration within each group\n",
    "pROC::auc(test$readmit[test$group == \"B\"], test$pred[test$group == \"B\"])")))
}

shinyApp(ui, server)
