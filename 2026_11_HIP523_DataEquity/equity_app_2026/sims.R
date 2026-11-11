# Simulated EHR-style data for the HIP 523 equity app.
# Every dataset is made up. The "true" effects are set by the sliders,
# so we know what the right answer is and can compare models against it.

# ---- Example 1: post-attribute bias -----------------------------------------
# ED visits for a long-bone fracture. Does the patient get opioid analgesia?
# Triage acuity (ESI) is assigned by a nurse, and can itself be biased.
simulate_ed <- function(n = 1500, triage_bias = 1.5, direct_bias = 0, seed = 1) {
  set.seed(seed)
  race <- factor(sample(c("White", "Black"), n, replace = TRUE, prob = c(0.7, 0.3)),
                 levels = c("White", "Black"))
  # true pain severity has the same distribution in both groups
  pain_true <- rnorm(n)
  # bias at triage: Black patients are less likely to be assigned high acuity
  high_acuity <- rbinom(n, 1, plogis(-0.3 + 1.2 * pain_true - triage_bias * (race == "Black")))
  # analgesia depends on acuity, plus any direct bias in prescribing
  analgesia <- rbinom(n, 1, plogis(-0.5 + 1.5 * high_acuity - direct_bias * (race == "Black")))
  tibble::tibble(
    race,
    triage_acuity = factor(ifelse(high_acuity == 1, "High (ESI 1-2)", "Lower (ESI 3-5)"),
                           levels = c("Lower (ESI 3-5)", "High (ESI 1-2)")),
    analgesia
  )
}

# ---- Example 2: omitted variable bias (confounding) -------------------------
# Adults with diabetes in an EHR cohort. Were they prescribed a statin?
# Medicaid patients are younger on average, and statin use rises with age.
simulate_statin <- function(n = 1500, age_gap = 10, insurance_effect = -0.4, seed = 2) {
  set.seed(seed)
  insurance <- factor(sample(c("Private", "Medicaid"), n, replace = TRUE, prob = c(0.6, 0.4)),
                      levels = c("Private", "Medicaid"))
  age <- round(pmin(pmax(rnorm(n, 58 - age_gap * (insurance == "Medicaid"), 9), 21), 85))
  statin <- rbinom(n, 1, plogis(-0.2 + 0.08 * (age - 55) + insurance_effect * (insurance == "Medicaid")))
  tibble::tibble(insurance, age, statin)
}

# ---- Example 3: disparity vs. mechanism -------------------------------------
# Heart failure hospitalizations. Readmitted within 30 days?
# Patients with limited English proficiency (LEP) are less likely to make it to
# a follow-up visit within 7 days (discharge communication is one reason).
simulate_hf <- function(n = 1500, followup_gap = 2, direct_effect = 0, seed = 3) {
  set.seed(seed)
  lep_status <- factor(sample(c("English proficient", "LEP"), n, replace = TRUE, prob = c(0.75, 0.25)),
                       levels = c("English proficient", "LEP"))
  followup_7d <- rbinom(n, 1, plogis(0.6 - followup_gap * (lep_status == "LEP")))
  readmit_30d <- rbinom(n, 1, plogis(-0.9 - 0.9 * followup_7d + direct_effect * (lep_status == "LEP")))
  tibble::tibble(
    lep_status,
    followup_7d = factor(ifelse(followup_7d == 1, "Yes", "No"), levels = c("No", "Yes")),
    readmit_30d
  )
}

# ---- Example 4: effect modification -----------------------------------------
# Remote blood pressure monitoring program. Change in systolic BP (mmHg) at 6 months.
# Does the program work as well in high-deprivation neighborhoods (high ADI)?
simulate_rpm <- function(n_per_group = 150, effect_low_adi = -8, effect_high_adi = -2, seed = 4) {
  set.seed(seed)
  d <- expand.grid(adi = c("Low ADI", "High ADI"), program = c("Usual care", "Remote monitoring"),
                   rep = seq_len(n_per_group))
  d$adi <- factor(d$adi, levels = c("Low ADI", "High ADI"))
  d$program <- factor(d$program, levels = c("Usual care", "Remote monitoring"))
  effect <- ifelse(d$program == "Remote monitoring",
                   ifelse(d$adi == "Low ADI", effect_low_adi, effect_high_adi), 0)
  d$sbp_change <- rnorm(nrow(d), mean = -2 + effect, sd = 12)
  tibble::as_tibble(d[, c("adi", "program", "sbp_change")])
}

# ---- Example 5: prediction model performance by group -----------------------
# A 30-day readmission risk model trained on EHR data.
# Group B can be underrepresented in training, have a different baseline risk,
# and have a noisier lab measurement (think pulse oximetry).
simulate_risk <- function(n = 3000, pct_b = 0.1, baseline_diff = 0.8, noise_b = 0.2, seed = 5) {
  set.seed(seed)
  group <- factor(ifelse(runif(n) < pct_b, "B", "A"), levels = c("A", "B"))
  prior_admits <- rpois(n, 1)
  lab_true <- rnorm(n)
  lab_recorded <- lab_true + rnorm(n, 0, ifelse(group == "B", noise_b, 0.2))
  readmit <- rbinom(n, 1, plogis(-1.8 + 0.5 * prior_admits + 1.0 * lab_true + baseline_diff * (group == "B")))
  tibble::tibble(group, prior_admits, lab_recorded, readmit)
}
