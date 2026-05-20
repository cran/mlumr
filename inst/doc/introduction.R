## ----eval = TRUE--------------------------------------------------------------
library(mlumr)
set.seed(2026)

# Toy IPD: trial A (index treatment, binary outcome)
n_a <- 300
trial_a_data <- data.frame(
  trt      = "Drug_A",
  response = rbinom(n_a, 1, 0.55),
  age_cat  = rbinom(n_a, 1, 0.40),
  sex      = rbinom(n_a, 1, 0.55)
)

# Toy AgD: trial B (comparator treatment)
trial_b_data <- data.frame(
  trt           = "Drug_B",
  n_total       = 400,
  n_events      = 160,
  age_cat_mean  = 0.35,
  sex_mean      = 0.50
)

## ----eval = TRUE--------------------------------------------------------------
# 1. Prepare IPD
ipd <- set_ipd(
  data = trial_a_data,
  treatment = "trt",
  outcome = "response",
  covariates = c("age_cat", "sex")
)

# 2. Prepare AgD
agd <- set_agd(
  data = trial_b_data,
  treatment = "trt",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = c("age_cat_mean", "sex_mean"),
  cov_types = c("binary", "binary")
)

# 3. Combine
dat <- combine_data(ipd, agd)

# 4. Add integration points (needed for ML-UMR)
dat <- add_integration(
  dat,
  n_int = 64,
  age_cat = distr(qbern, prob = age_cat_mean),
  sex = distr(qbern, prob = sex_mean)
)

