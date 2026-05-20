## ----eval = TRUE--------------------------------------------------------------
library(mlumr)
set.seed(2026)

# Example: Trial A data (index treatment)
trial_a <- data.frame(
  patient_id = 1:500,
  treatment = "Drug_A",
  response = rbinom(500, 1, 0.6),
  age_group = rbinom(500, 1, 0.4),   # 0 = young, 1 = old
  sex = rbinom(500, 1, 0.55)          # 0 = male, 1 = female
)

ipd <- set_ipd(
  data = trial_a,
  treatment = "treatment",
  outcome = "response",
  covariates = c("age_group", "sex")
)

ipd

## ----eval = TRUE--------------------------------------------------------------
trial_a_normal <- data.frame(
  patient_id = 1:500,
  treatment = "Drug_A",
  score = rnorm(500, mean = 3.0, sd = 1.2),
  age_group = rbinom(500, 1, 0.4),
  sex = rbinom(500, 1, 0.55)
)

ipd_normal <- set_ipd(
  data = trial_a_normal,
  treatment = "treatment",
  outcome = "score",
  covariates = c("age_group", "sex"),
  family = "normal"
)

## ----eval = TRUE--------------------------------------------------------------
trial_a_poisson <- data.frame(
  patient_id = 1:500,
  treatment = "Drug_A",
  events = rpois(500, lambda = 0.8),
  person_years = runif(500, 0.5, 2.0),
  age_group = rbinom(500, 1, 0.4),
  sex = rbinom(500, 1, 0.55)
)

ipd_poisson <- set_ipd(
  data = trial_a_poisson,
  treatment = "treatment",
  outcome = "events",
  covariates = c("age_group", "sex"),
  family = "poisson",
  exposure = "person_years"
)

## ----eval = TRUE--------------------------------------------------------------
# Example: Trial B data (comparator treatment)
trial_b <- data.frame(
  study = "Trial_B",
  treatment = "Drug_B",
  n_total = 400,
  n_events = 160,
  age_group_mean = 0.35,   # proportion in "old" group
  sex_prop = 0.50           # proportion female
)

agd <- set_agd(
  data = trial_b,
  treatment = "treatment",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = c("age_group_mean", "sex_prop"),
  cov_types = c("binary", "binary")
)

## ----eval = TRUE--------------------------------------------------------------
agd_normal <- set_agd(
  data = data.frame(
    trt = "Drug_B", y_mean = 3.2, se = 0.15, n = 400,
    age_group_mean = 0.35, sex_prop = 0.50
  ),
  treatment = "trt",
  family = "normal",
  outcome_mean = "y_mean",
  outcome_se = "se",
  outcome_n = "n",
  cov_means = c("age_group_mean", "sex_prop"),
  cov_types = c("binary", "binary")
)

## ----eval = TRUE--------------------------------------------------------------
agd_poisson <- set_agd(
  data = data.frame(
    trt = "Drug_B", n_events = 120, person_years = 800,
    age_group_mean = 0.35, sex_prop = 0.50
  ),
  treatment = "trt",
  family = "poisson",
  outcome_r = "n_events",
  outcome_E = "person_years",
  cov_means = c("age_group_mean", "sex_prop"),
  cov_types = c("binary", "binary")
)

## ----eval = TRUE--------------------------------------------------------------
agd_continuous <- set_agd(
  data = data.frame(
    trt = "B", n_total = 400, n_events = 160,
    bmi_mean = 25.3, bmi_sd = 4.2
  ),
  treatment = "trt",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = "bmi_mean",
  cov_sds = "bmi_sd",
  cov_types = "continuous"
)

## ----eval = TRUE--------------------------------------------------------------
dat <- combine_data(ipd, agd)
print(dat)

## ----eval = TRUE--------------------------------------------------------------
dat <- add_integration(
  dat,
  n_int = 64,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)

## ----eval = TRUE--------------------------------------------------------------
# Default: Spearman correlation from IPD
dat <- add_integration(dat, n_int = 64,
                       age_group = distr(qbern, prob = age_group_mean),
                       sex = distr(qbern, prob = sex_mean))

# Supply your own correlation matrix
my_cor <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
dat <- add_integration(dat, n_int = 64, cor = my_cor,
                       age_group = distr(qbern, prob = age_group_mean),
                       sex = distr(qbern, prob = sex_mean))

# No correlation adjustment
dat <- add_integration(dat, n_int = 64, cor_adjust = "none",
                       age_group = distr(qbern, prob = age_group_mean),
                       sex = distr(qbern, prob = sex_mean))

## ----eval = TRUE--------------------------------------------------------------
# Expand to long format
int_df <- unnest_integration(dat)
head(int_df)

## ----eval = TRUE--------------------------------------------------------------
# These work without add_integration()
dat_no_int <- combine_data(ipd, agd)
naive_result <- naive(dat_no_int)
stc_result <- stc(dat_no_int)

