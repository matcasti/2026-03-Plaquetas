
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(brms)
library(parameters)

## Load custom functions
source("R/_functions.R")

## Load data
data("platelets_d")

# -------------------------------------------------------------------------


platelets_d[, cd41_positive_cd62_negative_std := scale(cd41_positive_cd62_negative)][]
platelets_d[, cd41_positive_cd62_positive_std := scale(cd41_positive_cd62_positive)][]
platelets_d[, age_std := scale(age)][]
platelets_d[, imc_std := scale(imc)][]
platelets_d[, muscle_total_std := scale(muscle_total)][]
platelets_d[, fat_total_std := scale(fat_total)][]
platelets_d[, dinamometer_average_std := scale(dinamometer_average)][]
platelets_d[, sft_cst_std := scale(sft_cst)][]
platelets_d[, sft_elbow_flexion_std := scale(sft_elbow_flexion)][]
platelets_d[, sft_fut_std := scale(sft_fut)][]

# -------------------------------------------------------------------------

frail_model_simple <-
  bf(frail_total | trials(4) ~
       time * (mi(cd41_positive_cd62_negative_std) + mi(cd41_positive_cd62_positive_std)) + (1 | id),
     family = binomial("logit")) +
  bf(cd41_positive_cd62_negative_std | mi() ~ time + (1 | id), family = gaussian()) +
  bf(cd41_positive_cd62_positive_std | mi() ~ time + (1 | id), family = gaussian())

cc_model_simple <-
  bf(mvbind(muscle_total_std, fat_total_std) ~
       time * (mi(cd41_positive_cd62_negative_std) + mi(cd41_positive_cd62_positive_std)) + (1 | id),
     family = gaussian()) + set_rescor(TRUE) +
  bf(cd41_positive_cd62_negative_std | mi() ~ time + (1 | id), family = gaussian()) +
  bf(cd41_positive_cd62_positive_std | mi() ~ time + (1 | id), family = gaussian())

function_model_simple <-
  bf(mvbind(dinamometer_average_std, sft_cst_std, sft_elbow_flexion_std, sft_fut_std) ~
       time * (mi(cd41_positive_cd62_negative_std) + mi(cd41_positive_cd62_positive_std)) + (1 | id),
     family = gaussian()) + set_rescor(TRUE) +
  bf(cd41_positive_cd62_negative_std | mi() ~ time + (1 | id), family = gaussian()) +
  bf(cd41_positive_cd62_positive_std | mi() ~ time + (1 | id), family = gaussian())

# -------------------------------------------------------------------------

frail_model_adj <-
  bf(frail_total | trials(4) ~
       time * (mi(cd41_positive_cd62_negative_std) + mi(cd41_positive_cd62_positive_std) +
                 sex + age_std + imc_std) + (1 | id),
     family = binomial("logit")) +
  bf(cd41_positive_cd62_negative_std | mi() ~ time + (1 | id), family = gaussian()) +
  bf(cd41_positive_cd62_positive_std | mi() ~ time + (1 | id), family = gaussian())

cc_model_adj <-
  bf(mvbind(muscle_total_std, fat_total_std) ~
       time * (mi(cd41_positive_cd62_negative_std) + mi(cd41_positive_cd62_positive_std) +
                 sex + age_std + imc_std) + (1 | id),
     family = gaussian()) + set_rescor(TRUE) +
  bf(cd41_positive_cd62_negative_std | mi() ~ time + (1 | id), family = gaussian()) +
  bf(cd41_positive_cd62_positive_std | mi() ~ time + (1 | id), family = gaussian())

function_model_adj <-
  bf(mvbind(dinamometer_average_std, sft_cst_std, sft_elbow_flexion_std, sft_fut_std) ~
       time * (mi(cd41_positive_cd62_negative_std) + mi(cd41_positive_cd62_positive_std) +
                 sex + age_std + imc_std) + (1 | id) ,
     family = gaussian()) + set_rescor(TRUE) +
  bf(cd41_positive_cd62_negative_std | mi() ~ time + (1 | id), family = gaussian()) +
  bf(cd41_positive_cd62_positive_std | mi() ~ time + (1 | id), family = gaussian())


# -------------------------------------------------------------------------

get_prior(frail_model_simple, data = platelets_d)
get_prior(cc_model_simple, data = platelets_d)

# -------------------------------------------------------------------------

frail_m_simple <- brm(
  formula = frail_model_simple,
  data = platelets_d,
  prior = c(
    set_prior("normal(0,3)", class = "b", resp = c("frailtotal", "cd41positivecd62negativestd", "cd41positivecd62positivestd")),
    set_prior("normal(1,3)", class = "sd", lb = 0, resp = c("frailtotal", "cd41positivecd62negativestd", "cd41positivecd62positivestd")),
    set_prior("normal(1,3)", class = "sigma", lb = 0, resp = c("cd41positivecd62negativestd", "cd41positivecd62positivestd"))
  ),
  chains = 4, cores = 4, iter = 5000, warmup = 2500, seed = 1234,
  file = "models/frail_model_simple.rds",
  control = list(adapt_delta = 0.999,
                 max_treedepth = 50)
)

rstan::check_hmc_diagnostics(frail_m_simple$fit)
rstan::stan_ess(frail_m_simple$fit)

frail_m_adj <- brm(
  formula = frail_model_adj,
  data = platelets_d,
  prior = c(
    set_prior("normal(0,3)", class = "b", resp = c("frailtotal", "cd41positivecd62negativestd", "cd41positivecd62positivestd")),
    set_prior("normal(1,3)", class = "sd", lb = 0, resp = c("frailtotal", "cd41positivecd62negativestd", "cd41positivecd62positivestd")),
    set_prior("normal(1,3)", class = "sigma", lb = 0, resp = c("cd41positivecd62negativestd", "cd41positivecd62positivestd"))
  ),
  chains = 4, cores = 4, iter = 5000, warmup = 2500, seed = 1234,
  file = "models/frail_model_adj.rds",
  control = list(adapt_delta = 0.999,
                 max_treedepth = 50)
)

rstan::check_hmc_diagnostics(frail_m_adj$fit)
rstan::stan_ess(frail_m_adj$fit)

# report_model(frail_m_adj)

summary_model(frail_m_adj, variable = "^b|^rescor") |>
  fwrite(file = "output/frail_estimates.csv")

# -------------------------------------------------------------------------

cc_par <- c("muscletotalstd","fattotalstd","cd41positivecd62positivestd","cd41positivecd62negativestd")

cc_m_simple <- brm(
  formula = cc_model_simple,
  data = platelets_d,
  prior = prior_for(cc_par),
  chains = 4, cores = 4, iter = 5000, warmup = 2500, seed = 1234,
  file = "models/cc_model_simple.rds",
  control = list(adapt_delta = 0.999,
                 max_treedepth = 50)
)

rstan::check_hmc_diagnostics(cc_m_simple$fit)
rstan::stan_ess(cc_m_simple$fit)

cc_m_adj <- brm(
  formula = cc_model_adj,
  data = platelets_d,
  prior = prior_for(cc_par),
  chains = 4, cores = 4, iter = 5000, warmup = 2500, seed = 1234,
  file = "models/cc_model_adj.rds",
  control = list(adapt_delta = 0.999,
                 max_treedepth = 50)
)

rstan::check_hmc_diagnostics(cc_m_adj$fit)
rstan::stan_ess(cc_m_adj$fit)

summary_model(cc_m_adj, variable = "^b|^rescor") |>
  fwrite(file = "output/cc_estimates.csv")

# -------------------------------------------------------------------------

function_par <- c("dinamometeraveragestd","sftcststd","sftelbowflexionstd","sftfutstd",
                  "cd41positivecd62positivestd","cd41positivecd62negativestd")

function_m_simple <- brm(
  formula = function_model_simple,
  data = platelets_d,
  prior = prior_for(function_par),
  chains = 4, cores = 4, iter = 5000, warmup = 2500, seed = 1234,
  file = "models/function_model_simple.rds",
  control = list(adapt_delta = 0.999,
                 max_treedepth = 50)
)

rstan::check_hmc_diagnostics(function_m_simple$fit)
rstan::stan_ess(function_m_simple$fit)

# report_model(function_m_simple)

function_m_adj <- brm(
  formula = function_model_adj,
  data = platelets_d,
  prior = prior_for(function_par),
  chains = 4, cores = 4, iter = 5000, warmup = 2500, seed = 1234,
  file = "models/function_model_adj.rds",
  control = list(adapt_delta = 0.999,
                 max_treedepth = 50)
)

rstan::check_hmc_diagnostics(function_m_adj$fit)
rstan::stan_ess(function_m_adj$fit)

summary_model(function_m_adj, variable = "^b|^rescor") |>
  fwrite(file = "output/function_estimates.csv")

# -------------------------------------------------------------------------
