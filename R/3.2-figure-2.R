
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(brms)
library(tidybayes)
library(ggplot2)

## Load custom functions
source("R/_functions.R")

## Load data
data("platelets_d")

## Load models
frail_fit <- readRDS("models/frail_model_adj.rds")

# Frailty and plaatelets --------------------------------------------------

epred_data <- tidybayes::add_epred_draws(
  expand.grid(time = c("t-1","t-3"), sex = NA, age_std = 0, imc_std = 0,
              cd41_positive_cd62_negative_std = 0, cd41_positive_cd62_positive_std = 0),
  frail_fit,
  allow_new_levels = TRUE
) |> as.data.table()

epred_data <- epred_data[.category != "frailtotal"] |> droplevels()

epred_data[, `:=`(sex = NULL, age_std = NULL, imc_std = NULL,
                  .chain = NULL, .iteration = NULL,
                  cd41_positive_cd62_positive_std = NULL,
                  cd41_positive_cd62_negative_std = NULL)][]

epred_data[, time := `levels<-`(time, c("Baseline", "1-Year"))][]
epred_data[, .category := `levels<-`(.category, c("CD41+CD62-", "CD41+CD62+"))][]


fig2 <- ggplot(epred_data, aes(time, (.epred), group = .category)) +
  facet_grid(cols = vars(.category)) +
  stat_summary(geom = "line", fun = median, aes(color = .category), linewidth = 1, show.legend = FALSE) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = .category), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.95)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = .category), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.9)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = .category), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.8)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = .category), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.5)) +
  theme_classic() +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_color_brewer(aesthetics = c("color", "fill"), type = "qual", palette = 6, direction = -1) +
  labs(x = "Time",
       y = "Platelet proportion\n(Standardized units)")

ggsave(filename = "manuscript/fig-2.png", width = 400, height = 300, units = "px",
       plot = fig2, scale = 7, dpi = 500, device = "png")
