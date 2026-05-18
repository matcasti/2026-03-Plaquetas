
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
function_fit <- readRDS("models/function_model_adj.rds")

# Frailty and plaatelets --------------------------------------------------

epred_62neg_data <- tidybayes::add_epred_draws(
  expand.grid(time = NA, sex = NA, age_std = 0, imc_std = 0,
              cd41_positive_cd62_negative_std = seq(-3, 3, 0.1), cd41_positive_cd62_positive_std = 0),
  frail_fit,
  allow_new_levels = TRUE
) |> as.data.table()
epred_62neg_data$cd41_positive_cd62_positive_std <- NA

epred_62pos_data <- tidybayes::add_epred_draws(
  expand.grid(time = NA, sex = NA, age_std = 0, imc_std = 0,
              cd41_positive_cd62_negative_std = 0, cd41_positive_cd62_positive_std = seq(-3, 3, 0.1)),
  frail_fit,
  allow_new_levels = TRUE
) |> as.data.table()
epred_62pos_data$cd41_positive_cd62_negative_std <- NA

epred_data <- rbind(epred_62neg_data, epred_62pos_data)

epred_data[!is.na(cd41_positive_cd62_negative_std), z_score := cd41_positive_cd62_negative_std]
epred_data[!is.na(cd41_positive_cd62_negative_std), plaqueta := factor("CD41+CD62-")]
epred_data[!is.na(cd41_positive_cd62_positive_std), z_score := cd41_positive_cd62_positive_std]
epred_data[!is.na(cd41_positive_cd62_positive_std), plaqueta := factor("CD41+CD62+")]

epred_data <- epred_data[.category == "frailtotal"]
epred_data[, `:=`(time = NULL, sex = NULL, age_std = NULL, imc_std = NULL,
                  .category = NULL, .chain = NULL, .iteration = NULL,
                  cd41_positive_cd62_positive_std = NULL,
                  cd41_positive_cd62_negative_std = NULL)][]

fig1a <- ggplot(epred_data, aes(z_score, .epred, group = plaqueta)) +
  facet_wrap(~plaqueta, nrow = 1) +
  stat_summary(geom = "line", fun = median, aes(color = plaqueta), linewidth = 1, show.legend = FALSE) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.95)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.9)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.8)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.5)) +
  theme_classic() +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_brewer(aesthetics = c("color", "fill"), type = "qual", palette = 6, direction = -1) +
  labs(x = NULL,
       y = "Frailty score")


# Physical function and platelets -----------------------------------------

epred_62neg_data <- tidybayes::add_epred_draws(
  expand.grid(time = NA, sex = NA, age_std = 0, imc_std = 0,
              cd41_positive_cd62_negative_std = seq(-3, 3, 0.1), cd41_positive_cd62_positive_std = 0),
  function_fit,
  allow_new_levels = TRUE
) |> as.data.table()
epred_62neg_data$cd41_positive_cd62_positive_std <- NA

epred_62pos_data <- tidybayes::add_epred_draws(
  expand.grid(time = NA, sex = NA, age_std = 0, imc_std = 0,
              cd41_positive_cd62_negative_std = 0, cd41_positive_cd62_positive_std = seq(-3, 3, 0.1)),
  function_fit,
  allow_new_levels = TRUE
) |> as.data.table()
epred_62pos_data$cd41_positive_cd62_negative_std <- NA

epred_data <- rbind(epred_62neg_data, epred_62pos_data)

epred_data[!is.na(cd41_positive_cd62_negative_std), z_score := cd41_positive_cd62_negative_std]
epred_data[!is.na(cd41_positive_cd62_negative_std), plaqueta := factor("CD41+CD62-")]
epred_data[!is.na(cd41_positive_cd62_positive_std), z_score := cd41_positive_cd62_positive_std]
epred_data[!is.na(cd41_positive_cd62_positive_std), plaqueta := factor("CD41+CD62+")]

epred_data <- epred_data[!.category %like% "^cd41"] |> droplevels()

epred_data[, `:=`(time = NULL, sex = NULL, age_std = NULL, imc_std = NULL,
                  .chain = NULL, .iteration = NULL,
                  cd41_positive_cd62_positive_std = NULL,
                  cd41_positive_cd62_negative_std = NULL)][]



epred_data[, .category := `levels<-`(.category, c("Dinamometer (kg)", "Chair Sit-to-Stand (reps)", "Elbow flexion (reps)", "TUG (seconds)"))]

fig1b <- ggplot(epred_data, aes(z_score, (.epred), group = plaqueta)) +
  facet_grid(cols = vars(.category), rows = vars(plaqueta), scales = "free") +
  stat_summary(geom = "line", fun = median, aes(color = plaqueta), linewidth = 1, show.legend = FALSE) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.95)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.9)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.8)) +
  stat_summary(geom = "ribbon", fun.data = median_qi, aes(fill = plaqueta), alpha = 0.2, show.legend = FALSE,
               fun.args = list(.width = 0.5)) +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_brewer(aesthetics = c("color", "fill"), type = "qual", palette = 6, direction = -1) +
  labs(x = "Platelet Proportion\n(Standardized units)",
       y = "Standardized units")


# -------------------------------------------------------------------------

fig1 <- cowplot::plot_grid(fig1a, fig1b, align = "hv", axis = "lr", nrow = 2,
                   rel_heights = c(0.3,0.8), labels = "AUTO")

ggsave(filename = "manuscript/fig-1.png", width = 500, height = 500, units = "px",
       plot = fig1, scale = 7, dpi = 500, device = "png")
