
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

epred_data <- tidybayes::add_epred_draws(
  data.table(time = rep(NA, 150),
             sex = rep(NA, 150),
             age_std = seq(-3, 3.5, length.out = 150),
             imc_std = rep(0, 150),
            cd41_positive_cd62_negative_std = rev(seq(-3,3.5, length.out = 150)),
            cd41_positive_cd62_positive_std = rev(seq(-3,3.5, length.out = 150))),
  frail_fit,
  allow_new_levels = TRUE
) |> as.data.table()

epred_data <- epred_data[.category == "frailtotal"]

epred_data[, `:=`(time = NULL, sex = NULL, imc_std = NULL,
                  .category = NULL, .chain = NULL, .iteration = NULL)][]

epred_data <- epred_data[
  j = median_qi(.epred, .width = c(0.5,0.8,0.9,0.95)),
  by = list(age_std)
]

epred_data <- dcast(epred_data, age_std + y ~ .width, value.var = c("ymin", "ymax"))

epred_data[, grp := rleid(age_std > -2.377463 & age_std < 3.068054)][]
epred_data[, group := ifelse(grp == 2, "Data", "Extrapolation")]

epred_data[, age := age_std * 5.876393 + 70.97091]

fig3a <- ggplot(epred_data, aes(age, y, group = grp)) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.5, ymax = ymax_0.5), alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.8, ymax = ymax_0.8), alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.9, ymax = ymax_0.9), alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.95, ymax = ymax_0.95), alpha = 0.2, show.legend = FALSE) +
  geom_line(aes(color = group), linewidth = 1) +
  theme_classic() +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_brewer(aesthetics = c("fill", "color"), direction = -1, type = "qual", palette = 6) +
  labs(x = NULL,
       y = "Frailty score", color = NULL)


# Physical function and platelets -----------------------------------------

epred_data <- tidybayes::add_epred_draws(
  data.table(time = rep(NA, 150),
             sex = rep(NA, 150),
             age_std = seq(-3, 3.5, length.out = 150),
             imc_std = rep(0, 150),
             cd41_positive_cd62_negative_std = rev(seq(-3,3.5, length.out = 150)),
             cd41_positive_cd62_positive_std = rev(seq(-3,3.5, length.out = 150))),
  function_fit,
  allow_new_levels = TRUE
) |> as.data.table()

epred_data <- epred_data[!.category %like% "^cd41"] |> droplevels()

epred_data[, .category := `levels<-`(.category, c("Dinamometer (kg)", "Chair Sit-to-Stand (reps)", "Elbow flexion (reps)", "TUG (seconds)"))]

epred_data[, `:=`(time = NULL, sex = NULL, imc_std = NULL,
                  .chain = NULL, .iteration = NULL)][]

epred_data <- epred_data[
  j = median_qi(.epred, .width = c(0.5,0.8,0.9,0.95)),
  by = list(.category, age_std)
]

epred_data <- dcast(epred_data, .category + age_std + y ~ .width, value.var = c("ymin", "ymax"))

epred_data[, grp := rleid(age_std > -2.377463 & age_std < 3.068054), .category][]
epred_data[, group := ifelse(grp == 2, "Data", "Extrapolation")]

epred_data[, age := age_std * 5.876393 + 70.97091]


fig3b <- ggplot(epred_data, aes(age, y, group = grp)) +
  facet_grid(cols = vars(.category), scales = "free") +
  geom_ribbon(aes(fill = group, ymin = ymin_0.5, ymax = ymax_0.5), alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.8, ymax = ymax_0.8), alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.9, ymax = ymax_0.9), alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(fill = group, ymin = ymin_0.95, ymax = ymax_0.95), alpha = 0.2, show.legend = FALSE) +
  geom_line(aes(color = group), linewidth = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_brewer(aesthetics = c("fill", "color"), direction = -1, type = "qual", palette = 6) +
  labs(x = "Age (years)",
       y = "Standardized units", color = NULL)


# -------------------------------------------------------------------------

fig3 <- cowplot::plot_grid(fig3a, fig3b, align = "hv", axis = "lr", nrow = 2,
                   rel_heights = c(0.4,0.6), labels = "AUTO")

ggsave(filename = "manuscript/fig-3.png", width = 500, height = 450, units = "px",
       plot = fig3, scale = 7, dpi = 500, device = "png")
