
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(gtsummary)
library(skimr)

## Load data
data("platelets_d")

# -------------------------------------------------------------------------

tbl_variables <- c("sex", "age", "diabetes", "hypertension", "dyslipidemia",
                   "bp_systolic", "bp_diastolic", "bp_pam", "bp_pp",
                   "fc", "weight", "height", "imc", "waist_hip_index", "fat_total",
                   "muscle_total", "muscle_apendicular", "muscle_squeletal_index", "body_water", "bone_mass",
                   "dinamometer_average","dinamometer_left_hand", "dinamometer_right_hand", "frail_total",
                   "sft_cst", "sft_elbow_flexion", "sft_fut",
                   "cd41_negative_cd62_negative", "cd41_negative_cd62_positive",
                   "cd41_positive_cd62_negative", "cd41_positive_cd62_positive")

tbl_data <- platelets_d[, .SD[1], by = id][, .SD, .SDcols = tbl_variables]

skim(tbl_data)

tbl_data[, sex := `levels<-`(sex, c("Male", "Female"))]

# -------------------------------------------------------------------------

tbl_1 <- tbl_summary(tbl_data,
            by = sex,
            missing = "no",
            statistic = all_continuous() ~ "{mean} ± {sd}",
            digits = list(
              all_continuous() ~ 1,
              all_dichotomous() ~ 0
            ),
            label = list(
              age ~ "Age (years old)",
              diabetes ~ "Diabetes (n)",
              hypertension ~ "Hypertension (n)",
              dyslipidemia ~ "Dyslipidemia (n)",
              bp_systolic ~ "Systolic blood pressure (mmHg)",
              bp_diastolic ~ "Diastolic blood pressure (mmHg)",
              bp_pam ~ "Mean arterial pressure (mmHg)",
              bp_pp ~ "Pulse pressure (mmHg)",
              fc ~ "Heart rate (bpm)",
              weight ~ "Weight (kg)",
              height ~ "Height (cm)",
              imc ~ "BMI (km/m²)",
              waist_hip_index ~ "Waist to Hip ratio",
              fat_total ~ "Total fat mass (%)",
              muscle_total ~ "Total muscle mass (kg)",
              muscle_apendicular ~ "Apendicular muscle mass (kg)",
              muscle_squeletal_index ~ "Skeletal muscle index (km/m²)",
              body_water ~ "Body water (%)",
              bone_mass ~ "Bone mass (kg)",
              dinamometer_average ~ "Dinamometer - Average (kg)",
              dinamometer_left_hand ~ "Dinamometer - Left hand (kg)",
              dinamometer_right_hand ~ "Dinamometer - Right hand (kg)",
              frail_total ~ "Total FRAIL score",
              sft_cst ~ "Chair sit-to-stand (reps)",
              sft_elbow_flexion ~ "Elbow flexion test (reps)",
              sft_fut ~ "Timed-up and Go (seconds)",
              cd41_negative_cd62_negative ~ "CD41⁻CD62⁻ (%)",
              cd41_negative_cd62_positive ~ "CD41⁻CD62⁺ (%)",
              cd41_positive_cd62_negative ~ "CD41⁺CD62⁻ (%)",
              cd41_positive_cd62_positive ~ "CD41⁺CD62⁺ (%)"
            ),
            #type = list(
            #  diabetes ~ "categorical",
            #  hypertension ~ "categorical",
            #  dyslipidemia ~ "categorical"
            #)
          ) |>
  add_overall() |>
  add_difference(test = list(
    all_continuous() ~ "smd"
  ),
  include = !c(diabetes, hypertension, dyslipidemia)) |>
  remove_footnote_header() |>
  remove_abbreviation()

tbl_1

saveRDS(tbl_1, file = "output/tbl-1.rds")

