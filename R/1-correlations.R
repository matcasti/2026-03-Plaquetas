
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(correlation)

## Load data
data("platelets_d")

# -------------------------------------------------------------------------

correlation::correlation(data = platelets_d[, .SD[1], id],
                         method = "spearman") |>
  subset(p < 0.05)

#> Parameter1                  |                  Parameter2 |   rho |         95% CI |        S |         p
#> ---------------------------------------------------------------------------------------------------------
#> cd41_negative_cd62_negative | cd41_negative_cd62_positive |  0.71 | [ 0.58,  0.80] | 31013.17 | < .001***
#> cd41_negative_cd62_negative | cd41_positive_cd62_negative | -0.60 | [-0.72, -0.44] | 1.69e+05 | < .001***
#> cd41_negative_cd62_positive | cd41_positive_cd62_negative | -0.52 | [-0.66, -0.34] | 1.61e+05 | < .001***
#> cd41_positive_cd62_negative | cd41_positive_cd62_positive | -0.77 | [-0.84, -0.66] | 1.87e+05 | < .001***
#> bp_systolic                 |                bp_diastolic |  0.50 | [ 0.35,  0.62] | 2.01e+05 | < .001***
#> bp_systolic                 |                      bp_pam |  0.80 | [ 0.73,  0.86] | 79133.36 | < .001***
#> bp_systolic                 |                       bp_pp |  0.72 | [ 0.63,  0.80] | 1.11e+05 | < .001***
#> bp_diastolic                |                      bp_pam |  0.84 | [ 0.78,  0.88] | 64374.25 | < .001***
#> bp_diastolic                |                          fc |  0.36 | [ 0.19,  0.50] | 2.58e+05 | 0.009**
#> bp_diastolic                |             muscle_left_arm |  0.34 | [ 0.18,  0.49] | 2.58e+05 | 0.022*
#> weight                      |                         imc |  0.77 | [ 0.69,  0.83] | 90817.85 | < .001***
#> weight                      |             waist_hip_index |  0.42 | [ 0.26,  0.56] | 1.75e+05 | < .001***
#> weight                      |                   fat_total |  0.33 | [ 0.16,  0.48] | 2.58e+05 | 0.048*
#> weight                      |                muscle_total |  0.72 | [ 0.63,  0.80] | 1.06e+05 | < .001***
#> weight                      |             muscle_left_arm |  0.79 | [ 0.72,  0.85] | 79718.43 | < .001***
#> weight                      |            muscle_right_arm |  0.77 | [ 0.68,  0.83] | 89648.65 | < .001***
#> weight                      |            muscle_right_leg |  0.76 | [ 0.67,  0.82] | 93196.61 | < .001***
#> weight                      |             muscle_left_leg |  0.76 | [ 0.67,  0.82] | 93130.47 | < .001***
#> weight                      |                muscle_torso |  0.57 | [ 0.43,  0.67] | 1.67e+05 | < .001***
#> weight                      |                   bone_mass |  0.74 | [ 0.65,  0.81] | 1.00e+05 | < .001***
#> weight                      |          muscle_apendicular |  0.75 | [ 0.66,  0.82] | 95203.33 | < .001***
#> weight                      |      muscle_squeletal_index |  0.76 | [ 0.67,  0.82] | 92422.43 | < .001***
#> height                      |                   fat_total | -0.54 | [-0.65, -0.40] | 5.90e+05 | < .001***
#> height                      |                muscle_total |  0.67 | [ 0.57,  0.76] | 1.25e+05 | < .001***
#> height                      |             muscle_left_arm |  0.65 | [ 0.53,  0.74] | 1.38e+05 | < .001***
#> height                      |            muscle_right_arm |  0.67 | [ 0.56,  0.76] | 1.26e+05 | < .001***
#> height                      |            muscle_right_leg |  0.60 | [ 0.48,  0.70] | 1.52e+05 | < .001***
#> height                      |             muscle_left_leg |  0.59 | [ 0.47,  0.70] | 1.56e+05 | < .001***
#> height                      |                muscle_torso |  0.71 | [ 0.61,  0.79] | 1.11e+05 | < .001***
#> height                      |                  body_water |  0.53 | [ 0.39,  0.65] | 1.79e+05 | < .001***
#> height                      |                   bone_mass |  0.70 | [ 0.60,  0.78] | 1.15e+05 | < .001***
#> height                      |       dinamometer_left_hand |  0.60 | [ 0.47,  0.71] | 1.26e+05 | < .001***
#> height                      |      dinamometer_right_hand |  0.58 | [ 0.45,  0.69] | 1.33e+05 | < .001***
#> height                      |                     sft_fut | -0.35 | [-0.49, -0.18] | 4.93e+05 | 0.021*
#> height                      |          muscle_apendicular |  0.62 | [ 0.50,  0.72] | 1.47e+05 | < .001***
#> imc                         |                   fat_total |  0.69 | [ 0.58,  0.77] | 1.19e+05 | < .001***
#> imc                         |             muscle_left_arm |  0.34 | [ 0.18,  0.49] | 2.51e+05 | 0.019*
#> imc                         |            muscle_right_leg |  0.34 | [ 0.18,  0.49] | 2.52e+05 | 0.022*
#> imc                         |             muscle_left_leg |  0.35 | [ 0.19,  0.50] | 2.49e+05 | 0.014*
#> imc                         |                  body_water | -0.61 | [-0.71, -0.48] | 6.17e+05 | < .001***
#> imc                         |      muscle_squeletal_index |  0.62 | [ 0.50,  0.72] | 1.46e+05 | < .001***
#> waist_hip_index             |                muscle_total |  0.49 | [ 0.34,  0.62] | 1.50e+05 | < .001***
#> waist_hip_index             |             muscle_left_arm |  0.50 | [ 0.35,  0.63] | 1.50e+05 | < .001***
#> waist_hip_index             |            muscle_right_arm |  0.56 | [ 0.42,  0.67] | 1.30e+05 | < .001***
#> waist_hip_index             |            muscle_right_leg |  0.50 | [ 0.35,  0.63] | 1.47e+05 | < .001***
#> waist_hip_index             |             muscle_left_leg |  0.49 | [ 0.34,  0.62] | 1.51e+05 | < .001***
#> waist_hip_index             |                muscle_torso |  0.47 | [ 0.32,  0.61] | 1.55e+05 | < .001***
#> waist_hip_index             |                   bone_mass |  0.51 | [ 0.36,  0.64] | 1.43e+05 | < .001***
#> waist_hip_index             |          muscle_apendicular |  0.51 | [ 0.36,  0.63] | 1.44e+05 | < .001***
#> waist_hip_index             |      muscle_squeletal_index |  0.49 | [ 0.34,  0.62] | 1.50e+05 | < .001***
#> fat_total                   |                muscle_torso | -0.44 | [-0.57, -0.28] | 5.50e+05 | < .001***
#> fat_total                   |                  body_water | -0.98 | [-0.98, -0.97] | 7.58e+05 | < .001***
#> fat_total                   |                   bone_mass | -0.35 | [-0.50, -0.19] | 5.18e+05 | 0.015*
#> fat_total                   |       dinamometer_left_hand | -0.53 | [-0.65, -0.38] | 4.74e+05 | < .001***
#> fat_total                   |      dinamometer_right_hand | -0.50 | [-0.63, -0.35] | 4.66e+05 | < .001***
#> fat_total                   |                     sft_fut |  0.44 | [ 0.28,  0.57] | 2.02e+05 | < .001***
#> muscle_total                |             muscle_left_arm |  0.91 | [ 0.88,  0.94] | 34186.76 | < .001***
#> muscle_total                |            muscle_right_arm |  0.94 | [ 0.92,  0.96] | 21303.86 | < .001***
#> muscle_total                |            muscle_right_leg |  0.96 | [ 0.94,  0.97] | 15370.21 | < .001***
#> muscle_total                |             muscle_left_leg |  0.91 | [ 0.88,  0.94] | 32702.02 | < .001***
#> muscle_total                |                muscle_torso |  0.89 | [ 0.84,  0.92] | 43528.04 | < .001***
#> muscle_total                |                  body_water |  0.38 | [ 0.22,  0.52] | 2.37e+05 | 0.003**
#> muscle_total                |                   bone_mass |  0.97 | [ 0.95,  0.98] | 13050.70 | < .001***
#> muscle_total                |       dinamometer_left_hand |  0.62 | [ 0.49,  0.72] | 1.18e+05 | < .001***
#> muscle_total                |      dinamometer_right_hand |  0.61 | [ 0.48,  0.71] | 1.20e+05 | < .001***
#> muscle_total                |          muscle_apendicular |  0.93 | [ 0.91,  0.95] | 24967.13 | < .001***
#> muscle_total                |      muscle_squeletal_index |  0.76 | [ 0.67,  0.83] | 92213.36 | < .001***
#> muscle_left_arm             |            muscle_right_arm |  0.96 | [ 0.94,  0.97] | 15799.29 | < .001***
#> muscle_left_arm             |            muscle_right_leg |  0.90 | [ 0.86,  0.93] | 37674.79 | < .001***
#> muscle_left_arm             |             muscle_left_leg |  0.91 | [ 0.87,  0.93] | 35715.07 | < .001***
#> muscle_left_arm             |                muscle_torso |  0.82 | [ 0.75,  0.87] | 69999.70 | < .001***
#> muscle_left_arm             |                   bone_mass |  0.96 | [ 0.94,  0.97] | 15798.11 | < .001***
#> muscle_left_arm             |       dinamometer_left_hand |  0.63 | [ 0.50,  0.73] | 1.15e+05 | < .001***
#> muscle_left_arm             |      dinamometer_right_hand |  0.60 | [ 0.47,  0.71] | 1.24e+05 | < .001***
#> muscle_left_arm             |          muscle_apendicular |  0.92 | [ 0.89,  0.94] | 30254.02 | < .001***
#> muscle_left_arm             |      muscle_squeletal_index |  0.76 | [ 0.68,  0.83] | 90407.02 | < .001***
#> muscle_right_arm            |            muscle_right_leg |  0.94 | [ 0.91,  0.96] | 24070.19 | < .001***
#> muscle_right_arm            |             muscle_left_leg |  0.92 | [ 0.89,  0.95] | 29455.73 | < .001***
#> muscle_right_arm            |                muscle_torso |  0.86 | [ 0.80,  0.90] | 54031.80 | < .001***
#> muscle_right_arm            |                  body_water |  0.35 | [ 0.19,  0.50] | 2.49e+05 | 0.015*
#> muscle_right_arm            |                   bone_mass |  0.96 | [ 0.95,  0.98] | 13594.29 | < .001***
#> muscle_right_arm            |       dinamometer_left_hand |  0.63 | [ 0.50,  0.73] | 1.16e+05 | < .001***
#> muscle_right_arm            |      dinamometer_right_hand |  0.62 | [ 0.49,  0.72] | 1.19e+05 | < .001***
#> muscle_right_arm            |          muscle_apendicular |  0.94 | [ 0.92,  0.96] | 21116.16 | < .001***
#> muscle_right_arm            |      muscle_squeletal_index |  0.78 | [ 0.70,  0.84] | 84835.21 | < .001***
#> muscle_right_leg            |             muscle_left_leg |  0.94 | [ 0.92,  0.96] | 22330.45 | < .001***
#> muscle_right_leg            |                muscle_torso |  0.84 | [ 0.78,  0.88] | 61891.62 | < .001***
#> muscle_right_leg            |                  body_water |  0.34 | [ 0.17,  0.49] | 2.54e+05 | 0.027*
#> muscle_right_leg            |                   bone_mass |  0.95 | [ 0.94,  0.97] | 17275.17 | < .001***
#> muscle_right_leg            |       dinamometer_left_hand |  0.59 | [ 0.46,  0.70] | 1.26e+05 | < .001***
#> muscle_right_leg            |      dinamometer_right_hand |  0.59 | [ 0.45,  0.70] | 1.28e+05 | < .001***
#> muscle_right_leg            |          muscle_apendicular |  0.95 | [ 0.93,  0.97] | 17667.66 | < .001***
#> muscle_right_leg            |      muscle_squeletal_index |  0.84 | [ 0.78,  0.88] | 62027.24 | < .001***
#> muscle_left_leg             |                muscle_torso |  0.85 | [ 0.79,  0.89] | 56940.46 | < .001***
#> muscle_left_leg             |                   bone_mass |  0.94 | [ 0.91,  0.96] | 24347.41 | < .001***
#> muscle_left_leg             |       dinamometer_left_hand |  0.59 | [ 0.46,  0.70] | 1.28e+05 | < .001***
#> muscle_left_leg             |      dinamometer_right_hand |  0.60 | [ 0.47,  0.70] | 1.25e+05 | < .001***
#> muscle_left_leg             |          muscle_apendicular |  0.99 | [ 0.99,  0.99] |  3327.35 | < .001***
#> muscle_left_leg             |      muscle_squeletal_index |  0.88 | [ 0.83,  0.91] | 45980.53 | < .001***
#> muscle_torso                |                  body_water |  0.46 | [ 0.31,  0.59] | 2.05e+05 | < .001***
#> muscle_torso                |                   bone_mass |  0.89 | [ 0.85,  0.92] | 40447.06 | < .001***
#> muscle_torso                |       dinamometer_left_hand |  0.66 | [ 0.54,  0.75] | 1.06e+05 | < .001***
#> muscle_torso                |      dinamometer_right_hand |  0.68 | [ 0.57,  0.77] | 99387.72 | < .001***
#> muscle_torso                |          muscle_apendicular |  0.88 | [ 0.83,  0.92] | 45520.03 | < .001***
#> muscle_torso                |      muscle_squeletal_index |  0.67 | [ 0.56,  0.76] | 1.27e+05 | < .001***
#> body_water                  |                   bone_mass |  0.41 | [ 0.25,  0.54] | 2.27e+05 | < .001***
#> body_water                  |       dinamometer_left_hand |  0.56 | [ 0.42,  0.67] | 1.37e+05 | < .001***
#> body_water                  |      dinamometer_right_hand |  0.54 | [ 0.40,  0.66] | 1.42e+05 | < .001***
#> body_water                  |                     sft_cst |  0.35 | [ 0.18,  0.49] | 2.40e+05 | 0.021*
#> body_water                  |                     sft_fut | -0.45 | [-0.58, -0.29] | 5.18e+05 | < .001***
#> body_water                  |          muscle_apendicular |  0.33 | [ 0.16,  0.48] | 2.56e+05 | 0.039*
#> bone_mass                   |       dinamometer_left_hand |  0.67 | [ 0.55,  0.76] | 1.03e+05 | < .001***
#> bone_mass                   |      dinamometer_right_hand |  0.66 | [ 0.54,  0.75] | 1.06e+05 | < .001***
#> bone_mass                   |          muscle_apendicular |  0.95 | [ 0.93,  0.96] | 20249.28 | < .001***
#> bone_mass                   |      muscle_squeletal_index |  0.77 | [ 0.68,  0.83] | 89559.41 | < .001***
#> dinamometer_left_hand       |      dinamometer_right_hand |  0.87 | [ 0.81,  0.91] | 42293.16 | < .001***
#> dinamometer_left_hand       |           sft_elbow_flexion |  0.37 | [ 0.20,  0.52] | 1.91e+05 | 0.010*
#> dinamometer_left_hand       |                     sft_fut | -0.52 | [-0.64, -0.37] | 4.59e+05 | < .001***
#> dinamometer_left_hand       |          muscle_apendicular |  0.61 | [ 0.48,  0.71] | 1.21e+05 | < .001***
#> dinamometer_left_hand       |      muscle_squeletal_index |  0.40 | [ 0.24,  0.55] | 1.85e+05 | 0.001**
#> dinamometer_right_hand      |           sft_elbow_flexion |  0.35 | [ 0.17,  0.50] | 1.98e+05 | 0.036*
#> dinamometer_right_hand      |                     sft_fut | -0.47 | [-0.60, -0.31] | 4.43e+05 | < .001***
#> dinamometer_right_hand      |          muscle_apendicular |  0.61 | [ 0.48,  0.71] | 1.22e+05 | < .001***
#> dinamometer_right_hand      |      muscle_squeletal_index |  0.42 | [ 0.26,  0.56] | 1.80e+05 | < .001***
#> frail_total                 |                     sft_fut |  0.36 | [ 0.19,  0.50] | 2.35e+05 | 0.011*
#> sft_cst                     |           sft_elbow_flexion |  0.67 | [ 0.55,  0.75] | 1.23e+05 | < .001***
#> sft_cst                     |                     sft_fut | -0.57 | [-0.68, -0.43] | 5.74e+05 | < .001***
#> sft_elbow_flexion           |                     sft_fut | -0.43 | [-0.56, -0.27] | 5.11e+05 | < .001***
#> muscle_apendicular          |      muscle_squeletal_index |  0.87 | [ 0.82,  0.91] | 49902.30 | < .001***
