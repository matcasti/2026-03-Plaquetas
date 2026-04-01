
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)

## Load data and selected variables
data_raw <- fread(
  input = "data-raw/data-raw.csv",
  select = readLines("data-raw/selected_variables")
)

# -------------------------------------------------------------------------

data_raw[, id := rleid(record_id)]
data_raw[, record_id := NULL]

# -------------------------------------------------------------------------

platelet_raw <- melt(data_raw, id.vars = c("id","redcap_event_name"), measure.vars = patterns("^cd41_"))
platelet_raw[, variable := gsub("_fase_[1-4]", "", variable)][]
platelet_raw <- na.omit(platelet_raw)
platelet_wide <- dcast(platelet_raw, formula = id + redcap_event_name ~ variable, value.var = "value")

cols_to_remove <-
  data_raw[, cd41_negative_cd62_positive_fase_1:cd41_negative_cd62_negative_fase_4] |>
  names()

data_raw[, (cols_to_remove) := NULL][]

data_merged <- platelet_wide[data_raw, on = c("id", "redcap_event_name")]

data_merged <- data_merged[redcap_event_name != "initial_data_arm_1"]


# -------------------------------------------------------------------------

data_merged[, `:=`(
  redcap_survey_identifier = NULL,
  personal_code = NULL, drugs = NULL,
  first_name = NULL, last_name = NULL,
  email = NULL, phase_date = NULL,
  rut = NULL
)][]

# -------------------------------------------------------------------------

## Check which variables are defined as character
data_merged[, .SD, .SDcols = is.character]

data_merged[, sex := factor(sex, levels = c("M", "F"))][]

data_merged[, year := factor(redcap_event_name,
                     levels = c("fase_1_arm_1", "fase_2_arm_1", "fase_3_arm_1", "fase_4_arm_1"),
                     labels = c("Year_1", "Year_1", "Year_2", "Year_2"))][]

data_merged[, semester := factor(redcap_event_name,
                     levels = c("fase_1_arm_1", "fase_2_arm_1", "fase_3_arm_1", "fase_4_arm_1"),
                     labels = c("Semester_1", "Semester_2", "Semester_1", "Semester_2"))][]

data_merged[, time := factor(redcap_event_name,
                     levels = c("fase_1_arm_1", "fase_2_arm_1", "fase_3_arm_1", "fase_4_arm_1"),
                     labels = c("t-0", "t-1", "t-2", "t-3"), ordered = TRUE)][]

data_merged[, redcap_event_name := NULL][]


# -------------------------------------------------------------------------

data_merged[, `:=`(diabetes = factor("No"), hypertension = factor("No"), dyslipidemia = factor("No"))][]

data_merged[tolower(disease_physical) %like% "dm|insulin|diabet", diabetes := "Yes"][]
data_merged[tolower(disease_physical) %like% "hta|hiperten|arterial", hypertension := "Yes"][]
data_merged[tolower(disease_physical) %like% "hta|hiperten|arterial", dyslipidemia := "Yes"][]
data_merged[, disease_physical := NULL]

dm_ids <- data_merged[, (sum(diabetes == "Yes")/.N) > 0, id][V1 == TRUE, id]
ht_ids <- data_merged[, (sum(hypertension == "Yes")/.N) > 0, id][V1 == TRUE, id]
dl_ids <- data_merged[, (sum(dyslipidemia == "Yes")/.N) > 0, id][V1 == TRUE, id]

data_merged[id %in% dm_ids, diabetes := "Yes"][]
data_merged[id %in% ht_ids, hypertension := "Yes"][]
data_merged[id %in% dl_ids, dyslipidemia := "Yes"][]


# -------------------------------------------------------------------------

data_merged[, .SD, .SDcols = is.character]

data_merged[muscle_left_arm %like% "[0-9]\\,[0-9]|[0-9]\\/[0-9]", muscle_left_arm := gsub("\\,|\\/","\\.", muscle_left_arm)]
data_merged[muscle_left_arm %like% "\\.$", muscle_left_arm := gsub("\\.$","", muscle_left_arm)]
data_merged[muscle_left_arm == "", muscle_left_arm := NA_character_]
data_merged[, muscle_left_arm := as.numeric(muscle_left_arm)]
data_merged[muscle_left_arm > 10, muscle_left_arm := muscle_left_arm/10]
data_merged[muscle_right_arm > 10, muscle_right_arm := muscle_right_arm/10]
data_merged[, hist(muscle_left_arm)]
data_merged[, hist(muscle_right_arm)]

data_merged[, sft_fut := trimws(sft_fut)]
data_merged[sft_fut == "" | tolower(sft_fut) == "n/a", sft_fut := NA_character_]
data_merged[, sft_fut := gsub(" segundos", "", sft_fut)]
data_merged[, sft_fut := gsub("\\,|\\:","\\.",sft_fut) |> as.numeric()]
data_merged[sft_fut > 20, sft_fut := sft_fut/10]
data_merged[, hist(sft_fut)]

# -------------------------------------------------------------------------

data_merged[id %in% data_merged[is.na(sex), id], sex, id]
data_merged[id == 27, sex := "F"]
data_merged[id %in% c(91,104), sex := "M"]

# -------------------------------------------------------------------------

data_merged[, .N, list(year, semester)]
data_merged[, lapply(.SD, function(x) sum(!is.na(x))), list(year, semester)]

platelets_d <- data_merged[id %in% data_merged[!(year == "Year_1" & semester == "Semester_1"), id],
                           ][!(year == "Year_1" & semester == "Semester_1")]

# -------------------------------------------------------------------------

rm(dl_ids, ht_ids, dm_ids, data_merged, cols_to_remove, platelet_wide, platelet_raw, data_raw)

# -------------------------------------------------------------------------

platelets_d[height == 60, height := 160]
platelets_d[height < 100, height := height * 100]
platelets_d[, imc := weight / (height/100)^2]

platelets_d[body_water > 100, body_water := body_water/10]
platelets_d[id %in% platelets_d[body_water < 30, id]][body_water > 30, mean(body_water), id]

platelets_d[body_water < 30 & id == 24, body_water := 46.05]
platelets_d[body_water < 30 & id == 41, body_water := 39.95]
platelets_d[, hist(body_water)]

platelets_d[muscle_total > 100, muscle_total := muscle_total/100]
platelets_d[, hist(muscle_total)]

platelets_d[bone_mass > 5, bone_mass := bone_mass/10]
platelets_d[, hist(bone_mass)]


# -------------------------------------------------------------------------

platelets_d[, muscle_apendicular :=
              muscle_left_arm + muscle_right_arm +
              muscle_left_leg + muscle_right_leg]
platelets_d[, hist(muscle_apendicular)]

platelets_d[, muscle_squeletal_index := muscle_apendicular / (height/100)^2]
platelets_d[, hist(muscle_squeletal_index)]

platelets_d[, plot(muscle_squeletal_index)]

# -------------------------------------------------------------------------

platelets_d[, dinamometer_average := (dinamometer_left_hand + dinamometer_right_hand) / 2][]


# -------------------------------------------------------------------------

platelets_d <- droplevels(platelets_d)

# -------------------------------------------------------------------------

save(platelets_d, file = "data/platelets_d.RData")
