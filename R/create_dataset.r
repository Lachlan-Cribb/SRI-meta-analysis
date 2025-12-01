create_dataset <- function(
  core_file,
  demdeath_file,
  snp_file,
  diet_file,
  accel_file,
  is_file,
  sri_file,
  sleep_file,
  disease_file
) {
  # read UKB data

  d <- fread(core_file)

  # up to date event variables

  latest <- fread(demdeath_file)

  # rename variables
  setnames(
    latest,
    c("40000-0.0", "42018-0.0"),
    c("date_of_death", "date_all_cause_dementia")
  )
  d <- d[, !c("date_of_death", "date_all_cause_dementia")]
  d <- merge(latest, d, by = "eid", all.x = TRUE)

  # Add in SNPs #

  snps <- fread(snp_file)

  d <- merge(d, snps, by = "eid", all.x = TRUE)

  # Add in diet and alcohol variables #

  alc_diet <- fread(diet_file)

  setnames(
    alc_diet,
    c(
      "eid",
      "alc_freq",
      "veg_cooked",
      "veg_raw",
      "fruit_fresh",
      "fruit_dry"
    )
  )

  d <- merge(d, alc_diet, by = "eid", all.x = TRUE)

  ### Set categorical variables to factor

  d$apoe_e4 <- as.factor(d$apoe_e4)

  d$bp_med <- as.factor(d$bp_med)

  d$any_cvd <- as.factor(d$any_cvd)

  d$smok_status <- as.factor(d$smok_status)
  levels(d$smok_status) <-
    c("prefer not answer", "never", "former", "current")

  d$apoe_e4 <- as.factor(d$apoe_e4)

  d$shift <- ifelse(
    d$job_night_shift %in% c(3, 4) | d$job_shift_work %in% c(3, 4),
    1,
    0
  )

  # Highest qualification

  quals <- d[, .SD, .SDcols = patterns("eid|qualifications")]

  quals <- melt(quals, id.vars = "eid", variable.name = "name")

  quals[,
    highest_qual := fcase(
      value == 1,
      4,
      value == 6,
      4,
      value == 5,
      3,
      value == 2,
      2,
      value == 3,
      1,
      value == 4,
      1,
      default = as.numeric(value)
    )
  ]

  quals <- quals[,
    list(highest_qual = max(highest_qual, na.rm = TRUE)),
    by = eid
  ]

  quals[,
    highest_qual := ifelse(
      highest_qual == -Inf,
      NA_real_,
      highest_qual
    )
  ]

  d <- merge(d, quals, by = "eid", all.x = TRUE)

  d <- d[, .SD, .SDcols = !patterns("qualifications")]

  d$highest_qual <- fct_recode(as.factor(d$highest_qual), NULL = "-3")
  levels(d$highest_qual) <- c("Other", "CSE_GCSE", "A", "NVQ", "Grad")

  ## Employment

  d[,
    employ := fcase(
      employment_1 == -7,
      "Other",
      employment_1 == -3,
      "prefer not answer",
      employment_1 == 1,
      "Employed",
      employment_1 == 2,
      "Retired",
      employment_1 == 3,
      "Home/family",
      employment_1 == 4,
      "Sick/disabled",
      employment_1 == 5,
      "Unemployed",
      employment_1 == 6,
      "Other",
      employment_1 == 7,
      "Other"
    )
  ]

  # Set some categories to missing (prefer not answer)
  d$alc_freq <- ifelse(d$alc_freq == -3, NA_real_, d$alc_freq)
  d$smok_status <- fct_recode(d$smok_status, NULL = "prefer not answer")
  d$employ <- fct_recode(d$employ, NULL = "prefer not answer")

  # medications at UKB entry

  d$antidepressant_med <- as.numeric(d$antidepressant_med)
  d$antipsychotic_med <- as.numeric(d$antipsychotic_med)
  d$insomnia_med <- as.numeric(d$insomnia_med)

  ## Race

  d$ethnicity <- as.factor(d$ethnicity)

  # collapse categories

  d$ethnicity <- fct_recode(
    d$ethnicity,
    NULL = "-3",
    NULL = "-1",
    "white" = "1",
    "white" = "1001",
    "white" = "2001",
    "white" = "3001",
    "white" = "4001",
    "other" = "2",
    "other" = "1002",
    "other" = "2002",
    "other" = "3002",
    "other" = "4002",
    "other" = "3",
    "other" = "1003",
    "other" = "2003",
    "other" = "3003",
    "other" = "4",
    "other" = "2004",
    "other" = "3004",
    "other" = "4003",
    "other" = "5",
    "other" = "6"
  )

  ## sex
  d$sex <- as.factor(d$sex)
  levels(d$sex) <- c("female", "male")

  ## depression

  d$freq_depressed_twoweeks <- ifelse(
    d$freq_depressed_twoweeks < 0,
    NA_real_,
    d$freq_depressed_twoweeks
  )

  #### Trim dataset for selected sample ####

  # tracking sample size

  sample_size_info <- list()

  # Accelerometry data available

  sample_size_info$total_cohort <- nrow(d)

  d2 <- d[!is.na(accel_data_available), ]

  sample_size_info$accel_available <- nrow(d2)

  # apply UKB data cleaning thresholds to accelerometry data

  d2 <- d2[is.na(d2$accel_data_problem), ]

  sample_size_info$no_accel_data_problem <- nrow(d2)

  d2 <- d2[accel_good_wear_time == 1, ]

  sample_size_info$good_wear_time <- nrow(d2)

  d2 <- d2[accel_good_calibration == 1, ]

  d2 <- d2[accel_calibrated_own_data == 1, ]

  sample_size_info$good_calibration <- nrow(d2)

  ## must have GGIR data available

  # Read in accelerometry data

  a <- fread(accel_file)

  # filter to only those with GGIR data

  d2 <- d2[eid %in% a$eid, ]

  ## Merge in actigraphy data

  d3 <- merge(d2, a, by = "eid", all.x = TRUE)

  ## Add SRI data

  sri_data <- fread(sri_file)
  setnames(sri_data, c("ID", "SleepRegularityIndex"), c("eid", "SRI"))
  sri_data <- sri_data[, list(eid, calendar_date, SRI)]
  sri_data[,
    calendar_date := parse_date_time(calendar_date, "d/m/y")
  ]
  d3[,
    calendar_date := parse_date_time(calendar_date, "y-m-d")
  ]

  d3 <- merge(d3, sri_data, by = c("eid", "calendar_date"), all.x = TRUE)

  ### At least 4 days of data and 20 hours of wear per day

  ## Wear time
  # total time awake during sleep window
  d3$awake_sleep <-
    d3$dur_spt_wake_IN_min +
    d3$dur_spt_wake_LIG_min +
    d3$dur_spt_wake_MOD_min +
    d3$dur_spt_wake_VIG_min

  # total wear time
  d3$mins_worn <-
    d3$dur_spt_sleep_min +
    d3$dur_day_total_IN_min +
    d3$dur_day_total_LIG_min +
    d3$dur_day_total_MOD_min +
    d3$dur_day_total_VIG_min +
    d3$awake_sleep

  # At least 20 hours wear time per day
  d3[mins_worn >= 20 * 60, ]

  # At least 4 valid SRI
  d3[, n_valid := sum(!is.na(SRI)), by = "eid"]

  d3 <- d3[n_valid >= 4, ]

  # Variables normalised to 1440 relative to their proportion of total wear time

  mins_in_day <- 24 * 60

  d3$sleep_n <-
    (d3$dur_spt_sleep_min / d3$mins_worn) * mins_in_day

  d3$moderate_n <-
    ((d3$dur_day_total_MOD_min + d3$dur_spt_wake_MOD_min) /
      d3$mins_worn) *
    mins_in_day

  d3$vigorous_n <-
    ((d3$dur_day_total_VIG_min + d3$dur_spt_wake_VIG_min) /
      d3$mins_worn) *
    mins_in_day

  d3$mvpa_n <- d3$moderate_n + d3$vigorous_n

  ### Average accel variables over wear days

  sleep_data <- average_accel_vars(d3, "sleep_n", "avg_sleep")
  mvpa_data <- average_accel_vars(d3, "mvpa_n", "avg_mvpa")
  sri_data <- average_accel_vars(d3, "SRI", "avg_sri")

  ## Add averaged accel variables to data

  d2 <- merge(d2, sleep_data, by = "eid", all.x = TRUE)
  d2 <- merge(d2, mvpa_data, by = "eid", all.x = TRUE)
  d2 <- merge(d2, sri_data, by = "eid", all.x = TRUE)

  # remove those with insufficient sleep values/failed GGIR quality checks

  d2 <- d2[!is.na(avg_sri), ]

  sample_size_info$GGIR_checks <- nrow(d2)

  ## Add interdaily stability (IS)

  is_data <- fread(is_file)

  is_data <- is_data[, list(eid, IS_interdailystability)]

  setnames(is_data, c("eid", "IS"))

  d2 <- merge(d2, is_data, by = "eid", all.x = TRUE)

  ### Calculate time to dementia

  # baseline (accelerometry) date

  date <- a[!duplicated(eid), list(eid, calendar_date)]

  d2 <- merge(d2, date, by = "eid", all.x = TRUE)

  d2[, calendar_date := parse_date_time(calendar_date, orders = "ymd")]

  # Date of first dementia diagnosis

  demtime <- copy(d)

  demtime[, `:=`(
    date_all_cause_dementia = as_date(date_all_cause_dementia),
    dem_date_pc = as_date(dem_date_pc)
  )]

  demtime[, time_dif := date_all_cause_dementia - dem_date_pc]

  demtime <- demtime[, list(
    eid,
    date_all_cause_dementia,
    dem_date_pc,
    time_dif
  )]

  demtime <- melt(
    demtime,
    measure.vars = c("date_all_cause_dementia", "dem_date_pc"),
    value.name = "date_acdem2"
  )

  demtime <- demtime[order(eid, date_acdem2), ]

  demtime[, num := seq_len(.N), by = "eid"]

  demtime <- demtime[num == 1, list(eid, date_acdem2)]

  d2 <- merge(d2, demtime, by = "eid", all.x = TRUE)

  # dementia diagnosis

  d2$dem <- ifelse(!is.na(d2$date_acdem2), 1, 0)

  ## Construct risk set
  # Death from other causes remain in risk set until end of FU

  setnames(d2, "calendar_date", "date_accel")

  d2[, competing := ifelse(!is.na(date_of_death) & is.na(date_acdem2), 1, 0)]

  d2[,
    time_to_dem := as.integer(fcase(
      dem == 1,
      difftime(date_acdem2, date_accel),
      competing == 1,
      difftime("2023-01-01", date_accel),
      default = difftime("2023-01-01", date_accel)
    ))
  ]

  # create death and time to death variable

  d2$death <- ifelse(is.na(d2$date_of_death), 0, 1)

  d2$time_to_death <- ifelse(
    d2$death == 1,
    difftime(d2$date_of_death, d2$date_accel),
    difftime("2023-01-01", d2$date_accel)
  )

  ## remove those with prevalent dementia

  d2 <- d2[time_to_dem > 0, ]

  sample_size_info$no_prevalent_dementia <- nrow(d2)

  # Sleep disorders data
  sleep_dis_df <- fread(sleep_file)

  # Merge dataframes
  d2 <- d2[, !"insomnia_sr"]
  d2 <- merge(d2, sleep_dis_df, by = "eid", all.x = TRUE)

  # Update age variable to age at accelerometry study
  d2$age_accel <-
    d2$age_assessment +
    ((as.Date(d2$date_accel) - as.Date(d2$date_baseline)) / 365)

  d2$age_accel <- as.numeric(d2$age_accel)

  ## Age >= 55 years

  d2 <- d2[age_accel >= 55, ]

  sample_size_info$age_55_over <- nrow(d2)

  ### Add prevalent disease variables

  prev <- fread(disease_file, stringsAsFactors = TRUE)

  # add in date of actigraphy and number of cancers

  adate <- d2[, list(eid, date_accel, num_sr_cancers)]

  prev <- merge(prev, adate, by = "eid", all.x = TRUE)

  # create prevalent illness variables

  self_report_vars <- grep("^sr_", names(prev), value = TRUE)

  prev[,
    (self_report_vars) := lapply(.SD, function(.x) {
      ifelse(is.na(.x), 0, .x)
    }),
    .SDcols = self_report_vars
  ]

  prev[,
    prev_diabetes := fcase(
      sr_diabetes == 1,
      1,
      sr_diabetes == 0 & date_diabetes < date_accel,
      1,
      default = 0
    )
  ]

  prev[,
    prev_cancer := fcase(
      num_sr_cancers > 0,
      1,
      num_sr_cancers == 0 & date_neoplasm < date_accel,
      1,
      default = 0
    )
  ]

  prev[,
    prev_mental_disorder := fcase(
      sr_mental_disorder == 1,
      1,
      sr_mental_disorder == 0 & date_mental_disorder < date_accel,
      1,
      default = 0
    )
  ]

  prev[,
    prev_cvd := fcase(
      sr_cvd == 1,
      1,
      sr_cvd == 0 & date_CVD < date_accel,
      1,
      default = 0
    )
  ]

  prev <- prev[, .SD, .SDcols = patterns("eid|^prev_")]

  d2 <- merge(d2, prev, by = "eid", all.x = TRUE)

  ### Select model data

  dem_model_data <- d2[, list(
    eid,
    alc_freq,
    avg_sri,
    IS,
    avg_mvpa,
    dem,
    death,
    time_to_dem,
    time_to_death,
    bp_syst_avg,
    age_accel,
    sex,
    shift,
    employ,
    ethnicity,
    insomnia_med,
    highest_qual,
    apoe_e4,
    freq_depressed_twoweeks,
    BMI,
    smok_status,
    prev_diabetes,
    prev_cancer,
    prev_cvd
  )]

  list(df = dem_model_data, selection_process = sample_size_info)
}

add_sleepreg_sri <- function(data, sleepreg_sri_file) {
  sleepreg_sri_data <- fread(sleepreg_sri_file)
  setnames(sleepreg_sri_data, c("eid", "sri"))
  # combine duplicated observations from sleepreg output
  sleepreg_sri_data <- sleepreg_sri_data[,
    .(sri = mean(sri, na.rm = TRUE)),
    by = "eid"
  ]
  data[, eid := as.character(eid)]
  setnames(data, "avg_sri", "rri")
  merge(data, sleepreg_sri_data, by = "eid", all.x = TRUE)
}
