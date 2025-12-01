model1 <- function(data) {
  model_lin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x +
          ethnicity +
          highest_qual +
          age_accel +
          age_accel2 +
          sex
    )

  model_lin_int <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x *
          (age_accel +
            age_accel2) +
          x * sex +
          ethnicity +
          highest_qual
    )

  model_nonlin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x +
          x2 +
          ethnicity +
          age_accel +
          age_accel2 +
          +sex +
          highest_qual
    )

  model_cat <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x_cat +
          ethnicity +
          age_accel +
          age_accel2 +
          sex +
          highest_qual
    )
  list(
    model_lin = model_lin,
    model_nonlin = model_nonlin,
    model_lin_int = model_lin_int,
    model_cat = model_cat
  )
}

model2 <- function(data) {
  model_lin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x +
          ethnicity +
          age_accel +
          age_accel2 +
          sex +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa +
          employ
    )

  model_lin_int <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x *
          (age_accel +
            age_accel2) +
          x * sex +
          ethnicity +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa +
          employ
    )

  model_nonlin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x +
          x2 +
          ethnicity +
          age_accel +
          age_accel2 +
          sex +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa +
          employ
    )

  model_cat <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x_cat +
          ethnicity +
          age_accel +
          age_accel2 +
          sex +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa +
          employ
    )
  list(
    model_lin = model_lin,
    model_nonlin = model_nonlin,
    model_lin_int = model_lin_int,
    model_cat = model_cat
  )
}
