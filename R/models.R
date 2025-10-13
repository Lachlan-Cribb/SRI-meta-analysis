model1 <- function(data) {
  knots_age <- quantile(
    data[["age_accel"]],
    c(0.1, 0.5, 0.9)
  )
  knots_x <- quantile(
    data[["x"]],
    c(0.1, 0.5, 0.9)
  )
  model_lin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x +
          ethnicity +
          rcs(age_accel, knots_age) +
          sex
    )

  model_lin_int <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x *
          rcs(age_accel, knots_age) +
          x * sex +
          ethnicity
    )

  model_nonlin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        rcs(x, knots_x) +
          ethnicity +
          rcs(age_accel, knots_age) +
          sex
    )
  list(
    model_lin = model_lin,
    model_nonlin = model_nonlin,
    model_lin_int = model_lin_int
  )
}

model2 <- function(data) {
  knots_age <- quantile(
    data[["age_accel"]],
    c(0.1, 0.5, 0.9)
  )
  knots_deprivation <- quantile(
    data[["townsend_deprivation_index"]],
    c(0.1, 0.5, 0.9)
  )
  knots_x <- quantile(
    data[["x"]],
    c(0.1, 0.5, 0.9)
  )
  knots_BMI <- quantile(
    data[["BMI"]],
    c(0.1, 0.5, 0.9)
  )
  knots_mvpa <- quantile(
    data[["avg_mvpa"]],
    c(0.1, 0.5, 0.9)
  )
  knots_bp <- quantile(
    data[["bp_syst_avg"]],
    c(0.1, 0.5, 0.9)
  )

  model_lin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x +
          ethnicity +
          rcs(age_accel, knots_age) +
          sex +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          prev_cancer +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa
    )

  model_lin_int <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x *
          rcs(age_accel, knots_age) +
          x * sex +
          ethnicity +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          prev_cancer +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa
    )

  model_nonlin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        rcs(x, knots_x) +
          ethnicity +
          rcs(age_accel, knots_age) +
          sex +
          alc_freq +
          highest_qual +
          bp_syst_avg +
          BMI +
          smok_status +
          prev_diabetes +
          prev_cvd +
          prev_cancer +
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa
    )
  list(
    model_lin = model_lin,
    model_nonlin = model_nonlin,
    model_lin_int = model_lin_int
  )
}
