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
          highest_qual +
          rcs(age_accel, knots_age) +
          sex
    )

  model_lin_int <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x *
          rcs(age_accel, knots_age) +
          x * sex +
          ethnicity +
          highest_qual
    )

  model_nonlin <-
    as.formula(
      Surv(time_to_dem, dem) ~
        rcs(x, knots_x) +
          ethnicity +
          rcs(age_accel, knots_age) +
          sex +
          highest_qual
    )

  model_cat <-
    as.formula(
      Surv(time_to_dem, dem) ~
        x_cat +
          ethnicity +
          rcs(age_accel, knots_age) +
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
          freq_depressed_twoweeks +
          insomnia_med +
          avg_mvpa +
          employ
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
          rcs(age_accel, knots_age) +
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
