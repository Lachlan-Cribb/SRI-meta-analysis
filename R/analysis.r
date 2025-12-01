## test PH assumption
test_ph <- function(data, exposure, model_formula) {
  data <- data[tar_batch == 1, ] # first imputation
  data$x <- data[[exposure]]
  model_form <- switch(
    model_formula,
    model1 = model1(data)$model_lin,
    model2 = model2(data)$model_lin
  )
  coxph_lin <- coxph(model_form, data = data)
  cox.zph(coxph_lin)
}

## Create strata for stratified model fitting
stratify_data <- function(df, strata) {
  df[,
    stratum := fcase(
      age_accel >= 75,
      "75_and_over",
      age_accel >= 65 & age_accel < 75 & sex == "male",
      "65_to_75_males",
      age_accel >= 65 & age_accel < 75 & sex == "female",
      "65_to_75_females",
      age_accel < 65 & sex == "male",
      "under_65_males",
      age_accel < 65 & sex == "female",
      "under_65_females",
      default = "other"
    )
  ]
  if (!strata == "all") {
    df <- df[grep(strata, stratum), ]
  }
  if (strata %in% c("males", "females")) {
    df <- df[grep(strata, stratum), ]
  }
  df[, stratum := strata]
  df
}

## Add categorical versions of exposures for non-lin analysis
add_categories <- function(df) {
  df[, `:=`(
    rri_cat = cut(rri, breaks = c(-Inf, 50, 57.5, 65, 72.5, Inf)),
    sri_cat = cut(sri, breaks = c(-Inf, 65, 72.5, 80, 87.5, Inf)),
    IS_cat = cut(IS, breaks = c(-Inf, 0.5, 0.6, 0.7, 0.8, Inf))
  )]
  # set reference level
  df$rri_cat <- fct_relevel(df$rri_cat, "(57.5,65]")
  df$sri_cat <- fct_relevel(df$sri_cat, "(80,87.5]")
  df$IS_cat <- fct_relevel(df$IS_cat, "(0.6,0.7]")
  df
}

## Check model assumptions
check_model <- function(data, exposure, model_formula) {
  data <- data[tar_batch == 1, ] # first imputation
  data$x <- data[[exposure]]
  # Add spline bases to dataset
  data[, `:=`(
    age_accel2 = rcspline.eval(data$age_accel, nk = 3),
    x2 = rcspline.eval(data$x, nk = 3)
  )]
  model_form <- switch(
    model_formula,
    model1 = model1(data),
    model2 = model2(data)
  )
  model_lin <- model_form$model_lin
  model_lin_int <- model_form$model_lin_int
  model_nonlin <- model_form$model_nonlin
  coxph_lin <- coxph(model_lin, data = data)
  coxph_lin_int <- coxph(model_lin_int, data = data)
  coxph_nonlin <- coxph(model_nonlin, data = data)
  p_interaction <- anova(coxph_lin, coxph_lin_int)$`Pr(>|Chi|)`[2]
  p_linearity <- anova(coxph_lin, coxph_nonlin)$`Pr(>|Chi|)`[2]
  data.table(
    stratum = unique(data$stratum),
    exposure = exposure,
    model_formula = model_formula,
    p_linearity = p_linearity,
    p_interaction = p_interaction
  )
}

## Fit Cox model and pool across imputed datasets
pooled_results <- function(
  data,
  exposure,
  model_formula,
  fine_grey,
  firth = FALSE
) {
  data$x <- data[[exposure]]
  # rescale IS to match units of over measures
  if (exposure == "IS") {
    data$x <- data$x * 100
  }
  # Add spline bases to dataset
  data[, `:=`(
    age_accel2 = rcspline.eval(data$age_accel, nk = 3),
    x2 = rcspline.eval(data$x, nk = 3)
  )]
  sample_size <- median(data[, .(count = .N), by = "tar_batch"]$count)
  cases <- median(data[, .(count = sum(dem)), by = "tar_batch"]$count)
  median_fu <- median(data[, .(fu = median(time_to_dem)), by = "tar_batch"]$fu)
  mean_fu <- mean(data[, .(fu = mean(time_to_dem)), by = "tar_batch"]$fu)
  stratum <- unique(data$stratum)
  # collapse employment categories for >75 age stratum
  if (stratum == "75_and_over") {
    data[, employ := ifelse(employ == "Sick/disabled", "Other", employ)]
  }
  if (fine_grey) {
    data[, dem := ifelse(death == 1 & time_to_death < time_to_dem, 2, dem)]
    data[, dem := as.factor(dem)]
  }
  model_form <- switch(
    model_formula,
    model1 = model1(data)$model_lin,
    model2 = model2(data)$model_lin
  )
  if (grepl("males|females", stratum)) {
    model_form <- update.formula(model_form, ~ . - sex)
  }
  data_list <- split(data, data$tar_batch)
  if (firth) {
    fits <- lapply(data_list, \(.d) coxphf(model_form, data = .d, pl = FALSE))
  } else if (fine_grey == TRUE) {
    fits <- lapply(data_list, \(.d) crr(model_form, data = .d))
  } else {
    fits <- lapply(data_list, \(.d) coxph(model_form, data = .d))
  }
  results <- MIcombine(
    results = lapply(fits, coef),
    variances = lapply(fits, vcov)
  )
  out <- as.data.table(summary(results), keep.rownames = TRUE)
  setnames(out, c("term", "estimate", "se", "lower", "upper", "miss_info"))
  out[, `:=`(
    stratum = stratum,
    exposure = exposure,
    model_formula = model_formula,
    fine_grey = fine_grey,
    sample_size = sample_size,
    cases = cases,
    median_fu = median_fu / 365,
    mean_fu = mean_fu / 365,
    log_HR = estimate,
    log_HR_SE = se
  )]
  out[
    term == "x",
    list(
      stratum,
      exposure,
      model_formula,
      fine_grey,
      sample_size,
      cases,
      mean_fu,
      median_fu,
      log_HR,
      log_HR_SE
    )
  ]
}

## Fit categorical Cox model and pool across imputed datasets
pooled_results_cat <- function(data, exposure, model_formula, fine_grey) {
  data$x_cat <- data[[exposure]]
  stratum <- unique(data$stratum)
  # Add spline bases to dataset
  data[, `:=`(
    age_accel2 = rcspline.eval(data$age_accel, nk = 3),
    x2 = rcspline.eval(data$x, nk = 3)
  )]
  model_form <- switch(
    model_formula,
    model1 = model1(data)$model_cat,
    model2 = model2(data)$model_cat
  )
  cases <- data[, .(cases = sum(dem)), by = c("x_cat", "tar_batch")][,
    .(cases = median(cases)),
    by = "x_cat"
  ]
  cases[, x_cat := paste0("x_cat", x_cat)]
  setnames(cases, "x_cat", "term")
  cases[, `:=`(
    stratum = stratum,
    exposure = exposure,
    model_formula = model_formula
  )]
  if (fine_grey) {
    data[, dem := ifelse(death == 1 & time_to_death < time_to_dem, 2, dem)]
    data[, dem := as.factor(dem)]
  }
  data_list <- split(data, data$tar_batch)
  if (fine_grey) {
    fits <- lapply(data_list, \(.d) crr(model_form, data = .d))
  } else {
    fits <- lapply(data_list, \(.d) coxph(model_form, data = .d))
  }

  results <- MIcombine(
    results = lapply(fits, coef),
    variances = lapply(fits, vcov)
  )
  out <- as.data.table(summary(results), keep.rownames = TRUE)
  setnames(out, c("term", "estimate", "se", "lower", "upper", "miss_info"))
  out[, `:=`(
    stratum = stratum,
    exposure = exposure,
    model_formula = model_formula,
    HR = round(exp(estimate), 3),
    lower = round(exp(lower), 3),
    upper = round(exp(upper), 3)
  )]
  out <- out[
    grep("x_cat", term),
    list(
      stratum,
      exposure,
      term,
      model_formula,
      HR,
      lower,
      upper
    )
  ]
  out <- merge(
    out,
    cases,
    by = c("term", "stratum", "exposure", "model_formula"),
    all = TRUE
  )
  out[, list(
    stratum,
    exposure,
    term,
    cases,
    model_formula,
    HR,
    lower,
    upper
  )]
}
