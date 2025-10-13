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
      age_accel >= 65 & age_accel < 75 & sex == "male",
      "65_to_75_males",
      age_accel >= 65 & age_accel < 75 & sex == "female",
      "65_to_75_females",
      default = "other"
    )
  ]
  if (!strata == "all") {
    df <- df[stratum == strata, ]
  }
  df[, stratum := strata]
  df
}

## Check model assumptions
check_model <- function(data, exposure, model_formula) {
  data <- data[tar_batch == 1, ] # first imputation
  data$x <- data[[exposure]]
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
pooled_results <- function(data, exposure, model_formula) {
  data$x <- data[[exposure]]
  stratum <- unique(data$stratum)
  model_form <- switch(
    model_formula,
    model1 = model1(data)$model_lin,
    model2 = model2(data)$model_lin
  )
  median_cases <- median(data[, .(count = sum(dem)), by = "tar_batch"]$count)
  median_fu <- median(data[, .(fu = median(time_to_dem)), by = "tar_batch"]$fu)
  data_list <- split(data, data$tar_batch)
  fits <- lapply(data_list, \(.d) coxph(model_form, data = .d))
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
    median_cases = median_cases,
    median_fu = round(median_fu / 365, 1),
    HR = round(exp(estimate), 3),
    lower = round(exp(lower), 3),
    upper = round(exp(upper), 3)
  )]
  out[
    term == "x",
    list(
      stratum,
      exposure,
      model_formula,
      median_cases,
      median_fu,
      HR,
      lower,
      upper
    )
  ]
}
