## Create strata for stratified model fitting
stratify_data <- function(df, strata) {
  df[,
    stratum := fcase(
      age_accel < 65 & sex == "male",
      "<65 males",
      age_accel < 65 & sex == "female",
      "<65 females",
      age_accel >= 65 & age_accel < 75 & sex == "male",
      "65 to 75 males",
      age_accel >= 65 & age_accel < 75 & sex == "female",
      "65 to 75 females",
      default = "other"
    )
  ]
  if (!strata == "all") {
    df <- df[stratum == strata, ]
  }
  df
}

## Check model assumptions
check_model <- function(data, exposure, model_formula) {
  data <- data[[1]]
  data$x <- data[[exposure]]
  model_lin <- model_formula(data)$model_lin
  model_lin_int <- model_formula(data)$model_lin_int
  model_nonlin <- model_formula(data)$model_nonlin
  coxph_lin <- coxph(model_lin, data = data)
  coxph_lin_int <- coxph(model_lin_int, data = data)
  coxph_nonlin <- coxph(model_nonlin, data = data)
  p_interaction <- anova(coxph_lin, coxph_lin_int)$`Pr(>|Chi|)`[2]
  p_linearity <- anova(coxph_lin, coxph_nonlin)$`Pr(>|Chi|)`[2]
  resids <- cox.zph(coxph_lin)
  list(p_lin = p_linearity, p_int = p_interaction, resids = resids)
}

## Fit Cox models and pool across imputed datasets
fit_coxph <- function(imps, exposure, model_formula) {
  imps <- lapply(imps, function(d) {
    d$x <- d[[exposure]]
    d
  })
  first_imp <- imps[[1]]
  stratum <- ifelse(
    length(unique(first_imp$stratum)) > 1,
    "all",
    unique(first_imp$stratum)
  )
  model_form <- switch(
    model_formula,
    model1 = model1(first_imp)$model_lin,
    model2 = model2(first_imp)$model_lin
  )
  fits <- lapply(imps, \(d) coxph(model_form, data = d))
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
    HR = exp(estimate),
    lower = exp(lower),
    upper = exp(upper)
  )]
  out[term == "x", list(stratum, exposure, model_formula, HR, lower, upper)]
}
