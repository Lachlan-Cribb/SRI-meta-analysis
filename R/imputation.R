impute_data <- function(df, exposure_vars, m, maxit) {
  predmat <- quickpred(
    df,
    mincor = 0,
    exclude = c(
      "eid",
      "stratum"
    )
  )
  predmat[exposure_vars, ] <- 0

  methods <- make.method(df)
  methods[methods != ""] <- "rf"
  methods[exposure_vars] <- ""

  imp <- mice(
    df,
    m = m,
    predictorMatrix = predmat,
    maxit = maxit,
    method = methods
  )
  print(imp$loggedEvents)
  as.data.table(complete(imp))
}
