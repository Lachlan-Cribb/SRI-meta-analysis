impute_data <- function(df, m, maxit) {
  predmat <- quickpred(
    df,
    mincor = 0,
    exclude = c(
      "eid",
      "stratum"
    )
  )
  imp <- mice(
    df,
    m = m,
    predictorMatrix = predmat,
    maxit = maxit,
    method = "rf"
  )
  print(imp$loggedEvents)
  complete(imp, "all")
}
