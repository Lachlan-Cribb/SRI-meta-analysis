plot_spline_association <- function(data, exposure, model_formula) {
  data <- data[tar_batch == 1, ]
  stratum <- unique(data$stratum)
  data$x <- data[[exposure]]

  model_spec <- switch(
    model_formula,
    model1 = model1(data),
    model2 = model2(data)
  )

  model_nonlin <- model_spec$model_nonlin

  # Tidy data
  if (exposure == "IS") {
    data$x <- data$x * 100
  }
  data[, `:=`(
    age_accel2 = rcspline.eval(data$age_accel, nk = 3),
    x2 = rcspline.eval(data$x, nk = 3)
  )]
  if (grepl("males|females", stratum)) {
    model_nonlin <- update.formula(model_nonlin, ~ . - sex)
  }
  if (stratum == "75_and_over") {
    data[, employ := ifelse(employ == "Sick/disabled", "Other", employ)]
  }

  # Hard code reference
  ref_exposure <- switch(
    exposure,
    sri = 70,
    rri = 55,
    IS = 60
  )

  dd <- datadist(data)
  dd$limits["Adjust to", "x"] <- ref_exposure
  options(datadist = dd)

  fit <- cph(model_nonlin, data = data, x = TRUE, y = TRUE)

  x_range <- quantile(data$x, c(0.01, 0.99), na.rm = TRUE)
  x_seq <- seq(x_range[1], x_range[2], length.out = 100)
  pred <- as.data.table(rms::Predict(
    fit,
    x = x_seq,
    ref.zero = TRUE,
    fun = exp
  ))

  options(datadist = NULL)

  exposure_label <- switch(
    exposure,
    sri = "Sleep Regularity Index",
    rri = "Rest-activity Regularity Index",
    IS = "Interdaily Stability (× 100)"
  )

  model_label <- switch(
    model_formula,
    model1 = "Model 1 (Minimally adjusted)",
    model2 = "Model 2 (Fully adjusted)"
  )

  ggplot(pred, aes(x = x, y = yhat)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "steelblue"
    ) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      color = "red",
      alpha = 0.6
    ) +
    labs(
      x = exposure_label,
      y = "Hazard Ratio for Dementia"
    ) +
    scale_y_continuous(
      breaks = scales::breaks_extended(n = 6)
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
}

save_plots <- function(data, plots) {
  data <- data[tar_batch == 1, ]
  stratum <- unique(data$stratum)
  plot_title <- fcase(
    stratum == "all",
    "All participants",
    stratum == "males",
    "Males",
    stratum == "females",
    "Females",
    stratum == "under_65",
    "Aged <65 years",
    stratum == "65_to_75",
    "Aged 65 to 75 years",
    stratum == "75_and_over",
    "Aged >= 75 years",
    stratum == "under_65_males",
    "Males aged <65 years",
    stratum == "under_65_females",
    "Females aged <65 years",
    stratum == "65_to_75_males",
    "Males aged 65 to 75 years",
    stratum == "65_to_75_females",
    "Females aged 65 to 75 years"
  )

  ggsave(
    paste0("Non-linear-", stratum, ".tiff"),
    plots +
      plot_annotation(
        title = plot_title,
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      ),
    device = "tiff",
    width = 12,
    height = 16,
    bg = "white"
  )
}
