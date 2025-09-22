average_accel_vars <- function(data, variable, outname) {
    data$weekday <-
        factor(
            data$weekday,
            levels = c(
                "Sunday",
                "Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday",
                "Saturday"
            )
        )

    m1 <-
        lmer(
            as.formula(paste0(
                variable,
                " ~ weekday + accel_daylight_savings_crossover + (1 | eid)"
            )),
            data = data
        )

    # Estimate average SRI for each participant, standardising over
    # day of week.

    nd <- CJ(
        weekday = levels(data$weekday),
        accel_daylight_savings_crossover = 0,
        eid = unique(data$eid)
    )

    nd$pred <- predict(m1, newdata = nd)

    nd <- nd[, .(out = mean(pred)), by = "eid"]
    setnames(nd, "out", outname)
    nd
}
