# Shared feature-statistics helpers ----
# Input: unit-level feature observations.
# Output: response/equivalence statistics.

device_label <- function(x) dplyr::recode(as.character(x), SENS = "SENS motion", .default = as.character(x))

centripetal_g <- function(rpm) ((0.019/2) * (2 * pi * rpm/60)^2)/9.81

fmt <- function(x, digits = 1) ifelse(is.finite(x), formatC(x, format = "f", digits = digits), "NA")

fmt_p <- function(p) ifelse(!is.finite(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f",
    digits = 3)))

# Two one-sided Welch tests ----
welch_tost <- function(x1, x2, margin) {
    x1 <- x1[is.finite(x1)]
    x2 <- x2[is.finite(x2)]
    if (length(x1) < 3 || length(x2) < 3 || !is.finite(margin) || margin <= 0) {
        return(tibble::tibble(mean_difference = NA_real_, margin = margin, ci90_low = NA_real_, ci90_high = NA_real_,
            p_lower = NA_real_, p_upper = NA_real_, p_equivalence = NA_real_, equivalent = FALSE))
    }
    n1 <- length(x1)
    n2 <- length(x2)
    v1 <- stats::var(x1)
    v2 <- stats::var(x2)
    difference <- mean(x1) - mean(x2)
    se <- sqrt(v1/n1 + v2/n2)
    if (!is.finite(se) || se < .Machine$double.eps) {
        equivalent <- abs(difference) < margin
        return(tibble::tibble(mean_difference = difference, margin = margin, ci90_low = difference, ci90_high = difference,
            p_lower = ifelse(equivalent, 0, 1), p_upper = ifelse(equivalent, 0, 1), p_equivalence = ifelse(equivalent,
                0, 1), equivalent = equivalent))
    }
    df <- (v1/n1 + v2/n2)^2/((v1/n1)^2/(n1 - 1) + (v2/n2)^2/(n2 - 1))
    p_lower <- stats::pt((difference + margin)/se, df = df, lower.tail = FALSE)
    p_upper <- stats::pt((difference - margin)/se, df = df, lower.tail = TRUE)
    critical <- stats::qt(0.95, df = df)
    ci_low <- difference - critical * se
    ci_high <- difference + critical * se
    p_equivalence <- max(p_lower, p_upper)
    tibble::tibble(mean_difference = difference, margin = margin, ci90_low = ci_low, ci90_high = ci_high,
        p_lower = p_lower, p_upper = p_upper, p_equivalence = p_equivalence, equivalent = isTRUE(ci_low >
            -margin && ci_high < margin && p_equivalence < 0.05))
}

# Unit-level controlled-response fit ----
fit_response <- function(dat, input_type) {
    dat <- arrange(filter(dat, is.finite(value), is.finite(input_value)), rpm)
    if (nrow(dat) < 5 || length(unique(dat$input_value)) < 3) {
        return(tibble::tibble(slope = NA_real_, intercept = NA_real_, r2 = NA_real_, response_inl_pct = NA_real_,
            mean_abs_frequency_error_pct = NA_real_))
    }
    fit <- stats::lm(value ~ input_value, data = dat)
    fitted <- stats::fitted(fit)
    fitted_span <- max(fitted) - min(fitted)
    inl <- if (is.finite(fitted_span) && fitted_span > .Machine$double.eps)
        100 * max(abs(dat$value - fitted))/fitted_span else NA_real_
    frequency_error <- if (identical(input_type, "frequency"))
        mean(abs(dat$value - dat$input_value)/dat$input_value * 100) else NA_real_
    tibble::tibble(slope = stats::coef(fit)[["input_value"]], intercept = stats::coef(fit)[["(Intercept)"]],
        r2 = summary(fit)$r.squared, response_inl_pct = inl, mean_abs_frequency_error_pct = frequency_error)
}

# Physical-unit bootstrap intervals ----
bootstrap_response <- function(dat, reps = bootstrap_reps) {
    dat <- filter(dat, is.finite(slope), is.finite(r2), is.finite(response_inl_pct))
    n <- nrow(dat)
    if (n < 3)
        return(tibble::tibble(n_units = n, median_slope = NA_real_, slope_ci_low = NA_real_, slope_ci_high = NA_real_,
            median_r2 = NA_real_, r2_ci_low = NA_real_, r2_ci_high = NA_real_, median_inl_pct = NA_real_,
            inl_ci_low = NA_real_, inl_ci_high = NA_real_, median_frequency_error_pct = NA_real_))
    boot <- replicate(reps, {

        # Resample physical units ----
        index <- sample.int(n, n, replace = TRUE)
        c(slope = stats::median(dat$slope[index]), r2 = stats::median(dat$r2[index]), inl = stats::median(dat$response_inl_pct[index]),
            frequency_error = stats::median(dat$mean_abs_frequency_error_pct[index], na.rm = TRUE))
    })
    q <- function(x) stats::quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    slope_ci <- q(boot["slope", ])
    r2_ci <- q(boot["r2", ])
    inl_ci <- q(boot["inl", ])
    tibble::tibble(n_units = n, median_slope = stats::median(dat$slope), slope_ci_low = slope_ci[1],
        slope_ci_high = slope_ci[2], median_r2 = stats::median(dat$r2), r2_ci_low = r2_ci[1], r2_ci_high = r2_ci[2],
        median_inl_pct = stats::median(dat$response_inl_pct), inl_ci_low = inl_ci[1], inl_ci_high = inl_ci[2],
        median_frequency_error_pct = stats::median(dat$mean_abs_frequency_error_pct, na.rm = TRUE))
}
