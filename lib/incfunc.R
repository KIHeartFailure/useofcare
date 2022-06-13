incfunc <- function(data, xvar, ref = "Control", time, event, eventname, rep = FALSE, mod = TRUE) {
  xlevs <- levels(data %>% pull(!!sym(xvar)))

  nrow <- 1
  if (mod) nrow <- nrow + 1
  
  out <- data.frame(matrix(NA, ncol = length(xlevs) + 2, nrow = nrow))
  colnames(out) <- c("Outcome", "Model", xlevs)

  out[1, 1] <- eventname

  ## incidence rate
  out[1, 2] <- "Incidence"

  if (!rep) {
    ev <- data %>%
      group_by(!!sym(xvar)) %>%
      summarise(
        ev = sum(!!sym(event) == "Yes"),
        .groups = "drop"
      )
  }
  if (rep) {
    ev <- data %>%
      group_by(!!sym(xvar)) %>%
      summarise(
        ev = sum(!!sym(event)),
        .groups = "drop"
      )
  }

  s <- data %>%
    group_by(!!sym(xvar)) %>%
    summarise(
      s = sum(!!sym(time) / 365.25),
      .groups = "drop"
    )
  r <- pois.exact(x = ev$ev, pt = s$s / 1000)

  out[1, 3:(length(xlevs) + 2)] <- paste0(
    ev$ev, ", ",
    dF(s$s, dig = 0), ", ",
    dF(r$rate, dig = 0), " (",
    dF(r$lower, dig = 0), "-",
    dF(r$upper, dig = 0), ")"
  )

  if (mod) {
    if (xvar != "ef_casecontrol") {
      if (!rep) {
        # cox regressions
        out[2, 2] <- "Crude HR (95% CI), p-value"

        ## crude
        mod <- summary(coxph(formula(paste0(
          "Surv(", time, ",", event, "=='Yes') ~ relevel(",
          xvar, ", ref = '", ref, "')"
        )),
        data = data
        ))

        out[2, ref] <- "ref"
        out[2, xlevs[xlevs != ref]] <- paste0(
          fn(mod$conf.int[, 1], dig = 2),
          " (", fn(mod$conf.int[, 3], dig = 2),
          "-", fn(mod$conf.int[, 4], dig = 2), "), ",
          fn(mod$coef[, 5], dig = 3, p = TRUE)
        )
      }
      if (rep) {
        # neg bin regression
        out[2, 2] <- "Crude IRR (95% CI), p-value"

        ## crude
        mod <- summary(glm.nb(formula(paste0(event, " ~ relevel(", xvar, ", ref = '", ref, "') + offset(log(", time, "))")),
          data = data %>% filter(!!sym(time) > 0)
        ))

        ncoefs <- nrow(mod$coefficients)
        out[2, ref] <- "ref"
        out[2, xlevs[xlevs != ref]] <- paste0(
          fn(exp(mod$coefficients[2:ncoefs, 1]), dig = 2),
          " (", fn(exp(mod$coefficients[2:ncoefs, 1] - global_z05 * mod$coefficients[2:ncoefs, 2]), dig = 2),
          "-", fn(exp(mod$coefficients[2:ncoefs, 1] + global_z05 * mod$coefficients[2:ncoefs, 2]), dig = 2), "), ",
          fn(mod$coefficients[2:ncoefs, 4], dig = 3, p = TRUE)
        )
      }
    }

    if (xvar == "ef_casecontrol") {
      levsef <- levels(data %>% pull(shf_ef_cat))
      for (i in seq_along(levsef)) {
        if (!rep) {
          # cox regressions
          out[2, 2] <- "Crude HR (95% CI), p-value"

          ## crude
          mod <- summary(coxph(formula(paste0("Surv(", time, ",", event, "=='Yes') ~ relevel(casecontrol, ref = '", ref, "')")),
            data = data %>% filter(shf_ef_cat == levsef[i])
          ))

          out[2, paste0(levsef[i], " ", ref)] <- "ref"
          out[2, paste0(levsef[i], " Case")] <- paste0(
            fn(mod$conf.int[, 1], dig = 2),
            " (", fn(mod$conf.int[, 3], dig = 2),
            "-", fn(mod$conf.int[, 4], dig = 2), "), ",
            fn(mod$coef[, 5], dig = 3, p = TRUE)
          )
        }
        if (rep) {
          # neg bin regression
          out[2, 2] <- "Crude IRR (95% CI), p-value"

          ## crude
          mod <- summary(glm.nb(formula(paste0(event, " ~ relevel(casecontrol, ref = '", ref, "') + offset(log(", time, "))")),
            data = data %>% filter(!!sym(time) > 0 & shf_ef_cat == levsef[i])
          ))

          ncoefs <- nrow(mod$coefficients)
          out[2, paste0(levsef[i], " ", ref)] <- "ref"
          out[2, paste0(levsef[i], " Case")] <- paste0(
            fn(exp(mod$coefficients[2:ncoefs, 1]), dig = 2),
            " (", fn(exp(mod$coefficients[2:ncoefs, 1] - global_z05 * mod$coefficients[2:ncoefs, 2]), dig = 2),
            "-", fn(exp(mod$coefficients[2:ncoefs, 1] + global_z05 * mod$coefficients[2:ncoefs, 2]), dig = 2), "), ",
            fn(mod$coefficients[2:ncoefs, 4], dig = 3, p = TRUE)
          )
        }
      }
    }
  }
  return(out)
}
