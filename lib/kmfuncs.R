kmfunc <- function(data, xvar, time, event, eventname, legpos = c(50, 1)) {
  fit <- survfit(formula(paste0("Surv(", time, ",", event, "=='Yes') ~ ", xvar)),
    data = data
  )

  sfit <- summary(fit)$table

  medsurv <- paste0(
    dF(sfit[, "median"] / 365.25, 1)
  )
  medsurv <- str_replace_all(medsurv, "NA", "")
  medsurv[medsurv != ""] <- paste0(" median: ", medsurv[medsurv != ""], " years")

  ytext <- paste0(levels(data %>% pull(!!sym(xvar))), medsurv)

  nlevs <- length(ytext)
  if (nlevs == 2) {
    mycols <- rep(global_cols[5], 5)
    mylty <- c(1, 2)
  }
  if (nlevs == 3) {
    mycols <- global_cols[c(7, 5, 2)]
    mylty <- rep(1, 3)
  }
  if (nlevs == 6) {
    mycols <- rep(global_cols[c(7, 5, 2)], each = 2)
    mylty <- c(1, 2, 1, 2, 1, 2)
  }

  # c(bottom, left, top, right)
  par(mar = c(4, 4, .5, .5) + 0.1)
  plots <- plot(fit,
    fun = "event",
    ylab = eventname,
    xscale = 365.25,
    yscale = 100,
    col = mycols,
    mark.time = FALSE,
    bty = "n",
    xlim = c(0, 366 * 10),
    ylim = c(0, 1),
    xlab = "Years",
    axes = F,
    lwd = 3,
    lty = mylty,
    xaxs = "i", yaxs = "i"
  )

  axis(2, seq(0, 1, 0.25), seq(0, 100, 25), las = 2)
  axis(1, at = seq(0, 10, 2) * 365, seq(0, 10, 2))

  legend(x = legpos[1], y = legpos[2], ytext, col = mycols, lty = mylty, lwd = 3, bty = "n")

  medsurv <- sfit[, "median"]
  medsurv[medsurv >= 10 * 365] <- NA
  segments(
    x0 = medsurv, y0 = rep(0, nlevs), y1 = rep(0.5, nlevs),
    lty = mylty, lwd = 3, col = mycols
  )
  abline(h = 0.5, col = 1, lty = 3)
}

cifunc <- function(data, xvar, time, event, eventname, legpos = c(50, 1)) {
  fit <- cuminc(
    ftime = data %>% pull(!!sym(time)),
    fstatus = data %>% pull(!!sym(event)),
    cencode = 0,
    group = data %>% pull(!!sym(xvar))
  )

  levs <- levels(data %>% pull(!!sym(xvar)))
  nlevs <- length(levs)
  medsurv <- rep(NA, nlevs)
  for (i in seq_along(levs)) {
    ytmp <- fit[[i]]$est

    if (any(ytmp >= 0.5)) {
      xtmp <- fit[[i]]$time
      x <- max(xtmp[ytmp <= 0.5])
      medsurv[i] <- x
    }
  }

  medsurvprint <- medsurv
  medsurvprint[!is.na(medsurv)] <- paste0(" median: ", dF(medsurv / 365.25, 1)[!is.na(medsurv)], " years")
  medsurvprint[is.na(medsurv)] <- ""
  ytext <- paste0(levs, medsurvprint)

  if (nlevs == 2) {
    mycols <- rep(global_cols[5], 2)
    mylty <- c(1, 2)
    fitsn <- 1:2
  }
  if (nlevs == 3) {
    mycols <- global_cols[c(7, 5, 2)]
    mylty <- rep(1, 3)
    fitsn <- 1:3
  }
  if (nlevs == 6) {
    mycols <- rep(global_cols[c(7, 5, 2)], each = 2)
    mylty <- c(1, 2, 1, 2, 1, 2)
    fitsn <- 1:6
  }

  # c(bottom, left, top, right)
  par(mar = c(4, 4, .5, .5) + 0.1)

  plot(fit[fitsn],
    ylab = eventname,
    col = mycols,
    wh = c(1110, 1110),
    xlim = c(0, 10 * 365),
    ylim = c(0, 1),
    xlab = "Years",
    axes = F,
    lwd = 3,
    lty = mylty,
    xaxs = "i", yaxs = "i"
  )

  axis(2, seq(0, 1, 0.25), seq(0, 100, 25), las = 2)
  axis(1, at = seq(0, 10, 2) * 365, seq(0, 10, 2))

  legend(x = legpos[1], y = legpos[2], ytext, col = mycols, lty = mylty, lwd = 3, bty = "n")

  medsurv[medsurv >= 10 * 365] <- NA
  segments(
    x0 = medsurv, y0 = rep(0, nlevs), y1 = rep(0.5, nlevs),
    lty = mylty, lwd = 3, col = mycols
  )
  abline(h = 0.5, col = 1, lty = 3)
}

mcffunc <- function(data, xvar, eventname, legpos = NULL, ymax = NULL) {
  fits <- mcf(formula(paste0("Recur(sos_outtime, LopNcasecontrol, sos_out) ~ ", xvar)), data = data)

  if (is.null(ymax)) {
    ymax <- ceiling(max(fits@MCF$MCF[fits@MCF$time <= 365 * 10]))
  }

  ytext <- levels(data %>% pull(!!sym(xvar)))

  if (length(ytext) == 2) {
    mycols <- rep(global_cols[5], 2)
    mylty <- c(1, 2)
  }
  if (length(ytext) == 3) {
    mycols <- global_cols[c(7, 5, 2)]
    mylty <- rep(1, 3)
  }
  if (length(ytext) == 6) {
    mycols <- rep(global_cols[c(7, 5, 2)], each = 2)
    mylty <- c(1, 2, 1, 2, 1, 2)
  }

  # c(bottom, left, top, right)
  par(mar = c(4, 4, .5, .5) + 0.1)

  plot(fits@MCF$time[fits@MCF[xvar] == ytext[1]],
    fits@MCF$MCF[fits@MCF[xvar] == ytext[1]],
    type = "l",
    ylab = eventname,
    col = mycols[1],
    xlim = c(0, 10 * 365),
    ylim = c(0, ymax),
    xlab = "Years",
    axes = F,
    lwd = 3,
    lty = mylty[1],
    xaxs = "i", yaxs = "i"
  )

  for (i in 2:length(ytext)) {
    lines(fits@MCF$time[fits@MCF[xvar] == ytext[i]],
      fits@MCF$MCF[fits@MCF[xvar] == ytext[i]],
      col = mycols[i],
      lwd = 3,
      lty = mylty[i]
    )
  }

  axis(2, seq(0, ymax, 1), seq(0, ymax, 1), las = 2)
  axis(1, at = seq(0, 10, 1) * 365, seq(0, 10, 1))

  if (is.null(legpos)) legpos <- c(50, ymax)

  legend(x = legpos[1], y = legpos[2], ytext, col = mycols, lty = mylty, lwd = 3, bty = "n")
}
