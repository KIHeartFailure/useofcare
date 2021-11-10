kmfunc <- function(data, xvar, time, event, eventname, yposplus = NULL) {
  fit <- survfit(formula(paste0("Surv(", time, ",", event, "=='Yes') ~ ", xvar)),
    data = data
  )

  ytext <- levels(data %>% pull(!!sym(xvar)))

  if (length(ytext) == 2) {
    mycols <- rep(global_kicols[1], 2)
    mylty <- c(1, 2)
  }
  if (length(ytext) == 3) {
    mycols <- global_kicols[c(1, 2, 3)]
    mylty <- rep(1, 3)
  }
  if (length(ytext) == 6) {
    mycols <- rep(global_kicols[c(1, 2, 3)], each = 2)
    mylty <- c(1, 2, 1, 2, 1, 2)
  }

  # c(bottom, left, top, right)
  par(mar = c(5, 4, 4, 7) + 0.1)
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

  ypos <- 1 - summary(fit, 365 * 10, extend = TRUE)$surv

  ytext <- levels(data %>% pull(!!sym(xvar)))

  if (is.null(yposplus)) yposplus <- rep(0, length(ytext))
  ylabs <- bind_cols(ypos = ypos + yposplus, ytext = ytext)

  mtext(
    side = 4,
    line = .2,
    at = ylabs$ypos,
    ylabs$ytext,
    las = 1
  )
}

cifunc <- function(data, xvar, time, event, eventname, yposplus = NULL) {
  fit <- cuminc(
    ftime = data %>% pull(!!sym(time)),
    fstatus = data %>% pull(!!sym(event)),
    cencode = 0,
    group = data %>% pull(!!sym(xvar))
  )

  ytext <- levels(data %>% pull(!!sym(xvar)))

  if (length(ytext) == 2) {
    mycols <- rep(global_kicols[1], 2)
    mylty <- c(1, 2)
    fitsn <- 1:2
  }
  if (length(ytext) == 3) {
    mycols <- global_kicols[c(1, 2, 3)]
    mylty <- rep(1, 3)
    fitsn <- 1:3
  }
  if (length(ytext) == 6) {
    mycols <- rep(global_kicols[c(1, 2, 3)], each = 2)
    mylty <- c(1, 2, 1, 2, 1, 2)
    fitsn <- 1:6
  }

  # c(bottom, left, top, right)
  par(mar = c(5, 4, 4, 7) + 0.1)

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

  ypos <- timepoints(fit[fitsn], 365 * 10)$est

  ytext <- levels(pdata %>% pull(!!sym(xvar)))

  if (is.null(yposplus)) yposplus <- rep(0, length(ytext))

  ylabs <- bind_cols(ypos = ypos + yposplus, ytext = ytext)

  mtext(
    side = 4,
    line = .2,
    at = ylabs$ypos,
    ylabs$ytext,
    las = 1
  )
}

mcffunc <- function(data, xvar, eventname, yposplus = NULL, ymax = NULL) {
  fits <- mcf(formula(paste0("Recur(sos_outtime, LopNcasecontrol, sos_out) ~ ", xvar)), data = data)

  if (is.null(ymax)) {
    ymax <- ceiling(max(fits@MCF$MCF[fits@MCF$time <= 365 * 10]))
  }

  ytext <- levels(data %>% pull(!!sym(xvar)))

  if (length(ytext) == 2) {
    mycols <- rep(global_kicols[1], 2)
    mylty <- c(1, 2)
  }
  if (length(ytext) == 3) {
    mycols <- global_kicols[c(1, 2, 3)]
    mylty <- rep(1, 3)
  }
  if (length(ytext) == 6) {
    mycols <- rep(global_kicols[c(1, 2, 3)], each = 2)
    mylty <- c(1, 2, 1, 2, 1, 2)
  }

  ypos <- rep(NA, length(ytext))

  # c(bottom, left, top, right)
  par(mar = c(5, 4, 4, 7) + 0.1)

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

  ypos[1] <- last(fits@MCF$MCF[fits@MCF$time <= 365 * 10 & fits@MCF[xvar] == ytext[1]])

  for (i in 2:length(ytext)) {
    lines(fits@MCF$time[fits@MCF[xvar] == ytext[i]],
      fits@MCF$MCF[fits@MCF[xvar] == ytext[i]],
      col = mycols[i],
      lwd = 3,
      lty = mylty[i]
    )
    ypos[i] <- last(fits@MCF$MCF[fits@MCF$time <= 365 * 10 & fits@MCF[xvar] == ytext[i]])
  }

  axis(2, seq(0, ymax, 1), seq(0, ymax, 1), las = 2)
  axis(1, at = seq(0, 10, 1) * 365, seq(0, 10, 1))

  if (is.null(yposplus)) yposplus <- rep(0, length(ytext))
  ylabs <- bind_cols(ypos = ypos + yposplus, ytext = ytext)

  mtext(
    side = 4,
    line = .2,
    at = ylabs$ypos,
    ylabs$ytext,
    las = 1
  )
}
