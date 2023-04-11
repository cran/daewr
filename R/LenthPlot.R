LenthPlot<-
function (obj, alpha = 0.05, plt = TRUE, limits = TRUE, xlab = "factors", 
          ylab = "effects", faclab = NULL, cex.fac = graphics::par("cex.lab"), 
          cex.axis = graphics::par("cex.axis"), adj = 1, ...) 
{
  if (inherits(obj, "stats::lm")) {
    i <- pmatch("(Intercept)", names(stats::coef(obj)))
    if (!is.na(i)) 
      obj <- 2 * stats::coef(obj)[-pmatch("(Intercept)", names(stats::coef(obj)))]
  }
  b <- obj
  if (!is.null(faclab)) {
    if (!is.list(faclab)) 
      stop("* Argument 'faclab' has to be NULL or a list with 'idx' and 'lab' elements")
    names(b) <- rep("", length(b))
    names(b)[faclab$idx] <- faclab$lab
  }
  m <- length(b)
  d <- m/3
  s0 <- 1.5 * stats::median(abs(b))
  cj <- as.numeric(b[abs(b) < 2.5 * s0])
  PSE <- 1.5 * stats::median(abs(cj))
  ME <- stats::qt(1 - alpha/2, d) * PSE
  gamma <- (1 + (1 - alpha)^(1/m))/2
  SME <- stats::qt(gamma, d) * PSE
  if (plt) {
    n <- length(b)
    x <- seq(n)
    ylim <- range(c(b, 1.2 * c(ME, -ME)))
    plot(x, b, xlim = c(1, n + 1), ylim = ylim, type = "n", 
         xlab = xlab, ylab = ylab, frame = FALSE, axes = FALSE, 
         ...)
    idx <- x[names(b) != ""]
    graphics::text(x[idx], rep(graphics::par("usr")[3], length(idx)), labels = names(b)[idx], 
         cex = cex.fac, xpd = NA)
    graphics::axis(2, cex.axis = cex.axis)
    for (i in seq(along = x)) graphics::segments(x[i], 0, x[i], b[i], 
                                       lwd = 3, col = 1, lty = 1)
    graphics::abline(h = 0, lty = 4, xpd = FALSE)
    if (limits) {
      graphics::abline(h = ME * c(1, -1), xpd = FALSE, lty = 2, col = grDevices::grey(0.2))
      graphics::text(adj * (n + 1) * c(1, 1), (ME + graphics::strheight("M", 
                                                    cex = cex.axis)) * c(1, -1), labels = "ME", cex = 0.9 * 
             cex.axis, xpd = FALSE)
      graphics::abline(h = SME * c(1, -1), xpd = FALSE, lty = 3, 
             col = grDevices::grey(0.2))
      graphics::text(adj * (n + 1) * c(1, 1), (SME + graphics::strheight("M", 
                                                     cex = cex.axis)) * c(1, -1), labels = "SME", 
           cex = 0.9 * cex.axis, xpd = FALSE)
    }
  }
  return(c(alpha = alpha, PSE = PSE, ME = ME, SME = SME))
}