
#' Generic Plot Function for Class predict.DoubleGap
#' 
#' @param x An object of class \code{\link{predict.DoubleGap}}
#' @param show.legend Logical. Indicate whether to display the legend or not. Default: TRUE.
#' @param ylim Numeric vectors of length 2, giving the x and y coordinates ranges.
#' @param asp Numeric, giving the aspect ratio y/x.
#' @param xlab A title for the x axis: see \code{\link{title}}.
#' @param ylab A title for the y axis: see \code{\link{title}}.
#' @param ... Further graphical parameters as in \code{\link{par}}. 
#' @author Marius D. Pascariu
#' @seealso \code{\link{DoubleGap}}
#' @examples 
#' # Complete examples are provided in help page of the DoubleGap function.
#' @export
plot.predict.DoubleGap <- function(x, show.legend = TRUE, ylim = NULL, 
                                   asp = 1.8, xlab = "\nYear", 
                                   ylab = "\nLife Expectancy Level", ...){
  # Observed & fitted data
  fit    <- x$input$object
  bp_exT <- fit$fitted.values$bp_ex # Best-practice trend
  bp_exH <- fit$observed.values$bp_ex # Observed record ex
  exfH   <- fit$observed.values$exf
  exmH   <- fit$observed.values$exm
  hyr    <- fit$input$years # years
  
  # Forecast values
  bp_exF <- x$pred.values$bp_ex
  cif    <- x$pred.intervals$exf # confidence intervals females
  cim    <- x$pred.intervals$exm # confidence intervals males
  fyr    <- x$pred.values$Year #years
  yr     <- c(hyr, fyr) # all years
  
  # Plot
  if (is.null(ylim)) ylim <- trunc(range(bp_exH) * c(.9, 1.1))
  breaks_x1 <- c(min(hyr), round(mean(range(hyr)), -1), max(hyr))
  breaks_x2 <- c(max(hyr) + 0.5, max(fyr))
  breaks_y <- seq(ylim[1], ylim[2], by = 3)
  
  par(cex.lab = 1.4, cex.axis = 1.5)
  plot(hyr, bp_exH, type = "n", axes = F, asp = asp, ylim = ylim, 
       xlim = range(yr), xlab = xlab, ylab = ylab, ...)
  axis(1, at = breaks_x1, lwd = 2, col = 1)
  axis(1, at = breaks_x2, labels = c("", max(breaks_x2)), lwd = 2, col = 4)
  axis(2, at = breaks_y, lwd = 2, col = 1)
  
  abline(v = seq(min(hyr), max(fyr), by = 10), col = "grey80", lty = 3)
  abline(h = seq(ylim[1], ylim[2], by = 1.5), col = "grey80", lty = 3)
  
  polygon(c(rev(hyr), hyr), c(rev(bp_exT), exfH), col = 'mistyrose', 
          border = NA, density = 100, lty = 1, angle = 90)
  polygon(c(rev(hyr), hyr), c(rev(exfH), exmH), col = 'lightcyan1', 
          border = NA, density = 100, lty = 1, angle = 90 )
  
  lines(hyr, bp_exT, lwd = 2, col = 'grey70')
  lines(hyr, exfH, lwd = 2, col = 1, lty = 1)
  lines(hyr, exmH, lwd = 2, col = 1, lty = 1)
  
  polygon(c(rev(fyr), fyr), c(rev(cim[,2]), cim[,5]), col = '#dadaeb', density = 100, angle = 90)
  polygon(c(rev(fyr), fyr), c(rev(cif[,2]), cif[,5]), col = '#dadaeb', density = 100, angle = 90)
  polygon(c(rev(fyr), fyr), c(rev(cim[,3]), cim[,4]), col = '#bcbddc', density = 100, angle = 90)
  polygon(c(rev(fyr), fyr), c(rev(cif[,3]), cif[,4]), col = '#bcbddc', density = 100, angle = 90)
  
  lines(fyr, bp_exF, lwd = 2, col = "grey70", lty = 1)
  points(hyr, bp_exH, pch = 16, cex = 1.2, col = 1) 
  lines(fyr, cif[,1], lwd = 2, col = 4, lty = 2)
  lines(fyr, cim[,1], lwd = 2, col = 4, lty = 2)
  
  abline(v = max(hyr) + 0.3, lty = 2)
  # text(mean(hyr), ylim[1], "Fitted", cex = 1.3)
  # text(mean(fyr), ylim[1], "Forecast", cex = 1.3)
  
  if (show.legend) {
    l_names <- c("Record Life Expectancy", 
                 "Best-Practice Trend",
                 "Obs. F & M Life Expectancy", 
                 "DG Forecast", 
                 "BP-gap (D)", 
                 "Sex-gap (G)",
                 "CI: 80% level", "CI: 95% level")
    legend('topleft', legend = l_names, bty = "n",
           lty = c(NA, 1, 1, 2, NA, NA, NA, NA), 
           pch = c(16, NA, NA, NA, 15, 15, 15, 15),
           col = c(1, "grey70", 1, 4, "mistyrose", "lightcyan1", "#bcbddc", "#dadaeb"), 
           cex = 1, pt.cex = 2.0, lwd = 3)
  }
}



