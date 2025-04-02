# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Apr  2 07:34:37 2025
# ------------------------------------------------- #

# MortalityGaps: The Double-Gap Life Expectancy Forecasting Model

#' @details 
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "MortalityGaps")}
#' \insertNoCite{*}{MortalityGaps}
#' @importFrom forecast forecast Arima arimaorder auto.arima
#' @importFrom MASS mvrnorm
#' @importFrom crch crch
#' @importFrom stats lm fitted.values fitted complete.cases coef median 
#' cov quantile predict aggregate printCoefmat simulate
#' @importFrom pbapply startpb setpb closepb
#' @importFrom graphics abline axis legend lines par plot points polygon
#' @importFrom Rdpack Rdo_macro
#' @references \insertAllCited{}
#' @name MortalityGaps
#' @aliases NULL
#' @docType package
"_PACKAGE"


