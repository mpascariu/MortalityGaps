
#' @title Fit the DoubleGap Life Expectancy Forecasting Model 
#' @description Fit a DoubleGap model for forecasting life expectancy.
#' The method combines separate forecasts to obtain joint male and female 
#' life expectancies that are coherent with a best-practice trend. See the entire 
#' description and mathematical formulation of the model in 
#' \href{https://doi.org/10.1016/j.insmatheco.2017.09.011}{Pascariu et. al (2017)}. 
#' @param DF data.frame containing life expectancy records for females. 
#' The table must contain the following 4 columns: country, year, age, ex.
#' @param DM data.frame containing life expectancy records for males. 
#' The table must have the same format and dimensions as \code{DF}. 
#' @param age Indicate the age for which the model to be fitted. 
#' Assuming \code{DF} and \code{DM} contain recods for different ages, this 
#' argument it is used to subset the data. If you want to fit the model for age 0, 
#' add age = 0. Type: scalar.
#' @param country Indicate for which contry you want to fit the model. The country
#' name or code must exist in \code{DF} and \code{DM}.
#' Type: character.
#' @param years Period of time to be used. Type: numeric vector.
#' @param arima.order A specification of the the ARIMA model to be used in 
#' fitting the best-practice gap. The ARIMA order is country specific.
#' The three integer components (p, d, q) are the AR order, 
#' the degree of differencing, and the MA order. Format: numerical vector of length 3.
#' If \code{arima.order = NULL} the function conducts a search over possible models 
#' according to AIC. See \code{\link{auto.arima}} for details.
#' @param drift Indicate whether the ARIMA model should include a linear drift 
#' term or not. Type: logical value. If \code{drift = NULL}, it will be estimate 
#' automatically.
#' @param tau The level of female life expectancy at which the sex-gap is 
#' expected to stop widening and to start narrowing. If \code{NULL} 
#' then the model will run an algorithm to find it. 
#' @param A The level of female life expectancy where we assume no further 
#' change in the sex-gap. If \code{NULL} the model will estimate it. 
#' @return The output is of \code{"DoubleGap"} class with the components:
#' @return \item{input}{List with arguments provided in input. Saved for convenience.}
#' @return \item{call}{Short information about the model.}
#' @return \item{coefficients}{Estimated coefficients.}
#' @return \item{fitted.values}{Fitted values of the selected model.}
#' @return \item{observed.values}{Country specific observed values.}
#' @return \item{model.parts}{Object containing detailed results of the fitted model.} 
#' @return \item{residuals}{Deviance residuals.} 
#' @seealso \code{\link{predict.DoubleGap}}
#' @author Marius D. Pascariu
#' @references 
#' Pascariu M.D., Canudas-Romo V. and Vaupel W.J. 2017. 
#' \href{https://doi.org/10.1016/j.insmatheco.2017.09.011}{
#' The double-gap life expectancy forecasting model.}
#' Insurance: Mathematics and Economics Volume 78, January 2018, Pages 339-350.
#' @examples 
#' # Input data ------------------------------------
#' # Collection of life expectancies for female populations
#' exF <- MortalityGaps.data$exF
#' # Life expectancy for male populations
#' exM <- MortalityGaps.data$exM
#' 
#' # Example 1 ----------------------------------------------
#' # Fit DG model at age 0 for Australia using data from 1950 to 2014
#' M0 <- DoubleGap(DF = exF,
#'                 DM = exM,
#'                 age = 0,
#'                 country = "AUS",
#'                 years = 1950:2014)
#' M0
#' summary(M0)
#' ls(M0)
#' 
#' # Forecast life expectancy in Australia until 2030
#' P0 <- predict(M0, h = 16)
#' P0
#' # Plot the results
#' plot(P0)
#' 
#' \dontrun{
#' # Example 2 ----------------------------------------------
#' # Fit DG model at age 0 for Sweden. Provide details about models.
#' # Reproduce published results in the article.
#' M1 <- DoubleGap(DF = exF, 
#'                 DM = exM, 
#'                 age = 0, 
#'                 country = "SWE", 
#'                 years = 1950:2014, 
#'                 arima.order = c(2, 1, 1), 
#'                 drift = TRUE, 
#'                 tau = 75, 
#'                 A = 86)
#' summary(M1)
#' # Predict model 
#' P1 <- predict(M1, h = 36)
#' plot(P1)
#' 
#' # Example 3 ----------------------------------------------
#' # Fit DG model for USA at age 65.
#' M2 <- DoubleGap(DF = exF, 
#'                 DM = exM, 
#'                 age = 65, 
#'                 country = "USA", 
#'                 years = 1950:2014, 
#'                 arima.order = c(0, 1, 0), 
#'                 drift = FALSE, 
#'                 tau = 15, 
#'                 A = 24)
#' summary(M2)
#' # Predict model 
#' P2 <- predict(M2, h = 36)
#' plot(P2)
#' }
#' @export
DoubleGap <- function(DF, DM, age, country, years, 
                      arima.order = NULL, drift = NULL,
                      tau = NULL, A = NULL) {
  
  input <- c(as.list(environment())) # save for later use
  dta   <- prepare_data(input) # preliminary data preparation
  # M1: Fit linear model for best practice life expectancy
  year   <- dta$time_index1
  fit_M1 <- lm(dta$record.life.expectancy.data$ex ~ year)
  # M2: Fit best-practice gap model
  fit_M2 <- bp_gap.model(dta, benchmark = fitted(fit_M1), arima.order, drift)
  # M3: Fit sex-gap model
  fit_M3 <- sex_gap.model(dta, tau, A)
  # Model parts - a list containing all the details of the model
  parts <- list(bp_model = fit_M1, 
                bp_gap_model = fit_M2, 
                sex_gap_model = fit_M3)
  # coefficients
  coef <- list(bp_model = fit_M1$coefficients, 
               bp_gap_model = fit_M2$model$coef,
               sex_gap_model = c(fit_M3$model$coefficients[[1]], 
                                 tau = fit_M3$tau, A = fit_M3$A),
               sex_gap_bounds = c(L = dta$L_, U = dta$U_))
  
  # fitted values, observed values and residuals (life expectancies and gaps)
  fv  <- find_fitted_values(M1 = fit_M1, M2 = fit_M2, M3 = fit_M3)
  ov  <- find_observed_values(dta)
  res <- cbind(ov[, 1:3], ov[, 4:8] - fv[, 4:8])
  
  # Output object
  out <- structure(class = 'DoubleGap',
                   list(input = dta, coefficients = coef, 
                        fitted.values = fv, observed.values = ov, 
                        residuals = res, model.parts = parts))
  out$call <- match.call()
  return(out)
}


#' The role of this function is to prepare data in such a way that is 
#' ready to use right away in the other functions.
#' @param data Input data from DoubleGap function.
#' @keywords internal
prepare_data <- function(data) {
  with(data, {
    
    if (!all(dim(DF) == dim(DM))) {
      stop("the input data-sets do not have the same dimension.", call. = F)
    }
    if (!any(DF$Age %in% age)) {
      stop("age ", age, " cannot be found in input data.", call. = F)
    }
    if (!any(DF$country %in% country)) {
      stop("country ", country, " cannot be found in input data.", call. = F)
    }
    
    eF <- data.frame(sex = 'Female', DF)
    eM <- data.frame(sex = 'Male', DM)
    ltk <- eF[eF$country == country & eF$Age == age & eF$Year %in% years, ]
    yr  <- sort(ltk$Year)
    if (length(years) != length(yr)) {
      warning(country, " does not have data for all the specified years.", 
              " The 'years' vector has been adjusted.", call. = F)
    }
    eT   <- rbind(eF, eM)
    eT   <- eT[eT$Age == age & eT$Year %in% yr, ]
    eT   <- eT[complete.cases(eT), ]
    REX  <- find_record_ex(eT)
    cols <- c('country', 'Year', 'Age')
    gap  <- data.frame(DF[, cols], exf = eF$ex, exm = eM$ex,  
                       sex_gap = eF$ex - eM$ex)
    gap  <- gap[gap$Age == age & gap$Year %in% yr, ]
    gap  <- gap[complete.cases(gap), ]
    L_   <- min(gap$sex_gap) # minimum sex-gap observed at age x
    U_   <- max(gap$sex_gap) 
    ti1  <- yr - min(yr) + 1 # time index
    
    out <- list(record.life.expectancy.data = REX,
                life.expectancy.data = gap,
                L_ = L_, U_ = U_, age = age,
                country = country,
                countries = unique(eT$country), 
                years = yr, time_index1 = ti1)
    return(out)
  })
}


#' Find record life expectancy at age x given a set of life tables
#' @param X Data-set containing ex records for female and male populations. 
#' @keywords internal
find_record_ex <- function(X) {
  tbl  <- data.frame()
  yr   <- sort(unique(X$Year))
  for (i in 1:length(yr)) {
    record_ex_i <- max(X[X$Year == yr[i], 'ex'])
    record_i    <- X[X$Year == yr[i] & X$ex == record_ex_i, ][1, ]
    tbl         <- rbind(tbl, record_i)
  }
  cols <- c('country', 'Year', 'sex', 'ex')
  out  <- tbl[complete.cases(tbl), cols]
  return(out)
}


#' Find the fitted values of the DoubleGap model
#' @param M1 Model 1 - Linear regression bp-life-expectancy.
#' @param M2 Model 2 - Time-series D-gap.
#' @param M3 Model 3 - Sex-gap.
#' @keywords internal
find_fitted_values <- function(M1, M2, M3) {
  cntr    <- as.character(M2$modelled_data$country[1])
  age_    <- as.character(M2$modelled_data$Age[1])
  years_  <- M2$modelled_data$Year
  bp_ex   <- M1$fitted.values
  exf     <- bp_ex - M2$model$fitted
  bp_gap  <- bp_ex - exf
  dta     <- M3$modelled_data
  dta2    <- dta[dta$country == cntr, ]
  dta3    <- data.frame(Intercept = 1, dta2[, 7:9])
  coef_   <- M3$model$coefficients[[1]]
  sex_gap <- c(NA, NA, apply(dta3, 1, function(x) as.numeric(x) %*% coef_))
  exm     <- exf - sex_gap
  
  out <- data.frame(country = cntr, Year = years_, Age = age_, 
                    exf = exf, exm = exm, sex_gap,
                    bp_ex = bp_ex, bp_gap = bp_gap)
  return(out)
}


#' Find the observed values (from input data). Format.
#' @inheritParams sex_gap.model
#' @keywords internal
find_observed_values <- function(X) {
  A   <- X$record.life.expectancy.data
  B   <- X$life.expectancy.data
  out <- B[B$country == X$country, ]
  out$bp_ex  <- A$ex
  out$bp_gap <- out$bp_ex - out$exf
  return(out)
}


#' Fit sex-gap model. This is a modified version of the Raftery linear model
#' for sex differences in life expectancy. This is kind of a AR(2)-X
#' sex_gap ~ sex_gap1 + sex_gap2 + narrow_level
#' @param X Input data object generated by prepare_data function.
#' @inheritParams DoubleGap
#' @keywords internal
sex_gap.model <- function(X, tau, A){
  
  if (is.null(tau) | is.null(A)) tauA = find_tau(X)
  if (is.null(tau)) tau = tauA$tau
  if (is.null(A))   A = tauA$A
  
  W <- X$life.expectancy.data
  W$sex_gap2     <- W$sex_gap1 <- NA
  W$narrow_level <- pmax(0, W$exf - tau)
  
  all_countries <- X$countries
  for (i in 1:length(all_countries)) { # identify lag values
    cou <- all_countries[i]
    W[W$country == cou, 'sex_gap1'] <- c(NA, rev(rev(W[W$country == cou, 'sex_gap'])[-1]))
    W[W$country == cou, 'sex_gap2'] <- c(NA, NA, rev(rev(W[W$country == cou, 'sex_gap'])[-(1:2)]))
  }
  modelled_data <- W[complete.cases(W), ]
  
  # Fit the Raftery model using crch package in order to have Gaussian errors
  mdl <- crch::crch(sex_gap ~ sex_gap1 + sex_gap2 + narrow_level, 
                    data = modelled_data, dist = "gaussian", left = 0)
  out <- list(model = mdl, modelled_data = modelled_data, tau = tau, A = A)
  return(out)
}


#' Time series model for the best-practice gap
#' @inheritParams DoubleGap
#' @inheritParams sex_gap.model
#' @param benchmark Fitted values given by BP linear model.
#' @keywords internal
bp_gap.model <- function(X, benchmark, arima.order, drift) {
  Z <- X$life.expectancy.data
  Z <- Z[Z$country == X$country, ]
  Z$bp_ex  <- benchmark
  Z$bp_gap <- Z$bp_ex - Z$exf
  
  ts_auto = auto.arima(Z$bp_gap)
  if (is.null(arima.order)) arima.order = arimaorder(ts_auto)
  if (is.null(drift)) drift = any(names(coef(ts_auto)) %in% "drift")
  
  mdl <- Arima(y = Z$bp_gap, order = arima.order, include.drift = drift)
  out <- list(model = mdl, modelled_data = Z)
  return(out)
}


#' Function to find the value of tau
#' 
#' Function to find the value of female life expectancy where the sex-gap is no 
#' longer expanding (tau). This method is slightly different than the one indicated 
#' in the original paper (likelihood). The main advantage: speed.
#' @inheritParams sex_gap.model
#' @inheritParams stats::lowess 
#' @param a Adjustment factor for maximum female life expectancy.
#' @keywords internal
find_tau <- function(X, a = 1.05, f = 0.5) {
  K <- as.character(X$countries)
  Z <- X$life.expectancy.data
  
  tab <- data.frame(K, tau = NA, A = NA)
  for (i in 1:length(K)) {
    cou_k      <- K[i]
    W          <- Z[Z$country == cou_k, ]
    sc         <- with(W, lowess(x = exf, y = sex_gap, f = f)) #smooth_curve
    max_y      <- max(sc$y)
    yr         <- W[W$sex_gap == max(W$sex_gap), 'Year'][1]
    tab$tau[i] <- sc$x[sc$y == max_y][1]
    tab$A[i]   <- max(W[W$Year >= yr, 'exf']) * a
  }
  tau <- median(tab$tau)
  A   <- median(tab$A)
  out <- list(tau = tau, A = A)
  return(out)
}


#' Print DoubleGap
#' @param x Object of class \code{DoubleGap}.
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.DoubleGap <- function(x, ...) {
  cat("Double-Gap Model fit\n")
  cat("\nCountry     : ", x$input$country)
  cat("\nAge (x)     : ", x$input$age)
  cat("\nYears in fit: ", paste(range(x$input$years), collapse = ' - '))
  cat("\n\n")
}


#' Summary DoubleGap
#' @param object Object of class \code{DoubleGap}
#' @inheritParams print.DoubleGap
#' @keywords internal
#' @export
summary.DoubleGap <- function(object, ...) {
  C_M1 <- summary(object$model.parts$bp_model)$coefficients
  C_M2 <- object$model.parts$bp_gap_model$model$coef
  C_M3 <- summary(object$model.parts$sex_gap_model$model)$coefficients[[1]]
  coef <- coef(object)
  out  <- structure(class = 'summary.DoubleGap',
                    list(coef_bp_model = C_M1, 
                         coef_bp_gap_model = C_M2,
                         coef_sex_gap_model = C_M3,
                         coef = coef))
  return(out)
}


#' Print summary
#' @inheritParams print.DoubleGap
#' @keywords internal
#' @export
print.summary.DoubleGap <- function(x, ...){
  cat("\nCoefficients Double-Gap Model:\n")
  cat("\nM1: Best-Practice Life Expectancy Model\n")
  printCoefmat(x$coef_bp_model, signif.legend = FALSE)
  cat("\nM2: Best-Practice Gap Model (ARIMA)\n")
  print(x$coef_bp_gap_model)
  cat("\nM3: Sex-Gap Model\n")
  printCoefmat(x$coef_sex_gap_model)
  cat("\ntau =", paste0(x$coef$sex_gap_model['tau']))
  cat(" | A =", paste0(x$coef$sex_gap_model['A']))
  cat(" | L =", round(x$coef$sex_gap_bounds['L'], 2))
  cat(" | U =", round(x$coef$sex_gap_bounds['U'], 2))
  cat("\n\n")
}



