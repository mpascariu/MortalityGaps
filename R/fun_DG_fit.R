

#' @title Fit DoubleGap model 
#' @description Fit DoubleGap model 
#' @param LT_female Life tables for females
#' @param LT_male Life tables for males
#' @param age Age. The life expectancy model will be fitted at the indicated age.
#' @param country Country. Type: character
#' @param years Period of time to be used. Type: numeric vector.
#' @param arima.order A specification of the the ARIMA model to be used in 
#' fitting the best-practice gap. The ARIMA order is country specific.
#' The three integer components (p, d, q) are the AR order, 
#' the degree of differencing, and the MA order. 
#' @param drift Indicate whether the ARIMA model should include a linear drift 
#' term or not. Type: logical value.
#' @param tau The level of female life expectancy at which 
#' the sex-gap is expected to stop widening and to start narrowing. If \code{NULL} 
#' then the model will run an algorithm to find it. 
#' @param A The level of female life expectancy where we assume no further 
#' change in the sex-gap.
#' @return A \code{DoubleGap} object
#' @importFrom stats lm fitted.values
#' @seealso \code{\link{predict.DoubleGap}}
#' @author Marius Pascariu
#' @examples 
#' library(DoubleGap)
#' 
#' # Fit model
#' fit_model <- DoubleGap(LT_female = hmdlt$LTF, 
#'                        LT_male = hmdlt$LTM, 
#'                        age = 0, 
#'                        country = "SWE", 
#'                        years = 1950:2014, 
#'                        arima.order = c(2, 1, 1), 
#'                        drift = TRUE, 
#'                        tau = 75, 
#'                        A = 86)
#' ls(fit_model)
#' summary(fit_model)
#' 
#' # Predict model 
#' forecast_model <- predict(fit_model, last_forecast_year = 2050)
#' forecast_model
#' 
#' @export
DoubleGap <- function(LT_female, LT_male, age = 0, country, years, 
                      arima.order = c(0, 1, 0), drift = FALSE,
                      tau = NULL, A = NULL) {
  
  input <- c(as.list(environment())) # save for later use
  dta   <- prepare_data(input) # preliminary data preparation
  
  # M1: Fit linear model for best practice life expectancy
  year = dta$time_index1
  fit_bp <- lm(dta$record.life.expectancy.data$ex ~ year)
  # M2: Fit best-practice gap model
  fit_bp_gap <- bp_gap.model(data = dta, 
                             benchmark = fitted.values(fit_bp),
                             arima.order,
                             drift)
  # M3: Fit sex-gap model
  fit_sex_gap <- sex_gap.model(dta)
  # Model parts - a list containing all the details of the model
  parts <- list(bp_model = fit_bp, 
                bp_gap_model = fit_bp_gap, 
                sex_gap_model = fit_sex_gap)
  
  # coefficients
  coef = list(bp_model = fit_bp$coefficients, 
              bp_gap_model = fit_bp_gap$model$coef,
              sex_gap_model = c(fit_sex_gap$model$coefficients[[1]], 
                                tau = fit_sex_gap$tau, 
                                A = fit_sex_gap$A),
              sex_gap_bounds = c(L = dta$L_, U = dta$U_))
  
  # fitted values, observed values and residuals (life expectancies and gaps)
  fv  = find_fitted_values(M1 = fit_bp, 
                           M2 = fit_bp_gap, 
                           M3 = fit_sex_gap)
  ov  = find_observed_values(dta)
  res = cbind(ov[, 1:3], ov[, 4:8] - fv[, 4:8])
  
  # Output object
  out <- structure(class = 'DoubleGap',
                   list(data = dta, coefficients = coef, 
                        fitted.values = fv, observed.values = ov, 
                        residuals = res, model.parts = parts))
  out$call <- match.call()
  return(out)
}


#' The role of this function is to prepare data in such a way that is ready to use
#' right away in the other functions. 
#' @keywords internal
#' 
prepare_data <- function(data) {
  with(data, {
    input <- c(as.list(environment()))
    
    ltf <- data.frame(sex = 'Female', LT_female)
    ltf_country <- ltf[ltf$country == country & ltf$Age == age & ltf$Year %in% years, ]
    years_ <- sort(ltf_country$Year)
    if (length(years) != length(years_)) {
      warning(country, ' does not have data for all the specified years.', 
              ' The years vector was adjusted.')
      }
    
    ltm <- data.frame(sex = 'Male', LT_male)
    LT  <- rbind(ltf, ltm)
    LT  <- LT[LT$Age == age & LT$Year %in% years_, ]
    LT  <- LT[complete.cases(LT), ]
    REX <- find_record_ex(LT)
    
    all_countries <- unique(LT$country) 
    cols <- c('country', 'Year', 'Age')
    gap <- data.frame(LT_female[, cols], exf = ltf$ex, exm = ltm$ex,  
                      sex_gap = ltf$ex - ltm$ex)
    gap <- gap[gap$Age == age & gap$Year %in% years_, ]
    gap <- gap[complete.cases(gap), ]
    
    L_ = 0 
    U_ = max(gap$sex_gap) # minimum sex-gap observed at age x
    
    ti1 <- years_ - min(years_) + 1
    
    out <- list(record.life.expectancy.data = REX,
                life.expectancy.data = gap,
                L_ = L_, U_ = U_,
                age = age, tau = tau, A = A,
                country = country,
                countries = all_countries, 
                years = years_, time_index1 = ti1)
    return(out)
  })
}

#' Find record life expectancy at age x given a set of life tables
#' @keywords internal
#' 
find_record_ex <- function(data) {
  tbl  <- data.frame()
  yr   <- sort(unique(data$Year))
  for (i in 1:length(yr)) {
    record_ex_i <- max(data[data$Year == yr[i], 'ex'])
    record_i    <- data[data$Year == yr[i] & data$ex == record_ex_i, ][1, ]
    tbl         <- rbind(tbl, record_i)
  }
  cols <- c('country', 'Year', 'sex', 'ex')
  
  out <- tbl[complete.cases(tbl), cols]
  return(out)
}

#' Find the fitted values of the DoubleGap model
#' @keywords internal
#' 
find_fitted_values <- function(M1, M2, M3) {
  cntr   <- as.character(M2$modelled_data$country[1])
  age_   <- as.character(M2$modelled_data$Age[1])
  years_ <- M2$modelled_data$Year
  
  bp_ex  = M1$fitted.values
  exf    = bp_ex - M2$model$fitted
  bp_gap = bp_ex - exf
  
  dta   <- M3$modelled_data
  dta2  <- dta[dta$country == cntr, ]
  dta3  <- data.frame(Intercept = 1, dta2[, 7:9])
  coef_ <- M3$model$coefficients[[1]]
  
  sex_gap = c(NA, NA, apply(dta3, 1, function(x) as.numeric(x) %*% coef_))
  exm     = exf - sex_gap
  
  out <- data.frame(country = cntr, Year = years_, Age = age_, 
                    exf = exf, exm = exm, sex_gap,
                    bp_ex = bp_ex, bp_gap = bp_gap)
  return(out)
}

#' Find the observed values (from input data). Format.
#' @keywords internal
#'
find_observed_values <- function(x) {
  A <- x$record.life.expectancy.data
  B <- x$life.expectancy.data
  
  out <- B[B$country == x$country, ]
  out$bp_ex <- A$ex
  out$bp_gap <- out$bp_ex - out$exf
  return(out)
}

#' Fit sex-gap model. This is a modified version of the Raftery linear model
#' for sex differences in life expectancy. This is kind of a AR(2)-X
#' sex_gap ~ sex_gap1 + sex_gap2 + narrow_level
#' @importFrom crch crch
#' @importFrom stats complete.cases coef
#' @keywords internal
#' 
sex_gap.model <- function(data){
  
  tau <- data$tau
  A   <- data$A
  if (is.null(tau) | is.null(A)) tauA = find_tau(data)
  if (is.null(tau)) tau = tauA$tau
  if (is.null(A)) A = tauA$A
  
  dt_ <- data$life.expectancy.data
  dt_$sex_gap2 = dt_$sex_gap1 <- NA
  dt_$narrow_level <- pmax(0, dt_$exf - tau)
  
  all_countries <- data$countries
  for (i in 1:length(all_countries)) { # identify lag values
    cou <- all_countries[i]
    dt_[dt_$country == cou, 'sex_gap1'] = c(NA, rev(rev(dt_[dt_$country == cou, 'sex_gap'])[-1]))
    dt_[dt_$country == cou, 'sex_gap2'] = c(NA, NA, rev(rev(dt_[dt_$country == cou, 'sex_gap'])[-(1:2)]))
  }
  modelled_data <- dt_[complete.cases(dt_), ]
  
  # Fit the Raftery model using crch package in order to have Gaussian errors
  mdl <- crch::crch(sex_gap ~ sex_gap1 + sex_gap2 + narrow_level, 
                    data = modelled_data, 
                    dist = "gaussian", 
                    left = 0)
  out <- list(model = mdl, modelled_data = modelled_data, tau = tau, A = A)
  return(out)
}


#' Time series model for the best-practice gap
#' @keywords internal
#' 
bp_gap.model <- function(data, benchmark, arima.order, drift) {
  
  dta_c <- data$life.expectancy.data
  dta_c <- dta_c[dta_c$country == data$country, ]
  dta_c$bp_ex  <- benchmark
  dta_c$bp_gap <- dta_c$bp_ex - dta_c$exf
  
  mdl <- Arima(y = dta_c$bp_gap, order = arima.order, include.drift = drift)
  out <- list(model = mdl, modelled_data = dta_c)
  return(out)
}

#' Function to find the value of tau
#' 
#' Function to find the value of female life expectancy where the sex-gap is no 
#' longer expanding (tau). This method is slightly different than the one indicated 
#' in the original paper (likelihood). The main advantage: speed.
#' @importFrom stats median
#' @keywords internal
#' 
find_tau <- function(data, f_ = 1.05) {
  countries <- as.character(data$countries)
  dta <- data$life.expectancy.data
  
  tab <- data.frame(countries, tau = NA, A = NA)
  for (k in 1:length(countries)) {
    cou_k <- countries[k]
    dta_ <- dta[dta$country == cou_k, ]
    smooth_curve <- with(dta_, lowess(exf, sex_gap, f = 0.5))
    max_y <- max(smooth_curve$y)
    tab$tau[k] <- smooth_curve$x[smooth_curve$y == max_y][1]
    
    yr <- dta_[dta_$sex_gap == max(dta_$sex_gap), 'Year'][1]
    tab$A[k] <- max(dta_[dta_$Year >= yr, 'exf'])*f_
    
  }
  tau <- median(tab$tau)
  A   <- median(tab$A)
  out <- list(tau = tau, A = A)
  return(out)
}




