

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
#' @examples 
#' # Not yet.
#' @importFrom stats lm fitted.values
#' @author Marius Pascariu
#' @export
#' 
DoubleGap <- function(LT_female, LT_male, age = 0, country, years, 
                      arima.order = c(0, 1, 0), drift = FALSE,
                      tau = NULL, A = NULL) {
  
  input <- c(as.list(environment())) # save for later use
  dta   <- prepare.data(input) # preliminary data preparation
  table_w_records <- find.record.ex(data = dta)
  
  # Fit linear model for best practice life expectancy
  t_ = dta$time_index1
  fit_bp <- lm(table_w_records$ex ~ t_)
  
  # Fit best-practice gap model
  fit_bp_gap <- bp_gap.model(data = dta, 
                             benchmark = fitted.values(fit_bp),
                             arima.order,
                             drift)
  # Fit sex-gap model
  fit_sex_gap <- sex_gap.model(dta)
  # Output object
  out <- structure(class = 'DoubleGap',
                   list(data_input = input, data = dta, 
                        bp_model = fit_bp, 
                        bp_gap_model = fit_bp_gap,
                        sex_gap_model = fit_sex_gap))
  out$call <- match.call()
  return(out)
}

#' The role of this function is to prepare data in such a way that is ready to use
#' right away in the other functions. 
#' @keywords internal
#' 
prepare.data <- function(data) {
  with(data, {
    input <- c(as.list(environment()))
    
    ltf <- data.frame(sex = 'Female', LT_female)
    ltf_country <- ltf[ltf$country == country & ltf$Age == age & ltf$Year %in% years, ]
    years_ <- sort(ltf_country$Year)
    if (length(years) != length(years_)) {
      print(paste0(country, ' does not have data for all years. \nThe years vector was adjusted.'))}
    
    ltm <- data.frame(sex = 'Male', LT_male)
    LT  <- rbind(ltf, ltm)
    LT  <- LT[LT$Age == age & LT$Year %in% years_, ]
    LT  <- LT[complete.cases(LT), ]
    
    countries <- unique(LT$country) 
    
    cols <- c('country', 'Year', 'Age')
    gap <- data.frame(LT_female[, cols], exf = ltf$ex, exm = ltm$ex,  
                      sex_gap = ltf$ex - ltm$ex)
    gap <- gap[gap$Age == age & gap$Year %in% years_, ]
    gap <- gap[complete.cases(gap), ]
    
    L_ = 0 
    U_ = max(gap$sex_gap) # minimum sex-gap observed at age x
    
    ti1 <- years_ - min(years_) + 1
    
    out <- list(input = input, tab_LT = LT, tab_sex_gap = gap,
                L_ = L_, U_ = U_, countries = countries, 
                years = years_, time_index1 = ti1)
    return(out)
  })
}

#' Find record life expectancy at age x given a set of life tables
#' @keywords internal
#' 
find.record.ex <- function(data) {
  dta_ <- data$tab_LT
  tbl  <- data.frame()
  yr   <- sort(unique(dta_$Year))
  for (i in 1:length(yr)) {
    record_ex_i <- max(dta_[dta_$Year == yr[i], 'ex'])
    record_i    <- dta_[dta_$Year == yr[i] & dta_$ex == record_ex_i, ][1, ]
    tbl         <- rbind(tbl, record_i)
  }
  cols <- c('country', 'Year', 'sex', 'ex')
  
  out <- tbl[complete.cases(tbl), cols]
  return(out)
}

#' Fit sex-gap model. This is a modified version of the Raftery linear model
#' for sex differences in life expectancy. This is kind of a AR(2)-X
#' sex_gap ~ sex_gap1 + sex_gap2 + narrow_level
#' @importFrom crch crch
#' @importFrom stats complete.cases
#' @keywords internal
#' 
sex_gap.model <- function(data){
  
  tau <- data$input$tau
  A   <- data$input$A
  if (is.null(tau) | is.null(A)) tauA = find_tau(data)
  if (is.null(tau)) tau = tauA$tau
  if (is.null(A)) A = tauA$A
  
  dt_ <- data$tab_sex_gap
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
  
  dta_c <- data$tab_sex_gap
  dta_c <- dta_c[dta_c$country == data$input$country, ]
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
  dta <- data$tab_sex_gap
  
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




