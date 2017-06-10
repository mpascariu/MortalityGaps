
#' @title Predict DoubleGap model
#' @description Predict DoubleGap model
#' @param object A DoubleGap object
#' @param last_forecast_year Last year to be included in the forecast
#' @param iter Number of iterations. Default: 500
#' @param ci Confidence levels. Default: c(0.8, 0.95)
#' @param ... ...
#' @return A list containing predicted value of best-practice
#' life expectancy, best-practice gap, sex gap, and forecast life expectancy 
#' for females and males (together with prediction intervals).
#' @seealso \code{\link{DoubleGap}}
#' @author Marius Pascariu
#' @export
#'  
predict.DoubleGap <- function(object, last_forecast_year, 
                              iter = 500, ci = c(0.8, 0.95), ...) {
  x <- prepare_data_for_prediction(object, last_forecast_year, iter, ci)
  
  with(x, {
    data = object$data
    
    h_    <- forecast_horizon
    coef_ <- coef(m3$model)[1:4]
    D     <- data.frame(matrix(NA, nrow = h_, ncol = ncol(m3$modelled_data)))
    colnames(D) <- colnames(m3$modelled_data)
    L = data$L_
    U = data$U_
    
    D$country <- data$input$country
    D$Year    <- forecast_years
    D$Age     <- data$input$age
    MD        <- m3$modelled_data
    D[1, c('sex_gap1', 'sex_gap2')] <- rev(MD[MD$country == data$input$country, 'sex_gap'])[1:2]
    
    # Predict best-practice life expectancy
    pred_bp     <- as.numeric(predict(m1, data.frame(year = forecast_index))) 
    # Predict best-practice gap 
    pred_bp_gap <- as.numeric(forecast::forecast(m2$model, h = h_)$mean)
    pred_exf    <- pred_bp - pred_bp_gap 
    simulated_exf <- sim_exf(m1, m2, h_, forecast_index, iter)
    simulated_sg  <- simulated_exf*0
    
    # Predict sex-gap
    for (k in 1:iter) {
      D2 = D
      D2$exf = simulated_exf[, k]
      D2$narrow_level = pmax(0, D2$exf - m3$tau)
      D_ = rbind(D2, 0)
      
      for (i in 1:nrow(D)) {
        if (D_$exf[i] > m3$A) {gap_ <- D_$sex_gap1[i]} 
        else {gap_ <- c(1, as.numeric(D_[i, 7:9])) %*% coef_}
        gap_ <- min(max(gap_, L), U) # check
        
        D_$exm[i]          = D_$exf[i] - gap_
        D_$sex_gap1[i + 1] = D_$sex_gap[i] <- gap_
        D_$sex_gap2[i + 1] = D_$sex_gap1[i]
      }
      D__ = D_[1:h_,]
      simulated_sg[, k] <- D__$sex_gap
    }
    
    D$bp_ex   <- pred_bp
    D$bp_gap  <- pred_bp_gap
    D$sex_gap <- apply(simulated_sg, 1, median)
    D$exf     <- pred_exf
    D$exm     <- D$exf - D$sex_gap
    
    cols <- c('country', 'Year', 'Age', 'exf', 
              'exm', 'sex_gap', 'bp_ex', 'bp_gap')
    results = D[, cols]
    
    CI <- compute_CI(results, m1, m2, m3, 
                     h = h_, iter, ci, 
                     cou = data$input$country)
    
    out <- structure(class = 'predict.DoubleGap',
                     list(pred.values = results, 
                          pred.intervals = CI))
    return(out)  
  })
}

#' Prepare additional data in order to perform predictions
#' @keywords internal
#' 
prepare_data_for_prediction <- function(object, last_forecast_year, iter, ci) {
  m1 = object$model.parts$bp_model
  m2 = object$model.parts$bp_gap_model
  m3 = object$model.parts$sex_gap_model
  
  years_ = object$data$years
  fh  <- last_forecast_year - max(years_)
  fcy <- max(years_ + 1):last_forecast_year
  ti2 <- seq(max(years_) - min(years_) + 2, 
             length.out = last_forecast_year - max(years_), 
             by = 1)
  out <- list(last_forecast_year = last_forecast_year,
              forecast_horizon = fh, forecast_years = fcy, 
              forecast_index = ti2, iter = iter, ci = ci,
              m1 = m1, m2 = m2, m3 = m3)
  return(out)
}

#' Simulate female life expectancy (to be used in predict function)
#' @importFrom stats predict
#' @importFrom forecast Arima simulate.Arima
#' @keywords internal
#' 
sim_exf <- function(m1, m2, h_, forecast_index, iter) {
  sim_bp_gap <- data.frame(matrix(NA, nrow = h_, ncol = iter))
  for (j in 1:iter) {
    sim_bp_gap[, j] <- as.numeric(forecast::simulate.Arima(m2$model, nsim = h_))
  }
  predicted_bp <- as.numeric(stats::predict(m1, data.frame(year = forecast_index)))  
  out <- predicted_bp - sim_bp_gap
  return(out)
}

#' Function to generate correlated prediction intervals from a mvrnorm
#' @importFrom MASS mvrnorm
#' @importFrom stats cov quantile
#' @keywords internal
#' 
compute_CI <- function(pred_results, m1, m2, m3, h, iter, ci, cou){
  
  # Raw residuals
  res1 <- as.numeric(m1$residuals)[-c(1:2)]
  res2 <- as.numeric(m2$model$residuals)[-c(1:2)]
  res3_ <- as.numeric(m3$model$residuals/m3$model$fitted.values$scale[1])
  res3_ <- data.frame(m3$modelled_data, resid = res3_)
  res3  <-  res3_[res3_$country == cou, 'resid']
  data_resid <- cbind(res1, res2, res3)
  
  # Covariance matrix
  cov_matrix <- stats::cov(data_resid)
  
  # Generate random numbers from multivariate normal distribution 
  # with mean 0 and cov.matrix
  mat1 = mat2 = mat3 <- matrix(0, nrow = h, ncol = iter)
  for (i in 1:iter) {
    random.no <- MASS::mvrnorm(n = h, mu = rep(0, nrow(cov_matrix)), 
                               Sigma = cov_matrix)
    mat1[, i] <- cumsum(random.no[, 1])
    mat2[, i] <- cumsum(random.no[, 2])
    mat3[, i] <- cumsum(random.no[, 3])
  }
  # Function to return quantiles
  ci.levels <- sort(c((1 - ci)/2, ci + (1 - ci)/2))
  fun.ci <- function(x, y = ci.levels) stats::quantile(x, y) 
  
  PR = pred_results
  CI1 <- cbind(median = PR$bp_ex, PR$bp_ex + t(apply(mat1, 1, fun.ci))) 
  CI2 <- cbind(median = PR$bp_gap, PR$bp_gap + t(apply(mat2, 1, fun.ci)))
  CI3 <- cbind(median = PR$sex_gap, PR$sex_gap + t(apply(mat3, 1, fun.ci))) 
  CI4 <- PR$bp_ex - CI2[, c(1, ncol(CI2):2)]
  CI5 <- PR$exf - CI3[, c(1, ncol(CI3):2)]
  colnames(CI4) = colnames(CI5) <- colnames(CI1)
  rownames(CI1) = rownames(CI2) = rownames(CI3) = rownames(CI4) = rownames(CI5) <- PR$Year 
  
  out <- list(bp = CI1, bp_gap = CI2, sex_gap = CI3, 
              exf = CI4, exm = CI5)
  return(out)
}

