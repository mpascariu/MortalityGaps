
#' @title Generic Predict Function for Class \code{\link{DoubleGap}}
#' 
#' @description Predict DoubleGap model
#' @param object A DoubleGap object
#' @param last_forecast_year Last year to be included in the forecast
#' @param iter Number of iterations. Default: 500
#' @param ci Confidence levels. Default: c(0.8, 0.95)
#' @param ... Additional arguments affecting the predictions produced.
#' @return A list containing predicted value of best-practice
#' life expectancy, best-practice gap, sex gap, and forecast life expectancy 
#' for females and males (together with prediction intervals).
#' @seealso \code{\link{DoubleGap}}
#' @author Marius D. Pascariu
#' @export
#'  
predict.DoubleGap <- function(object, last_forecast_year, 
                              iter = 500, ci = c(0.8, 0.95), ...) {
  input <- c(as.list(environment())) # save for later use
  pb    <- startpb(0, iter) # Start the clock!
  x     <- prepare_data_for_prediction(object, last_forecast_year, iter, ci)
  
  with(x, {
    data    <- object$data
    country <- data$country
    L       <- data$L_
    U       <- data$U_
    A       <- m3$A
    tau     <- m3$tau
    
    # Predict best-practice life expectancy
    pred_bp     <- as.numeric(predict(m1, data.frame(year = forecast_index))) 
    # Predict best-practice gap 
    pred_bp_gap <- as.numeric(forecast(m2$model, h = forecast_horizon)$mean)
    pred_exf    <- pred_bp - pred_bp_gap 
    # Predict sex-gap ----
    M3_coef <- coef(m3$model)[1:4]
    M3_data <- m3$modelled_data[m3$modelled_data$country == country, ]
    
    D <- data.frame(matrix(NA, nrow = forecast_horizon, ncol = ncol(M3_data)))
    colnames(D) <- colnames(M3_data)
    D$country   <- country
    D$Year      <- forecast_years
    D$Age       <- data$age
    D[1, c(7L, 8L)] <- rev(M3_data[, 6L])[1:2]
    
    D2 = D
    simulated_sg <- matrix(NA, nrow = x$forecast_horizon, ncol = iter)
    for (k in 1:iter) {
      exf_k <- pred_bp - as.numeric(simulate(m2$model, nsim = forecast_horizon))
      D2[, c(4L, 9L)] <- cbind(exf_k, pmax(0, exf_k - tau))
      D_ <- rbind(D2, 0)
      
      for (i in 1:nrow(D)) {
        exf      <- D_[i, 4L]
        sex_gap1 <- D_[i, 7L]
        sex_gap0 <- if (exf > A) sex_gap1 else c(1, as.numeric(D_[i, 7L:9L])) %*% M3_coef
        sex_gap0 <- min(max(sex_gap0, L), U) # check
        D_[i, c(5L, 6L)]     <- c(exf - sex_gap0, sex_gap0)
        D_[i + 1, c(7L, 8L)] <- c(sex_gap0, sex_gap1)
      }
      simulated_sg[, k] <- D_[-nrow(D_), 6L]
      setpb(pb, k)
    }
    
    D$bp_ex   <- pred_bp
    D$bp_gap  <- pred_bp_gap
    D$sex_gap <- apply(simulated_sg, 1, median)
    D$exf     <- pred_exf
    D$exm     <- D$exf - D$sex_gap
    D         <- D[, -c(7L, 8L, 9L)]
    CI  <- compute_CI(D, m1, m2, m3, forecast_horizon, iter, ci, country)
    out <- structure(class = 'predict.DoubleGap',
                     list(input = input, pred.values = D, pred.intervals = CI))
    closepb(pb) # Stop clock on exit.
    return(out)  
  })
}

#' Prepare additional data in order to perform predictions
#' @keywords internal
#' 
prepare_data_for_prediction <- function(object, last_forecast_year, iter, ci) {
  m1  <- object$model.parts$bp_model
  m2  <- object$model.parts$bp_gap_model
  m3  <- object$model.parts$sex_gap_model
  yr  <- object$data$years
  fh  <- last_forecast_year - max(yr)
  fcy <- max(yr + 1):last_forecast_year
  ti2 <- seq(max(yr) - min(yr) + 2, 
             length.out = last_forecast_year - max(yr), by = 1)
  out <- list(last_forecast_year = last_forecast_year,
              forecast_horizon = fh, forecast_years = fcy, 
              forecast_index = ti2, iter = iter, ci = ci,
              m1 = m1, m2 = m2, m3 = m3)
  return(out)
}

#' Function to generate correlated prediction intervals from a mvrnorm
#' @keywords internal
#' 
compute_CI <- function(pred_results, m1, m2, m3, h, iter, ci, cou){
  # Raw residuals
  res1  <- as.numeric(m1$residuals)[-c(1:2)]
  res2  <- as.numeric(m2$model$residuals)[-c(1:2)]
  res3_ <- as.numeric(m3$model$residuals/m3$model$fitted.values$scale[1])
  res3_ <- data.frame(m3$modelled_data, resid = res3_)
  res3  <- suppressWarnings(aggregate(res3_, 
           by = list(res3_$Year, res3_$Age), FUN = mean))$resid
  data_resid <- cbind(res1, res2, res3)
  
  # Generate random numbers from multivariate normal distribution 
  # with mean 0 and cov.matrix
  cov_matrix <- cov(data_resid)
  mat1 = mat2 = mat3 <- matrix(0, nrow = h, ncol = iter)
  for (i in 1:iter) {
    random.no <- mvrnorm(n = h, mu = rep(0, nrow(cov_matrix)), Sigma = cov_matrix)
    mat1[, i] <- cumsum(random.no[, 1])
    mat2[, i] <- cumsum(random.no[, 2])
    mat3[, i] <- cumsum(random.no[, 3])
  }
  # Function to return quantiles
  ci.levels <- sort(c((1 - ci)/2, ci + (1 - ci)/2))
  fun.ci    <- function(x, y = ci.levels) stats::quantile(x, y) 
  
  PR  <- pred_results
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

