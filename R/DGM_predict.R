
#' @title Generic Predict Function for Class \code{\link{DoubleGap}}
#' 
#' @description Predict DoubleGap model
#' @param object An object of class \code{DoubleGap}.
#' @param h Number of periods for forecasting.
#' @param iter Number of iterations. Default: 500
#' @param ci Confidence levels. Default: c(0.8, 0.95)
#' @param ... Additional arguments affecting the predictions produced.
#' @return A list containing predicted value of best-practice
#' life expectancy, best-practice gap, sex gap, and forecast life expectancy 
#' for females and males (together with prediction intervals).
#' @seealso \code{\link{DoubleGap}}
#' @author Marius D. Pascariu
#' @examples 
#' # Complete examples are provided in help page of the DoubleGap function.
#' @export
predict.DoubleGap <- function(object, h, 
                              iter = 500, ci = c(0.8, 0.95), ...) {
  input <- c(as.list(environment())) # save for later use
  pb    <- startpb(0, iter) # Start the clock!
  x     <- prepare_data_for_prediction(object, h, iter, ci)
  
  with(x, {
    data    <- object$input
    country <- data$country
    L       <- data$L_
    U       <- data$U_
    A       <- M3$A
    tau     <- M3$tau
    
    # Predict best-practice life expectancy
    pred_bp     <- as.numeric(predict(M1, data.frame(year = forecast_index))) 
    # Predict best-practice gap 
    pred_bp_gap <- as.numeric(forecast(M2$model, h = h)$mean)
    pred_exf    <- pred_bp - pred_bp_gap 
    # Predict sex-gap ----
    M3_coef <- coef(M3$model)[1:4]
    M3_data <- M3$modelled_data[M3$modelled_data$country == country, ]
    
    D <- data.frame(matrix(NA, nrow = h, ncol = ncol(M3_data)))
    colnames(D) <- colnames(M3_data)
    D$country   <- country
    D$Year      <- forecast_years
    D$Age       <- data$age
    D[1, c(7L, 8L)] <- rev(M3_data[, 6L])[1:2]
    
    D2 = D
    simulated_sg <- matrix(NA, nrow = x$h, ncol = iter)
    for (k in 1:iter) {
      exf_k <- pred_bp - as.numeric(simulate(M2$model, nsim = h))
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
    CI        <- compute_CI(D, M1, M2, M3, h, iter, ci, country)
    out <- structure(class = 'predict.DoubleGap',
                     list(input = input, pred.values = D, pred.intervals = CI))
    closepb(pb) # Stop clock on exit.
    return(out)  
  })
}


#' Prepare additional data in order to perform predictions
#' @inheritParams predict.DoubleGap
#' @keywords internal
prepare_data_for_prediction <- function(object, h, iter, ci) {
  M1  <- object$model.parts$bp_model
  M2  <- object$model.parts$bp_gap_model
  M3  <- object$model.parts$sex_gap_model
  yr  <- object$input$years
  fcy <- max(yr + 1):max(yr + h)
  ti2 <- seq(max(yr) - min(yr) + 2, length.out = h, by = 1)
  out <- list(h = h, forecast_years = fcy, 
              forecast_index = ti2, iter = iter, ci = ci,
              M1 = M1, M2 = M2, M3 = M3)
  return(out)
}

#' Function to generate correlated prediction intervals from a mvrnorm
#' @param pred_results An object containing predicted values 
#' @inheritParams find_fitted_values
#' @inheritParams predict.DoubleGap
#' @param h The number of steps ahead for which prediction is required.
#' @param cou Country name.
#' @keywords internal
compute_CI <- function(pred_results, M1, M2, M3, h, iter, ci, cou){
  # Raw residuals
  res1  <- as.numeric(M1$residuals)[-c(1:2)]
  res2  <- as.numeric(M2$model$residuals)[-c(1:2)]
  res3_ <- as.numeric(M3$model$residuals/M3$model$fitted.values$scale[1])
  res3_ <- data.frame(M3$modelled_data, resid = res3_)
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


#' Print function for predict.DoubleGap
#' @param x An object of class \code{predict.DoubleGap}.
#' @inheritParams print.DoubleGap
#' @keywords internal
#' @export
print.predict.DoubleGap <- function(x, ...) {
  cat("Predicted values generated by the Double-Gap Model\n")
  cat(paste0("Country: ", x$pred.values$country[1], "\n"))
  cat(paste0("Age (x): ", x$pred.values$Age[1], "\n\n"))
  print(x$pred.values[, c(-1, -3)])
  cat("\n\n")
}
