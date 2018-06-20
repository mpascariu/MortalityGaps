rm(list = ls())
library(MortalityGaps)

# Input data
exF <- MortalityGaps.data$exF
exM <- MortalityGaps.data$exM

# ----------------------------------------------
# Fit DG model for Sweden at age 0
M1 <- DoubleGap(DF = exF, 
                DM = exM, 
                age = 0, 
                country = "SWE", 
                years = 1950:2014, 
                arima.order = c(2, 1, 1), 
                drift = TRUE, 
                tau = 75, 
                A = 86)
summary(M1)
M1
ls(M1)
M1$model.parts

# Predict model 
P1 <- predict(M1, last_forecast_year = 2050)
P1
plot(P1)


# ----------------------------------------------
# Fit DG model for USA at age 65
M2 <- DoubleGap(DF = exF, 
                DM = exM, 
                age = 65, 
                country = "USA", 
                years = 1950:2014, 
                arima.order = c(0, 1, 0), 
                drift = FALSE, 
                tau = 15, 
                A = 24)
summary(M2)

# Predict model 
P2 <- predict(M2, last_forecast_year = 2050)
P2
plot(P2)

# ----------------------------------------------

M3 <- DoubleGap(DF = exF, 
                DM = exM, 
                age = 0, 
                country = "AUS", 
                years = 1950:2014)

summary(M3)
P3 <- predict(M3, last_forecast_year = 2050)
M3
plot(P3)
