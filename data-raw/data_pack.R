# Sat Jun 10 09:25:41 2017 ------------------------------
### This code was used to download and subset HMD data to be added in the package

rm(list = ls())
# devtools::install_github("mpascariu/MortalityLaws")
library(MortalityLaws)
library(dplyr)


user_ = 'your@email.com'
password_ = 'your.password'

# Download HMD data. Take approx 1m30s (depending on the internet speed).
HMD_LT_F <- ReadHMD(what = 'LT_f',
                    interval = '1x1',
                    username = user_,
                    password = password_,
                    save = FALSE)

HMD_LT_M <- ReadHMD(what = 'LT_m',
                    interval = '1x1',
                    username = user_,
                    password = password_,
                    save = FALSE)


# cnu = countries not used  
# these are populations that need to be taken out of the dataset
cnu <- c("FRACNP","DEUTNP", "NZL_NM", "NZL_MA",
         "GBR_NP","GBRCENW", "GBR_NIR", "CHL", "LUX", "HRV")

years = 1950:2014

LTF <- HMD_LT_F$data %>% filter(Year %in% years & !(country %in% cnu))
LTM <- HMD_LT_M$data %>% filter(Year %in% years & !(country %in% cnu))

exF <- LTF %>% filter(Age %in% c(0, 65)) %>% select(country, Year, Age, ex)
exM <- LTM %>% filter(Age %in% c(0, 65)) %>% select(country, Year, Age, ex)

# verify that the two data.frames are of equal length
nrow(exF) == nrow(exM)
ncol(exF) == ncol(exM)

MortalityGaps.data <- structure(class = "MortalityGaps.data", 
                                list(exF = exF, exM = exM))
  

devtools::use_data(MortalityGaps.data, overwrite = TRUE)




