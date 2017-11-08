# The Double-Gap Forecasting Model (R package)
[![Build Status](https://travis-ci.com/mpascariu/DoubleGap.svg?token=VBqVNpqeEEqqDsXcFpYL&branch=master)](https://travis-ci.com/mpascariu/DoubleGap)

This repository contains the source code for the Double-Gap model for forecasting life expectancy in different populations. 

# Description
Life expectancy is highly correlated over time among countries and 
between males and females. These associations can be used to improve forecasts. 
Here we have implemented a method for forecasting female life expectancy based on 
analysis of the gap between female life expectancy in a country compared with
the record level of female life expectancy in the world. Second, to forecast 
male life expectancy, the gap between male life expectancy and female life 
expectancy in a country is analysed. We named this method the Double-Gap model.
For a detailed description of the method see [Pascariu et al. (2017)](https://doi.org/10.1016/j.insmatheco.2017.09.011). 

# Installation

1. Make sure you have installed the most recent version of R ( https://www.r-project.org )
2. Install the package in R using **devtools** by running the following code in your R console:

```r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("mpascariu/DoubleGap")
```

# Help
All functions are documented in the standard way, which means that 
once you load the package using ```library(DoubleGap)```
you can just type ```?DoubleGap``` to see the help file. 

