# <img src="inst/figures/MortalityGaps_logo.png" align="right" width="150" height="150" /> The Double-Gap Forecasting Model

[![CRAN_Version](https://www.r-pkg.org/badges/version/MortalityGaps)](https://cran.r-project.org/package=MortalityGaps)
[![Linux Build Status](https://travis-ci.com/mpascariu/MortalityGaps.svg?branch=master)](https://travis-ci.com/mpascariu/MortalityGaps)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mpascariu/MortalityGaps?branch=master&svg=true)](https://ci.appveyor.com/project/mpascariu/MortalityGaps)
[![codecov](https://codecov.io/github/mpascariu/MortalityGaps/branch/master/graphs/badge.svg)](https://codecov.io/github/mpascariu/MortalityGaps)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/MortalityGaps.svg)](https://github.com/mpascariu/MortalityGaps/issues)

[![license](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/mpascariu/MortalityGaps/blob/master/LICENSE)
[![CRAN_Download_Badge1](https://cranlogs.r-pkg.org/badges/grand-total/MortalityGaps)](https://CRAN.R-project.org/package=MortalityGaps)
[![CRAN_Download_Badge2](https://cranlogs.r-pkg.org/badges/MortalityGaps)](https://CRAN.R-project.org/package=MortalityGaps)



This repository contains source code for the Double-Gap model for forecasting 
life expectancy in human population. 

# Description
Life expectancy is highly correlated over time among countries and 
between males and females. These associations can be used to improve forecasts. 
Here we have implemented a method for forecasting female life expectancy based on 
analysis of the gap between female life expectancy in a country compared with
the record level of female life expectancy in the world. Second, to forecast 
male life expectancy, the gap between male life expectancy and female life 
expectancy in a country is analysed. We named this method the Double-Gap model.
For a detailed description of the method see Pascariu et al. (2017).

## Installation

1. Make sure you have the most recent version of R
2. Run the following code in your R console 

```R
install.packages("MortalityGaps")
```

## Updating to the latest version of the package

You can track and contribute to the development of `MortalityGaps` on [GitHub](https://github.com/mpascariu/MortalityLaws). To install it:

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Make sure you have a working development environment.
    * **Windows**: Install [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/).
    * **Mac**: Install `Xcode` from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).

3. Install the development version of `MortalityGaps`.

   ```R
   devtools::install_github("mpascariu/MortalityGaps")
   ```

# Help
All functions are documented in the standard way, which means that 
once you load the package using ```library(MortalityGaps)```
you can just type ```?DoubleGap``` to see the help file. 

## References
Pascariu M.D., Canudas-Romo V. and Vaupel W.J. 2018. [The double-gap life expectancy forecasting model.](https://doi.org/10.1016/j.insmatheco.2017.09.011) Insurance: Mathematics and Economics Volume 78, Pages 339-350.

