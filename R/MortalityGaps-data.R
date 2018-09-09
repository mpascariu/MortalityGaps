

#' DATA - for testing purposes
#'
#' Dataset containing records of life expectancy at birth and at age 65 for 
#' female and male populations living in 38 countries between 1950 and 2014.
#' This dataset is used in \insertCite{pascariu2018;textual}{MortalityGaps} 
#' article. The data is provided in the package for testing purposes and to 
#' ensure the reproducibility of the results and figures published in the article. 
#' By the time you are using this package, the data might be outdated. Download actual 
#' demographic data free of charge from \insertCite{hmd2017;textual}{MortalityGaps}. 
#' Once a username and a password is created on the 
#' \href{https://www.mortality.org}{website} the
#' \href{https://CRAN.R-project.org/package=MortalityLaws}{MortalityLaws}
#' R package can be used to extract data directly into your R console.
#' \insertNoCite{MortalityLaws160;textual}{MortalityGaps}  
#' @source \href{https://www.mortality.org}{Human Mortality Database}
#' @references \insertAllCited{}
#' @seealso \code{\link[MortalityLaws]{ReadHMD}}
"MortalityGaps.data"


#' Print function for HMD4mx data
#' @param x A \code{MortalityEstimateData} object.
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @keywords internal
print.MortalityGaps.data <- function(x, ...) {
  cat("\nMortalityGaps Test Data\n")
  cat(" Populations: 38 countries\n")
  cat(" Series     : Life expectancy\n")
  cat(" Years      : 1950 - 2014\n")
  cat(" Ages       : 0 and 65\n")
  cat(" Format     : List containing 2 data frames\n")
  cat(" Source     : Human Mortality Database\n")
  cat(" Download   : June 10, 2017\n")
  cat(" Note       : Dataset used in Pascariu et. al. (2018)\n")
}

