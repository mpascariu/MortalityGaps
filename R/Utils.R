
#' onAttach
#' @description Print a message when the package is loaded first time in R
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nMortalityGaps: The Double-Gap Life Expectancy Forecasting Model",
                        "\nAuthor       : Marius D. Pascariu",
                        "\nLast Update  : July 17, 2018\n")
}

