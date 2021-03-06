% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graphics.R
\name{plot.predict.DoubleGap}
\alias{plot.predict.DoubleGap}
\title{Generic Plot Function for Class predict.DoubleGap}
\usage{
\method{plot}{predict.DoubleGap}(x, show.legend = TRUE, ylim = NULL,
  asp = 1.8, xlab = "\\nYear", ylab = "\\nLife Expectancy Level",
  ...)
}
\arguments{
\item{x}{An object of class \code{\link{predict.DoubleGap}}}

\item{show.legend}{Logical. Indicate whether to display the legend or not. Default: TRUE.}

\item{ylim}{Numeric vectors of length 2, giving the x and y coordinates ranges.}

\item{asp}{Numeric, giving the aspect ratio y/x.}

\item{xlab}{A title for the x axis: see \code{\link{title}}.}

\item{ylab}{A title for the y axis: see \code{\link{title}}.}

\item{...}{Further graphical parameters as in \code{\link{par}}.}
}
\description{
Generic Plot Function for Class predict.DoubleGap
}
\examples{
# Complete examples are provided in help page of the DoubleGap function.
}
\seealso{
\code{\link{DoubleGap}}
}
\author{
Marius D. Pascariu
}
