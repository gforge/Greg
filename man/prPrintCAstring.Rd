% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printCrudeAndAdjustedModel.R
\name{prPrintCAstring}
\alias{prPrintCAstring}
\title{Prep for printing}
\usage{
prPrintCAstring(x, ...)
}
\arguments{
\item{x}{The output object from the \code{printCrudeAndAdjustedModel} function}

\item{...}{outputs from \code{printCrudeAndAdjusted}. If mixed then it defaults to rbind.data.frame}
}
\description{
Since we have both the \code{\link[base]{print}()} and the
\code{\link[knitr]{knit_print}()} that we need to call it is
useful to have a common string preparation.
\emph{Note:} Currently knit_print doesn't work as expected...
}
\keyword{internal}
