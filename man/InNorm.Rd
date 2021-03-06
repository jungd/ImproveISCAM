% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InverseNormalProbability.R
\name{InNorm}
\alias{InNorm}
\title{Inverse normal Probability}
\usage{
InNorm(prob1, mean = 0, sd = 1, direction)
}
\arguments{
\item{prob1}{Calculates the normal quantile of a specified probability}

\item{mean}{mean = 0}

\item{sd}{sd = 1}

\item{direction}{above,below, outside, or between}
}
\value{
Inverse Normal Probability
}
\description{
Inverse normal Probability
}
\examples{
InNorm(0.88, direction="above")
InNorm(0.43, direction="below")
InNorm(0.95, direction="outside")
InNorm(0.88, direction="between")
}

