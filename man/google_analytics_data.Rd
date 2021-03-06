% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_analytics_data.R
\docType{data}
\name{google_analytics_data}
\alias{google_analytics_data}
\title{Google Analytics Data}
\format{A data frame with 2187 rows and 4 variables:
\describe{
  \item{ds}{a datestamp as POSIXct}
  \item{transactions}{aggregated eCommerce transactions per hour}
  \item{sessions}{aggregated eCommerce sessions per hour}
  \item{conversion_rate}{calculated conversion rate as transactions/sessions}
}}
\usage{
google_analytics_data
}
\description{
A dataset containing simulated time series data for three months of
eCommerce transactions, sessions and a conversion rate.
}
\keyword{datasets}
