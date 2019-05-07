#' Google Analytics Data
#'
#' A dataset containing simulated time series data for three months of
#' eCommerce transactions, sessions and a conversion rate.
#'
#' @format A data frame with 2187 rows and 4 variables:
#' \describe{
#'   \item{ds}{a datestamp as POSIXct}
#'   \item{transactions}{aggregated eCommerce transactions per hour}
#'   \item{sessions}{aggregated eCommerce sessions per hour}
#'   \item{conversion_rate}{calculated conversion rate as transactions/sessions}
#' }
"google_analytics_data"
