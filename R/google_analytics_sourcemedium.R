#' Google Analytics SourceMedium Data
#'
#' A dataset containing google analytics sessions with the extra dimension
#' of sourcemedium. Using an extra dimension (in addition to time based dims)
#' provides a way to subset data by a desired dimension.
#'
#' @format A data frame with 3920 rows and 4 variables:
#' \describe{
#'   \item{date}{date of the ga session}
#'   \item{hour}{hour of the ga session}
#'   \item{sourceMedium}{ga sourceMedium dimension}
#'   \item{sessions}{number of sessions metric from ga}
#' }
"google_analytics_sourcemedium"
