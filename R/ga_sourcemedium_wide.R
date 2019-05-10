#' Google Analytics SourceMedium Wide
#'
#' A dataset containing google analytics sessions with the extra dimension
#' of sourcemedium. Using an extra dimension (in addition to time based dims)
#' provides a way to subset data by a desired dimension. The dataset has been
#' spread into wide format.
#'
#' @format A data frame with 2111 rows and 5 variables:
#' \describe{
#'   \item{ds}{datetime of the session}
#'   \item{facebook / social}{ga sourceMedium dimension value}
#'   \item{(direct) / (none)}{ga sourceMedium dimension value}
#'   \item{google / cpc}{ga sourceMedium dimension value}
#'   \item{google / organic}{ga sourceMedium dimension value}
#' }
"ga_sourcemedium_wide"


