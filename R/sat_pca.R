#' Satellite Interpretable PCA
#'
#' Performs PCA on a matrix of observations and calculates a measure of variable
#' importance.
#'
#' For a given matrix of values, the function will perform principal component
#' analysis and calculate a variable importance measure based on the PCA
#' scores and loadings to determine a local measure of variable importance.
#'
#' @param x matrix. A data frame. First column are date values,
#' then the numeric vectors for PCA
#' @param min_nonzero numeric. The minimum number of non-zero records for
#' exclusion.
#' @param min_val numeric. The minimum value in a column for exclusion.
#'
#' @return a list containing inputs and improtance measures.

sat_pca <- function(x, min_nonzero = 1, min_val = 0.01){
  dates <- as.vector(x[,1])
  x <- x[,-1]
  ratio <- apply(x, MARGIN = 2, function (x) sum(x != 0) > min_nonzero)
  contrib <- apply(x, MARGIN = 2, function (x) max(x) > min_val)
  x_clean <- x[, ratio & contrib]
  pca <- stats::prcomp(x = x_clean, retx = TRUE, center = TRUE, scale. = TRUE)
  A <- as.matrix(pca$x[, 1L:2L])
  B <- as.matrix(pca$rotation[, 1L:2L])
  dp <- A %*% t(B)
  out <- list(inputs = x_clean,
              importance = dp,
              dates = dates,
              sdev = pca$sdev)
  return(out)
}


