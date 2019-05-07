#' Satellite Outlier Detect
#'
#' A function for time series outlier detection.
#'
#' The function takes a vector of time series observations and applies multiple
#' period time series decomposition using a robust version of STL decomposition.
#' An inverse hyperbolic sine transformation is applied to the residuals to restore
#' some normality and allow the use of an Inner Quantile Range to be calculated for
#' outlier detection. Rather than the standard 3 * IQR, a weighted vector is
#' accepted to allow variation in the IQR multipler between two fixed limits set
#' by the user.
#'
#' @param x A numeric vector of length n. The original time series.
#' @param wt A numeric vector of length n. A vector of optional weights
#' @param index A vector of Dates for the time series
#' @param tr logical. Whether an arcsinh transformation is applied to the residuals
#' @param seasonal_periods A numeric vector. The seasonal periods to use for
#' time series decomposition
#' @param iqr_scaling_factor A numeric vector. The scaling range for the IQR
#' multiplier. This scales the wt parameter.
#' @param iqr_range A numeric vector. The range to use in calculating the IQR.
#' @return a list containing the original data with indicated outliers and a list
#' of just the outliers.

sat_outlier_detect <- function(x, wt, index, tr = TRUE, seasonal_periods = c(24, 168),
                         iqr_scaling_factor = c(1.5, 3),
                         iqr_range = c(0.25 ,0.75)){

  # decomposition
  xx <- forecast::msts(data = x, seasonal.periods = seasonal_periods)
  d <- forecast::mstl(x = xx, robust = TRUE)
  residuals <- as.numeric(d[,"Remainder"])

  # transformation
  if (tr== TRUE) {
    residuals <- asinh(residuals)
  }

  # weight scaling
  wt_tf <- asinh(wt)
  wt_scaled <- (wt_tf - min(wt_tf))/(max(wt_tf) - min(wt_tf)) * -diff(iqr_scaling_factor) + iqr_scaling_factor[2]

  # iqr calc
  resid.q <- stats::quantile(residuals, prob = iqr_range, na.rm = TRUE)
  iqr <- unname(diff(resid.q))
  scaled_iqr <- iqr * wt_scaled
  resid_lower <- resid.q[1] - scaled_iqr
  resid_upper <- resid.q[2] + scaled_iqr

  # outlier flagging
  outlier <- ifelse(test = residuals > resid_upper | residuals < resid_lower, yes = "Yes", no = "No")

  # back transformation
  if (tr==TRUE) {
    residuals <- sinh(residuals)
    resid_lower <- sinh(resid_lower)
    resid_upper <- sinh(resid_upper)
  }

  # results out
  out_df <- data.frame(index,
                       data = x,
                       residuals,
                       resid_lower,
                       resid_upper,
                       outlier)

  outlier_df <- out_df[out_df$outlier == "Yes",]

  out <- list(output = out_df,
              outliers = outlier_df)

  return(out)

}

























