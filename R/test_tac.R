#' Compute (partial) autocorrelation functions and test for significance
#'
#' @param x numeric; an evenly spaced time vector which should be tested for temporal
#'    autocorrelation. Temporal gaps should be included as NAs.
#' @return
#' The function returns a list with 2 components:
#' \describe{
#'   \item{\code{tac}}{TRUE if any lag in the (partial) autocorrelation functions
#'     is significantly correlated (if correlation value > 0.4), else FALSE.}
#'   \item{\code{max_lag}}{The maximal lag that is correlated.}
#'  }
#' @export
#' @examples
#' test_tac(x = 1:20)
#' test_tac(x = rnorm(20))
#'
test_tac <- function(x) {

  # Get acf values
  acf_val <- as.vector(stats::acf(x,
    na.action = stats::na.pass, plot = FALSE)$acf)
  # Get pacf values
  pacf_val <- as.vector(stats::pacf(x, na.action = stats::na.pass,
      plot = FALSE)$acf)

  # Is there temporal autocorrelation? TRUE = tac occurs
  tac <- any((abs(acf_val[2:6]) > 0.4) & (abs(pacf_val[1:5]) >
      0.4))
  lags <- which(abs(pacf_val[1:5]) > 0.4)
  if (length(lags) == 0) {
    max_lag <- 0
  } else {
    max_lag <- max(lags)
  }

  # Create output list
  res <- list(tac = tac, max_lag = max_lag)
  return(res)
}
