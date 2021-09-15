#' Imputation of missing values by local mean substitution
#'
#' The function substitutes missing values in time series with a mean of the \emph{n} previous and following years
#' (or less depending on the position of NAs, whether it is at the beginning or end of the time series, and
#' the presence of further NAs in the selected time period).
#'
#' @name impute
#' @param x numeric vector containing NAs.
#' @param n integer; the number of previous and following values to be included in the mean.
#'   The default is 2 so the mean is based on a 5yr-period (including the year of the missing
#'   value).
#'
#' @return
#' The function returns the same numeric input vector, but with replaced missing values.
#' @export
#' @author Saskia A. Otto
#' @examples
#' x <- c(NA, 1, 4, 2, 5, NA, 9, NA, 12, 11, NA)
#' impute(x)

impute <- function(x, n = 2) {

  ### Data input validation
  if (!is.numeric(x)) {
    stop("x needs to be numeric.")
  }
  if (!any(is.na(x))) {
    return(x)
  }
  # --------------------------
  out <- x
  loc_na <- which(is.na(x))
  for(i in loc_na) {
    choose <- (i-n):(i+n)
    # Remove negative indices
    choose <- choose[choose>0]
    out[i] <- mean(x[choose], na.rm = TRUE)
  }
  return(out)
}
