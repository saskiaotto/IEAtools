#' Calculate the variance inflation factors
#'
#' Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy
#' eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam
#' voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet
#' clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
#'
#' @name calc_vif
#' @param x matrix or data frame containing only numerical variables.
#' @return
#' The function returns a matrix with 3 columns:
#' \describe{
#'   \item{\code{Rsquared}}{}
#'   \item{\code{Tolerance}}{}
#'   \item{\code{VIF}}{}
#'  }
#' @export
#' @examples
#' x <- data.frame(a = 1:20, b = 1:20*2 + rnorm(20) )
#' calc_vif(x)

calc_vif <- function(x) {
	result <- matrix(NA, nrow = ncol(x), ncol = 3)
	rownames(result) <- colnames(x)
	colnames(result) <- c("Rsquared", "Tolerance", "VIF")
	for (v in 1:ncol(x)) {
		v.name <- colnames(x)[v]
		other.v.names <- colnames(x)[-v]
		mod.formula <- stats::as.formula(paste(v.name, "~", paste(other.v.names, collapse = "+")))
		mod <- stats::lm(mod.formula, data = x)
		R2 <- summary(mod)$r.squared
		result[v, "Rsquared"] <- R2
		result[v, "Tolerance"] <- 1 - R2
		result[v, "VIF"] <- 1 / (1 - R2)
	}
	return(result)
}


