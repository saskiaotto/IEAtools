#' Time series plot of Euclidean distance
#'
#' \code{plot_statespace_ed} generates a time series plot of the Euclidean
#' distance in indicator state space from a defined reference conditions.
#'
#' @param x The output tibble from the \code{\link{statespace_ed}} function.
#'
#' @return The function returns a \code{\link[ggplot2]{ggplot}} object.
#'
#' @author Saskia A. Otto
#'
#' @export
#' @examples
#' x <- as.data.frame(matrix(rnorm(250), ncol = 10))
#' ed <- statespace_ed(x, time = 1976:2000, ref_time = 1976)
#' plot_statespace_ed(ed)
plot_statespace_ed <- function(x) {

  # Data input validation ---------
  if (missing(x)) {
    stop("Argument x is missing.")
  }
  # x <- INDperform::check_input_tbl(x, tbl_name = "x", parent_func = "statespace_ed()")
  # -------------------------------

  # Set general layout theme
  ggplot2::theme_set(ggplot2::theme_bw())

  edplot <- ggplot2::ggplot(x,
    ggplot2::aes(x = !!rlang::sym("time"), y = !!rlang::sym("ed"))) +
    ggplot2::geom_smooth(col = "firebrick3",
    fill = "cadetblue", alpha = 0.2) +
  	ggplot2::geom_line(col = "black", size = 0.5) +
  	ggplot2::geom_point(shape = 16, col = "black", size = 1.5) +
  	ggplot2::ylab(paste0("Euclidean distance s from reference point (",
    x$time[which(x$ref_time == TRUE)], ")")) +
    ggplot2::xlab("") + # General layout
  		ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14))

  return(edplot)
}
