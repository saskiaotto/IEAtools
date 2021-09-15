#' Show correlation coefficient, histogram or regression lines in pairs plot
#'
#' \code{panel_cor}, \code{panel_lines}  and \code{panel_hist} are utility functions
#' for the pairs plot. \code{panel_cor} shows the Pearson product-moment correlation
#' coefficient in the selected panels. \code{panel_lines} shows the regression line and
#' \code{panel_hist} histograms.
#'
#' @param x numeric vector
#' @param y numeric vector
#' @name panel_funcs

#' @seealso \code{\link{pairs}}
#' @examples
#' z <- matrix(rnorm(40), ncol = 4)
#' pairs(z, upper.panel = panel_cor,
#' lower.panel = panel_lines, diag.panel = panel_hist)
NULL

#' @rdname panel_funcs
#' @export
panel_cor <- function(x, y) {
  par(usr = c(0, 1, 0, 1))
  r <- stats::cor(x, y, use = "pairwise.complete.obs")
  txt <- format(r, digits = 1)
  text(0.5, 0.5, txt, cex = 0.9/strwidth(txt) * abs(r))
}

#' @rdname panel_funcs
#' @export
panel_lines <- function (x, y) {
  points(x, y, bg = NA, cex = 1)
  sel <- is.finite(x) & is.finite(y)
  if (any(sel)){
    lml <- stats::lm(y[sel] ~ x[sel])
    abline(lml, col = "blue")}
}

#' @rdname panel_funcs
#' @export
panel_hist <- function(x) {
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts / max(h$counts)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey80")
}
