#' Create multiple plots to explore gaps in the time series
#'
#' This function creates an image plot where available values for the different variables and years
#' are indicated in gray and missing values in white. At the right side and bottom, two barplots
#' are added showing the frequency of available variables per year and the available years per
#' variable.
#'
#' @name explore_na
#' @param x matrix or data frame containing time series of multiple variables.
#' @param time vector of time units that will be used for the x ax
#' @param cex_x double; the magnification to be used for the x labels relative to the
#'   current setting of cex.
#' @param cex_y double; the magnification to be used for the y labels relative to the
#'   current setting of cex.
#' @param madj_y double; adjustment of left margin relative to the
#'   current setting for longer or short variable names in the image plot and right barplot.
#' @param hadj_y double; horizontal adjustment of the y labels in the image plot.
#' @param respect logical; this argument controls whether a unit column-width is the
#'   same physical measurement on the device as a unit row-height (default is TRUE).
#'
#' @import graphics
#' @export
#' @author Saskia A. Otto
#' @examples
#' mat <- matrix(1:200, ncol = 10)
#' mat[sample(1:200, sample(10:80, 1))] <- NA
#' colnames(mat) <- paste0("variable", 1:10)
#' explore_na(x = mat, time = 1981:2000)
#' colnames(mat) <- LETTERS[1:10]
#' explore_na(x = mat, time = 1981:2000, madj_y = -2.5, hadj_y = 2, respect = FALSE)

explore_na <- function(x, time = NULL, cex_x = 1, cex_y = 1,
  madj_y = 0, hadj_y = 1, respect = TRUE){

  if(!is.null(time)) {
    rownames(x) <- time
  }

  x[!is.na(x)] <- 1
  x[is.na(x)] <- 0
  x <- t(as.matrix(x))

  ylabel <- rownames(x)
  xlabel <- colnames(x)

  # Reverse Y axis
  reverse <- nrow(x) : 1
  ylabel <- ylabel[reverse]
  x <- x[reverse,]

  # Data Map
  layout(matrix(c(1,1,2,1,1,2,3,3,4), ncol=3, byrow = TRUE), widths = c(2.5,2,1.5),
    heights = c(2,1,1.5), respect = respect)
  # graphics::layout(matrix(c(1,1,1,1), ncol = 2), widths = c(3.5,3.5), heights = c(2,2),
  #   respect = respect)
  # layout.show(plot_layout)
  par(mar = c(3, 7+madj_y, 1.5, .5))
    image(1:length(xlabel), 1:length(ylabel), t(x), col = c("white", "grey60"),
    ylab = "", xlab = "", axes = FALSE, zlim = c(0,1))
    axis(side = 1,  at = 1:length(xlabel), labels = xlabel,
      las = 2, cex.axis = cex_x, padj = 0.5)
    axis(side = 2,  at = 1:length(ylabel), labels = ylabel,
      las = 1, cex.axis = cex_y, hadj = hadj_y)
  box()
  # barplot variables
  par(mar = c(3, 7+madj_y, 1.5, 1.5))
  barplot(apply(x>0, 1, sum), xlab = "", las = 2, horiz = TRUE,
    cex.axis = cex_x, space = 0, axis.lty = 1, col = "#8cb2b2", yaxs = "i")
  title(xlab="# of years", line=2, cex.lab=1)

  #barplot stations
  par(mar = c(4, 7+madj_y, 1.5, .5))
  barplot(apply(x>0, 2, sum), ylab = "",
    las = 2, cex.axis = cex_y, space = 0, col = "#db7170", xaxs = "i")
  title(ylab="# of variables", line=2.5, cex.lab=1)

}
