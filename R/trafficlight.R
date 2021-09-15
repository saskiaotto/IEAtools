#' Create a traffic light plot
#'
#' This function creates for multiple time series an image plot where the color
#' code is based on selected quantiles or evenly spaced intervals.
#'
#' @name trafficlight
#' @param x matrix or data frame containing time series of multiple variables.
#' @param time vector of time units that will be used for the x axis.
#' @param sort_5yrmean logical; should the variables be sorted by the first 5yr mean? Default
#'   is set to TRUE.
#' @param sort_vec integer vector; if specific order of variable is desired the sorting index should be
#'   provided here.
#' @param method character; the type of methods to create the color code. Choose
#'   between using "quantiles" (default) or "intervals".
#' @param probs a vector of probabilities to calculate the quantiles using the function
#'   \code{quantile}. Should include the probabilities 0 and 1.
#'   numeric vector of probabilities with values in [0,1]. (Values up to
#'   2e-14 outside that range are accepted and moved to the nearby endpoint.)
#' @param quantile_type an integer between 1 and 9 selecting one of the nine quantile
#'   algorithms detailed below to be used. Default is 7 (see also \code{\link{quantile}}).
#' @param intervals logical; number of evenly spaced intervals. Default is 5.
#' @param cols a character vector with colors for each quantile or interval.
#' @param main a title.
#' @param xlab The x axis title. Default is none.
#' @param ylab The y axis title. Default is none.
#' @param adj_xlab integer; vertical adjustment of the x axis title.
#' @param adj_ylab integer; horizontal adjustment of the y axis title.
#' @param hadj_x double; horizontal adjustment of the x labels.
#' @param vadj_x double; vertical adjustment of the x labels.
#' @param hadj_y double; horizontal adjustment of the y labels.
#' @param vadj_y double; vertical adjustment of the y labels.
#' @param madj_bottom double; adjustment of bottom margin relative to the
#'   current setting.
#' @param madj_left double; adjustment of left margin relative to the
#'   current setting.
#' @param madj_top double; adjustment of top margin relative to the
#'   current setting.
#' @param madj_right double; adjustment of right margin relative to the
#'   current setting.
#' @param legend_pos character; the legend is shown on the "top" (default),
#'   "center", or "bottom" right of the plot.
#' @param legend_intersp_x double; character interspacing factor for horizontal (x) spacing in legend.
#' @param legend_intersp_y double; interspacing factor for vertical (y) line distances in legend.
#' @param tick_x logical; set to TRUE if ticks on x-axis should be displayed.
#' @param tick_y logical; set to TRUE if ticks on x-axis should be displayed.
#' @param cex_x double; the magnification to be used for the x labels relative to the
#'   current setting of cex.
#' @param cex_y double; the magnification to be used for the y labels relative to the
#'   current setting of cex.
#' @param cex_xlab double; the magnification to be used for the x axis title relative
#'   to the current setting of cex.
#' @param cex_ylab double; the magnification to be used for the y axis title  relative
#'   to the current setting of cex.
#' @param cex_legend double; the magnification to be used for the legend relative to
#'   the current setting of cex.
#' @param cex_main double; the magnification to be used for the title relative to the
#'   current setting of cex.
#' @param respect logical; this argument controls whether a unit column-width is the
#'   same physical measurement on the device as a unit row-height (default is TRUE).
#'
#' @export
#' @author Saskia A. Otto
#' @examples
#' df <- data.frame(variable1 = 1:20, variable2= rnorm(20, 100, 30), variable3 = 1:20 + rnorm(20))
#' trafficlight(x = df, time = 1981:2000)
#' df <- matrix(rnorm(100), ncol = 5)
#' colnames(df) <- letters[1:5]
#' trafficlight(x = df, time = 1981:2000, legend_pos = "bottom", method = "intervals")

trafficlight <- function(x, time = NULL, sort_5yrmean = TRUE, sort_vec = NULL,
  method = "quantiles", probs = seq(0, 1, 0.2), quantile_type = 7, intervals = 5,
  cols = c("green3", "greenyellow", "yellow", "gold","red"),
  main = "", xlab = "", ylab = "", adj_xlab = NULL, adj_ylab = NULL,
  hadj_x = 0.5, vadj_x = -0.7, hadj_y = -0.7, vadj_y = 0.3,
  madj_bottom = 0, madj_left = 0, madj_top = 0, madj_right = 0,
  legend_pos = "top", legend_intersp_x = 1, legend_intersp_y = 1,
  tick_x = FALSE, tick_y = FALSE,
  cex_x = 0.8, cex_y = 0.8, cex_xlab = 1, cex_ylab = 1,
  cex_legend = 0.8, cex_main = 1, respect = TRUE) {


  ### Data input validation
  if (!method %in% c("quantiles", "intervals")) {
    stop("Choose as method either 'quantiles' or 'intervals'.")
  }
  if (method == "intervals") {
    if (length(cols) != intervals) {
      stop("The number of colours need to match your chosen interval number.")
    }
  }
  # --------------------------

  z <- x

  # Save the dimensions (number of rows[1] and columns[2] ) in a vector
  n <- dim(z)
  # Get names for the y-axis
  ylabel <- colnames(z)
  if (!is.null(time)) {
    xlabel <- time
  } else {
    xlabel <- 1:n[1]
  }

  # Converting the original values into quantiles or even intervals
  convert2quantiles <- function(v, probs, type, var) {
    br <- stats::quantile(v, probs = probs, na.rm = TRUE, type = type)
    if (any(diff(br) == 0)) {
      sel <- which(diff(br) == 0)
      br[sel+1] <- br[sel+1] + 0.0001 # br[sel]/10000
      print(paste0("For variable '", var, "' the ", probs[sel+1]*100,
        "%-quantile is the same as the ", probs[sel]*100,
        "%-quantile. All values are grouped under the lower quantile!"))
    }
    qv <- cut(v, breaks = br, include.lowest = TRUE, labels = FALSE)
    return(qv)
  }

  convert2intervals <- function(v, intervals) {
    qv <- cut(v, breaks = intervals, include.lowest = TRUE, labels = FALSE)
    return(qv)
  }

  zc <- z
  if (method == "quantiles") {
    for (i in 1:n[2]) {
      zc[,i] <- convert2quantiles(zc[,i], probs, type = quantile_type, var = names(z)[i])
      legend_txt <- paste0("< ", probs[-1]*100, "%")
      nl <- length(legend_txt)
      legend_txt[nl] <- paste0("> ", probs[nl]*100, "%")
      legend_txt <- c(legend_txt, "missing value")
    }
  } else {
    if (intervals %% 1 == 0) { # is full number?
      for (i in 1:n[2]) {
        zc[,i] <- convert2intervals(zc[,i], intervals)
        legend_txt <- as.character(1:intervals)
        legend_txt[1] <- paste0(legend_txt[1], " (low)")
        nl <- length(legend_txt)
        legend_txt[nl] <- paste0(legend_txt[nl], " (high)")
        legend_txt <- c(legend_txt, "missing value")
      }
    } else {
      stop(" If you want to use evenly spaced intervals, provide an integer for the number of intervals!")
    }
  }


  ### Sort variables according to settings
  if (isTRUE(sort_5yrmean)) {
    m5 <- vector(length = n[2])
    # Then fill the vector with the standardised variable averages over the
    # first five data points (mean of first 5 years - mean of full time series/
    # standard deviation of full time series)
    for (i in 1:(n[2])) {
      m5[i] <- (mean(x[c(1:5), i], na.rm = TRUE) -
          mean(x[, i], na.rm = TRUE)) /
        stats::sd(x[, i], na.rm = TRUE) }
    # Finally, order the variable and create an index vector
    ordvar <- order(m5)
  } else {
    if (is.null(sort_vec)) {
      ordvar <- n[2]:1
    } else {
      ordvar <- rev(sort_vec)
    }
  }
  zc_sort <- as.matrix(zc[ ,ordvar])


  ### Plot settings
  x <- 1:n[1]
  y <- 1:n[2]
  # Position of legend:
  if (legend_pos == "top") {
    xleg <- max(x)+1
    yleg <- max(y)+.5
    yjustleg <- 1
  } else {
    if (legend_pos == "center") {
      xleg <- max(x)+1
      yleg <- max(y)/2+.5
      yjustleg <- 0.5
    } else {
      if (legend_pos == "bottom") {
        xleg <- max(x)+1
        yleg <- min(y)-.5
        yjustleg <- 0
      } else {
        stop("You need to choose as legend position 'bottom', 'center' or 'top'.")
      }
    }
  }

  mar <- c(2+madj_bottom, 5+madj_left, 1+madj_top, 8+madj_right)
  if (nchar(xlab) > 0) mar[1] <- mar[1] + 1
  if (nchar(ylab) > 0) mar[2] <- mar[2] + 1
  if (nchar(main) > 0) mar[3] <- mar[3] + 2
  if (is.null(adj_xlab)) {
    adj_xlab <- mar[1]-1
  } else {
    adj_xlab <- adj_xlab
  }
  if (is.null(adj_ylab)) {
    adj_ylab <- mar[2]-1
  } else {
    adj_ylab <- adj_ylab
  }

  ### Plot
  graphics::layout(matrix(c(1,1,1,1), ncol = 2), widths = c(3.5,3.5), heights = c(2,2),
    respect = respect)
  graphics::par(mar = c(mar[1], mar[2], mar[3], mar[4]), oma = c(0.5,.5,0,0), xpd = TRUE)
  graphics::image(x, y, z = zc_sort, zlim = c(1,5), col = cols,
    axes = FALSE, xlab = "", ylab = "")
  if (isTRUE(tick_x)) graphics::axis(1, at = x, tick = -.015, labels = NA)
  if (isTRUE(tick_y)) graphics::axis(2, at = y, tick = -.015, labels = NA)
  graphics::axis(1, at = x, tick = FALSE, labels = xlabel, cex.axis = cex_x, las = 3,
    line = vadj_x, padj = hadj_x)
  graphics::axis(2, at = y, tick = FALSE, labels = ylabel[ordvar], cex.axis = cex_y,
    las = 1, line = hadj_y, padj = vadj_y)
  graphics::box()
  graphics::legend(x = xleg, y = yleg, legend = legend_txt, fill = c(cols, "white"),
    cex = cex_legend, bty = "n", xjust = .1, yjust = yjustleg,
    x.intersp = legend_intersp_x, y.intersp = legend_intersp_y, )
  graphics::title(main, cex.main = cex_main)
  if (nchar(xlab) > 0) {
    graphics::mtext(text = xlab, side = 1, line = adj_xlab, cex = cex_xlab)
  }
  if (nchar(ylab) > 0) {
    graphics::mtext(text = ylab, side = 2, line = adj_ylab, cex = cex_ylab)
  }

}
