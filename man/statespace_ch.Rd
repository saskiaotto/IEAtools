% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statespace_ch.R
\name{statespace_ch}
\alias{statespace_ch}
\title{Convex hull in 2-dimensional space}
\usage{
statespace_ch(x, y, time, period_ref, period_current)
}
\arguments{
\item{x}{The coordinates of points in the first dimension
(e.g. indicator 1 or PC1 scores from a PCA).}

\item{y}{The coordinates of points in the second dimension
(e.g. indicator 2 or the PC2 scores from a PCA).}

\item{time}{A vector containing the actual time series.}

\item{period_ref}{Vector of time units (e.g. years) used as reference
period (minimum of 3 time units required).}

\item{period_current}{Vector of time units (e.g. years) used as current
period to compare with the reference period (minimum of 3 time units
required).}
}
\value{
The function returns a list with the following elements
\describe{
  \item{\code{ch_ref}}{A vector of the position of the convex hull of the
  reference period.}
  \item{\code{ch_cur}}{A vector of the position of the convex hull of the
  current period.}
  \item{\code{inside_ch_ref}}{A logical vector indicating whether each
  year (time step) of the current period lies inside (TRUE) or outside
  (FALSE) the state space domain of the reference period.}
  \item{\code{xy}}{A data frame of the x and y coordinates.}
  \item{\code{time}}{A vector of the full time series.}
  \item{\code{period_ref}}{A vector of years (time steps) defined as
  the reference period.}
  \item{\code{period_current}}{A vector of years (time steps) defined as
  the current period.}
}
}
\description{
\code{statespace_ch} calculates the convex hull in 2-dimensional space, e.g.
for two selected indicators, using the \code{\link[tripack]{tri.mesh}}
function.
}
\details{
\code{statespace_ch} implements a second state space approach to assess
the development of a suite of ecological state indicators
(Otto \emph{et al.} 2018, Tett \emph{et al.} 2008). While unidimensional
approaches such as the Euclidean distance (see \code{\link{statespace_ed}})
feature the disadvantage of defining one particular year or time step as
reference condition, this approach accounts for inter-annual variation by
defining a reference domain in state space based on several years: more
recent observations might be characterized as either within or outside this
domain.

The reference domain can be described by a convex hull, which is a multivariate
measure derived from computational geometry representing the smallest convex set
containing all the points in Euclidean plane or in Euclidean space
(de Berg \emph{et al.}, 2008). While the convex hull can be calculated for
high-dimensional data, reducing the space to two dimensions allows for an easier
visualization and interpretation. Therefore, the \code{statespace_ch} function only
calculates the convex hull for two dimensions, i.e. for two indicators or principal
axes obtained by multivariate analysis such as a Principal Component Analysis
(PCA).
}
\examples{
x <- rnorm(25)
y <- rnorm(25)
statespace_ch(x, y, time = 1981:2005, period_ref = 1981:1986, period_current = 1995:2005)
}
\references{
de Berg, M., Cheong, O., van Kreveld, M., Overmars, M. (2008) Computational
Geometry - Algorithms and Applications. Springer Berlin Heidelberg, 386pp.

Otto, S.A., Kadin, M., Casini, M., Torres, M.A., Blenckner, T. (2018)
A quantitative framework for selecting and validating food web indicators.
\emph{Ecological Indicators}, 84: 619-631,
doi: https://doi.org/10.1016/j.ecolind.2017.05.045

Tett, P., Carreira, C., Mills, D.K., van Leeuwen, S., Foden, J., Bresnan, E.,
Gowen, R.J. (2008) Use of a Phytoplankton Community Index to assess the
health of coastal waters. \emph{ICES Journal of Marine Science} 65, 1475-1482.
}
\seealso{
\code{\link[tripack]{tri.mesh}} for the computation of the convex hull.
}
\author{
Saskia A. Otto
}
