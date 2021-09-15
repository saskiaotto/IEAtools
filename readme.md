
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IEAtools

<!-- <img src="tools/images/rquiz_logo.png" align="right" width="100" height="112" /> -->

## Overview

IEAtools is an R package that provides supporting functions for
conducting Integrated Ecosystem Assessments (IEA).

## Installation

The package is in its early development and not yet on CRAN. To install
the development version from GitHub directly use the
[remotes](https://remotes.r-lib.org/) package:

``` r
# install.packages("remotes")
remotes::install_github("saskiaotto/IEAtools")
```

If you encounter a clear bug, please file a minimal reproducible example
on github. For questions email me any time.

## Functions

Here is a short overview of functions and methods included in the
package:

### Data exploration

  - Explore gaps in the multiple time series missing values:
    `explore_na()`
      - → This function creates an image plot where available values for
        the different variables and years are indicated in gray and
        missing values in white. At the right side and bottom, two
        barplots are added showing the frequency of available variables
        per year and the available years per variable.
  - Impute missing values by local mean: `impute()`
      - → The function substitutes missing values in time series with a
        mean of the  previous and following years (or less depending on
        the position of NAs, whether it is at the beginning or end of
        the time series, and the presence of further NAs in the selected
        time period).
  - Calculate the variance inflation factor (VIF): `calc_vif()`
  - Compute (partial) autocorrelation functions and test for
    significance: `test_tac()`
  - Create a traffic light plot or heatmap: `trafficlight()`
      - → This function creates for multiple time series an image plot
        where the color code is based on selected quantiles or evenly
        spaced intervals.

### Assessment of current state status

Two approaches are included in IEAtools that are based on trajectories
in state space to determine the current state of the system in
comparison to an earlier period as reference using the selected IND
suite (state space = n-dimensional space of possible locations of
variables). The functions are imported from the INDperform package and
will be in the future only available in IEAtools.

1.  Calculation of the **Euclidean distance** in state space of any
    dimensionality between each single year (or any other time step
    used) and a defined reference year.
    
      - `statespace_ed()` calculates the Euclidean distance over time.
      - `plot_statespace_ed()` creates a ggplot2 object of the Euclidean
        distance trend.

2.  Given the identification of a reference domain in state space, more
    recent observations might lie within or outside this domain. The
    **convex hull** is a multivariate measure derived from computational
    geometry representing the smallest convex set containing all the
    reference points in Euclidean plane or space. For visualization,
    only 2 dimensions considered (dimension reduction through
    e.g. Principal Component Analysis suggested).
    
      - `statespace_ch()` calculates the convex hull for 2 defined
        periods (current and reference) in the x-y space (i.e. 2 IND or
        2 Principal Components).
      - `plot_statespace_ch()` creates a ggplot2 object showing all
        observed combinations in x-y space as well as the convex hull of
        both periods. The proportion of the recent time period within
        the reference space is additionally provided.
