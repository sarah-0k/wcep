
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wcep

<!-- badges: start -->

[![R-CMD-check](https://github.com/sarah-0k/wcep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sarah-0k/wcep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of wcep is to analyze time-to-event data with multiple event
types and/or recurrent events (e.g., death and shock) with related
severity weights. The weight of terminal events (death) is always 1.
Non-terminal and recurrent events have a weight on the interval (0, 1).
Right censoring occurs when an event is given weight 0.

This package provides three main functions: `wcep()` estimates
event-weighted survival probabilities with estimated variance, `plot()`
produces Kaplan-Meier-like curves of event-weighted survival with
pre-specified confidence intervals, and `wcep_logrank()` provides a
log-rank type test statistic with corresponding p-value to compare two
groups. A simple dataset (`toyexample`) is provided.

## Installation

You can install the released version of wcep from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wcep")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("sarah-0k/wcep")
```

## Example

Basic examples provided in the Help document.
