h-likelihood with bias correction in SAE (hglmbc2) vignette
================
Nirosha Rathnayake, Dai (Daisy) Hongying

<!-- README.md is generated from README.Rmd. Please edit that file -->

# hglmbc2 <img src="man/figures/logohglmbc2.png" align="right" height="120" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/niroshar/hglmbc2.svg?branch=master)](https://travis-ci.com/niroshar/hglmbc2)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/niroshar/hglmbc2?branch=master&svg=true)](https://ci.appveyor.com/project/niroshar/hglmbc2)
[![Codecov test
coverage](https://codecov.io/gh/niroshar/hglmbc2/branch/master/graph/badge.svg)](https://codecov.io/gh/niroshar/hglmbc2?branch=master)
[![R build
status](https://github.com/niroshar/hglmbc2/workflows/R-CMD-check/badge.svg)](https://github.com/niroshar/hglmbc2/actions)
<!-- badges: end -->

The goal of `hglmbc2` is to make inferences in Small Area Estimation
based on Hierarchical (\(h-\))likelihood approach with bias correction.
The model parameters are obtained through an iterative approximation
based on Newton Raphson method combined with bias correction of
estimates. The bias correction approach enhances the accuracy of maximum
hierarchical likelihood estimates (MHLEs). This R package can be used to
obtain improved MHLEs for `fixed effects`, `random effects`, and
`dispersion parameters` for exponential family distributions with random
effect \(u\sim N(0, \sigma^2)\).

## Installation

You can install the released version of hglmbc2 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hglmbc2")
```

or, you can install the development version of hglmbc2 using
[devtools](https://devtools.r-lib.org/) with:

``` r
# devtools::install_github("niroshar/hglmbc2", force = TRUE)
```

#### Example 1

This is a basic example which shows you how to solve a common problem:
