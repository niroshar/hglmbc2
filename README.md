h-likelihood with bias correction in SAE (hglmbc) vignette
================
Nirosha Rathnayake, Dai (Daisy) Hongying

<!-- README.md is generated from README.Rmd. Please edit that file -->

# hglmbc <img src="man/figures/logoHGLMBC1.png" align="right" height="120" />

# hglmbc2

<!-- badges: start -->

<!-- badges: end -->

The goal of hglmbc2 is to make inferences in Small Area Estimation based
on Hierarchical (\(h-\))likelihood approach with bias correction. The
model parameters are obtained through an iterative approximation based
on Newton Raphson method. The bias correction approach enhances the
accuracy of maximum hierarchical likelihood estimates (MHLEs). This R
package can be used to obtain improved MHLEs for `fixed effects`,
`random effects`, and `dispersion parameters` for exponential family
distributions with random effect `u` being normally distributed.

## Installation

You can install the released version of hglmbc from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hglmbc")
```

or, you can install the development version of hglmbc using
[devtools](https://devtools.r-lib.org/) with:

``` r
# devtools::install_github("niroshar/hglmbc", force = TRUE)
```

#### Example 1

This is a basic example which shows you how to solve a common problem:
