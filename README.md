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

#### Example: method 1

`Binomial-Normal HGLM` is also known as the `mixed logit` model in GLM
family with the binary response variable and the random effect
\(u\sim N(0,\sigma^2)\).

``` r
library(hglmbc2)
## basic example code
data <- eversmoke
mformula <- "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race) + as.factor(year) + povt_rate"
dom <- "county"
y.family <- "binomial"
rand.family <- "gaussian"
## Fit the model
# hglmbc.fit <- hglmbc(data = eversmoke, mformula, dom = "county", y.family = "binomial")
# hglmbc.fit$summary
```

#### Example: method 2

``` r
# mformula is not defined,
data <- eversmoke
resp <- "smoke_ever"
dom <- "county"
fe.disc <- c("year","gender","race","age")
fe.cont <- "povt_rate"

# hglmbc.fit <- hglmbc(data = eversmoke, resp, dom = "county",fe.disc = fe.disc,fe.cont = fe.cont, y.family = "binomial")
# hglmbc.fit
```
