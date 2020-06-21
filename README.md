
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
based on Hierarchical \((h-)\)likelihood approach with bias correction.
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
\(u \sim N(0,\sigma^2)\).

``` r
library(hglmbc2)

# basic example code
data <- eversmoke
mformula <- "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race) + as.factor(year) + povt_rate"
dom <- "county"
y.family <- "binomial"
rand.family <- "gaussian"

# Fit the model
hglmbc.fit <- hglmbc(data = eversmoke, mformula, dom = "county", y.family = "binomial")
#> Fixed effects and random effects are not defined, selected by variable type !!!!

## MHLEs of fixed effects
hglmbc.fit$est.fe
#>           Estimate Std.Error   Z Value P(>|Z|)
#> Intercept -1.77212   0.15260 -11.61284 0.00000
#> age2       0.47708   0.05213   9.15174 0.00000
#> age3       1.00272   0.06069  16.52200 0.00000
#> gender2    0.13512   0.02828   4.77793 0.00000
#> povtrate   3.80385   0.86778   4.38343 0.00001
#> race2     -0.47891   0.05268  -9.09093 0.00000
#> race3      0.04269   0.03866   1.10424 0.26949
#> race4     -0.17499   0.04980  -3.51386 0.00044
#> year2     -0.32813   0.05561  -5.90056 0.00000


# The distribution of y|u not defined,
hglmbc.fit1 <- hglmbc(data = eversmoke, mformula, dom = "county")
#> Fixed effects and random effects are not defined, selected by variable type !!!!

# Model fit summary
hglmbc.fit1$summary
#> $`Model formula`
#> [1] "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race) + as.factor(year) + povt_rate"
#> 
#> $`random effect`
#> [1] "county"
#> 
#> $`fixed effects estimates`
#>           Estimate Std.Error   Z Value P(>|Z|)
#> Intercept -1.77212   0.15260 -11.61284 0.00000
#> age2       0.47708   0.05213   9.15174 0.00000
#> age3       1.00272   0.06069  16.52200 0.00000
#> gender2    0.13512   0.02828   4.77793 0.00000
#> povtrate   3.80385   0.86778   4.38343 0.00001
#> race2     -0.47891   0.05268  -9.09093 0.00000
#> race3      0.04269   0.03866   1.10424 0.26949
#> race4     -0.17499   0.04980  -3.51386 0.00044
#> year2     -0.32813   0.05561  -5.90056 0.00000
#> 
#> $`dispersion paramerer`
#>         [,1]
#> [1,] 0.20757
#> 
#> $`hglm model inference`
#>        AIC      BIC      hLik
#> 1 29221.83 29303.13 -14600.91
#> 
#> $iter
#> [1] 11
#> 
#> $` `
#> [1] "Converged in 11 iterations with tol = 5.01376296657696e-06."
#> 
#> attr(,"class")
#> [1] "summary.hglm.fit"
```

#### Example: method 2

``` r
# mformula is not defined,
data <- eversmoke
resp <- "smoke_ever"
dom <- "county"
fe.disc <- c("year","gender","race","age")
fe.cont <- "povt_rate"

# hglmbc.fit <- hglmbc(data = eversmoke, resp = "smoke_ever", dom = "county",fe.disc = fe.disc,fe.cont = fe.cont, y.family = "binomial")
# hglmbc.fit
```
