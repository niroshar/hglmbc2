% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FitHGLMBC.R
\name{hglmbc}
\alias{hglmbc}
\title{Fitting Hierarchical Generalized Linear Models with Bias Correction in Small Area Estimation}
\usage{
hglmbc(
  data,
  mformula = NULL,
  dom = NULL,
  y.family = "binomial",
  rand.family = "gaussian",
  tol = 1e-05,
  ...
)
}
\arguments{
\item{data}{a data frame.}

\item{mformula}{an object of class \code{\link{myFormula}}: a symbolic description of the model to
be fitted. The details of the \code{mformula} is given under details section.}

\item{dom}{a domain/cluster/small area to specify the random effect. e.g. numeric zipcode, county,
or state code, and also name of the county or name of the state.}

\item{y.family}{a distribution from _exponential family_ to describe the error distribution. See
\code{"\link[=family]{family}"}.}

\item{rand.family}{a discription of the distribution of random effects.}

\item{tol}{predefined tolerance value. Default value is tol=1e-10.}

\item{...}{other arguments, See details section.}
}
\value{
An object of class \code{hglmbc} consists of the hierarchical maximum likelihood estimates (HMLEs)
of fixed effects, random effects, and variance parameters with other values,
    \item{est.beta}{HMLE of fixed effects.}
    \item{re}{HMLE of the random effects.}
    \item{var.par}{HMLE of the dispersion parameter for the random effects.}
    \item{fe.cov}{the estimated variance-covariance matrix of the fixed effects.}
    \item{fe.cov}{the estimated vaiance-covaraince matrix of the random effects.}
    \item{iter}{number of iterations at convergence.}
    \item{AICBIC}{A list of likelihood values for model selection purposes,where \code{AIC} is
    the \code{AIC} value, BIC is the \code{BIC} value (\code{"\link[=AIC]{AIC}"}), \code{hLik}
    is the \code{h-likelihood} value.}
    \item{summary}{a summary object of the fitted model.}
}
\description{
\code{hglmbc} is used to fit hierarchical generalized linear models using h-likelihood
with bias correction in small area estimation (SAE) for exponential
family distributions with normally distributed random effects.
}
\details{
A typical model has the form \code{response ~ terms} where the response is the (numeric)
vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}.
A terms are specified as a linear combination of predictors, in Small Area Estiamtion (SAE), it is
a vector of fixed effects. e.g. \code{y ~ x1 + x2 + as.factor(x3)}.

If a \code{formula} is not defined, the user can input the response variable, \code{resp}, a vector
of fixed effects, such as \code{fe.disc} for categorical variables and \code{fe.cont} for continuous
variables. If not, the user can define the \code{resp}, then, it will automatically select the
\code{fixed effects} based on the variable types.

The \code{hglmbc} function also has the flexibility to use different reference category for factor
variables. e.g. \code{ref.group} can be defined for each categorical variable. By default the
reference group will be considered in alphebetical order or numerical order. In order to use a different
reference group, \code{ref.group} needs to be defined. e.g. for categorical variables
age (groups: 1, 2, 3), if the prefered reference group is 2, then set \code{ref.group = "age2"}.
}
\examples{
\dontrun{
# Using ever use of smoke data set. Discrete and continuous variables are defined.
data <- eversmoke
mformula <- "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race)
+ as.factor(year) + povt_rate"
dom <- "county"
y.family <- "binomial"
rand.family <- "gaussian"
hglmbc.fit <- hglmbc(data=eversmoke, mformula, dom = "county", y.family="binomial")

# mformula is not defined,
resp <- "smoke_ever"
dom <- "county"
catX <- c("year","gender","race","age")
contX <- "povt_rate"

hglmbc.fit <- hglmbc(data = eversmoke, resp, dom = "county",fe.disc = catX,
fe.cont = contX, y.family = "binomial")


# Poisson-Normal HGLM
N = 1000
p = 20
nVars = 5
x = matrix(rnorm(N * p), N, p)
beta = rnorm(nVars)
f = x[, seq(nVars)] \%*\% beta
mu = exp(f)
y = rpois(N, mu)

}
}
\seealso{
\code{\link{family}}, \code{\link{DesignM}}
}
\author{
Nirosha Rathnayake, Dai (Daisy) Hongying
}
