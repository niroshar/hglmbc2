#' Fitting Hierarchical Generalized Linear Models with Bias Correction in Small Area Estimation
#'
#' @description \code{hglmbc} is used to fit hierarchical generalized linear models using h-likelihood
#' with bias correction in small area estimation (SAE) for exponential
#' family distributions with normally distributed random effects.
#' @param data a data frame.
#' @param mformula an object of class \code{\link{myFormula}}: a symbolic description of the model to
#' be fitted. The details of the \code{mformula} is given under details section.
#' @param dom a domain/cluster/small area to specify the random effect. e.g. numeric zipcode, county,
#' or state code, and also name of the county or name of the state.
#' @param y.family a distribution from _exponential family_ to describe the error distribution. See
#' \code{"\link[=family]{family}"}.
#' @param rand.family a discription of the distribution of random effects.
#' @param tol predefined tolerance value. Default value is tol=1e-10.
#' @param ... other arguments, See details section.
#'
#' @import dplyr
#' @importFrom fastDummies dummy_cols
#' @importFrom Matrix Diagonal
#' @importFrom corpcor pseudoinverse
#'
#' @details A typical model has the form \code{response ~ terms} where the response is the (numeric)
#' vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}.
#' A terms are specified as a linear combination of predictors, in Small Area Estiamtion (SAE), it is
#' a vector of fixed effects. e.g. \code{y ~ x1 + x2 + as.factor(x3)}.
#'
#' If a \code{formula} is not defined, the user can input the response variable, \code{resp}, a vector
#' of fixed effects, such as \code{fe.disc} for categorical variables and \code{fe.cont} for continuous
#' variables. If not, the user can define the \code{resp}, then, it will automatically select the
#' \code{fixed effects} based on the variable types.
#'
#' The \code{hglmbc} function also has the flexibility to use different reference category for factor
#' variables. e.g. \code{ref.group} can be defined for each categorical variable. By default the
#' reference group will be considered in alphebetical order or numerical order. In order to use a different
#' reference group, \code{ref.group} needs to be defined. e.g. for categorical variables
#' age (groups: 1, 2, 3), if the prefered reference group is 2, then set \code{ref.group = "age2"}.
#'
#' @return An object of class \code{hglmbc} consists of the hierarchical maximum likelihood estimates (HMLEs)
#' of fixed effects, random effects, and variance parameters with other values,
#'     \item{est.beta}{HMLE of fixed effects.}
#'     \item{re}{HMLE of the random effects.}
#'     \item{var.par}{HMLE of the dispersion parameter for the random effects.}
#'     \item{fe.cov}{the estimated variance-covariance matrix of the fixed effects.}
#'     \item{fe.cov}{the estimated vaiance-covaraince matrix of the random effects.}
#'     \item{iter}{number of iterations at convergence.}
#'     \item{AICBIC}{A list of likelihood values for model selection purposes,where \code{AIC} is
#'     the \code{AIC} value, BIC is the \code{BIC} value (\code{"\link[=AIC]{AIC}"}), \code{hLik}
#'     is the \code{h-likelihood} value.}
#'     \item{summary}{a summary object of the fitted model.}
#'
#' @author Nirosha Rathnayake, Dai (Daisy) Hongying
#'
#' @seealso \code{\link{family}}, \code{\link{DesignM}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using ever use of smoke data set. Discrete and continuous variables are defined.
#' data <- eversmoke
#' mformula <- "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race)
#' + as.factor(year) + povt_rate"
#' dom <- "county"
#' y.family <- "binomial"
#' rand.family <- "gaussian"
#' hglmbc.fit <- hglmbc(data=eversmoke, mformula, dom = "county", y.family="binomial")
#'
#' # mformula is not defined,
#' resp <- "smoke_ever"
#' dom <- "county"
#' catX <- c("year","gender","race","age")
#' contX <- "povt_rate"
#'
#' hglmbc.fit <- hglmbc(data = eversmoke, resp, dom = "county",fe.disc = catX,
#' fe.cont = contX, y.family = "binomial")
#'
#'
#' # Poisson-Normal HGLM
#' N = 1000
#' p = 20
#' nVars = 5
#' x = matrix(rnorm(N * p), N, p)
#' beta = rnorm(nVars)
#' f = x[, seq(nVars)] %*% beta
#' mu = exp(f)
#' y = rpois(N, mu)
#'
#' }
hglmbc <- function(data, mformula = NULL, dom = NULL, y.family = "binomial",rand.family = "gaussian", tol=1e-05, ...)
{

  fit.hglmbc <- match.call()
  namedList <- list()

  # print(head(data))

  source("R/integratedFuncs.R")

  # cat(paste0("y family: ", y.family, "\n"))

  # final_data <- dataIn(data, y, dis_vars, cont_vars, re, ref_grp)
  # data set and dom need to be required
  if(!exists("data") | is.null(dom)){
    stop("Error! please define the data frame and random effect component.")
  }

  data <- data[order(data[ ,dom]), ]

  # If mformula and dom is defined
  if(!is.null(mformula) & exists("mformula") & exists("dom")){
    varOut <- getVars(mformula)
    resp <- as.character(varOut[[1]])
    fe.disc <- as.vector(varOut[[2]])
    fe.cont <- as.vector(varOut[[3]])
  }

  # if mformula is not defined, but resp is defined
  if(is.null(mformula) & exists("resp") & exists("dom")){
    myformula <- myFormula(data, resp, dom)
    # mformula <- as.formula(myformula)
    varOut <- getVars(myformula)
    resp <- as.character(varOut[[1]])
    fe.disc <- as.vector(varOut[[2]])
    fe.cont <- as.vector(varOut[[3]])

  }

  if(is.null(mformula) & !exists("resp")){
    stop("At least mformula or the response variable (resp) with dom need to be defined!")
  }

  # cov_data <- data[ ,!colnames(data) %in% paste0(y,re)]
  fe.data <- data[ ,colnames(data) %in% c(fe.cont,fe.disc)]

  ## Get the design matri for user defined referenced group
  # Reference group for each categorical variable
  if(!exists("ref.group")){
    minVal <- apply(data[ ,fe.disc], 2, min)
    ref.group <- as.vector(paste0(names(minVal),minVal))
  }

  X <- DesignM(data=fe.data,fe.cont,fe.disc,ref.group)

  y <- data[ ,paste0(resp)]
  uDom <- data[ ,paste0(dom)]

  ## Initial parameters
  m <- length(unique(uDom))
  theta0 <- 0.1
  u0 <- as.matrix(rnorm(m,0,sd=sqrt(theta0)))
  X <- as.matrix(X)
  N <- nrow(data)
  p <- ncol(X)
  Z <- model.matrix(~0+as.factor(data[ ,dom]))

  resp <- resp

  # cat(paste0("resp: ", resp, "\n"))

  # cat(paste0("dom: ", dom, "\n"))

  ## Call function to obtain initial parameters
  beta0 <- initPar(data, resp, dom)
  beta00 <- bInitOrder(beta0,fe.cont)

  beta_new <- as.matrix(beta00$est)
  u_new <- u0
  theta_new <- theta0
  zeta_new <- rep(1,m)

  cnty <- unique(uDom)



  # re_names <- c()
  # for(i in 1:length(cnty)){
  #   re_names <- c(re_names,paste0(cnty[i]))
  # }
  # # re_names <- as.numeric(re_names)
  # u0 <- data.frame(county = re_names,est=u0)

  delta <- NULL
  beta_new_all <- beta_new ;u_new_all <- u_new; theta_all <- theta_new
  theta_new_all <- c(theta_new); delta_final <- 0 ; conv_iter <- 0; iter <- 0
  col_names <- c()


  repeat{

    beta <- as.vector(beta_new)
    beta <- round(beta,10)
    # beta <- beta_new
    u <- as.vector(u_new)
    theta <- theta_new

    BU_old <- rbind(as.matrix(beta),as.matrix(u))

    theta_inv <- solve(theta)
    G_inv <- kronecker(theta_inv, diag(m))

    P <- 1/(1+exp(-(X%*%beta+Z%*%u)))
    pp <- P*(1-P)
    W <- Diagonal(x=pp)

    D2h_BB <- t(X)%*%W%*%X   # or crossprod(t(crossprod(X,W)),X)
    D2h_BU <- t(X)%*%W%*%Z
    D2h_UB <- t(Z)%*%W%*%X
    D2h_UU <- t(Z)%*%W%*%Z + G_inv
    J <- rbind(cbind(D2h_BB,D2h_BU),cbind(D2h_UB,D2h_UU))   # (m+p) by  (p+m) Jacobian
    J <- as.matrix(J)

    # library(corpcor)
    # J_inv <- solve(J, tol=10^-20)
    # For non-singular matrices the pseudoinverse is
    # equivalent to the standard inverse.
    J_inv <- pseudoinverse(J)

    # J_inv <- func_Jinv(beta,u_new,G_inv)

    # yp <- y - P
    D1h_B <- t(X)%*%(y-P)
    D1h_u <- t(Z)%*%(y-P)- G_inv%*%u

    S <- as.vector(rbind(D1h_B, D1h_u))    # Gradient (score function) vector

    BU_new = BU_old + (J_inv%*%S)    # Newton Raphson

    beta_new <- as.vector(BU_new[1:length(beta)])
    u_new <- as.vector(BU_new[(length(beta)+1):length(BU_new)])

    # convergence_beta <- abs(beta_new[-1]-beta[-1])
    convergence_beta <- abs(beta_new-beta)
    convergence_u <- abs(u_new-u)
    max(convergence_beta)
    max(convergence_u)

    beta_new_all <- cbind.data.frame(beta_new_all,value=round(beta_new,6))
    u_new_all <- cbind.data.frame(u_new_all,value=round(u_new,6))

    rm(P); rm(W); rm(J);
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
    #                                                                                 #
    #                                Bias Correction                                  #
    #                                                                                 #
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # Bias Corr step 1: update J with estimated beta and u

    P <- 1/(1+exp(-(X%*%beta_new+Z%*%u_new)))
    pp <- P*(1-P)
    W <- Diagonal(x=pp)

    D2h_BB <- t(X)%*%W%*%X   # or crossprod(t(crossprod(X,W)),X)
    D2h_BU <- t(X)%*%W%*%Z
    D2h_UB <- t(Z)%*%W%*%X
    D2h_UU <- t(Z)%*%W%*%Z + G_inv
    J <- rbind(cbind(D2h_BB,D2h_BU),cbind(D2h_UB,D2h_UU))   # (m+p) by  (p+m) Jacobian
    J <- as.matrix(J)

    Jhat_Inv <- pseudoinverse(J)
    Tau <- Jhat_Inv[(p+1):(p+m),(p+1):(p+m)]
    Taut <- as.vector(diag(Tau))
    zeta <- as.vector(theta)/(as.vector(theta)+Taut)
    zeta <- as.vector(zeta)
    corr_u <- zeta*u_new     # Corrected u_new

    rm(P); rm(W); rm(J);


    iter <- iter + 1
    ########### ---------------------------------------------------------#############
    ###########             Estimate theta=sigma^2 using Dh/Dtheta=0     #############
    ########### ---------------------------------------------------------#############


    ## Using SAE6 to estimate theta
    theta_inv <- solve(theta)
    G_inv <- kronecker(theta_inv, diag(m))

    P <- 1/(1+exp(-(X%*%beta_new+Z%*%corr_u)))
    # W <- diag(as.vector(P*(1-P)))
    W <- Diagonal(x=as.vector(P*(1-P)))

    D2h_BB <- t(X)%*%W%*%X   # or crossprod(t(crossprod(X,W)),X)
    D2h_BU <- t(X)%*%W%*%Z
    D2h_UB <- t(Z)%*%W%*%X
    D2h_UU <- t(Z)%*%W%*%Z + G_inv
    J <- rbind(cbind(D2h_BB,D2h_BU),cbind(D2h_UB,D2h_UU))   # (143+9) by  (9+143) Jacobian
    J <- as.matrix(J)
    J_inv <- pseudoinverse(J)

    # J_inv <- func_Jinv(beta=beta_new,u_new,G_inv)


    ###Estimate theta
    J_inv22 <- J_inv[(p+1):(p+m),(p+1):(p+m)]
    theta_new <- 1/m*(t(u_new)%*%u_new)+1/m*func_trace(J_inv22)
    theta_new_all <- cbind.data.frame(theta_new_all,value=theta_new)



    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

    ### To get the variance of u based on J
    theta_inv <- solve(theta_new)
    G_inv <- kronecker(theta_inv, diag(m))

    P <- 1/(1+exp(-(X%*%beta_new+Z%*%u_new)))
    W <- Diagonal(x=as.vector(P*(1-P)))

    D2h_BB <- t(X)%*%W%*%X   # or crossprod(t(crossprod(X,W)),X)
    D2h_BU <- t(X)%*%W%*%Z
    D2h_UB <- t(Z)%*%W%*%X
    D2h_UU <- t(Z)%*%W%*%Z + G_inv
    J <- rbind(cbind(D2h_BB,D2h_BU),cbind(D2h_UB,D2h_UU))   # (143+9) by  (9+143) Jacobian
    J <- as.matrix(J)
    J_inv <- pseudoinverse(J)

    # J_inv <- func_Jinv(beta=beta_new,u_new,G_inv)

    fe.cov <- J_inv[1:p,1:p]       ## Var Cov matrix of fixed effects
    re.cov  <- J_inv[(p+1):(p+m),(p+1):(p+m)]    ## Var Cov matrix of random effects

    fe.var <- diag(fe.cov)    ### Var of fixed effects
    fe.var <- round(fe.var,10)
    fe.var <- fe.var[fe.var != 0]  ## Remove coefs of ref group(which has very small values)
    fe.std.err <- round(sqrt(fe.var), 5)

    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

    convergence_theta <- abs(theta_new-theta)
    delta <- max(convergence_beta,convergence_theta)
    delta_final <- cbind.data.frame(delta_final,value=delta)

    col_names <- c(col_names,paste0("iter",iter))

    # cat(paste0("delta: ",delta,"\n \n"))

    if(delta <= tol){break}

  }

  est.beta <- round(beta_new, 5)
  est.theta <- round(theta_new, 5)
  est.re <- data.frame(Domain = cnty, est=round(u_new, 5))

  z1 <- est.beta/fe.std.err
  est.fe <- data.frame(est=est.beta,
                       std.err=fe.std.err,
                       Z0 = round(z1,5),
                       P = round(2*pnorm(-abs(z1)),5))

  colnames(est.fe) <- c("Estimate","Std.Error","Z Value","P(>|Z|)")
  rownames(est.fe) <- beta00[ ,1]

  fit.hglmbc$est.fe <- est.fe
  fit.hglmbc$iter <- iter
  fit.hglmbc$est.beta <- est.beta
  fit.hglmbc$re <- est.re
  fit.hglmbc$var.par <- est.theta
  fit.hglmbc$fe.cov <- fe.cov
  fit.hglmbc$re.cov <- re.cov


  ## Calculate AIC
  P <- 1/(1+exp(-(X%*%est.beta+Z%*%(est.re$est))))
  yt <- 1-y
  ylik <- y*log(P) +yt*log(1-P)
  sumlogylik <- sum(ylik)

  theta_inv <- solve(theta_new)
  G_inv <- kronecker(theta_inv, diag(m))
  u_new <- as.matrix(u_new)

  ulik <- -1/2*t(u_new)%*%G_inv%*%u_new -m/2*log(det(theta_new))+m/2*log(2*pi)
  sumlogulik <- ulik

  hlik <- sumlogylik + sumlogulik

  AIC <- -2*sum(hlik) + 2*(length(est.beta)+1)
  BIC <- -2*sum(hlik) + (length(est.beta)+1)*log(N)
  model.sel <- cbind.data.frame(AIC, BIC,hLik = hlik)

  fit.hglmbc$AICBIC <- model.sel

  namedList <- list(`Model formula` = mformula, `random effect` = dom ,`fixed effects estimates`= est.fe,
                    `dispersion paramerer` = est.theta,
                    `hglm model inference`= model.sel, iter = iter,
                    ` `= paste0("Converged in ",iter," iterations with tol = ",delta,".")
  )
  class(namedList) <- "summary.hglm.fit"

  fit.hglmbc$summary <- namedList

  return(fit.hglmbc)

  rm(mformula); rm(fe.cont); rm(fe.disc)
  # cat(paste0("Converged in ",iter," iterations with tol = ",delta,". \n \n"))

}

