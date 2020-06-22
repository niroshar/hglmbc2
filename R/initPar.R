
#' Obtain initial parameters
#'
#' @param data the data set.
#' @param resp the response variable.
#' @param dom domain.
#' @param fe.disc fixed effects (categorical)
#' @param fe.cont fixed effects (continuous)
#' @param y.family distribution of y given u
#' @param ... other parameters.
#'
#' @return
#' @export
#'
#' @examples
initPar <- function(data, resp, dom, fe.disc = NULL, fe.cont = NULL, y.family = NULL, ...){

  source("R/myFormula.R")
  myFormula <- myFormula(data, resp, dom)
  mformula <- as.formula(myFormula)

  # Returns the dist of y if not specified
  if(is.null(y.family)){
    yVal <- data[ ,resp]

    if(length(unique(yVal))==2){
      y.family <- "binomial"
    }else if(length(unique(yVal)) > 2 & is.numeric(yVal)){
      y.family <- "gaussian"
    }else{
      stop("Error: Family is not defined !!!")
    }
  }else{
    y.family <- y.family
  }

  # cat(paste0("dist. of family = ", y.family, "\n"))

  if(y.family == "binomial"){
    mdlFit <- glm(formula = mformula,family = binomial(link=logit),data=data)
  }else if(y.family == "Poisson"){
    mdlFit <- glm(formula = mformula,family = poisson(link=log),data=data)
  }else if(y.family == "gaussian"){
    mdlFit <- glm(formula = myFormula,family = gaussian(link="identity"),data=data)
  }


  beta0 <- data.frame(summary(mdlFit)$coefficients[,1])
  beta0 <- data.frame(parameter=rownames(beta0),est=beta0[,1])
  rownames(beta0) <- NULL

  return(beta0)
}
