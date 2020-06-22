# Try for different cases
# beta0 <- initPar(data, resp, dom)
# beta0 <- initPar(data, resp, dom, fe.disc, fe.cont)
# beta0 <- initPar(data, resp, dom, fe.disc, fe.cont, y.family = "binomial")

initPar <- function(data, resp, dom, fe.disc = NULL, fe.cont = NULL, y.family = NULL, ...){

  if(is.factor(temp_data[ ,dom])){
    temp_data[ ,dom] <- as.character(temp_data[ ,dom])
  }

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


# bInitOrder(beta0,fe.cont)
bInitOrder <- function(beta0,fe.cont){

  beta0_cont <- beta0[grepl("^as.factor", beta0$parameter)==FALSE, ] ## beta for non-discrete parameters(including intercept)
  beta0_Int <- beta0_cont[!beta0_cont$parameter %in% fe.cont, ]        ## Intercept
  beta0_cont <- beta0_cont[beta0_cont$parameter %in% fe.cont, ]
  # beta0_cont <- beta0[!beta0$parameter %like% "as.factor",] ## beta for Cont vars
  # beta0_Int <- beta0_cont[!beta0_cont$parameter %in% cont_vars, ]
  # beta0_cont <- beta0_cont[beta0_cont$parameter %in% cont_vars, ]
  beta0_cont$parameter <- gsub("_","",beta0_cont$parameter)
  beta0_Int$parameter <- gsub("\\(Intercept\\)","Intercept",beta0_Int$parameter)

  beta0_dis <- beta0[grepl("^as.factor", beta0$parameter)==TRUE, ]  # beta for disc vars
  beta0_dis$parameter <- gsub("as.factor\\(","",beta0_dis$parameter)
  beta0_dis$parameter <- gsub("\\)","_",beta0_dis$parameter)

  beta0_dis$col <- gsub("_.*","",beta0_dis$parameter)
  unq <- unique(beta0_dis$col)
  beta0_dis$parameter <- gsub("_","",beta0_dis$parameter)
  beta0_dis$col <- NULL

  beta0 <- rbind(beta0_cont,beta0_dis)
  if(!is.character(beta0$parameter)){beta0$parameter <- as.character(beta0$parameter)}
  beta0 <- beta0[order(beta0$parameter), ]
  beta0$est <- round(as.numeric(beta0$est),5)
  beta00 <- rbind.data.frame(beta0_Int,beta0)

  return(beta00)

}






## Function to get trace of a matrix
func_trace <- function(X){
  n <- dim(X)[1]
  tr <- 0    ### initialize trace

  for (j in 1:n){
    k <- X[j,j]
    tr <- tr + k
  }
  return(tr[[1]])
}



# ## W, mu, Delta1lu, Delta2lu depends on distribution of y given u
# genPar <- function(y.family, BiasCor = TRUE){
#
#   # then u~N(0,sigma^2)
#   if(BiasCor == TRUE){
#     Delta1lu <-
#     Delta2lu <-
#   }
#
#   if(y.family=="binomial"){
#     # mu <- P
#     mu <- 1/(1+exp(-(X%*%beta+Z%*%u)))
#     pp <- mu*(1-mu)
#     W <- Diagonal(x=pp)
#   }else if(y.family=="poisson"){
#     mu <- exp(X%*%beta + Z%*%u)
#     pp <- exp(X%*%beta + Z%*%u)
#     W <- Diagonal(x=pp)
#   }
#
# }

