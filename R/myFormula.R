#' model formula
#'
#' @description \code{myFormula} outputs the model formula based on discrete variables and continuous variables.
#'
#' @param data the data frame.
#' @param resp the response variable.
#' @param dom the random effect \code{dom} could be \code{"county","FIPS","state","zipcode"}.
#' @param ... Other parameters. \code{fe.disc} A vector of discrete/categorical fixed effects. \code{fe.cont} A vector of continuous fixed effects.
#'
#'
#' @return Returns an object of class for model \code{formula} which is a symbolic formula based on the variables in the data set.
#'
#' @importFrom stringi stri_replace_last
#'
#' @seealso \code{\link{as.formula}}
#'
#' @examples
#' # Using discrete and continuous variables.
#' data <- eversmoke
#' y <- "smoke_ever"
#' dom <- "county"
#' catX <- c("age","gender","year","race")
#' contX <- "povt_rate"
#' \dontrun{
#' myFormula(data, resp = y, fe.disc = catX, fe.cont = contX)
#' as.formula(myFormula(data, resp = y, fe.disc = catX, fe.cont = contX))
#'
#'
#'
#' # Only using discrete variables
#' resp <- "smoke_ever"
#' dom <- "county"
#' fe.disc <- c("age","gender","year")
#' myFormula(data=eversmoke, resp="smoke_ever", dom="county", fe.disc)
#' as.formula(myFormula(resp,fe.disc))
#'
#'
#'
#' # None of the variables are defined, this only output the response variable.
#' resp <- "smoke_ever"
#' dom <- "county"
#' myFormula(data=eversmoke, resp="smoke_ever", dom="county")
#'}
#'
#'
#' @export
myFormula <-  function(data,resp=NULL,dom=NULL, ...){

  if(!exists("resp") | !exists("dom")){
      stop("Please define the response variable and random effect!")
  }

  IntParFomular0 <- paste0(resp,"~")

  # IntParFomular <- paste0(resp)

  dataF <- data[ ,!colnames(data) %in% c(resp,dom)]

  if(ncol(dataF) > 0){
    if(!exists("fe.cont") & !exists("fe.disc")){
    # if(!exists("fe.cont")&!exists("fe.disc")){

      # If discrete and continous variables are not defined, consider based on variable type

      cat("Fixed effects and random effects are not defined, selected by variable type !!!!", "\n\n\n")

      # Get discrete variables by variable type
      unqV <- lapply(dataF, unique)
      unqVC <- unlist(lapply(unqV, length))
      fe.disc <- c(names(unqVC[unqVC<=5]))
      # fe.disc <- c(names(which(lapply(dataF, is.character)==TRUE)))
      # Continuous variables by variable type
      fe.cont <- c(names(unqVC)[!names(unqVC) %in% fe.disc])
      fe.cont <- c(fe.cont[fe.cont != c(dom) & fe.cont != c(resp)])

      # cat("\nfe.disc= ", fe.disc)
      # cat("\nfe.cont= ", fe.cont)
      # print(fe.disc)

      discFomular <- IntParFomular0
      if(length(fe.disc)!= 0){
        for(i in 1:length(fe.disc)){
          temp <- paste0("as.factor(",fe.disc[i],")+")
          discFomular <- paste0(discFomular,temp)
        }
      }

      if(length(fe.cont) != 0){
        for(j in 1:length(fe.cont)){
          temp <- paste0(fe.cont[j])
          discFomular <- paste0(discFomular,temp)
        }
      }

      IntParFomular <- discFomular
      # IntParFomular
      # cat("\nIntParFomular= ", IntParFomular)

    }else if(!exists("fe.cont") & exists("fe.disc")){
      fe.cont <- c(names(which(lapply(dataF, is.character)==FALSE)))
      fe.cont <- c(fe.cont[fe.cont != c(dom) & fe.cont != c(resp)])

      discFomular <- IntParFomular0
      for(i in 1:length(fe.disc)){
        temp <- paste0("as.factor(",fe.disc[i],")+")
        discFomular <- paste0(discFomular,temp)
      }

      if(length(fe.cont) != 0){
        for(j in 1:length(fe.cont)){
          temp <- paste0(fe.cont[j])
          discFomular <- paste0(discFomular,temp)
        }
      }

      IntParFomular <- discFomular


    }else if(exists("fe.cont") & !exists("fe.disc")){
      fe.disc <- c(names(which(lapply(dataF, is.character)==TRUE)))

      discFomular <- IntParFomular0
      if(length(fe.disc) != 0){
        for(i in 1:length(fe.disc)){
          temp <- paste0("as.factor(",fe.disc[i],")+")
          discFomular <- paste0(discFomular,temp)
        }
      }


      for(j in 1:length(fe.cont)){
        temp <- paste0(fe.cont[j])
        discFomular <- paste0(discFomular,temp)
      }
      IntParFomular <- discFomular

    }else{
      fe.disc <- fe.disc
      fe.cont <- fe.cont

      discFomular <- IntParFomular0

      for(i in 1:length(fe.disc)){
        temp <- paste0("as.factor(",fe.disc[i],")+")
        discFomular <- paste0(discFomular,temp)
      }

      for(j in 1:length(fe.cont)){
        temp <- paste0(fe.cont[j])
        contFomular <- paste0(discFomular,temp)
      }
      IntParFomular <- contFomular

    }

    if(sapply(strsplit(as.character(IntParFomular), ""), tail, 1)=="+"){
      rmP <- sapply(strsplit(as.character(IntParFomular), ""), tail, 1)
      IntParFomular <- stri_replace_last(IntParFomular, fixed = "+", "")
    }

    mformula <- IntParFomular


  }else{

    stop("Error! No other variables exist except response response variable and random effect")

  }
  return(mformula)

}





