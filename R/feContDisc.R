xDisc <- function(data, resp, dom, fe.disc=NULL){

  dataF <- data[ ,!colnames(data) %in% c(resp,dom)]

  if(is.null(fe.disc)){

    # Get discrete variables by variable type
    fe.disc <- c(names(which(lapply(dataF, is.character)==TRUE)))
    fe.disc
  }

}
