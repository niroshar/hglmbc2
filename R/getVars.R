
#' Function to get discrete, numeric and response variables if only the model fit formula is defined.
#'
#' @param mformula
#'
#' @return
#' @export
#'
#' @examples
# myformula <- "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race) + as.factor(year) + povt_rate"
# varOut <- predsFunc(mformula=myformula)
getVars <- function(mformula){
  # resp <- as.character(as.character(mformula)[2])
  mformula <- as.character(mformula)
  resp <- unlist(strsplit(mformula, split = "~"))[1] #  # extract response variable , fixed = TRUE
  resp <- gsub(" ","",resp)    # remove space in response variable
  # preds0 <- as.character(mformula)[3]
  preds0 <- unlist(strsplit(mformula, split = "~", fixed = TRUE))[2]
  # preds1 <- as.vector(unlist(strsplit(preds0, split = " ~ ", fixed = TRUE)))
  # preds <- preds1[!preds1 %in% resp]
  # fe.disc0 <-  preds[grepl("^as.factor", preds)==TRUE]   # Disc variables
  preds <- unlist(strsplit(preds0, split = "+", fixed = TRUE))
  preds <- gsub(" ","",preds)
  fe.disc0 <-  preds[grepl("^as.factor", preds)==TRUE]
  fe.disc0 <- gsub("as.factor\\(","",fe.disc0)
  fe.disc0 <- gsub(")","",fe.disc0)
  fe.disc <- as.vector(fe.disc0)

  fe.cont <- preds[grepl("^as.factor", preds)==FALSE]   # numeric variables

  varOut <- list(resp, fe.disc, fe.cont)
  return(varOut)
}
