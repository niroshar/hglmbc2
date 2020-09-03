#' Extract model variables from formula.
#'
#' @description \code{myFormula} outputs the model formula based on discrete variables and continuous variables.
#'
#' @param mformula the model formula.
#'
#'
#' @return Returns a list with y, discrete variables, and continuous variables.
#'
#' @importFrom tidyselect starts_with
#' @importFrom tidyselect vars_select
#' @importFrom nlme splitFormula
#'
#' @seealso \code{\link{as.formula}}
#'
#' @examples
#' # Using discrete and continuous variables.
#' mformula <- "smoke_ever ~ as.factor(age) + as.factor(gender) + as.factor(race)
#' + as.factor(year) + povt_rate"
#' \dontrun{
#' predFunc(mformula)
#'
#'}
#'
#'
#' @export
predsFunc <- function(mformula = NULL)
{
  if(!is.null(mformula)){
    mform <- as.formula(mformula)
    y_var <- sub("\\~.*", "", mform)
    y_var <- gsub(" ", "",y_var)
    mform <- as.character(unlist(splitFormula(mform, sep = "+")))
    mform <- as.character(gsub("~", "", mform))
    x_disc <- as.vector(vars_select(mform, starts_with("as.factor")))
    x_cont <- mform[!mform %in% x_disc]
    x_disc <- gsub("as.factor\\(","",x_disc)
    x_disc <- as.vector(gsub("\\)","",x_disc))

    varOut <- list(y_var,x_disc,x_cont)
    return(varOut)
  }
}





