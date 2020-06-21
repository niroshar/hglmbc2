#' Creates design matrix for fixed effects
#'
#' @description \code{DesignM} function creates the design matrix for fixed effects.
#'
#' @param data The data frame.
#' @param fe.disc Vector of discrete/categorical variables.
#' @param fe.cont Vector of continuous variables.
#' @param ref.group Define the reference group for each discrete variable.
#'
#' @return A matrix.
#'
#' @export
#'
#' @import dplyr
#'
#' @import fastDummies
#'
#' @examples
#' data <- eversmoke
#' re <- "county"
#' fe.disc <- c("year","gender","race","age")
#' fe.cont <- "povt_rate"
#' ref.group <- c("age1", "gender2","race4","year1")





DesignM <- function(data,fe.cont,fe.disc,ref.group){

  # k <- lapply(dataDisc, unique)
  # lengths(k)

  DX_out <- data[, c(fe.cont)]
  for(i in 1:length(fe.disc)){
    # i <- 1
    var_temp <- fe.disc[i]
    DX_temp <- data[,var_temp]
    DX_temp <- as.factor(DX_temp)
    DX_out <- cbind.data.frame(DX_out,DX_temp)
  }
  colnames(DX_out) <- c(fe.cont,fe.disc)

  DX11 <- dummy_cols(DX_out, select_columns = paste0(fe.disc)) %>%
    select(-c(paste0(fe.disc)))
  # DX11 <- data.frame(Intercept=rep(1,nrow(DX_out)),DX11)
  colnames(DX11) <- gsub("_","",colnames(DX11))
  DX11 <- DX11[ ,!colnames(DX11) %in% ref.group]    ### Remove ref group
  DX <- DX11[ ,order(colnames(DX11))]
  X <- data.frame(Intercept=rep(1,nrow(DX)),DX)

  return(X)
}



