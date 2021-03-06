#' @title Smoke-Ever data set
#'
#' @description The \code{eversmoke} data set is ever-smoke data with 25091 observations
#' and 7  columns which is obtained from \url{"https://www.cdc.gov/healthyyouth/data/yrbs/index.htm"}.
#'
#' @format A \code{data frame} with 25091 observations and 7 columns, which are:
#' \describe{
#' \item{smoke_ever}{status of being a ever smoker or not (1 - Yes, 0 - No)}
#' \item{county}{US county (FIPS)}
#' \item{year}{Year, 1 - 2015, 2- 2017}
#' \item{gender}{1 - female, 2 - male}
#' \item{age}{1 - 14 years or less, 2 - 15 to 17 years, 3 - 18 years or above}
#' \item{race}{1 - White, 2 - African American, 3 - Hispanic, 4 - Other races}
#' \item{povt_rate}{Poverty rate, is obtained by poverty level / county population}
#' }
#' @source  \url{"https://www.cdc.gov/healthyyouth/data/yrbs/index.htm"}
"eversmoke"
