
#' Sample csv data set for analysis
#'
#' @param path path to filename
#'
#' @return a \code{tibble}
#'
#' @export
#' @importFrom readr read_csv
#'
#' @examples
#' csv = system.file("extdata", "csv_data.csv", package = "hglmbc2")
#' csv_read(csv)
csv_read <- function(path){
  readr::read_csv(path)
}
