## code to prepare `csv_data` dataset goes here
# This data set will be saved in "inst/extdata" folder,
# document csv_data using roxygen2 R file in the "R" folder
# does not have to be saved as a "rda" format
set.seed(1000)

x1 <- rnorm(100)
x2 <- rnorm(100,4, 2)
y <- x1*2 + 2*x2 + rnorm(100)
cls <- rep(c("A","B","c","D","E"),20)
csv_data <- data.frame(x1 = x1, x2 = x2, cls = cls, y = y)

readr::write_csv(csv_data, path = "inst/extdata/csv_data.csv")

# usethis::
