## code to prepare `temp_data` dataset goes here
# This data set will be saved in data folder
set.seed(1000)

x1 <- rnorm(100)
x2 <- rnorm(100,4, 2)
y <- x1*2 + 2*x2 + rnorm(100)
cls <- rep(c("A","B","c","D","E"),20)
temp_data <- data.frame(x1 = x1, x2 = x2, cls = cls, y = y)


# usethis::use_data(temp_data, compress = "xz")

usethis::use_data(temp_data, compress = "xz", overwrite = TRUE)
