context("Model formula")
library(hglmbc2)

test_that("Model formula created correctly", {
  expect_equal(myFormula(data=eversmoke, resp="smoke_ever", dom="county"), "smoke_ever~as.factor(year)+as.factor(gender)+as.factor(race)+as.factor(age)+povt_rate")
  expect_equal(myFormula(data=temp_data, resp="y", dom="cls"), "y~x1+x2")
})
