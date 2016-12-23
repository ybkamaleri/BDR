context("Convert fail to data frame")

test_that("error for Excel format", {

    data <- "test.xlsx"
    data2 <- "test.xls"
    expect_error(rfile(data), "Data har fail format")
    expect_error(rfile(data2))

})
