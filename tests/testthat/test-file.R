context("Convert fail to data frame")

test_that("error for Excel format", {

    data <- "test.xlsx"
    expect_error(rfile(data), "Wrong format")
})
