context("Testing for rclean")

test_that("error if data not in data.frame", {

    data <- "data"
    expect_error(rclean(data), "Should be an R data.frame format!" )

})
