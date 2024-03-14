test_that("1. Is the estimated coefficient for “gdpPercap” in the linear regression model equal to 0.00076?", {
    testthat::expect_equal(unname(coef(model)[2]), 0.00076, tolerance = 0.00001)
})

test_that("2. Is the number of observations in our model equal to 1704?", {
    testthat::expect_equal(model$nobs,1704)
})

test_that("3. Data validation", {
    testdat::expect_unique(c("country", "year"), data = data)
    testdat::expect_range(gdpPercap, 0,1000000, data = data)
    testthat::expect_equal(
        all(
            as.character(c("Asia", "Europe", "Africa", "Americas", "Oceania")) %in%
            levels(as.data.frame(table(data[,"continent"]))[,"continent"])
            ) &
        all(
            levels(as.data.frame(table(data[,"continent"]))[,"continent"]) %in%
            as.character(c("Asia", "Europe", "Africa", "Americas", "Oceania"))
            ),
        TRUE) # I had built that test before seeing the comment on testdat, I keep it because I find it fun!!
})


