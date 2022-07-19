library(testthat)
library(descfars)
devtools::load_all()


# test_that("Check number of fatalities in July 2015", {
#   testthat::expect_equal(nrow((fars_summarize_years(2015))[7,2]), 2998)
# })


# test_that("Check number of rows for 2013 accidents file", {
#   testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
# })
#
# expect_that(make_filename(2012),matches("accident_2012.csv.bz2"))



#this test loads in the 2013 fars data file, and checks the length of the STATE column
data = fars_read(make_filename(2013))
expect_that(length(data$STATE),equals(30202))



#Run this from Console
#testthat::test_file("tests/testthat/test_descsummary.R")
