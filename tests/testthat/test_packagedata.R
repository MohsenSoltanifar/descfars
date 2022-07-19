library(testthat)
library(descfars)
devtools::load_all()
test_that(".csv data descfars files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "descfars")),
                         c("accident_2013.csv.bz2",
                           "accident_2014.csv.bz2",
                           "accident_2015.csv.bz2"))
})

#Run this from Console
#testthat::test_file("tests/testthat/test_packagedata.R")
