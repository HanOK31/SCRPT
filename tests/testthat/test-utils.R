testthat::test_that("Boolean user search Syntax is correctly to corpustools syntax", {
  testText <- readRDS("tests/fixures/testText.RDS")
  ids <- names(testText)
  userInput <- list("\"Alpha Bravo\"", "Bravo AND Echo")
  result <- "<Alpha Bravo> OR (Bravo AND Echo)"
  testthat::expect_equal(formatSearchString(userInput),result)
})

testthat::test_that("Adjacency user syntax is correctly translated to corpustools syntax", {
  testText <- readRDS("tests/fixures/testText.RDS")
  ids <- names(testText)
  userInput <- "India NEAR/2 Uniform"
  result <-"'India Uniform'~2"
  testthat::expect_equal(formatSearchString(userInput),
               result)
})

