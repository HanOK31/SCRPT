testthat::test_that("Boolean user search Syntax is correctly to corpustools syntax", {
  userInput <- list("\"Alpha Bravo\"", "Bravo AND Echo")
  result <- "<Alpha Bravo> OR (Bravo AND Echo)"
  testthat::expect_equal(formatSearchString(userInput),result)
})

testthat::test_that("Adjacency user syntax is correctly translated to corpustools syntax", {
  userInput <- "India NEAR/2 Uniform"
  result <-"<India Uniform>~2"
  testthat::expect_equal(formatSearchString(userInput),
               result)
})

testthat::test_that("Running the search strategy retrieves the correct documents for phrase searches and AND combinations", {
  testText <-  readRDS(testthat::test_path("fixtures","testText.RDS"))
  ids <- names(testText)
  testCorpus <- corpustools::create_tcorpus(testText, doc_id = ids)
  formattedSearch <- "<Alpha Bravo> OR (Bravo AND Echo)"
  result <- runSearchString(substring = formattedSearch, frame = testCorpus)
  expect_equal(as.character(result$doc_id), c("AtoE", "AtoM", "AtoZ") )
})

testthat::test_that("Running the search strategy retrieves the correct documents for phrase searches and AND combinations", {
  testText <-  readRDS(testthat::test_path("fixtures","testText.RDS"))
  ids <- names(testText)
  testCorpus <- corpustools::create_tcorpus(testText, doc_id = ids)
  formattedSearch <- "<India Uniform>~2"
  result <- runSearchString(substring = formattedSearch, frame = testCorpus)
  expect_equal(as.character(result$doc_id), c("Vowels") )
})


gsub("(\\b.*\\b)( NEAR/)([[:digit:]])(\\b.*\\b)", "\\'\\1\\4\\'\\~\\3", userInputAdjacent)
substring <- paste0(userInput, collapse = " OR " )
