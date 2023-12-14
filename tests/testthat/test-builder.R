#replace final searchuilder function for 'corpustools::search_context'
testthat::test_that("Search Syntax is correctly translated from User Syntax to corpustools syntax", {
  testText <- readRDS("tests/fixures/testText.RDS")
  ids <- names(testText)
  userInput <- '"Alpha Bravo"'
  searchContextResult <- '<Alpha Bravo>'
  userInputAdjacent <- 'India NEAR/2 Uniform'
  resultAdjacent <-"'India Uniform'~2"
  
})


testthat::test_that("Search syntax finds the correct texts", {
  testText <- readRDS("tests/fixures/testText.RDS")
  ids <- names(testText)
  testCorpus <- corpustools::create_tcorpus(testText, doc_id = ids)
  searchOR <- "Alpha Zulu"
  searchAND <- "Bravo AND Echo"
  searchPhrase <- "'Alpha Bravo'"
  searchAdjacent <- "'India Uniform'~2"
  searchHyphen <- "'X-ray Whiskey'~4"
  complexSearch <- ("(Alpha OR 'Yankee Zulu') AND (Victor OR Echo OR ('India Uniform'~2)) AND (Charlie OR Oscar)")
  
  testthat::expect_equal(
    as.character(corpustools::search_contexts(testCorpus, searchOR)$hits$doc_id),
    c("AtoE", "AtoM", "AtoZ", "NtoZ", "UtoZ", "Vowels"))
  
  testthat::expect_equal(
    as.character(corpustools::search_contexts(testCorpus, searchAND)$hits$doc_id),
    c("AtoE", "AtoM", "AtoZ")
  )
})
