text <- c("Alpha",
          "Bravo",
          "Charlie",
          "Delta",
          "Echo",
          "Foxtrot",
          "Golf",
          "Hotel",
          "India",
          "Juliett",
          "Kilo",
          "Lima",
          "Mike",
          "November",
          "Oscar",
          "Papa",
          "Quebec",
          "Romeo",
          "Sierra",
          "Tango",
          "Uniform",
          "Victor",
          "Whiskey",
          "X-ray",
          "Yankee",
          "Zulu")
names(text) <- text
testText <- list(text,
                 text[1:13],
                 text[14:26],
                 text[1:5],
                 text[6:10],
                 text[11:15],
                 text[16:20],
                 text[21:26],
                 text[c(1,5,9,15,21)]) |> 
  purrr::map(paste, collapse = " ") |> 
  purrr::flatten_chr()
ids <- c("AtoZ", "AtoM", "NtoZ", "AtoE", "FtoJ", "KtoO", "PtoT", "UtoZ", "Vowels")
names(testText) <- ids

testTCorpus <- corpustools::create_tcorpus(testText, ids)
saveRDS(testText, "tests/fixures/testText.RDS")

