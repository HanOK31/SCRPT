# This holds functions that are shared across the package

formatSearchString <- function(list) {
    list <- gsub("^", "<",list)
    list <- gsub("$", ">", list)
    list <- gsub("(.*)( NEAR/)([[:digit:]]+)(.*)", '"\\1\\4\\\"~\\3', list)
    list <- paste0("(", list, ")")
    substring <- paste0(list, collapse = " OR ")
    return(substring)
}

cleaning <- function(text) {
    a1 <- c(
        "acknowledgments",
        "acknowledgements",
        "conflicts of interest",
        "conflict of interest",
        "financial contributions",
        "financial support",
        "declaration of interest",
        "supplementary material",
        "references",
        "supporting information",
        "author contributions"
    )
    a2 <- stringi::stri_locate_last(text, regex = a1)
    a3 <- which.min(a2[, 1])
    a4 <- a1[a3]
    input <- gsub(paste0("(.*)", a4, ".*$"), "\\1", text)
    return(input)
}

makeCorpus <- function(input, columns = c("title", "abstract", "keywords")){
    input <- cleaning(input)
    ##check text columns - may need adjustign with IF statement
    textcorpus <- corpustools::create_tcorpus(
        input,
        doc_id = seq_along(input),
        text_columns = columns
    )
    textcorpus <- textcorpus$preprocess(remove_stopwords = TRUE)
    return(textcorpus)
}

runSearchString <- function(substring, textcorpus){
    #corpus tools
    results <- corpustools::search_contexts(frame, substring)
    return(results$hits)
}
