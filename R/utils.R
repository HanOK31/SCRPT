# This holds functions that are shared across the package

formatSearchString <- function(list){
    gsub("^\"", "<",list)
    gsub("\"$", ">", list)
    gsub("(.*)( NEAR/)([[:digit:]])(.*)", "\\1\\3\\~([[:digit:]])", list)
    substring <- paste0(list, collapse = "OR")
    return (substring)
}

runSearchString <- function(substring, frame){
    #corpus tools
    results <- corpustools::search_context(frame, substring)
    return(results$hits)
}