# This holds functions that are shared across the package

formatSearchString <- function(list){
    list <- gsub("^\"", "<",list)
    list <-  gsub("\"$", ">", list)
    list <- gsub("(.*)( NEAR/)([[:digit:]])(.*)", '"\\1\\4\\\"~3', list)
    list <- paste0("(", list,")")
    substring <- paste0(list, collapse = " OR ")
    return (substring)
}

runSearchString <- function(substring, frame){
    #corpus tools
    results <- corpustools::search_context(frame, substring)
    return(results$hits)
}