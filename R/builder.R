# This holds functions relating to search string building and manipulation


#' initialiseSearchTerms
#'
#' Initialise the search terms dataframe
#'
#' @return dataframe containing an empty history column
#' @examples
#' st <- initialiseSearchTerms()
initialiseSearchTerms <- function(){
    frame <- data.frame(History = character())
    return(frame)
}

#' addSearchTerm
#'
#' Adds a search term to the search dataframe
#'
#' @param df the dataframe to add the term to
#' @param term a string containing the term to add
#'
#' @return the dataframe with a list added
#' @examples
#' searchTerms <- addSearchTerm(searchTerms, "Paramedic AND Ambulance")
addSearchTerm <- function(df, term){
    frame <- rbind(data.frame(df, data.frame(History = term, stringsAsFactors = FALSE)))
    return(frame)
}

removeSearchTerm <- function(list, id){
    frame <- list[-as.numeric(id),]
    return(frame)
}

replaceSearchTerm <- function(list, id, newTerm){
    frame <- list[id, "History"] <- as.character(newTerm)
    return(frame)
}

getSearchTerm <- function(list, id){
    term <- list[id, "History"]
    return(term)
}

combineSearchTerms <- function(list, ids){
    searchString <- paste0("(", list[ids], collapse = " OR ", ")")
    return(searchString)
}

makeCorpusSearchString <- function(searchString){

}