# This holds functions relating to ris file and bibliography manipulation


#' importBibliography
#'
#' imports a bibliography from a file
#' uses synthesisr under the hood, so accepts
#' any filetypes synthesisr will accept
#'
#' @param path path to a bibliography file
#' @param columns the columns to extract
#' @param keywords whether or not to add keyword column
#'
#' @return the bibliography data frame
#' @examples
#' bib <- importBibliography("./inst/extdata/test.ris")
importBibliography <- function(
    path,
    columns =
        c(
            "author",
            "title",
            "abstract",
            "keywords",
            "Label"
        ),
    keywords = TRUE
) {
    bib <- synthesisr::read_refs(path)
    cols <- columns
    bibframe <- data.frame(
        matrix(
            ncol = 0,
            nrow = nrow(
                bib
            )
        )
    )
    for (i in cols){
        if (
            i %in% colnames(bib)
        ) {
            bibframe[i] <- gsub(
                "([[:upper:]])",
                "\\L\\1",
                bib[,i],
                perl = TRUE
            )
        }
    }
    if (keywords == TRUE) {
        bibframe$Keyword_count <- 0
    }
    return(bibframe)
}

#' listUniqueWordsInBibliography
#'
#' creates a character vector containing the unique words in a bibliography
#'
#' @param bibframe the bibliography dataframe
#' @param columns the columns to include
#'
#' @return a character vector containing the unique words in the bibliography
#' @examples
#' unique1 <- listUniqueWordsInBibliography(bibframe)
listUniqueWordsInBibliography <- function(
    bibframe,
    columns = c(
        "title",
        "abstract",
        "keywords"
    )
) {
    newbibframe <- as.data.frame(
        matrix(
            0,
            ncol = length(columns),
            nrow = nrow(bibframe)
        )
    )
    colnames(newbibframe) <- columns
    for (i in columns){
        if (i %in% colnames(bibframe)){
            newbibframe[i] <- bibframe[, i]
        }
    }
    newbibframe <- unique(
        unlist(
            strsplit(
                cleanText(
                    as.character(
                        newbibframe
                    )
                ),
                "\\s+"
            )
        )
    )
    return(newbibframe)
}