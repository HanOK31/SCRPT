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