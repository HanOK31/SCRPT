# This holds functions relating to full text (e.g. PDF file) manipulation


    readTexts <- function(names, path){
        for (i in 1:length(unload$name)){
        if (endsWith(names[i], ".pdf")){
                    placeholder <- extract_text(path[i])   
                }else{
                    placeholder <- read_document(path[i], combine = TRUE, remove.empty = TRUE)
                }
                if(is.null(placeholder)){
                    errorlist$a <<- c(errlist$a, paste0(unload$name[i],'\n'))
                    namelist$a <<- namelist$a[!namelist$a %in% unload$name[i]]
                }else{
                    textlist$a <<- c(textlist$a, placeholder)
                }}
    }


    importFullTexts <- function(docs, path){
        for (i in 1:length(docs)){
            if(endsWith(docs[i], ".zip")){
                    unload <- unzip(path[i], list = TRUE)
                }else{
                    unload <- docs[i]
                }
                namelist$a <<- c(namelist$a, unload$name)
                readTexts(unload$name)
                return(textlist)
        }
    }
