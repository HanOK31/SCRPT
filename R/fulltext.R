# This holds functions relating to full text (e.g. PDF file) manipulation

#
#readTexts <- function(names, path){
#    for (i in seq_along(unload$name)){
#    if (endsWith(names[i], ".pdf")){
#                placeholder <- extract_text(path[i])   
#            }else{
#                placeholder <- read_document(path[i], combine = TRUE, remove.empty = TRUE)
#            }
#            if(is.null(placeholder)){
#                errorlist$a <<- c(errlist$a, paste0(unload$name[i],'\n'))
#                namelist$a <<- namelist$a[!namelist$a %in% unload$name[i]]
#            }else{
#                textlist$a <<- c(textlist$a, placeholder)
#            }}
#}
#importFullTexts <- function(files) {
#    for (i in seq_along(files)){
#        if (endsWith(file[i], ".zip")) {
#            unload$name <- unzip(files$datapath[i], list = TRUE)
#        } else {
#            unload$name <- docs[i]
#        }
#        namelist <- c(namelist, unload$name)
#        readTexts(unload$name, unload$path)
#    }
#    return(textlist)
#}