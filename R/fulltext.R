# This holds functions relating to full text (e.g. PDF file) manipulation

##list  = names
##path = datapath



    readTexts <- function(list, path){
        placeholder <- NULL
        for (i in 1:length(list)){
        if (endsWith(list[i], ".pdf")){
                    placeholder <- extract_text(path[i])   
                }else{
                    placeholder <- read_document(path[i], combine = TRUE, remove.empty = TRUE)
                }
                if(is.null(placeholder)){
                    errorlist$err <<- c(err_list$err, paste0(list[i],'\n'))
                    namelist$a <<- name_list$l[!name_list$l %in% list[i]]
                }else{
                    textlist$a <<- c(textlist$a, placeholder)
                }
    }


    importFullTexts <- function(path){

    }

    observeEvent(input$zip_file,{
        b <- NULL
        name_list2s <- path
        for (i in 1:length(name_list2s$name)){
            if(endsWith(name_list2s$name[i], ".zip")){
                Q <- unzip(name_list2s$datapath[i], list = TRUE)
                name_list$l <<- c(name_list$l, Q$Name)
                for (j in 1:length(Q$Name)){
                    if (endsWith(Q$Name[j], ".pdf")){
                        b <- extract_text(unzip(name_list2s$datapath[i], Q$Name[j]))
                        
                    }else{
                        b <- read_document(unzip(name_list2s$datapath[i], Q$Name[j]), combine = TRUE, remove.empty = TRUE)
                    }
                    if (is.null(b)){
                        err_list$err <<- c(err_list$err, paste0(Q$Name[j],'\n'))
                        name_list$l <<- name_list$l[!name_list$l %in% Q$Name[j]]
                    }else{
                        text_list$c <<- c(text_list$c, b)
                    }
                }
            }else{
                name_list$l <<- c(name_list$l, name_list2s$name[i])
                if (endsWith(name_list2s$name[i], ".pdf")){
                    b <- extract_text(name_list2s$datapath[i])
                    
                }else{
                    b <- read_document(name_list2s$datapath[i], combine = TRUE, remove.empty = TRUE)
                }
                if(is.null(b)){
                    err_list$err <<- c(err_list$err, paste0(name_list2s$name[i],'\n'))
                    name_list$l <<- name_list$l[!name_list$l %in% name_list2s$name[i]]
                }else{
                    text_list$c <<- c(text_list$c, b)
                }
            }}
    })