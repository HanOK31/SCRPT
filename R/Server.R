server <- function(input, output, session) {

    ##find unique words from included studies

    ##variables
    bib1 <- reactiveValues()
    bib2 <- reactiveValues()
    
    ##upload bibliographic file of includes
    observeEvent(input$uniq1_file,{
        bib1 <- importBibliography(input$uniq1_file)
        bib1 <- renderDataTable(data.frame(listUniqueWordsInBibliography(bib1)), options = list(dom = "tp"))
    })
    
    ##upload bibliographic file
    observeEvent(input$uniq2_file,{
        bib2 <- importBibliography(input$uniq2_file)
        bib2 <- renderDataTable(data.frame(listUniqueWordsInBibliography(bib2)), options = list(dom = "tp"))
    })
    
    ##find uniqie words
    observeEvent(input$find,{
        output$Unique <- setdiff(bib1, bib2)
    })
    
    ##download list of unique words
    output$uniqout <- downloadHandler(
        downloadxlxs(output$Unique)
    )
    
    #-----------------------------------------------------------
    ##keywords

    ##variables
    kw1 <- reactiveValues()
    termsFinal <- reactiveValues()
    
    ##Set up keywords list
    observeEvent(input$KWgo,{
        initialiseSearchTerms ()
        kw1 <- addSearchTerms(kw1,input$keywords)
        updateTextAreaInput(session, "keywords", value = "")
        #table of keywords
        output$KWhistory <- renderDataTable(Kw1, escape = FALSE, server = FALSE, extension = c("Select", "Buttons"), selection = 'none', editable = "cell",
                                            options = list(dom = "Btp",
                                                           buttons = list("selectAll", "selectNone",
                                                                          list(
                                                                              extend = "collection",
                                                                              text = 'Delete',
                                                                              action = DT::JS("function (e, dt, node, config) {
                                                                      Shiny.setInputValue('delrow', true, {priority:'event'});
                                                                                 }")
                                                                          ),
                                                                          list(
                                                                              extend = "collection",
                                                                              text = 'Combine',
                                                                              action = DT::JS("function (e, dt, node, config) {
                                                                      Shiny.setInputValue('Combine', true, {priority:'event'});
                                                                                 }")
                                                                          )
                                                           ),
                                                           columnDefs = list(list(className = "select-checkbox", targets = 0, orderable = FALSE, width = "50px"), list(className = "dt-left", targets = "_all")),
                                                           select = list(style = "multi", selector = "td:first-child" )
                                            ))
    })    
    
    #removing line from search
    observeEvent(input$delrow,{
        kw1 <- removeSearchTerms(kw1, input$KWhistory_rows_selected)
    })
    
    #editing search
    observeEvent(input$KWhistory_cell_edit,{
        kw1 <- replaceSearchTerm(kw1, input$KWhistory_cell_edit$row,input$KWhistory_cell_edit$value)
    })
    
    #prepare search pattern
    observeEvent(input$Combine, {
        output$KWfinal <- renderText(combineSearchTerms(kw1, input$KWhistory_rows_selected))
        termsFinal <- formatSearchString(output$KWfinal)
    })
    
    #-----------------------------------------------------------------
    
    ###Title and Abstract screening
    
    #variables
    biblist <- reactiveValues()
    thresh <- reactiveValues(a1 = NULL, a2 = NULL, t1 = NULL, t2 = NULL)
    
    ##upload bibliographic file
    observeEvent(input$ref_file,{
        biblist <- importBibliography(input$ref_file)
        biblist <- cleaning(biblist$file)
        biblist <- makeCorpus(biblist$file)
    })
    
    
    #Analysis for T&A
    observeEvent(input$Screengo,{
        output$test <- renderDataTable(runSearchString(termsFinal, biblist))
    })
    
    
    ##download ris file
    output$risout <- downloadHandler(
        downloadRis(output$test)
    )
    
    #----------------------------------------------------------------
    
    ##variables
    textlist <- reactiveValues()
    
    ##get list of full texts
    observeEvent(input$zip_file,{
        b <- NULL
        name_list2s <- input$zip_file
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
    
    
    #Analysis for full texts
    observeEvent(input$Screengo2,{
        output$test2 <- renderDataTable(runSearchString(termsFinal, textlist))
    })
    
    
    ##download excel file
    output$exout <- downloadHandler(
        downloadxlxs(output$test2)
    )
    
}

#-------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
