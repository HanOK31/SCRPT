library(shiny)
library(devtools)
library(tm)
library(DT)
library(tabulizer)
library(stringr)
library(dplyr)
library(tidyr)
library(stringi)
library(readxl)
library(revtools)
library(writexl)
library(shinycssloaders)

options(shiny.maxRequestSize = 10000*1024^2, "repos" = c("CRAN" = "https://cran.rstudio.com", "tabulizer" = "https://github.com/ropensci/tabulizer", "tabulizerjars" = "https://github.com/ropensci/tabulizerjars"))

ui <- navbarPage("SCRPT: SCReening Prioritisation Tool", header = tags$style(HTML(".navbar-nav li a {color: #003366 !important;}.navbar-brand{color: #003366 !important;}")),
                 tabPanel("Home", icon = icon("home"),
                          fluidRow(column(width = 6, offset = 3,
                                          wellPanel(
                                              style = "background-color: #003366; color:white;",
                                              br(),
                                              hr(style = "border-color: #FF6600; width: 25%; margin-left: 0"),
                                              h4("SCRPT allows you to build up a set of keywords which will be used to screen through titles and abstract or full text files."),
                                              br(),
                                              h4("Start by working up your terms using the keyword tab. Next, select the Title & Abstract or Full Text tab to enter your data. To start screening, press the screen button on the appropriate tab."),
                                              br()
                                          )
                          )
                          )
                 ),
                 navbarMenu("Menu",
                            tags$head(
                                tags$style(
                                    HTML(
                                        "button.dt-button {
                             background: #003366 !important;
                             color: white !important;
                             border-color: #FF6600 !important;
                                        }
                                        thead {
                             background: #003366 !important;
                             color: white !important;
                                        }
                                        th {
                             border-top: 1px solid #FF6600 !important;
                             border-bottom: 1px solid #FF6600 !important;
                                        }
                                        td {
                             border-bottom: 1px solid #FF6600 !important;
                                        }
                                        html {
                             position: relative;
                             min-height: 100%;
                                        }
                                        body {
                             margin-bottom: 60px;
                                        }
                                        .footer{
                             position:absolute;
                             bottom:0;
                             width:100%;
                             height:50px;
                             color: #003366;
                             padding: 10px;
                             background-color: #F5F5F5; 
                             border-top: 1px solid #E0E0E0
                                        }"
                                    )
                                )
                            ),
                            tabPanel("Unique Word Finder",
                                sidebarPanel(width = 3,
                                    style = "background-color: #003366; color:white;",
                                    br(), 
                                    br(),
                                    hr(style = "border-color: #FF6600; width: 50%; margin-left: 0"),
                                    h4("This page allows you to identify", strong(" unique words"), "which are only found in included studies"),
                                    br(),
                                    h4("This works by comparing words in the title, abstract and keywords of a user specified set of included studies and excluded studies"),
                                    br(),
                                    h4(strong("For example: "), "if included studies contained 'Happy', 'Sunshine' and 'Violet', and exlcuded studies contained 'Happy' and 'Violet', the unique word would be ", strong("'Sunshine'.")),
                                    br()
                                ),
                                column(width = 6,
                                       wellPanel(
                                           style = "background-color: #003366; color:white; padding: 35px",
                                           br(),
                                           hr(style = "border-color: #FF6600; width: 50%; margin-left:0"),
                                           h3(strong("Find unique words"), "- please enter two bibliographic files to compare"),
                                           fileInput("uniq1_file", h3("upload included studies bibliographic file:"), multiple = FALSE),
                                           fileInput("uniq2_file", h3("upload excluded studies bibliographic file:"), multiple = FALSE),
                                           h4("Accepts bibliographies in .ris format"),
                                           br(),
                                           h5("upload your ris files and press the", strong("Find"), "button to start."),
                                           h5("Results will appear in a table below and can be downloaded as an excel file."),
                                           br(),
                                           fluidRow(
                                               column(1, offset = 0, actionButton("find", "Find", style = "background-color: #003366; color:white; border-color:#FF6600")),
                                               column(1, offset = 1, downloadButton("uniqout", "Download Results", style = "background-color: #003366; color:white; border-color:#FF6600"))
                                           ),
                                           br(),
                                           br(),
                                           dataTableOutput("Unique") %>% withSpinner(type = 4, color = "#FF6600", size = 2),
                                           br(),
                                           br(),
                                           br()
                                       )
                                )
                            ),
                            tabPanel("Search Builder",
                                     sidebarPanel(width = 3,
                                                  style = "background-color: #003366; color:white;",
                                                  br(), 
                                                  br(),
                                                  hr(style = "border-color: #FF6600; width: 50%; margin-left: 0"),
                                                  h4("Terms can be entered one at a time or as a string. If using", strong(" AND/OR"), "please enter statements in capitals, lower case and/or will be registered as keywords words."),
                                                  br(),
                                                  h4("To truncate words please stem the word at the appropriate point, you do not need use truncation symbols."),
                                                  br(),
                                                  h4("For adjacency please use", strong("NEAR/1"), ". Outer brackets around adjacency statements are not required but please use inner brackets if incorporating AND/OR statements."),
                                                  br(),
                                                  h4(strong("For example: Hockey NEAR/3 (ice OR rink OR skate)")),
                                                  br()
                                     ),
                                     column(width = 6,
                                            wellPanel(
                                                style = "background-color: #003366; color:white; padding: 35px",
                                                br(),
                                                hr(style = "border-color: #FF6600; width: 50%; margin-left:0"),
                                                h3(strong("Search Terms"), "- please enter your search string here:"),
                                                fluidRow(
                                                    column(8, offset = 0,
                                                           textAreaInput("keywords", label = NULL, height = 50)),
                                                    column(1, offset = 0, actionButton("KWgo", "Add", style = "margin: 8px; background-color: #003366; color:white; border-color:#FF6600",))
                                                ),
                                                br(),
                                                br(),
                                                hr(style = "border-color: #FF6600; width: 25%; margin-left:0"),
                                                h4(tags$b("Search history table")),
                                                h5("Lines can be edited by double clicking on the row"),
                                                h5("When you are happy with your keyword list please select rows you wish to include and use the", strong("Combine"), "button to finalise the list"),
                                                br(),
                                                br(),
                                                dataTableOutput("KWhistory") %>% withSpinner(type = 4, color = "#FF6600", size = 2),
                                                br(),
                                                br(),
                                                br()
                                            )
                                     ),
                                     sidebarPanel(width = 3,
                                                  style = "background-color: #003366; color:white;",
                                                  br(), 
                                                  br(),
                                                  hr(style = "border-color: #FF6600; width: 50%; margin-left: 0"),
                                                  wellPanel(
                                                      style = "color:black;",
                                                      textOutput("KWfinal")
                                                  ),
                                                  br(),
                                                  h5("This is the finalised string that will be searched for within you texts"),
                                                  br(),
                                                  h5("Please select if you would like to screen", strong("Title & Abstracts"), "or", strong("Full Texts"), "from the Menu at the top left of this page."),
                                                  br(),                                            
                                     )
                            ),
                            tabPanel("Title & Abstract Screener",
                                     fluidRow(column(width = 6, offset = 3,
                                                     wellPanel(
                                                         style = "background-color: #003366; color:white;",
                                                         br(),
                                                         hr(style = "border-color: #FF6600; width: 25%; margin-left:0"),
                                                         fileInput("ref_file", h3("upload a bibliographic file:"), multiple = TRUE),
                                                         h4("Accepts bibliographies in .ris format"),
                                                         br(),
                                                         h5("upload your ris file and press the", strong("Screen"), "button to start."),
                                                         h5("Results will appear in a table below and can be downloaded as a ris file. Please note, the", strong("Decisions"), "will appear in the", strong("Language"), "field of the ris file."),
                                                         br(),
                                                         fluidRow(
                                                             column(1, offset = 0, actionButton("Screengo", "Screen", style = "background-color: #003366; color:white; border-color:#FF6600")),
                                                             column(1, offset = 1, downloadButton("risout", "Download Results", style = "background-color: #003366; color:white; border-color:#FF6600"))
                                                         ),
                                                         br(),
                                                         br(),
                                                         dataTableOutput("test") %>% withSpinner(type = 4, color = "#FF6600", size = 2),
                                                         br()
                                                     )
                                     ))
                            ),
                            tabPanel("Full Text Screener",
                                     fluidRow(column(width = 6, offset = 3,
                                                     wellPanel(
                                                         style = "background-color: #003366; color:white;",
                                                         br(),
                                                         hr(style = "border-color: #FF6600; width: 25%; margin-left:0"),
                                                         fileInput("zip_file", h3("Upload files:"), multiple = TRUE, accept = c(".doc", ".docx", ".pdf", ".epub", ".html", ".txt", ".odt", ".rtf", ".zip")),
                                                         h4("Accepts .zip, .pdf, .doc, .docx and .txt files"),
                                                         br(),
                                                         h5("upload your file(s) and press the", strong("Screen"), "button to start."),
                                                         h5("Results will appear in a table below and can be downloaded as an excel file."),
                                                         br(),
                                                         br(),
                                                         fluidRow(
                                                             column(1, offset = 0, actionButton("Screengo2", "Screen", style = "background-color: #003366; color:white; border-color:#FF6600")),
                                                             column(1, offset = 1, downloadButton("exout", "Download Results", style = "background-color: #003366; color:white; border-color:#FF6600"))
                                                         ),
                                                         br(),
                                                         br(),
                                                         dataTableOutput("test2") %>% withSpinner(type = 4, color = "#FF6600", size = 2),
                                                         br()
                                                     )
                                     ))
                            )
                 ),
                 footer = tags$footer("Hannah O'Keefe, Information Retrieval Team, Newcastle University", align = "center", class = "footer")
)



server <- function(input, output, session) {
    
    ##prep
    biblist <- reactiveValues(l = NULL, m = NULL, n = NULL, o = NULL)
    text_list <- reactiveValues(c = NULL)
    name_list <- reactiveValues(l = NULL)
    err_list <- reactiveValues(err = NULL)
    name_list2 <- reactiveValues(s = NULL)
    KWhist <- reactiveValues(h = NULL)
    KWList <- reactiveValues(l = NULL)
    KWcount <- reactiveValues(a = NULL, t = NULL)
    FTexts <- reactiveValues(t = NULL)
    thresh <- reactiveValues(a1 = NULL, a2 = NULL, t1 = NULL, t2 = NULL)
    
    ##cleaning texts
    editcol <- function(x){
        x <- enc2utf8(x)
        x <- gsub("([[:upper:]])", "\\L\\1", x, perl = T)
        x <- sub(".*?(?i:Abstract)", "", x)
        x <- gsub("http[^ ]+", "", x)
        x <- gsub("\\S+@\\S+", "", x)
        x <- gsub("(\r?\n[a-z])\r?\n", "\\1", x)
        x <- gsub("(\\s+[a-z])\r?\n", "\\1", x)
        x <- gsub("\\.(?=[\r?\n|\r])", ". ", x, perl = TRUE)
        x <- gsub("\\.(?=[a-zA-Z|])", ".", x, perl = TRUE)
        x <- gsub("-\r?\n|\r", " ", x)
        x <- gsub("\r?\n|\r", " ", x)
        x <- gsub(",", "", x)
        x <- gsub("\\s+-", " ", x)
        x <- gsub("-", " ", x)
        x <- gsub("-\\s+", " ", x)
        x <- gsub("\\s*\\([^\\)]+\\)", "", x)
        x <- gsub("\"", "", x)
        x <- gsub(";", "", x)
        x <- gsub("<.*?>", "", x)
        x <- gsub(":", "", x)
        x <- gsub("\\\\", " ", x)
        x <- gsub("\\?", "", x)
        x <- gsub("/", " ", x)
        x <- gsub("\\*", "", x)
        x <- gsub("\\(", "", x)
        x <- gsub("\\)", "", x)
        x <- gsub("\\[", "", x)
        x <- gsub("\\]", "", x)
        x <- gsub("\'", "", x)
        x <- gsub("\\s(al)\\.", "al", x)
        x <- gsub("\\s(fig)\\.", "fig", x)
        x <- gsub("\\s(e)\\.(g)\\.", "eg", x)
        x <- gsub("\\s(app|appen|append)\\.", "appendix", x)
        x <- gsub("\\s(sup|supp|suppl)\\.", "supplemental", x)
        x <- gsub("[^[:alnum:][:punct:]]+", " ", x)
        a1 <- c("acknowledgments", "acknowledgements", "conflicts of interest", "conflict of interest", "financial contributions", "financial support", "declaration of interest", "supplementary material", "references", "supporting information", "author contributions")
        a2 <- stri_locate_last(x, regex = a1)
        a3 <- which.min(a2[,1])
        a4 <- a1[a3]
        x <- gsub(paste0("(.*)", a4, ".*$"),"\\1", x)
        return(x)
    }
    
    
    ##find unique words from included studies
    
    ##upload bibliographic file of includes
    observeEvent(input$uniq1_file,{
        b <- read_bibliography(input$uniq1_file$datapath)
        cols <- c("title", "abstract", "keywords")
        biblist$m <- data.frame(matrix(ncol = 0, nrow = nrow(b)))
        for (i in cols){
            if (i %in% colnames(b)){
                biblist$m[i] <- gsub("([[:upper:]])", "\\L\\1", b[,i], perl = T)
            }
        }
        biblist$m <- unique(unlist(strsplit(editcol(as.character(biblist$m)), "\\s+")))
    })
    
    
    ##upload bibliographic file
    observeEvent(input$uniq2_file,{
        b <- read_bibliography(input$uniq2_file$datapath)
        cols <- c("title", "abstract", "keywords")
        biblist$n <- data.frame(matrix(ncol = 0, nrow = nrow(b)))
        for (i in cols){
            if (i %in% colnames(b)){
                biblist$n[i] <- gsub("([[:upper:]])", "\\L\\1", b[,i], perl = T)
            }
        }
        biblist$n <- unique(unlist(strsplit(editcol(as.character(biblist$n)), "\\s+")))
    })
    
    
    ##find uniqie words
    observeEvent(input$find,{
        biblist$o <- setdiff(biblist$m, biblist$n)
        output$Unique <- renderDataTable(data.frame(biblist$o), options = list(dom = "tp"))
        print("done")
    })

    
    ##download list of unique words
    output$uniqout <- downloadHandler(
        filename <- "Unique words.xlsx",
        content <- function(file){
            DL <- data.frame(biblist$o)
            write_xlsx(DL, file)
            #writeLines(paste(biblist$o, collapse = ", "), file)
        }
    )
    
    
    ##keywords
    
    ##Set up keywords list
    observeEvent(input$KWgo,{
        kw1 <- input$keywords
        KWhist$h <- rbind(KWhist$h, data.frame(History = input$keywords, stringsAsFactors = FALSE))
        updateTextAreaInput(session, "keywords", value = "")
        #table of keywords
        output$KWhistory <- renderDataTable(KWhist$h, escape = FALSE, server = FALSE, extension = c("Select", "Buttons"), selection = 'none', editable = "cell",
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
        selrn <- input$KWhistory_rows_selected
        KWhist$h <- KWhist$h[-as.numeric(selrn),]
        KWhist$h <- data.frame(History = KWhist$h, stringsAsFactors = FALSE)
    })
    
    #editing search
    observeEvent(input$KWhistory_cell_edit,{
        KWhist$h[input$KWhistory_cell_edit$row, "History"] <- as.character(input$KWhistory_cell_edit$value)
    })
    
    #prepare search pattern
    observeEvent(input$Combine, {
        selrn <- input$KWhistory_rows_selected
        output$KWfinal <- renderText(paste0("(", KWhist$h$History[selrn], collapse = " OR ", ")"))
        KWList$l <- paste0("(", gsub("\\)\\)", "\\)", gsub("\\(\\(", "\\(", gsub("( near/)([[:digit:]])(\\s)", ")(?:\\W+\\w+){0,\\2}?\\\\W+(", gsub("\\)\\)", "\\)", gsub("([[:upper:]])", "\\L\\1", gsub( " AND ", ".*", gsub(" OR ", "|",paste0("(", KWhist$h$History[selrn], collapse = "|", ")"))), perl = TRUE))))), ")")
        print(KWList$l)
    })
    
    
    
    ###Title and Abstract screening
    
    ##upload bibliographic file
    observeEvent(input$ref_file,{
        b <- read_bibliography(input$ref_file$datapath)
        cols <- c("author", "title", "abstract", "keywords", "Label")
        biblist$l <- data.frame(matrix(ncol = 0, nrow = nrow(b)))
        for (i in cols){
            if (i %in% colnames(b)){
                biblist$l[i] <- gsub("([[:upper:]])", "\\L\\1", b[,i], perl = T)
            }
        }
        biblist$l$Keyword_count <- "0"
        biblist$l$Decision <- "1"
    })
    
    
    #Analysis for T&A
    observeEvent(input$Screengo,{
        for (i in 1:nrow(biblist$l)){
            KWcount$a[i] <- sapply(gregexpr(KWList$l, biblist$l$abstract[i], perl=TRUE), function(x) sum(x >0))
            KWcount$t[i] <- sapply(gregexpr(KWList$l, biblist$l$title[i], perl=TRUE), function(x) sum(x > 0))
            biblist$l$Keyword_count[i] <- KWcount$a[i] + KWcount$t[i]
        }
        biblist$l$Keyword_count[is.na(biblist$l$Keyword_count)] <- 0
        thresh$a1 <- 0 #quantile(as.numeric(biblist$l$Keyword_count), 0.25)
        thresh$a2 <- 1 #quantile(as.numeric(biblist$l$Keyword_count), 0.50)
        for (j in 1:nrow(biblist$l)){
            ifelse(as.numeric(biblist$l$Keyword_count[j]) >= thresh$a2,
                biblist$l$Decision[j] <- "Priority",
                ifelse(as.numeric(biblist$l$Keyword_count[j]) <= thresh$a1,
                    biblist$l$Decision[j] <- "Non-priority",
                    biblist$l$Decision[j] <- "Manual check"))
        }
        ##render table of bibliography
        output$test <- renderDataTable(biblist$l[, !names(biblist$l) %in% c("abstract", "keywords")], options = list(dom = "tp"))
    })
    
    
    ##download ris file
    output$risout <- downloadHandler(
        filename <- "TA_Screening.ris",
        content <- function(file){
            ris1 <- biblist$l[, !names(biblist$l) %in% "Keyword_count"]
            colnames(ris1)[colnames(ris1) == "Decision"] <- "language"
            ris1 <- as.bibliography(ris1)
            write_bibliography(ris1, filename = file, format = "ris") 
        }
    )
    
    
    
    
    
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
        FTexts$t <- data.frame(Title = name_list$l, Text = editcol(text_list$c), Keyword_count = "0", Decision = "1", stringsAsFactors = FALSE)
        for (i in 1:nrow(FTexts$t)){
            FTexts$t$Keyword_count[i] <- sapply(gregexpr(KWList$l, FTexts$t$Text[i], perl=TRUE), function(x) sum(x >0))
        }
        FTexts$t$Keyword_count[is.na(FTexts$t$Keyword_count)] <- 0
        thresh$t1 <- quantile(as.numeric(FTexts$t$Keyword_count), 0.25)
        thresh$t2 <- quantile(as.numeric(FTexts$t$Keyword_count), 0.75)
        for (j in 1:nrow(FTexts$t)){
            ifelse (as.numeric(FTexts$t$Keyword_count[j]) >= thresh$t2,
                FTexts$t$Decision[j] <- "Priority",
                ifelse(as.numeric(FTexts$t$Keyword_count[j]) <= thresh$t1,
                    FTexts$t$Decision[j] <- "Non-priority",
                    FTexts$t$Decision[j] <- "Manual Check"))
        }
        ##ft as table
        output$test2 <- renderDataTable(FTexts$t[, !names(FTexts$t) %in% "Text"], options = list(dom = "tp"))
        return(FTexts$t)
    })
    
    
    ##download excel file
    output$exout <- downloadHandler(
        filename <- "FT_Screening.xlsx",
        content <- function(file){
            ex1 <- FTexts$t[, !names(FTexts$t) %in% "Text"]
            ex1 <- data.frame(ex1)
            write_xlsx(ex1, file) 
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
