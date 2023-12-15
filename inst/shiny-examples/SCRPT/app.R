library(shiny)
library(devtools)
library(textreadr)
library(DT)
library(tabulizer)
library(stringr)
library(dplyr)
library(tidyr)
library(stringi)
library(readxl)
library(synthesisr)
library(writexl)
library(shinycssloaders)

options(
    shiny.maxRequestSize = 10000*1024^2,
    "repos" = c(
        "CRAN" = "https://cran.rstudio.com",
        "tabulizer" = "https://github.com/ropensci/tabulizer",
        "tabulizerjars" = "https://github.com/ropensci/tabulizerjars"
    )
)

ui <- navbarPage(
    "SCRPT: SCReening Prioritisation Tool",
    header = tags$style(
        HTML(
            ".navbar-nav li a {color: #003366 !important;}.navbar-brand{color: #003366 !important;}" #nolint
        )
    ),
    tabPanel(
        "Home",
        icon = icon("home"),
        fluidRow(
            column(
                width = 6,
                offset = 3,
                wellPanel(
                    style = "background-color: #003366; color:white;",
                    br(),
                    hr(
                        style = "border-color: #FF6600; width: 25%; margin-left: 0" #nolint
                    ),
                    h4(
                        "SCRPT allows you to build up a set of keywords",
                        "which will be used to screen through titles and",
                        "abstract or full text files."
                    ),
                    br(),
                    h4(
                        "Start by working up your terms using the keyword tab.",
                        " Next, select the Title & Abstract or Full Text tab ",
                        "to enter your data. To start screening, ",
                        "press the screen button on the appropriate tab."
                    ),
                    br()
                )
            )
        )
    ),
    navbarMenu(
        "Menu",
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
        tabPanel(
            "Unique Word Finder",
            sidebarPanel(
                width = 3,
                style = "background-color: #003366; color:white;",
                br(),
                br(),
                hr(style = "border-color: #FF6600; width: 50%; margin-left: 0"),
                h4(
                    "This page allows you to identify",
                    strong(" unique words "),
                    "which are only found in included studies"
                ),
                br(),
                h4(
                    "This works by comparing words in the title, ",
                    "abstract and keywords of a user specified set of ",
                    "included studies and excluded studies"
                ),
                br(),
                h4(
                    strong("For example: "),
                    "if included studies contained 'Happy', ",
                    "'Sunshine' and 'Violet', ",
                    "and exlcuded studies contained",
                    " 'Happy' and 'Violet',",
                    " the unique word would be ",
                    strong("'Sunshine'.")
                ),
                br()
            ),
            column(
                width = 6,
                wellPanel(
                    style =
                        "background-color: #003366;
                        color:white;
                        padding: 35px",
                    br(),
                    hr(
                        style =
                        "border-color: #FF6600;
                        width: 50%;
                        margin-left:0"
                    ),
                    h3(
                        strong(
                            "Find unique words"
                        ),
                        "- please enter two bibliographic files to compare"
                    ),
                    fileInput(
                        "uniq1_file",
                        h3("upload included studies bibliographic file:"),
                        multiple = FALSE
                    ),
                    fileInput(
                        "uniq2_file",
                        h3("upload excluded studies bibliographic file:"),
                        multiple = FALSE
                    ),
                    h4("Accepts bibliographies in .ris format"),
                    br(),
                    h5(
                        "upload your ris files and press the",
                        strong("Find"),
                        "button to start."
                    ),
                    h5(
                        "Results will appear in a table below and can",
                        " be downloaded as an excel file."
                    ),
                    br(),
                    fluidRow(
                        column(
                            1,
                            offset = 0,
                            actionButton(
                                "find",
                                "Find",
                                style =
                                    "background-color: #003366;
                                    color:white;
                                    border-color:#FF6600"
                            )
                        ),
                        column(
                            1,
                            offset = 1,
                            downloadButton(
                                "uniqout",
                                "Download Results",
                                style =
                                    "background-color: #003366;
                                    color:white;
                                    border-color:#FF6600"
                            )
                        )
                    ),
                    br(),
                    br(),
                    dataTableOutput("Unique") %>% withSpinner(
                        type = 4,
                        color = "#FF6600",
                        size = 2
                    ),
                    br(),
                    br(),
                    br()
                )
            )
        ),
        tabPanel(
            "Search Builder",
            sidebarPanel(
                width = 3,
                style = "background-color: #003366; color:white;",
                br(),
                br(),
                hr(style = "border-color: #FF6600; width: 50%; margin-left: 0"),
                h4(
                    "Terms can be entered one at a time ",
                    "or as a string. If using",
                    strong(" AND/OR"),
                    "please enter statements in capitals, lower case",
                    "and/or will be registered as keywords words."
                ),
                br(),
                h4(
                    "To truncate words please stem the word at",
                    " the appropriate point,",
                    " you do not need use truncation symbols."
                ),
                br(),
                h4(
                    "For adjacency please use",
                    strong("NEAR/1"),
                    ". Outer brackets around adjacency",
                    " statements are not required",
                    " but please use inner brackets",
                    " if incorporating AND/OR statements."
                ),
                br(),
                h4(strong("For example: Hockey NEAR/3 (ice OR rink OR skate)")),
                br()
            ),
            column(
                width = 6,
                wellPanel(
                    style =
                        "background-color: #003366;
                        color:white;
                        padding: 35px",
                    br(),
                    hr(
                        style =
                        "border-color: #FF6600;
                        width: 50%;
                        margin-left:0"
                    ),
                    h3(
                        strong("Search Terms"),
                        "- please enter your search string here:"
                    ),
                    fluidRow(
                        column(
                            8,
                            offset = 0,
                            textAreaInput(
                                "keywords",
                                label = NULL,
                                height = 50
                            )
                        ),
                        column(
                            1,
                            offset = 0,
                            actionButton(
                                "KWgo",
                                "Add",
                                style =
                                    "margin: 8px;
                                    background-color: #003366;
                                    color:white;
                                    border-color:#FF6600"
                            )
                        )
                    ),
                    br(),
                    br(),
                    hr(
                        style =
                        "border-color: #FF6600;
                        width: 25%;
                        margin-left:0"
                    ),
                    h4(tags$b("Search history table")),
                    h5("Lines can be edited by double clicking on the row"),
                    h5(
                        "When you are happy with your keyword list",
                        " please select rows you wish to include and use the",
                        strong("Combine"),
                        "button to finalise the list"
                    ),
                    br(),
                    br(),
                    dataTableOutput("KWhistory") %>% withSpinner(
                        type = 4,
                        color = "#FF6600",
                        size = 2
                    ),
                    br(),
                    br(),
                    br()
                )
            ),
            sidebarPanel(
                width = 3,
                style = "background-color: #003366; color:white;",
                br(),
                br(),
                hr(style = "border-color: #FF6600; width: 50%; margin-left: 0"),
                wellPanel(
                    style = "color:black;",
                    textOutput("KWfinal")
                ),
                br(),
                h5(
                    "This is the finalised string that",
                    " will be searched for within your texts"
                ),
                br(),
                h5(
                    "Please select if you would like to screen",
                    strong("Title & Abstracts"),
                    "or",
                    strong("Full Texts"),
                    " from the Menu at the top left of this page."
                ),
                br()
            )
        ),
        tabPanel(
            "Title & Abstract Screener",
            fluidRow(
                column(
                    width = 6,
                    offset = 3,
                    wellPanel(
                        style = "background-color: #003366; color:white;",
                        br(),
                        hr(
                            style =
                                "border-color: #FF6600;
                                width: 25%;
                                margin-left:0"
                        ),
                        fileInput(
                            "ref_file",
                            h3("upload a bibliographic file:"),
                            multiple = TRUE
                        ),
                        h4("Accepts bibliographies in .ris format"),
                        br(),
                        h5(
                            "upload your ris file and press the",
                            strong("Screen"),
                            "button to start."
                        ),
                        h5(
                            "Results will appear in a table below and can be",
                            " downloaded as a ris file. Please note, the",
                            strong("Decisions"),
                            "will appear in the",
                            strong("Language"),
                            "field of the ris file."
                        ),
                        br(),
                        fluidRow(
                            column(
                                1,
                                offset = 0,
                                actionButton(
                                    "Screengo",
                                    "Screen",
                                    style =
                                        "background-color: #003366;
                                        color:white;
                                        border-color:#FF6600"
                                )
                            ),
                            column(
                                1,
                                offset = 1,
                                downloadButton(
                                    "risout",
                                    "Download Results",
                                    style =
                                        "background-color: #003366;
                                        color:white;
                                        border-color:#FF6600"
                                )
                            )
                        ),
                        br(),
                        br(),
                        dataTableOutput("test") %>% withSpinner(
                            type = 4,
                            color = "#FF6600",
                            size = 2
                        ),
                        br()
                    )
                )
            )
        ),
        tabPanel(
            "Full Text Screener",
            fluidRow(
                column(
                    width = 6,
                    offset = 3,
                    wellPanel(
                        style = "background-color: #003366; color:white;",
                        br(),
                        hr(
                            style =
                                "border-color: #FF6600;
                                width: 25%;
                                margin-left:0"
                        ),
                        fileInput(
                            "zip_file",
                            h3("Upload files:"),
                            multiple = TRUE,
                            accept = c(
                                ".doc",
                                ".docx",
                                ".pdf",
                                ".epub",
                                ".html",
                                ".txt",
                                ".odt",
                                ".rtf",
                                ".zip"
                            )
                        ),
                        h4("Accepts .zip, .pdf, .doc, .docx and .txt files"),
                        br(),
                        h5(
                            "upload your file(s) and press the",
                            strong("Screen"),
                            "button to start."
                        ),
                        h5(
                            "Results will appear in a table below",
                            " and can be downloaded as an excel file."
                        ),
                        br(),
                        br(),
                        fluidRow(
                            column(
                                1,
                                offset = 0,
                                actionButton(
                                    "Screengo2",
                                    "Screen",
                                    style =
                                        "background-color: #003366;
                                        color:white;
                                        border-color:#FF6600"
                                )
                            ),
                            column(
                                1,
                                offset = 1,
                                downloadButton(
                                    "exout",
                                    "Download Results",
                                    style =
                                        "background-color: #003366;
                                        color:white;
                                        border-color:#FF6600"
                                )
                            )
                        ),
                        br(),
                        br(),
                        dataTableOutput("test2") %>% withSpinner(
                            type = 4,
                            color = "#FF6600",
                            size = 2
                        ),
                        br()
                    )
                )
            )
        )
    ),
    footer = tags$footer(
        "Hannah O'Keefe, Information Retrieval Team, Newcastle University",
        align = "center",
        class = "footer"
    )
)

server <- function(input, output, session) {
    ##variables
    ## Unique Words
    includesBib <- shiny::reactiveValues()
    excludesBib <- shiny::reactiveValues()

    ## variables (keywords)
    kw1 <- shiny::reactiveValues()
    termsFinal <- shiny::reactiveValues()

    #variables (title and abstract screening)
    biblist <- reactiveValues()
    thresh <- reactiveValues(a1 = NULL, a2 = NULL, t1 = NULL, t2 = NULL)

    #variables (full text)
    placeholder <- reactiveValues()
    textlist <- reactiveValues(a = NULL)
    namelist <- reactiveValues(a = NULL)
    errorlist <- reactiveValues(a = NULL)

    ## find unique words from included studies ##

    ##upload bibliographic file of includes
    shiny::observeEvent(
        input$uniq1_file, {
            includesBib <- importBibliography(input$uniq1_file)
            includesBib <- makeCorpus(includesBib)
        }
    )
    ##upload bibliographic file (excludes)
    shiny::observeEvent(
        input$uniq2_file, {
            excludesBib <- importBibliography(input$uniq2_file)
            excludesBib <- makeCorpus(excludesBib)
        }
    )

    ##find unique words
    shiny::observeEvent(
        input$find, {
            output$Unique <- corpustools::compare_corpus(
                includesBib,
                excludesBib,
                feature = "token"
            )
        }
    )

    ##download list of unique words
    output$uniqout <- shiny::downloadHandler(
        "SCRPT_output.xlsx",
        function(fn) {
            writexl::write_xlsx(data.frame(output$Unique), fn)
        }
    )

    #-----------------------------------------------------------
    ##Search Builder
    ##Set up search builder list
    observeEvent(
        input$KWgo,
        {
            kw1 <- initialiseSearchTerms()
            kw1 <- addSearchTerm(
                kw1,
                input$keywords
            )
            updateTextAreaInput(
                session,
                "keywords",
                value = ""
            )
            #table of keywords
            output$KWhistory <- renderDataTable(
                kw1,
                escape = FALSE,
                server = FALSE,
                extension = c("Select", "Buttons"),
                selection = "none",
                editable = "cell",
                options = list(
                    dom = "Btp",
                    buttons = list(
                        "selectAll",
                        "selectNone",
                        list(
                            extend = "collection",
                            text = "Delete",
                            action = DT::JS(
                                "function (e, dt, node, config) {
                                    Shiny.setInputValue(
                                        'delrow',
                                        true,
                                        {priority:'event'}
                                    );
                                }"
                            )
                        ),
                        list(
                            extend = "collection",
                            text = "Combine",
                            action = DT::JS(
                                "function (e, dt, node, config) {
                                    Shiny.setInputValue(
                                        'Combine',
                                        true,
                                        {priority:'event'}
                                    );
                                }"
                            )
                        )
                    ),
                    columnDefs = list(
                        list(
                            className = "select-checkbox",
                            targets = 0,
                            orderable = FALSE,
                            width = "50px"
                        ),
                        list(
                            className = "dt-left",
                            targets = "_all"#
                        )
                    ),
                    select = list(style = "multi", selector = "td:first-child")
                )
            )
        }
    )

    #removing line from search
    observeEvent(
        input$delrow,
        {
            kw1 <- removeSearchTerm(kw1, input$KWhistory_rows_selected)
        }
    )

    #editing search
    observeEvent(
        input$KWhistory_cell_edit,
        {
            kw1 <- replaceSearchTerm(
                kw1,
                input$KWhistory_cell_edit$row,
                input$KWhistory_cell_edit$value
            )
        }
    )

    #prepare search pattern
    observeEvent(
        input$Combine,
        {
            output$KWfinal <- renderText(
                combineSearchTerms(
                    kw1,
                    input$KWhistory_rows_selected
                )
            )
            termsFinal <- formatSearchString(output$KWfinal)
        }
    )

    #-----------------------------------------------------------------

    ##upload bibliographic file
    observeEvent(
        input$ref_file,
        {
            biblist <- importBibliography(input$ref_file)
            biblist <- cleaning(biblist$file)
            biblist <- makeCorpus(biblist$file)
        }
    )

    #Analysis for T&A
    observeEvent(
        input$Screengo,
        {
            output$test <- renderDataTable(runSearchString(termsFinal, biblist))
        }
    )
    
    ##download ris file
    #output$risout <- downloadHandler(
    #    downloadRis(output$test)
    #)
    
    #----------------------------------------------------------------
   
    ##get list of full texts
    observeEvent(input$zip_file,{
        importFullTexts(input$zip_file$name, input$zip_file$datapath)
        return(textlist)
    })

    #Analysis for full texts
    observeEvent(input$Screengo2,{
        output$test2 <- renderDataTable(runSearchString(termsFinal, textlist$a))
    })

    ##download excel file
    output$exout <- downloadHandler(
        downloadxlxs(output$test2)
    )
}

#-------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)