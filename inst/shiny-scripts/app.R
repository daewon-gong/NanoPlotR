source("modals.R")
ui <- fluidPage(
  tags$head(tags$style(
    HTML(".shiny-notification {
             position:fixed;
             top: 0px;
             right: 0px;
             }
             ")
  )),
  # App title
  titlePanel(tags$h1(tags$b("NanoPlotR"))),
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      fileInput("modResults",
                "Modification result file:",
                accept = ".csv, .rds"),
      checkboxGroupInput("plotSelection", "Select the plots to display:",
                         c("Top Kmers" = "topKmers",
                           "Count Matrix" = "countMatrix",
                           "Histogram" = "hist")),
      hr(),
      actionButton(inputId = "start", label = "Start!"),
      br()
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tab"
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  numKmers <- reactiveVal(NULL)
  matrixNumTopIds <- reactiveVal(NULL)
  matrixModSites <- reactiveVal(NULL)
  plotSelection <- reactiveVal(NULL)
  modResults <- reactiveVal(NULL)

  observeEvent(input$start, {
    input_file <- input$modResults
    if (is.null(input_file)) {
      error_message <- "Please Input modification result file."
      showNotification(error_message, type = "error")
      return(NULL)
    }

    plotSelection(input$plotSelection)
    if (is.null(plotSelection)) {
      error_message <- "Please select atleast 1 plot to begin"
      showNotification(error_message, type = "error")
      return(NULL)
    }
    inputFilePath <- input_file$datapath
    if (tools::file_ext(inputFilePath) == "csv") {
      tryCatch({
        modResults(read.csv(inputFilePath))
      },
      warning = function(warn) {
        showNotification(paste0(warn), type = 'warning')
      },
      error = function(error) {
        showNotification(paste0(error), type = 'err')
        return(NULL)
      })
    }

    if (tools::file_ext(inputFilePath) == "rds") {
      tryCatch({
        modResults(readRDS(inputFilePath))
      },
      warning = function(warn) {
        showNotification(paste0(warn), type = 'warning')
      },
      error = function(error) {
        showNotification(paste0(error), type = 'err')
        return(NULL)
      })
    }
    if ("topKmers" %in% plotSelection()) {
      shiny::showModal(topKmersModal())
    } else if ("countMatrix" %in% plotSelection()){
      shiny::showModal(countMatrixModal())
    }
    if ("hist" %in% plotSelection()) {
      insertTab(
        inputId = "main_tab",
        tab = tabPanel("Histogram",
                       plotOutput("histPlot"))
      )
      output$histPlot <- renderPlot({
        plotModHist(modResults())
      })
    }
  })

  observeEvent(input$top_kmer_next, {
    numKmers(input$numKmers)
    insertTab(
      inputId = "main_tab",
      tab = tabPanel("Top Kmers Bar Plot",
                     plotOutput("topKmersPlot"))
    )
    removeModal()
    output$topKmersPlot <- renderPlot({
      plotTopKmers(modResults(), numKmers())
    })
    if ("countMatrix" %in% plotSelection()) {
      showModal(countMatrixModal())
    }
  })

  observeEvent(input$matrix_next, {
    matrixNumTopIds(input$matrixNumTopIds)
    matrixModSites(input$matrixModSites)
    insertTab(
      inputId = "main_tab",
      tab = tabPanel("Count Matrix",
                     plotOutput("countMatrixPlot"))
    )
    output$countMatrixPlot <- renderPlot({
      plotCountMatrix(modResults(), matrixModSites(), as.integer(matrixNumTopIds()))
    })
    removeModal()
  })
}


# Create Shiny app ----
shiny::shinyApp(ui = ui, server = server)

# [END]
