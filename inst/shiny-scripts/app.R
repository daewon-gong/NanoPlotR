source("modals.R")

# UI for shiny application
ui <- fluidPage(
  # Styling for shiny error/warning notifications
  tags$head(tags$style(
    HTML(".shiny-notification {
             position:fixed;
             top: 0px;
             right: 0px;
             }
             ")
  )),
  # Application title
  titlePanel(tags$h1(tags$b("NanoPlotR"))),
  sidebarLayout(
    sidebarPanel(
      fileInput("modResults",
                "Modification result file:",
                accept = ".csv, .rds"),
      # Allow users to select which plots to display.
      checkboxGroupInput("plotSelection", "Select the plots to display:",
                         c("Top Kmers" = "topKmers",
                           "Count Matrix" = "countMatrix",
                           "Histogram" = "hist")),
      hr(),
      # Start button.
      actionButton(inputId = "start", label = "Start!"),
      br()
    ),
    mainPanel(
      # tabsetPanel for plots, no panels added here to dynamically update
      # according to user selection.
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

  # Server logic for when user clicks Start button.
  observeEvent(input$start, {
    # Read input file
    input_file <- input$modResults
    # Display error if user starts without uploading input file.
    if (is.null(input_file)) {
      error_message <- "Please Input modification result file."
      showNotification(error_message, type = "error")
      return(NULL)
    }

    plotSelection(input$plotSelection)
    # Display error if user doesn't select any plots to display.
    if (is.null(plotSelection)) {
      error_message <- "Please select atleast 1 plot to begin"
      showNotification(error_message, type = "error")
      return(NULL)
    }

    inputFilePath <- input_file$datapath
    # Logic for reading csv input.
    if (tools::file_ext(inputFilePath) == "csv") {
      # trycatch for possible read.csv errors caused by incorrect input.
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

    # Logic for reading rds input.
    if (tools::file_ext(inputFilePath) == "rds") {
      # tryCatch for possible read.csv errors caused by incorrect input
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

    # Display appropriate option selection modal depending on user selection
    # Always show topKmersModal first if user selected. If user did not select
    # topKmers, show countMatrixModal.
    if ("topKmers" %in% plotSelection()) {
      shiny::showModal(topKmersModal())
    } else if ("countMatrix" %in% plotSelection()){
      shiny::showModal(countMatrixModal())
    }

    # Histogram requires no modals.
    if ("hist" %in% plotSelection()) {
      # Dynamically insert tab if user selected hist.
      insertTab(
        inputId = "main_tab",
        tab = tabPanel("Histogram",
                       plotOutput("histPlot"))
      )
      # Render histogram plot.
      output$histPlot <- renderPlot({
        plotModHist(modResults())
      })
    }
  })

  observeEvent(input$top_kmer_next, {
    # Store user input for numKmers
    numKmers(input$numKmers)
    # Dynamically insert tab if user selected topKmers.
    insertTab(
      inputId = "main_tab",
      tab = tabPanel("Top Kmers Bar Plot",
                     plotOutput("topKmersPlot"))
    )
    removeModal()
    # Render top kmers plot.
    output$topKmersPlot <- renderPlot({
      plotTopKmers(modResults(), numKmers())
    })
    # If user also selected countMatrix, show the matrix option modal next.
    if ("countMatrix" %in% plotSelection()) {
      showModal(countMatrixModal())
    }
  })

  observeEvent(input$matrix_next, {
    # Store user input options for countMatrix.
    matrixNumTopIds(input$matrixNumTopIds)
    matrixModSites(input$matrixModSites)
    # Dynamically insert tab if user selected countMatrix.
    insertTab(
      inputId = "main_tab",
      tab = tabPanel("Count Matrix",
                     plotOutput("countMatrixPlot"))
    )
    # Render countMatrix plot.
    output$countMatrixPlot <- renderPlot({
      plotCountMatrix(modResults(), matrixModSites(), as.integer(matrixNumTopIds()))
    })
    removeModal()
    # No more modals needed, additional modals required when more plots supported.
  })
}

# Create Shiny app ----
shiny::shinyApp(ui = ui, server = server)

# [END]
