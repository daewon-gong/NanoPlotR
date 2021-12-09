library(shiny)
# Shiny UI modal components for NanoPlotR

# Modal for user input options for plotting count matrix.
countMatrixModal <- function(failed = FALSE) {
  modalDialog(
    title = "Count Matrix Options",
    # Ask user to input desired modification sites.
    checkboxGroupInput("matrixModSites", "Select modification sites:",
                       c("A" = "A",
                         "C" = "C",
                         "G" = "G",
                         "T" = "T")),
    # Ask user to input number of top kmers to display.
    numericInput(
      "matrixNumTopIds",
      "Input number of top ids to display:",
      1,
      min = 1,
    ),
    # Error handling.
    if (failed) {
      div(tags$b("Please make sure to select atleast one modification site and provide an integer greater than 0.", style = "color: red;"))
    },
    footer = tagList(
      actionButton("matrix_next", "Next"),
      modalButton("Cancel")
    )
  )
}

# Modal for user input options for plotting top kmers.
topKmersModal <- function(failed = FALSE) {
  modalDialog(
    title = "Top Kmers Options",
    # Ask user to input number of top kmers to display.
    numericInput(
      "numKmers",
      "Input number of top kmers to display:",
      1,
      min = 1,
    ),
    # Error handling.
    if (failed) {
      div(tags$b("Please provide integer greater than 0", style = "color: red;"))
    },
    footer = tagList(
      actionButton("top_kmer_next", "Next"),
      modalButton("Cancel")
    )
  )
}
