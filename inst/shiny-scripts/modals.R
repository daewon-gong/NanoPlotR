countMatrixModal <- function(failed = FALSE) {
  modalDialog(
    title = "Count Matrix Options",
    checkboxGroupInput("matrixModSites", "Select modification sites:",
                       c("A" = "A",
                         "C" = "C",
                         "G" = "G",
                         "T" = "T")),
    numericInput(
      "matrixNumTopIds",
      "Input number of top ids to display:",
      100,
      min = 1,
      max = 100,
    ),
    if (failed) {
      div(tags$b("Error", style = "color: red;"))
    },
    footer = tagList(
      actionButton("matrix_next", "Next"),
      modalButton("Cancel")
    )
  )
}

topKmersModal <- function(failed = FALSE) {
  modalDialog(
    title = "Top Kmers Options",
    numericInput(
      "numKmers",
      "Input number of top kmers to display:",
      100,
      min = 1,
      max = 100,
    ),
    if (failed) {
      div(tags$b("Error", style = "color: red;"))
    },
    footer = tagList(
      actionButton("top_kmer_next", "Next"),
      modalButton("Cancel")
    )
  )
}
