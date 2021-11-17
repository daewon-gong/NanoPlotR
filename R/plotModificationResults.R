#globalVariables call to avoid "no visible binding for global variable" during R package checking
utils::globalVariables(c("id", ".", "dmr_mean", "kmer",
                         "Count", "margin", "freq"))

#' Plot frequency bar plot of top n kmers
#'
#' @param modResults RNA modification detection results in a dataframe
#' @param numKmers Integer value specifying how many top kmers to display
#'
#'
#' @return a bar plot of the frequencies of the top n kmers
#'
#' @examples
#'
#' plotTopKmers(RnaModificationResults, numKmers = 10)
#'
#' @author {Dae-won Gong, \email{daewon.gong@mail.utoronto.ca}}
#'
#' @references
#' 1. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @importFrom ggplot2 ggplot aes geom_bar xlab ylab ggtitle theme_classic
#' @importFrom ggplot2 scale_y_continuous expansion
#' @importFrom magrittr %>%
#' @importFrom dplyr slice count
#' @export
#'
plotTopKmers <- function (modResults, numKmers = 10){
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modification results")
  }

  if (!"kmer" %in% colnames(modResults)) {
    stop("No kmer column in modResults. Please check if input satisfies required modResults format.")
  }

  if (!(numKmers %% 1 == 0)) {
    stop("Invalid data type of numKmers. Please input a valid integer.")
  }

  count <- modResults %>% count(kmer, sort = TRUE, name = "freq")

  if (numKmers > nrow(count)){
    warning("numKmers is greater than total number of kmers. Displaying all kmers instead.")
    numKmers <- nrow(count)
  }

  count %>%
    slice(1:numKmers) %>%
    ggplot(., aes(x=kmer, y=freq)) +
      geom_bar(stat = "identity", width = 0.8, fill = "steelblue") +
      xlab("Kmers") +
      ylab("Frequency") +
      ggtitle(paste("Frequency of top", numKmers, "kmers")) +
      theme_classic() +
      scale_y_continuous(expand = expansion(mult = c(0, .1)))
}

#' Plot count matrix of top gene/transcript Ids of selected modSites.
#'
#' @param modResults RNA modification detection results in a dataframe.
#' @param modSites Vector of modification sites that are included in the plot.
#' @param numTopIds Number of top transcript/ids to display.
#'
#'
#' @return a count matrix plot of inputted modSites and top gene/transcript id's ranked by DMR.
#'
#' @examples
#'
#' plotCountMatrix(RnaModificationResults, modSites = c("A"), numTopIds = 2)
#'
#' @author {Dae-won Gong, \email{daewon.gong@mail.utoronto.ca}}
#'
#' @references
#' 1. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient margin
#' @importFrom ggplot2 scale_x_discrete coord_equal theme_void labs theme element_text
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by tally
#'
#' @export
#'
plotCountMatrix <- function (modResults, modSites = c("A"), numTopIds = 20) {
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modresults")
  }

# xlabel <- if (geneName == FALSE) "ID" else "Gene"
  topIds <- getTopIds(modResults, numTopIds)

  modResults %>%
    filter(., id %in% topIds) %>%
    filter(., substr(kmer, 3, 3) %in% modSites) %>%
    group_by(id, kmer) %>%
    tally(., name = "Count") %>%
    ggplot(., aes(x = id, y = kmer, fill = Count, label = Count)) +
      geom_tile(aes(width = 0.7, height = 0.7)) +
      geom_text() +
      scale_fill_gradient(high = "red2", low = "white") +
      scale_x_discrete(position = "top") +
      coord_equal(ratio = 1) +
      theme_void() +
      labs(x = "ID", y = "Number of Sites") +
      theme(axis.text.x=element_text(angle = 90),
            axis.text.y = element_text(),
            axis.title.x = element_text(colour = "black",
                                        face = "bold",
                                        margin = margin(t = 0, r = 0, b = 10, l = 0)),
            axis.title.y = element_text(colour = "black",
                                        face = "bold",
                                        angle = 90,
                                        margin = margin(t = 0, r = 20, b = 0, l = 0)))
}

#' Get the top transcript/ids ranked by Differential modification rate
#'
#' @param modResults RNA modification detection results in a dataframe.
#' @param numTopIds Number of Top Ids to return.
#'
#'
#' @return Vector of
#'
#' @examples
#'
#' getTopIds(RnaModificationResults, numTopIds = 1)
#'
#' @author {Dae-won Gong, \email{daewon.gong@mail.utoronto.ca}}
#'
#' @references
#' 1. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @importFrom dplyr group_by summarise slice_max pull
#' @importFrom magrittr %>%
#' @export
#'
getTopIds <- function (modResults, numTopIds = 20) {
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modresults")
  }

  if (numTopIds > length(unique(modResults$id))){
    warning("numTopIds is greater than total number of unique ids. Displaying all Ids instead.")
    numTopIds <- length(unique(modResults$id))
  }

  #Dynamically get the label for differential modification rate according to xpore output template
  dmr_label <- colnames(modResults)[grepl("diff_mod_rate", colnames(modResults))]

  topIds <- modResults %>%
    group_by(id) %>%
    summarise(dmr_mean = mean(!!as.name(dmr_label))) %>%
    slice_max(., order_by = dmr_mean, n = numTopIds) %>%
    pull(id)

  return(topIds)
}

#' Plot histogram of modification rates.
#'
#' @param modResults RNA modification detection results in a data frame.
#'
#' @return Histograms of all modification rates in 1 panel.
#'
#' @examples
#'
#' plotModHist(RnaModificationResults)
#'
#' @author {Dae-won Gong, \email{daewon.gong@mail.utoronto.ca}}
#'
#' @references
#' 1. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @importFrom Hmisc hist.data.frame
#' @export
#'
plotModHist <- function (modResults) {
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modresults")
  }
  modNames <- colnames(modResults)[grepl("\\<mod_rate", colnames(modResults))]
  hist.data.frame(modResults[ , modNames], mtitl = "Histograms of Modification Rates")
}
