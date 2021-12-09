#globalVariables call to avoid "no visible binding for global variable" during R package checking
utils::globalVariables(c("id", ".", "dmr_mean", "kmer",
                         "Count", "margin", "freq"))

#' Plot frequency bar plot of top n kmers. The modResults requires a kmer column.
#' A ggplot bar plot of the frequencies of the top n kmers is returned.
#' The n kmers can be specified through the numKmers parameter which is defaulted to 10.
#'
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
#' 1. Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of
#' Data Manipulation. R package version 1.0.7. https://CRAN.R-project.org/package=dplyr
#'
#' 2. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' 3. H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#'
#'
#' @importFrom ggplot2 ggplot aes geom_bar xlab ylab ggtitle theme_classic
#' @importFrom ggplot2 scale_y_continuous expansion
#' @importFrom magrittr %>%
#' @importFrom dplyr slice count
#' @export
#'
plotTopKmers <- function (modResults, numKmers = 10){
  # Check if inputted modResults is a dataframe.
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modification results")
  }
  # Check if modResults has required columns.
  if (!"kmer" %in% colnames(modResults)) {
    stop("No kmer column in modResults. Please check if input satisfies required modResults format.")
  }
  # Check if numKmers is a valid integer.
  if (!(numKmers %% 1 == 0)) {
    stop("Invalid data type of numKmers. Please input a valid integer.")
  }

  # Count the kmers.
  count <- modResults %>% count(kmer, sort = TRUE, name = "freq")

  # Warn users if inputted numKmers is greater than number of kmers present in modResults.
  if (numKmers > nrow(count)){
    warning("numKmers is greater than total number of kmers. Displaying all kmers instead.")
    numKmers <- nrow(count)
  }
  count %>%
    # Slice to only take the top kmers
    slice(1:numKmers) %>%
    # Plot top kmers bar graph using ggplot2
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
#' The modResults requires to be a dataframe with format of xPore's output format.
#'
#' The number of top n transcript/ids to display can be specified through the
#' numTopIds parameter. The numTopIds paramter is Defaulted to 20
#'
#' The desired modification sites can be given through the modSites parameter
#' which requires a vector of characters specifying a RNA nucleotide. The
#' function then plots a count matrix of top gene/transcript Ids of modifications
#' sites where the middle kmer is part of vector modSites. Defaulted to c("A").
#'
#' A ggplot of the count matrix is returned.
#'
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
#'
#' 1. Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of
#' Data Manipulation. R package version 1.0.7. https://CRAN.R-project.org/package=dplyr
#'
#' 2. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' 3. H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient margin
#' @importFrom ggplot2 scale_x_discrete coord_equal theme_void labs theme element_text
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by tally
#'
#' @export
#'
plotCountMatrix <- function (modResults, modSites = c("A"), numTopIds = 20) {
  # Check if inputted modResults is a dataframe.
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modresults")
  }
  # Check if inputted modResults has all required columns.
  requiredColumns <- c("kmer", "id")
  if (!all(requiredColumns %in% colnames(modResults))) {
    stop("Missing required Columns in modResults. Please check if input has kmer and id columns.")
  }
  # Check if inputted modSites is valid.
  if (!(is.vector(modSites) && is.atomic(modSites))) {
    stop("Invalid modSites input. Please check if modSites is a valid character vector")
  }

# xlabel <- if (geneName == FALSE) "ID" else "Gene"
  topIds <- getTopIds(modResults, numTopIds)

  modResults %>%
    # Filter to get only the topIds.
    dplyr::filter(., id %in% topIds) %>%
    # Filter to get only the kmers specified by user.
    dplyr::filter(., substr(kmer, 3, 3) %in% modSites) %>%
    # Group and tally counts of kmers for each ids.
    dplyr::group_by(id, kmer) %>%
    dplyr::tally(., name = "Count") %>%
    # Plot countmatrix with the computed counts using ggplot2.
    ggplot2::ggplot(., ggplot2::aes(x = id, y = kmer, fill = Count, label = Count)) +
      ggplot2::geom_tile(ggplot2::aes(width = 0.7, height = 0.7)) +
      ggplot2::geom_text() +
      ggplot2::scale_fill_gradient(high = "red2", low = "white") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::coord_equal(ratio = 1) +
      ggplot2::theme_void() +
      ggplot2::labs(x = "ID", y = "Number of Sites") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
            axis.text.y = ggplot2::element_text(),
            axis.title.x = ggplot2::element_text(colour = "black",
                                        face = "bold",
                                        margin = margin(t = 0, r = 0, b = 10, l = 0)),
            axis.title.y = element_text(colour = "black",
                                        face = "bold",
                                        angle = 90,
                                        margin = margin(t = 0, r = 20, b = 0, l = 0)))
}

#' Get the top transcript/ids ranked by differential modification rate.
#' The modResults requires to be a dataframe with format of xPore's output format.
#' The number of top Ids to return can be specified by the numTopIds paramter.
#'
#'
#' @param modResults RNA modification detection results in a dataframe.
#' @param numTopIds Number of Top Ids to return.
#'
#'
#' @return Vector of number of top Ids ranked by differencial modification rate.
#'
#' @examples
#'
#' getTopIds(RnaModificationResults, numTopIds = 1)
#'
#' @author {Dae-won Gong, \email{daewon.gong@mail.utoronto.ca}}
#'
#' @references
#' 1. Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of
#' Data Manipulation. R package version 1.0.7. https://CRAN.R-project.org/package=dplyr
#'
#' 2. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @importFrom dplyr group_by summarise slice_max pull
#' @importFrom magrittr %>%
#' @export
#'
getTopIds <- function (modResults, numTopIds = 20) {
  # Check if inputted modResults is a dataframe.
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modresults")
  }
  # Warn users if inputted numKmers is greater than number of kmers present in modResults.
  if (numTopIds > length(unique(modResults$id))){
    warning("numTopIds is greater than total number of unique ids. Displaying all Ids instead.")
    numTopIds <- length(unique(modResults$id))
  }

  # Dynamically get the label for differential modification rate according to xpore output template
  dmr_label <- colnames(modResults)[grepl("diff_mod_rate", colnames(modResults))]

  topIds <- modResults %>%
    # Group by id to calculate mean by id
    dplyr::group_by(id) %>%
    # Calculate dmr_mean for each id
    dplyr::summarise(dmr_mean = mean(!!as.name(dmr_label))) %>%
    # Only take the top n ids specified in numTopIds
    dplyr::slice_max(., order_by = dmr_mean, n = numTopIds) %>%
    # Take the top ids as a vector.
    dplyr::pull(id)

  return(topIds)
}

#' Plot histograms of the modification rates for all replications and conditions.
#' The modResults parameter requires a dataframe with modification rate
#' comlumns with the following format "mod_rate_<condition>_<replication>",
#' Ex. column: "mode_rate_KO_rep1". The function will read all columns with
#  this format and plot a histogram for each modification rate columns.
#'
#' Check format of xPore's output format for required modResults format.
#'
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
#'
#' 1. Frank E Harrell Jr (2021). Hsmisc: Harrell Miscellaneous.
#' R package version 4.6.0. https://CRAN.R-project.org/package=Hmisc
#'
#' 2. Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @importFrom Hmisc hist.data.frame
#' @export
#'
plotModHist <- function (modResults) {
  # Check if inputted modResults is a dataframe.
  if (!is.data.frame(modResults)) {
    stop("Please input a valid dataframe for modresults")
  }
  # Dynamically extract mod_rate columns.
  modNames <- colnames(modResults)[grepl("\\<mod_rate", colnames(modResults))]
  # Plot singe or multiple histograms using the Hmisc package.
  Hmisc::hist.data.frame(modResults[ , modNames], mtitl = "Histograms of Modification Rates")
}

#[END].
