% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotModificationResults.R
\name{getTopIds}
\alias{getTopIds}
\title{Get the top transcript/ids ranked by differential modification rate.
The modResults requires to be a dataframe with format of xPore's output format.
The number of top Ids to return can be specified by the numTopIds paramter.}
\usage{
getTopIds(modResults, numTopIds = 20)
}
\arguments{
\item{modResults}{RNA modification detection results in a dataframe.}

\item{numTopIds}{Number of Top Ids to return.}
}
\value{
Vector of number of top Ids ranked by differencial modification rate.
}
\description{
Get the top transcript/ids ranked by differential modification rate.
The modResults requires to be a dataframe with format of xPore's output format.
The number of top Ids to return can be specified by the numTopIds paramter.
}
\examples{

getTopIds(RnaModificationResults, numTopIds = 1)

}
\references{
\enumerate{
\item Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of
Data Manipulation. R package version 1.0.7. https://CRAN.R-project.org/package=dplyr
\item Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
}
}
\author{
{Dae-won Gong, \email{daewon.gong@mail.utoronto.ca}}
}
