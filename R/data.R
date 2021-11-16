#' RNA modification detection result output from xPore.
#'
#' @source (Pratanwanich et al., 2021)
#'
#' @format{
#' Dataframe with 129 rows and 18 variables
#' \describe{
#' \item{id}{Transcript or gene id}
#' \item{position}{Transcript or gene position}
#' \item{kmer}{5-mer where modified base sits in the middle if modified.}
#' \item{diff_mod_rate_KO_vs_WT}{Differential modification rate between condition1 and condition2 (modification rate of condition1 - modification rate of condition2).}
#' \item{z_score_KO_vs_WT}{Z score obtained from z-test of the differential modification rate.}
#' \item{pval_KO_vs_WT}{Significance level from z-test of the differential modification rate.}
#' \item{mod_rate_KO.rep1}{Modification rate of a replicate 1 of KO condition.}
#' \item{mod_rate_WT.rep1}{Modification rate of a replicate 1 of KO condition.}
#' \item{coverage_KO.rep1}{Read coverage of genes in replicate 1 of KO condition.}
#' \item{coverage_WT.rep1}{Read coverage of genes in replicate 1 of WT condition.}
#' \item{mu_unmod}{Inferred mean of the unmodified RNAs distribution.}
#' \item{mu_mod}{Inferred mean of the modified RNAs distribution.}
#' \item{sigma2_unmod}{Inferred sigma^2 of the unmodified RNAs distribution.}
#' \item{sigma2_mod}{Inferred sigma^2 of the modified RNAs distribution.}
#' \item{conf_mu_unmod}{Confidence level of mu_unmod compared to the unmodified reference signal.}
#' \item{conf_mu_mod}{Confidence level of mu_unmod compared to the unmodified reference signal.}
#' \item{mod_assignment}{Lower if mu_mod < mu_unmod and higher if mu_mod > mu_unmod}
#' \item{t.test}{P value of one-tailed t-test}
#' }
#' }
#'
#' @references
#' Pratanwanich, P. N., Yao, F., Chen, Y., Koh, C. W. Q., Wan, Y. K., Hendra, C., Poon, P., Goh,
#' Y. T., Yap, P. M. L., Chooi, J. Y., Chng, W. J., Ng, S. B., Thiery, A., Goh, W. S. S., & Göke, J. (2021).
#' Identification of differential RNA modifications from nanopore direct RNA sequencing with xPore.
#' Nature Biotechnology,39(11), 1394–1402. https://doi.org/10.1038/s41587-021-00949-w
#'
#' @examples
#' \dontrun{
#'  RnaModificationResults
#' }
"RnaModificationResults"
