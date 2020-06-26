#' Calculation of Pooled Mean
#'
#' This function computes the pooled mean from two samples/groups.
#' @param n1 Sample size of sample/group 1.
#' @param n2 Sample size of sample/group 2.
#' @param mean1 Mean of sample/group 1.
#' @param mean2 Mean of sample/group 2.
#' @keywords mean
#' @export
#' @author Nils Kappelmann
#' @examples
#' meanpooled(n1 = 30, n2 = 20, mean1 = 15, mean2 = 20)

meanpooled <- function(n1 = NA, n2 = NA, mean1 = NA, mean2 = NA)  {

  # Check input arguments
  if(sum(is.na(c(n1, n2, mean1, mean2))) >= 1)  {
    stop("Please define all input arguments for computation of the pooled mean")
  }

  # Pooled Mean formula
  mean_pooled <- (n1 * mean1 + n2 * mean2) / (n1 + n2)

  # Return output
  return(mean_pooled)
}
