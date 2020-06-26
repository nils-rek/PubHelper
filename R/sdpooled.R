#' Calculation of Pooled Standard Deviation (SD)
#'
#' This function computes the pooled SD from two samples/groups.
#' @param n1 Sample size of sample/group 1.
#' @param n2 Sample size of sample/group 2.
#' @param sd1 SD of sample/group 1.
#' @param sd2 SD of sample/group 2.
#' @keywords standard deviation
#' @export
#' @author Nils Kappelmann
#' @examples
#' sdpooled(n1 = 30, n2 = 20, sd1 = 15, sd2 = 20)

sdpooled <- function(n1 = NA, n2 = NA, sd1 = NA, sd2 = NA)  {

  # Check input arguments
  if(sum(is.na(c(n1, n2, sd1, sd2))) >= 1)  {
    stop("Please define all input arguments for computation of the pooled standard deviation")
  }

  # Pooled SD formula
  sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

  # Return output
  return(sd_pooled)
}
