#' Inference of Standard Deviation (SD) from Confidence Interval (CI)
#'
#' This function infers the SD from a given 95% CI.
#' @param n Sample size.
#' @param ci.lb Lower bound of 95% CI.
#' @param ci.ub Upper bound of 95% CI.
#' @keywords standard deviation; confidence interval
#' @export
#' @author Nils Kappelmann
#' @examples
#' SDfromCI(n = 30, ci.lb = -0.1, ci.ub = 0.2)

SDfromCI <- function(n = NA, ci.lb = NA, ci.ub = NA)  {

  # Check input arguments
  if(sum(is.na(c(n, ci.lb, ci.ub))) >= 1)  {
    stop("Please define all input arguments for computation of the standard deviation")
  }

  # Formula for SD inference
  sd <- sqrt(n) * (ci.ub - ci.lb) / 3.92

  # Return output
  return(sd)
}
