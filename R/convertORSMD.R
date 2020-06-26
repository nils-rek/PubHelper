#' Conversion between Odds Ratios (OR) and Standardised Mean Differences (SMD)
#'
#' This function converts ORs to SMDs or vice versa using the formula SMD=log(OR)/1.81, which was proposed by Chinn et al. (2000).
#' @param input Input needs to be a value/vector of ORs or SMDs. Type of metric is defined in the initialMetric argument.
#' @param initialMetric Initial metric provided to input argument needs to be defined as "OR" or "SMD". Default is "OR".
#' @keywords Effect size conversion; SMD, OR
#' @export
#' @author Nils Kappelmann
#' @references Chinn S. A simple method for converting an odds ratio to effect size for use in meta-analysis. Stat Med 2000; 19: 3127–3131.
#' @examples
#' convertORSMD(input = 1.2, initialMetric = "OR")
#' convertORSMD(input = -0.2, initialMetric = "SMD")

convertORSMD <- function(input, initialMetric = "OR")  {
  if(initialMetric == "OR")     {

    output = log(input) / 1.81
    return(output)

  } else if(initialMetric == "SMD")   {

    output = exp(input * 1.81)
    return(output)

  } else{

    stop("Please set initialMetric to OR or SMD for this function to execute properly.")

  }
}
