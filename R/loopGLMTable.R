#' Loop over getGLMTable or formatGLMTable IN PROGRESS!
#'
#' This function is a wrapper that loops over getGLMTable or formatGLMTable to create a single output data frame
#' @param data Data.frame including model variables
#' @param x Vector of predictor variables
#' @param y Vector of outcome variables
#' @param covars Vector of covariates
#' @param ... Other arguments from PubHelper
#' @keywords GLM; table
#' @export
#' @author Nils Kappelmann
#' @examples
#' data(airquality)
#' model = lm(Temp ~ Wind, data = airquality)
#' getGLMTable(model = model)

loopGLMTable(
  data = NULL,
  x = NULL,
  y = NULL,
  covars = NULL,
  ...
) {



}
