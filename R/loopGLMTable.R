#' Loop over getGLMTable or formatGLMTable IN PROGRESS!
#'
#' This function is a wrapper that loops over getGLMTable or formatGLMTable to create a single output data frame
#' @param data Data.frame including model variables
#' @param x Vector of predictor variables
#' @param y Vector of outcome variables
#' @param z Vector of covariates if covariates are included
#' @param glm_fun Function for glm model
#' @param ... Other arguments for GLM
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
  z = NULL,
  glm_fun = lm,
  ...
) {

  ## Check if input arguments are present
  if(is.null(data))  {stop("data needs to be specified.")}
  if(is.null(x))  {stop("x needs to be specified.")}
  if(is.null(y))  {stop("y needs to be specified.")}

  ## Get grid
  output = expand.grid(y = y, x = x, stringsAsFactors = FALSE)

  ## Get formula
  output$formula = paste0(output$y, "~", output$x)
  if(!is.null(z)) {
    z.comb = paste(z, collapse = "+")
    output$formula = paste(output$formula, z.comb, sep = "+")
  }

  ## Run models
  output$models = map(output$formula, glm_fun, data = data)

  ## Results
  #output$GLMTables = map(models, PubHelper::getGLMTable)

  return(output)

}
