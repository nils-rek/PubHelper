#' mapGLMTables: Loop over regression models and extract results.
#'
#' mapGLMTables loops over regression models (linear, logistic, & ordinal logistic) and returns summarised output from getGLMTable. It is possible to include covariates, which are automatically excluded from the output. However, the full output can be obtained as a more complex output by setting simplify=FALSE. Note that a complex model output is returned if predictor variables include factors with >2 levels.
#' @param data Data.frame including model variables
#' @param x Vector of predictor variables
#' @param y Vector of outcome variables
#' @param z Vector of covariates if covariates are included
#' @param model.type Specify which statistical model to run. Options are "lm" for linear regression, "glm" for logistic regression, and "polr" for ordinal logistic regression
#' @param format.output Should output be formatted for scientific publications? Default is FALSE.
#' @param simplify Should full lm models and GLMTables be excluded from output? Default is TRUE.
#' @keywords GLM; table
#' @export
#' @author Nils Kappelmann
#' @examples
#' data(airquality)
#' mapGLMTables(data = airquality, y = "Ozone", x = c("Solar.R", "Wind"), z = "Temp")

mapGLMTables = function(
  data = NULL,
  y = NULL,
  x = NULL,
  z = NULL,
  model.type = "lm",
  format.output = FALSE,
  simplify = TRUE
) {

  ## Add required packages
  require("tidyverse")

  ## Rename data to avoid recursive errors
  d = data

  ## Check if input arguments are present
  if(is.null(d))  {stop("data needs to be specified.")}
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
  if(model.type == "lm")  {
    models = map(output$formula, lm, data = d)
  } else if(model.type == "glm")  {
    models = map(output$formula, glm, data = d, family = "binomial")
  } else if(model.type == "polr") {
    models = map(output$formula, MASS::polr, data = d, Hess = TRUE)
  } else{stop("model.type not defined.")}


  ## Check if any x variables are factors with >2 levels
  if("factor" %in% map_chr(d[,x], class))  {
    x_factor = x[map_chr(d[,x], class) == "factor"]
    factor_with_many_levels = d[,x_factor] %>%
      map_dbl(nlevels) > 2
    factor_with_many_levels = any(factor_with_many_levels) == TRUE
  } else  {factor_with_many_levels = FALSE}


  ## Add results to data
  if(format.output == FALSE)  {
    GLMTables = map_dfr(models, getGLMTable,
                        intercept = FALSE, exclude.covariates = z)
  } else if(format.output == TRUE)  {
    GLMTables = map_dfr(models, formatGLMTable,
                        intercept = FALSE, exclude.covariates = z)
  } else  {stop("format.output must be logical.")}


  # Add results if x includes no factor w/ >2 levels
  if(factor_with_many_levels == FALSE)  {
    output[,colnames(GLMTables)] = GLMTables
  }

  # Add complex model results if simplify = FALSE
  if(simplify == FALSE) {
    output$models = models
    output$GLMTables = map(models, getGLMTable)
    if(factor_with_many_levels == TRUE) {
      message("Note that a factor with >2 levels was present in x.\nTherefore, the output object is more complex and\nindividual model results can be obtained using: $GLMTables[[n]].")
    }
  }

  # Add GLMTables if simplify = TRUE and x includes factor w/ >2 levels
  if(simplify == TRUE & factor_with_many_levels == TRUE)  {
    message("Note that a factor with >2 levels was present in x.\nTherefore, the output object is more complex and\nindividual model results can be obtained using: $GLMTables[[n]].")
    output$GLMTables = map(models, getGLMTable,
                           intercept = FALSE, exclude.covariates = z)
  }


  return(output)

}
