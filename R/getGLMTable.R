#' Get a Table of GLM results
#'
#' This function creates a table including output from common GLM models. formatGLMtable offers further formatting for direct inclusion in scientific publications
#' @param model Data needs to be entered that includes relevant variables for the baseline table
#' @param intercept Should intercepts be included in the output? Default is TRUE
#' @param exclude.covariates Specify covariates that should be excluded from the output.
#' @keywords GLM; table
#' @export
#' @author Nils Kappelmann
#' @examples
#' data(airquality)
#' model = lm(Temp ~ Wind, data = airquality)
#' getGLMTable(model = model)


getGLMTable = function(
  model = NULL,
  intercept = TRUE,
  exclude.covariates = NULL
) {

  # Check if model was specified
  if(is.null(model))  {stop("Model must be specified.")}

  # Get GLM class
  glm_class = class(model)

  ## Call correct formatting function depending on glm_class
  if(identical(glm_class, "lm")) {output = format_lm(model = model)}
  else if(identical(glm_class, c("glm", "lm"))) {output = format_loglm(model = model)}
  else  {stop("GLMTable function not yet defined for model class.")}


  ## Exclude intercept if indicated
  if(intercept == FALSE)  {output = output[output$Predictor != "(Intercept)",]}

  ## Exclude covariates if indicated
  if(!is.null(exclude.covariates))  {

    ## Save model fit statistics, so these won't be deleted
    if(identical(glm_class, "lm") &
       sum(!is.na(output[output$Predictor %in% exclude.covariates, "r.squared"])) == 1) {
      lm_model.fit = output[nrow(output), c("r.squared", "adj.r.squared")]

    }

    ## Exclude covariate rows
    output = output[output$Predictor %in% exclude.covariates == FALSE,]

    ## Include fit statistics again if necessary
    if(exists("lm_model.fit"))  {
      output[nrow(output), c("r.squared", "adj.r.squared")] = lm_model.fit
    }
  }


  ## Return output
  return(output)

}



# format_lm--------------------------
format_lm = function(
  model = model
) {

  ## Create data.frame with model output
  output = data.frame(
    Predictor = names(coef(model)),
    Estimate = coef(model),
    SE = sqrt(diag(vcov(model))),
    tval = summary(model)[["coefficients"]][, "t value"],
    pval = summary(model)[["coefficients"]][, "Pr(>|t|)"],
    r.squared = c(rep(NA, length(coef(model)) - 1), summary(model)$r.squared),
    adj.r.squared = c(rep(NA, length(coef(model)) - 1), summary(model)$adj.r.squared)
  )

  ## Calculate 95% CI
  output$ci.lb = with(output, Estimate - 1.96* SE)
  output$ci.ub = with(output, Estimate + 1.96* SE)

  ## Return output
  return(output)

}



# format_loglm-----------------------

format_loglm = function(
  model = model
) {

  ## Create data.frame with model output
  output = data.frame(
    Predictor = names(coef(model)),
    OR = exp(coef(model)),
    Estimate = coef(model),
    SE = sqrt(diag(vcov(model))),
    zval = summary(model)[["coefficients"]][, "z value"],
    pval = summary(model)[["coefficients"]][, "Pr(>|z|)"]
  )

  ## Calculate 95% CI
  output$ci.lb = exp(with(output, Estimate - 1.96* SE))
  output$ci.ub = exp(with(output, Estimate + 1.96* SE))

  ## Return output
  return(output)

}
