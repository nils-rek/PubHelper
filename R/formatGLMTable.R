#' Formatting GLM results for table export
#'
#' This function creates a baseline table including descriptive statistics and automates work for a "Table 1" in scientific publications.
#' @param model Data needs to be entered that includes relevant variables for the baseline table
#' @param intercept Should intercepts be included in the output? Default is TRUE
#' @param exclude.covariates Specify covariates that should be excluded from the output.
#' @param round_dec Number of decimal spaces to-be-included in baseline Table. Default is 2
#' @param lm.ci Indicate if 95% confidence interval from LM should be included in output. Default is FALSE.
#' @keywords GLM; table
#' @export
#' @author Nils Kappelmann
#' @examples
#' data(airquality)
#' model = lm(Temp ~ Wind, data = airquality)
#' formatGLMTable(model = model)


formatGLMTable = function(
  model = NULL,
  intercept = TRUE,
  exclude.covariates = NULL,
  round_dec = 2,
  lm.ci = FALSE
) {

  # Check if model was specified
  if(is.null(model))  {stop("Model must be specified.")}

  # Get GLM class
  glm_class = class(model)

  ## Call correct formatting function depending on glm_class
  if(identical(glm_class, "lm")) {output = format_lm(model = model)}
  else if(identical(glm_class, c("glm", "lm"))) {output = format_loglm(model = model)}
  else  {stop("GLM formatting function not yet defined for model class.")}


  ## Format P-value
  output$pval = ifelse(output$pval < 0.001, "<0.001",
                       as.character(round(output$pval, 3)))

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

  ## Return output depending on glm_class
  if(identical(glm_class, "lm")) {

    output = with(output, data.frame(
      Predictor = Predictor,
      Estimate.SE = paste0(round(Estimate, round_dec), " (",
                               round(SE, round_dec), ")"),
      CI = paste0(round(ci.lb, round_dec), "-", round(ci.ub, round_dec)),
      T.Value = round(tval, round_dec),
      P = pval,
      R2 = round(r.squared, round_dec),
      R2.adj = round(adj.r.squared, round_dec)
    ))

    ## Delete CI if indicated
    if(lm.ci == FALSE)  {output$CI = NULL}

    ## Return lm output
    return(output)

  } else if(identical(glm_class, c("glm", "lm"))) {

    output = with(output, data.frame(
      Predictor = Predictor,
      Estimate.SE = paste0(round(Estimate, round_dec), " (",
                           round(SE, round_dec), ")"),
      OR.CI = paste0(round(OR, round_dec), " (",
                     round(ci.lb, round_dec), "-",
                     round(ci.lb, round_dec), ")"),
      Z.Value = round(zval, round_dec),
      P = pval
    ))

    ## Set Intercept OR to "-"
    output[output$Predictor == "(Intercept)", "OR.CI"] = "-"

    ## Return lm output
    return(output)

  }


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
