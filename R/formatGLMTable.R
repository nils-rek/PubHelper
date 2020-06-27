#' Formatting GLM results for table export
#'
#' This function creates a formatted table including output from common GLM models.
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

  # Run getGLMTable to get output data.frame
  output = getGLMTable(model = model)

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
