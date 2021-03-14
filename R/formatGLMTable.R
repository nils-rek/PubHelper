#' Formatting GLM results for table export
#'
#' This function creates a formatted table including output from common GLM models.
#' @param model Data needs to be entered that includes relevant variables for the baseline table
#' @param round_dec Number of decimal spaces to-be-included in baseline Table. Default is 2
#' @param lm.ci Indicate if 95% confidence interval from LM should be included in output. Default is FALSE.
#' @param ... Other arguments from getGLMTable
#' @keywords GLM; table
#' @export
#' @author Nils Kappelmann
#' @examples
#' data(airquality)
#' model = lm(Temp ~ Wind, data = airquality)
#' formatGLMTable(model = model)


formatGLMTable = function(
  model = NULL,
  round_dec = 2,
  lm.ci = FALSE,
  ...
) {

  # Get GLM class
  glm_class = class(model)

  # Run getGLMTable to get output data.frame
  output = getGLMTable(model = model, ...)

  ## Format P-value
  output$p.value = ifelse(output$p.value < 0.001, "<0.001",
                          as.character(round(output$p.value, 3)))

  ## Return output depending on glm_class
  if(identical(glm_class, "lm")) {

    output = with(output, data.frame(
      Term = term,
      Estimate.SE = paste0(round(estimate, round_dec), " (",
                               round(std.error, round_dec), ")"),
      CI = paste0(round(conf.low, round_dec), "-", round(conf.high, round_dec)),
      T.Value = round(statistic, round_dec),
      P = p.value,
      R2 = round(r.squared, round_dec),
      R2.adj = round(adj.r.squared, round_dec),
      N = nobs
    ))

    ## Delete CI if indicated
    if(lm.ci == FALSE)  {output$CI = NULL}

    ## Return lm output
    return(output)

  } else if(identical(glm_class, c("glm", "lm"))) {

    output = with(output, data.frame(
      Term = term,
      Estimate.SE = paste0(round(estimate, round_dec), " (",
                           round(std.error, round_dec), ")"),
      OR.CI = paste0(round(OR, round_dec), " (",
                     round(conf.low, round_dec), "-",
                     round(conf.high, round_dec), ")"),
      Z.Value = round(statistic, round_dec),
      P = p.value
    ))

    ## Set Intercept OR to "-"
    output[output$Term == "(Intercept)", "OR.CI"] = "-"

    ## Return lm output
    return(output)

  } else if(identical(glm_class, "polr")) {

    output = with(output, data.frame(
      Term = term,
      Estimate.SE = paste0(round(estimate, round_dec), " (",
                           round(std.error, round_dec), ")"),
      OR.CI = paste0(round(OR, round_dec), " (",
                     round(conf.low, round_dec), "-",
                     round(conf.high, round_dec), ")"),
      T.Value = round(statistic, round_dec),
      P = p.value,
      Proportionality.Test = ifelse(round(prop.test, 3) == 0, "<0.001",
                                    as.character(round(prop.test, 3)))
    ))

    ## Set Intercept OR to "-"
    output[output$Term == "(Intercept)", "OR.CI"] = "-"

    ## Return lm output
    return(output)

  }


}

