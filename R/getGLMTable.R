#' Get a Table of GLM results
#'
#' This function creates a table including output from common GLM models. formatGLMtable offers further formatting for direct inclusion in scientific publications
#' @param model Data needs to be entered that includes relevant variables for the baseline table
#' @param intercept Should intercepts be included in the output? Default is TRUE.
#' @param conf.int Should confidence intervals be added to the output? Default is TRUE.
#' @param conf.level If confidence intervals are added, what is the confidence level? Default is 0.95.
#' @param exclude.covariates Specify covariates that should be excluded from the output.
#' @param fit.indices Should all fit indices from broom::glance be included? Default is FALSE.
#' @param polr.assumptioncheck Only if ordinal logistic regression is used. This calculates a multinomial model using the nnet package with the same formula as the ordinal logistic regression model. If included, a p-value for the proportionality assumption will be included in the output under column name prop.test. Note that this code only works if variables aren't converted inside the fomula (i.e., factor(Y) ~ X does not work).
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
  conf.int = TRUE,
  conf.level = 0.95,
  exclude.covariates = NULL,
  fit.indices = FALSE,
  polr.assumptioncheck = FALSE
) {

  # Load required broom package
  require("broom")

  # Check if model was specified
  if(is.null(model))  {stop("Model must be specified.")}

  # Get GLM class
  glm_class = class(model)

  ## Call correct formatting function depending on glm_class
  if(identical(glm_class, "lm")) {output = format_lm(model = model)}
  else if(identical(glm_class, c("glm", "lm"))) {output = format_loglm(model = model)}
  else if(identical(glm_class, "polr")) {
    output = format_polr(model = model, mlm.model = mlm.model)
    } else  {stop("GLMTable function not yet defined for model class.")}


  ## Exclude intercept if indicated
  if(intercept == FALSE)  {output = output[output$term != "(Intercept)",]}

  ## Return output
  return(output)

}



# format_lm--------------------------
format_lm = function(
  model = model
) {

  ## Extract model output
  output = broom::tidy(model, conf.int = conf.int, conf.level = conf.level)

  ## Exclude covariates
  output = excludeCovariates(output = output, exclude.covariates = exclude.covariates)

  ## Exctract fit indices
  glance.model = glance(model)

  # Delete non-required fit indices
  if(fit.indices == FALSE)  {
    glance.model = glance.model[, c("r.squared", "adj.r.squared", "nobs")]
  }

  # Add fit indices to output
  if(nrow(output) > 1)  {
    for(i in 1:(nrow(output) - 1))  {
      glance.model = rbind(rep(NA, ncol(glance.model)), glance.model)
      }
  }
  output[, colnames(glance.model)] = glance.model

  ## Return output
  return(output)

}



# format_loglm-----------------------

format_loglm = function(
  model = model
) {

  ## Extract model output
  output = broom::tidy(model, conf.int = conf.int, conf.level = conf.level)

  ## Add OR and convert CI
  output$OR = exp(output$estimate)
  if(conf.int == TRUE)  {
    temp.conf.int = exp(output[, c("conf.low", "conf.high")])
    output[, c("conf.low", "conf.high")] = NULL
    output[, c("conf.low", "conf.high")] = temp.conf.int
    rm(temp.conf.int)
  }

  ## Exclude covariates
  output = excludeCovariates(output = output, exclude.covariates = exclude.covariates)


  ## Exctract fit indices
  glance.model = glance(model)

  # Delete non-required fit indices
  if(fit.indices == FALSE)  {
    glance.model = glance.model[, "nobs"]
  }

  # Add fit indices to output
  if(nrow(output) > 1)  {
    for(i in 1:(nrow(output) - 1))  {
      glance.model = rbind(rep(NA, ncol(glance.model)), glance.model)
    }
  }
  output[, colnames(glance.model)] = glance.model


  ## Return output
  return(output)

}




# format_polr------------------------

format_polr = function(
  model = model,
  polr.assumptioncheck = polr.assumptioncheck
) {

  ## Extract model output
  output = broom::tidy(model, conf.int = conf.int, conf.level = conf.level)

  ## Delete intercepts and coef.type column
  output = output[output$coef.type == "coefficient",]
  output$coef.type = NULL

  ## Add OR and convert CI
  output$OR = exp(output$estimate)
  if(conf.int == TRUE)  {
    temp.conf.int = exp(output[, c("conf.low", "conf.high")])
    output[, c("conf.low", "conf.high")] = NULL
    output[, c("conf.low", "conf.high")] = temp.conf.int
    rm(temp.conf.int)
  }

  ## Exclude covariates
  output = excludeCovariates(output = output, exclude.covariates = exclude.covariates)


  ## Exctract fit indices
  glance.model = glance(model)

  # Delete non-required fit indices
  if(fit.indices == FALSE)  {
    glance.model = glance.model[, "nobs"]
  }

  # Add fit indices to output
  if(nrow(output) > 1)  {
    for(i in 1:(nrow(output) - 1))  {
      glance.model = rbind(rep(NA, ncol(glance.model)), glance.model)
    }
  }
  output[, colnames(glance.model)] = glance.model


  ## Run mlm assumption if polr.assumptioncheck = TRUE
  if(polr.assumptioncheck == TRUE)  {
    # Get mlm data and change colnames
    mlm.dat = model$model

    mlm.model = nnet::multinom(formula = deparse(formula(model)), data = model$model,
                               Hess = "Hessian" %in% names(model))

    M1 = logLik(model)
    M2 = logLik(mlm.model)

    G = -2 * (M1[1] - M2[1])

    output[nrow(output), "prop.test"] = pchisq(G, 3, lower.tail = FALSE)

    ## Remove temporary variables
    rm(M1); rm(M2); rm(G)
  } else  {output[nrow(output), "prop.test"] = NA}

  ## Return output
  return(output)

}



# format_lm--------------------------

# excludeCovariates------------------

excludeCovariates = function(
  output = output,
  exclude.covariates = exclude.covariates
  )  {
  output = output[!grepl(paste(exclude.covariates, collapse = "|"), output$term),]
  return(output)
}


