#' Get a Table of GLM results
#'
#' This function creates a table including output from common GLM models. formatGLMtable offers further formatting for direct inclusion in scientific publications
#' @param model Data needs to be entered that includes relevant variables for the baseline table
#' @param intercept Should intercepts be included in the output? Default is TRUE.
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
  if(identical(glm_class, "lm")) {
    output = format_lm(lm.object = model,
                       covars.to.exclude = exclude.covariates,
                       fit.stats = fit.indices)
  } else if(identical(glm_class, c("glm", "lm"))) {
    output = format_loglm(loglm.object = model,
                          covars.to.exclude = exclude.covariates,
                          fit.stats = fit.indices)
  } else if(identical(glm_class, "polr")) {
    output = format_polr(polr.object = model,
                         covars.to.exclude = exclude.covariates,
                         fit.stats = fit.indices)
  } else  {stop("GLMTable function not yet defined for model class.")}


  ## Exclude intercept if indicated
  if(intercept == FALSE)  {output = output[output$term != "(Intercept)",]}

  ## Return output
  return(output)

}



# format_lm--------------------------
format_lm = function(
  lm.object = model,
  covars.to.exclude = exclude.covariates,
  fit.stats = fit.indices
) {

  ## Extract model output
  output = broom::tidy(lm.object, conf.int = TRUE, conf.level = 0.95)

  ## Exclude covariates
  if(!is.null(covars.to.exclude))  {
    output = excludeCovariates(output, covars.to.exclude)
  }

  ## Exctract fit indices
  glance.model = glance(lm.object)

  # Delete non-required fit indices
  if(fit.stats == FALSE)  {
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
  loglm.object = model,
  covars.to.exclude = exclude.covariates,
  fit.stats = fit.indices
) {

  ## Extract model output
  output = broom::tidy(loglm.object, conf.int = TRUE, conf.level = 0.95)

  ## Add OR and convert CI
  output$OR = exp(output$estimate)

  temp.conf.int = exp(output[, c("conf.low", "conf.high")])
  output[, c("conf.low", "conf.high")] = NULL
  output[, c("conf.low", "conf.high")] = temp.conf.int
  rm(temp.conf.int)


  ## Exclude covariates
  if(!is.null(covars.to.exclude))  {
    output = excludeCovariates(output, covars.to.exclude)
  }


  ## Exctract fit indices
  glance.model = glance(loglm.object)

  # Delete non-required fit indices
  if(fit.stats == FALSE)  {
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
  polr.object = model,
  covars.to.exclude = exclude.covariates,
  fit.stats = fit.indices,
  polr.check = polr.assumptioncheck
) {

  ## Extract model output
  output = broom::tidy(polr.object, conf.int = TRUE, conf.level = 0.95)

  ## Delete intercepts and coef.type column
  output = output[output$coef.type == "coefficient",]
  output$coef.type = NULL

  ## Add OR and convert CI
  output$OR = exp(output$estimate)

  temp.conf.int = exp(output[, c("conf.low", "conf.high")])
  output[, c("conf.low", "conf.high")] = NULL
  output[, c("conf.low", "conf.high")] = temp.conf.int
  rm(temp.conf.int)

  ## Exclude covariates
  if(!is.null(covars.to.exclude))  {
    output = excludeCovariates(output, covars.to.exclude)
  }

  ## Exctract fit indices
  glance.model = glance(polr.object)

  # Delete non-required fit indices
  if(fit.stats == FALSE)  {
    glance.model = glance.model[, "nobs"]
  }

  # Add fit indices to output
  if(nrow(output) > 1)  {
    for(i in 1:(nrow(output) - 1))  {
      glance.model = rbind(rep(NA, ncol(glance.model)), glance.model)
    }
  }
  output[, colnames(glance.model)] = glance.model


  ## Run mlm assumption if polr.check = TRUE
  if(polr.check == TRUE)  {
    # Get mlm data and change colnames
    mlm.dat = polr.object$model

    mlm.model = nnet::multinom(formula = deparse(formula(polr.object)),
                               data = polr.object$model,
                               Hess = "Hessian" %in% names(polr.object))

    M1 = logLik(polr.object)
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
  x,
  exclude.from.x
)  {

  x = x[!grepl(paste(exclude.from.x, collapse = "|"), x$term),]

  return(x)
}


