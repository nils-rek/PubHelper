#' A Baseline Table/ Descriptive Statistics function
#'
#' This function creates a baseline table including descriptive statistics and automates work for a "Table 1" in scientific publications.
#' @param data Data needs to be entered that includes relevant variables for the baseline table
#' @param vars Vector of variables to be included as rows for baseline Table
#' @param labels Vector of labels to be set in rows for baseline Table. Needs to be the same length as vars. Defaults to vars values.
#' @param grouping.var Optional variable name if you want to include extra rows (and statistical tests) for baseline variables split by groups.
#' @param round_dec Number of decimal spaced to-be-included in baseline Table. Default is 2
#' @param placeholder The placeholder can be specified with indents descriptive statistics labels (e.g., Mean (SD)) by a given string. The default is "   " (i.e., three spaces).
#' @param welch.test If grouping.var is specified and includes two groups only, The Welch test will be used to test for significant differences between groups on continuous variables. Default is FALSE.
#' #' @param print.vars Should variable names be included in the output (in addition to labels)? Default is FALSE.
#' @keywords Baseline Table
#' @export
#' @author Nils Kappelmann
#' @examples
#' data("airquality")
#' airquality$Period = ifelse(airquality$Month %in% 5:7, "Early Summer", "Late Summer")
#'
#' baselineTable(data = airquality, vars = c("Ozone", "Solar.R", "Wind", "Temp", "Period"))
#' baselineTable(data = airquality, vars = c("Ozone", "Solar.R", "Wind", "Temp", "Period"),
#' labels = c("Ozone (ppb)", "Solar R (lang)", "Wind (mph)", "Temperature (degrees F)", "Period"))
#' baselineTable(data = airquality, vars = c("Ozone", "Solar.R", "Wind", "Temp"),
#' labels = c("Ozone (ppb)", "Solar R (lang)", "Wind (mph)", "Temperature (degrees F)"),
#' grouping.var = "Period", round_dec = 1)


baselineTable <- function(
  data, vars,
  labels = NULL,
  grouping.var = NULL,
  round_dec = 2,
  placeholder = "   ",
  welch.test = FALSE,
  print.vars = FALSE
  )     {

  # Set labels
  if(is.null(labels)){labels = vars}

  # Give error if labels have different length
  if(length(labels) != length(vars))  {
    stop("labels needs to be the same length as vars.")
  }


  # Infer classes and levels of variables
  var.details = data.frame(var = vars,
                           class = NA,
                           levels = NA)
  for(i in vars) {
    var.details[var.details$var == i, "class"] = class(data[, i])
    var.details[var.details$var == i, "levels"] = length(unique(data[!is.na(data[, i]), i]))
  }

  # Define table class
  var.details$output = ifelse(var.details$class %in% c("numeric", "integer") &
                                var.details$levels > 2, "cont", "cat")

  ## Create output table
  output = createOutputTable(data = data, vars = vars, labels = labels,
                             var.details = var.details, round_dec = round_dec,
                             placeholder = placeholder)

  ## Extend table with grouping variables
  if(!is.null(grouping.var)) {
    output = addGroupInfoToTable(data = data, output = output,
                                 vars = vars, grouping.var = grouping.var,
                                 placeholder = placeholder,  welch.test = welch.test)
  }

  ## Remove temporary variables
  output[, c("output.type", "levels")] = NULL

  ## Remove vars if indicated
  if(print.vars == FALSE) {output$vars = NULL}

  ## Return output
  return(output)

}
