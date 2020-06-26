#' A helper function for baselineTable to create the general output table
#'
#' This function is called within baselineTable and creates additional rows for each level specified in grouping.var. It also uses standard statistical tests to compare descriptive statistics between groups.
#' @param ... It takes arguments from baselineTable
#' @param var.details This additional argument specifies nature of variables (i.e., continuous or categorical). For detailed documentation, see the original code from baselineTable.
#' @keywords Baseline Table
#' @author Nils Kappelmann
#' @export
#' @examples
#' createOutputTable()

createOutputTable <- function(
  data,
  vars,
  labels,
  round_dec,
  placeholder,
  var.details
  )   {

  ## Create empty table
  output = data.frame(vars = character(),
                      levels = character(),
                      output.type = character(),
                      description = character(),
                      statistic = character())

  ## Get nrow
  nrow_data = nrow(data)

  for(i in 1:length(vars))    {

    ## Get new row numbers
    output.type = var.details[i, "output"]
    newrows_start = nrow(output) + 1
    newrows_end = newrows_start + ifelse(output.type == "cont", 3,
                                         var.details[i, "levels"] + 1)
    newrows = newrows_start:newrows_end

    ## Get unique values if categorical output.type
    if(output.type == "cat")      {
      output.values = unique(data[, vars[i]])
      output.values = sort(output.values[!is.na(output.values)])
    }

    ## Create new empty rows
    output[newrows,] = NA

    ## Set vars, levels, output.type & labels
    output[newrows, "vars"] = vars[i]
    output[newrows, "output.type"] = output.type
    output[newrows_start, "description"] = labels[i]
    output[newrows_end, "description"] = paste0(placeholder, "N Missing (%)")

    ## Write differently for continuous and categorical variables
    if(output.type == "cont")     {
      # Set descriptions
      output[newrows[2:3], "description"] = paste0(placeholder, c("Mean (SD)",
                                                                  "Median (IQR)"))

      # Fill Mean (SD)
      output[newrows[2], "statistic"] =
        paste0(round(mean(data[,vars[i]], na.rm = TRUE), round_dec), " (",
               round(sd(data[, vars[i]], na.rm = TRUE), round_dec), ")")

      # Fill Median (IQR)
      output[newrows[3], "statistic"] =
        paste0(round(median(data[,vars[i]], na.rm = TRUE), round_dec), " (",
               round(summary(data[, vars[i]])[2], round_dec), "-",
               round(summary(data[, vars[i]])[5], round_dec), ")")

    } else      {

      # Set descriptions
      output[newrows[2:(length(newrows) - 1)], "description"] =
        as.character(output.values)

      # Fill N (%)
      for(j in output.values) {
        output[output$description == j, "statistic"] =
          paste0(sum(data[!is.na(data[, vars[i]]), vars[i]] == j), " (",
                 round(sum(data[!is.na(data[, vars[i]]), vars[i]] == j) /
                         nrow_data * 100, round_dec),
                 "%)")
      }

      # Save output.value as levels and add zeroes to output.value descriptions
      output[newrows[2:(length(newrows) - 1)], "levels"] =
        output[newrows[2:(length(newrows) - 1)], "description"]
      output[newrows[2:(length(newrows) - 1)], "description"] =
        paste0(placeholder, output.values)
    }

    ## Set missing N (%)
    output[newrows_end, "statistic"] =
      paste0(sum(is.na(data[, vars[i]])), " (",
             round(sum(is.na(data[, vars[i]])) / nrow_data * 100, round_dec), "%)")

  }

  ## Return output
  return(output)

}
