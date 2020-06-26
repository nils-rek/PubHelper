#' A helper function for baselineTable to specify additional rows split by Group
#'
#' This function is called within baselineTable and creates additional rows for each level specified in grouping.var. It also uses standard statistical tests to compare descriptive statistics between groups.
#' @param ... Function takes arguments from baselineTable
#' @keywords Baseline Table
#' @export
#' @author Nils Kappelmann
#' @examples
#' addGroupInfoToTable()

addGroupInfoToTable <- function(
  data,
  output,
  vars,
  placeholder,
  round_dec,
  grouping.var,
  welch.test
  )   {

  ## Get levels of new variable
  group.levels = unique(data[, grouping.var])
  group.levels = group.levels[!is.na(group.levels)]

  ## Create additional columns
  output[, c(group.levels, "test")] = NA

  ## Fill new columns in loop
  for(i in vars) {

    # Save output.type
    output.type = output[output$vars == i, "output.type"][1]

    ## Run tests and fill test column depending on output.type
    if(output.type == "cont")   {

      # T-test if 2 levels and ANOVA if more levels
      if(length(group.levels) == 2) {

        # Run t-test
        t.test_results = t.test(data[, i] ~ data[, grouping.var], var.equal = !welch.test)
        t.test_p = ifelse(t.test_results$p.value < 0.001, "<0.001",
                          as.character(round(t.test_results$p.value, 3)))

        # Save
        output[max(which(output$vars == i)), "test"] = t.test_p

      } else   {

        # Run ANOVA
        anova_results = summary(aov(data[, i] ~ data[, grouping.var]))
        anova_p = ifelse(anova_results[[1]][["Pr(>F)"]] < 0.001, "<0.001",
                         as.character(round(anova_results[[1]][["Pr(>F)"]], 3)))

        # Save
        output[max(which(output$vars == i)), "test"] = anova_p

      }
    } else   {

      ## Run chi.square test
      chisq_results = chisq.test(data[, i], data[, grouping.var], correct = TRUE)
      chisq_p = ifelse(chisq_results$p.value < 0.001, "<0.001",
                       as.character(round(chisq_results$p.value, 3)))

      # Save
      output[max(which(output$vars == i)), "test"] = chisq_p

    }

    ## Fill group column
    for(g in group.levels)  {

      nrow_group = nrow(data[data[, grouping.var] == g,])

      if(output.type == "cat")   {

        # Add proportion
        for(j in 2:(length(which(output$vars == i))- 1)) {

          temp_rows = which(output$vars == i)
          temp_cell_no = nrow(data[data[, grouping.var] == g &
                                     data[,i] == output[temp_rows[j], "levels"],])

          output[temp_rows[j], g] =
            paste0(temp_cell_no, " (",
                   round(temp_cell_no / nrow_group * 100, round_dec), "%)")

        }

      } else   {

        # Mean (SD)
        output[output$vars == i & output$description == paste0(placeholder, "Mean (SD)"), g] =
          paste0(round(mean(data[data[,grouping.var] == g, i], na.rm = TRUE), round_dec), " (",
                 round(sd(data[data[,grouping.var] == g, i], na.rm = TRUE), round_dec), ")")

        # Median (IQR)
        output[output$vars == i & output$description == paste0(placeholder, "Median (IQR)"), g] =
          paste0(round(median(data[data[,grouping.var] == g, i], na.rm = TRUE), round_dec),
                 " (", round(summary(data[data[,grouping.var] == g, i])[2], round_dec), "-",
                 round(summary(data[data[,grouping.var] == g, i])[5], round_dec), ")")


      }

      # Add missing
      output[output$vars == i & output$description == paste0(placeholder, "Missing (%)"), g] =
        paste0(sum(is.na(data[data[,grouping.var] == g, i])), " (",
               round(sum(is.na(data[data[,grouping.var] == g, i])) / nrow_group * 100,
                     round_dec), "%)")

    }

  }

  return(output)


}
