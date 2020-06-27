#' exportPubHelpercsv
#'
#' This function is a short pre-specified version of write.table with options indicated for table inclusion in scientific publications.
#' @param object Output data.frame for export as .csv file.
#' @param file Filename (and location).
#' @param sep Separator for csv file. Defaults is ",".
#' @keywords csv; export
#' @export
#' @author Nils Kappelmann
#' @examples
#' data(airquality)
#' model = lm(Temp ~ Wind, data = airquality)
#' outputTable = formatGLMTable(model = model)
#' exportPubHelpercsv(outputTable, file = "PubHelperTest.csv")

exportPubHelpercsv <- function(
  object = NULL,
  file = NULL,
  sep = ","
) {

  ## Check if input is specified
  if(is.null(object)) {stop("object needs to be specified")}
  if(is.null(file)) {stop("file needs to be specified")}

  write.table(object, file = file, quote = FALSE, sep = sep,
              row.names = FALSE, col.names = TRUE, dec = ".",
              na = "")

  cat(paste0("Output written to ", file, "."))

}
