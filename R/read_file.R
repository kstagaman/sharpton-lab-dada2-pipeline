#' Read File
#'
#' An auxilliary function to read in metadata (or other) files in common format types
#' @param file character; a filename in a common format: csv, tsv/txt, xls/xlsx, or rds
#' @param has.header logical; for csv or tsv/txt files, whether the first row is column names. Default is TRUE.
#' @param ... additional arguments passed to read.table, read_xls, or read_xlsx as needed.
#' @seealso \code{\link{read.table}}, \code{\link{readRDS}}, \code{\link{read_xls}}, \code{\link{read_xlsx}}
#' @export

read.file <- function(file, has.header = TRUE, ...) {
  varargs <- rlang::list2(...)
  extension <- tolower(str_extract(file, "\\.[a-z]+$"))
  if (extension %in% paste0(".", c("tsv", "txt", "csv"))) {
    if (is.null(varargs$sep)) { varargs$sep <- ifelse(extension == ".csv", ",", "\t") }
    return(
      read.table(
        file = file,
        sep = varargs$sep,
        header = has.header,
        ...
      )
    )
  } else if (extension == ".xls") {
    return(readxl:read_xls(path =file, ...))
  } else if (extension == ".xlsx") {
    return(readxl:read_xlsx(path = file, ...))
  } else if (extension == ".rds") {
    return(readRDS(file = file))
  } else {
    rlang::abort("Filetype not recognizable from extension (csv/tsv/txt/xls/xlsx/rds)")
  }
}

