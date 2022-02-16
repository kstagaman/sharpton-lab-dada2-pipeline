#' Add to PATH
#'
#' This function adds a directory to the system PATH variable
#' @param dirs Character or character vector of directories to be added to the PATH variable.
#' @seealso \code{\link{Sys.getenv}}, \code{\link{Sys.setenv}}
#' @export
#' @examples
#' add2PATH("~/src/mothur")

add2PATH <- function(dirs) {
  base.path <- Sys.getenv("PATH")
  Sys.setenv(
    PATH = paste0(paste(dirs, collapse = ":"), ":", base.path)
  )
}
