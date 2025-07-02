#' @title printCurrentFunction
#' @description Print the name of the current function to assist with logging and debugging.
#' @param comment.string An optional string to be printed
#' @return None, console output generated.
#' @seealso
#'  \code{\link[utils]{flush.console}}
#' @rdname printCurrentFunction
#' @export
#' @importFrom utils flush.console
printCurrentFunction <- function(comment.string=NA) {
  cat("=========================================\n")
  curcall <- sys.call(sys.parent(n=1))[[1]]
  cat(curcall,"\n")
  if(!is.null(comment.string) && !is.na(comment.string))	cat(comment.string,"(", format(Sys.time(), usetz = TRUE),")\n")
  cat("=========================================\n")
  utils::flush.console()
}
