#--------------------------------------------------------------------------------------
#' @#' set SQL connection to the database
#' @param server SQL server on which relevant database lives
#' @param user SQL username to access database
#' @param password SQL password corresponding to username
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param port PARAM_DESCRIPTION, Default: -1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname setDBConn
#' @export 
#--------------------------------------------------------------------------------------
setDBConn <- function(server="ccte-mysql-res.epa.gov",user="user",password=NA,port=-1) {
  printCurrentFunction()
  DB.SERVER <<- server
  DB.USER <<- user
  DB.PASSWORD <<- password
  DB.PORT <<- port
}











