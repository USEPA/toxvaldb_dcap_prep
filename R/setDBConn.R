#--------------------------------------------------------------------------------------
#' set SQL connection to the database
#' @param server SQL server on which relevant database lives
#' @param user SQL username to access database
#' @param password SQL password corresponding to username
#--------------------------------------------------------------------------------------
setDBConn <- function(server="ccte-mysql-res.epa.gov",user="rjudson",password=NA,port=-1) {
  toxvaldbBMDh::printCurrentFunction()
  DB.SERVER <<- server
  DB.USER <<- user
  DB.PASSWORD <<- password
  DB.PORT <<- port
}











