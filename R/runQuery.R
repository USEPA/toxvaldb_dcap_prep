#--------------------------------------------------------------------------------------
#' @#' Runs a database query and returns a result set
#'
#' @param query a properly formatted SQL query as a string
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @export 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[RMySQL]{character(0)}}, \code{\link[RMySQL]{MySQLDriver-class}}
#'  \code{\link[utils]{flush.console}}
#' @rdname runQuery
#' @importFrom RMySQL dbConnect MySQL dbSendQuery dbFetch dbHasCompleted dbClearResult dbDisconnect
#' @importFrom utils flush.console
#------------------------------------------Q--------------------------------------------
runQuery <- function(query,db,do.halt=T,verbose=F) {

  if(!exists("DB.SERVER")) {
    cat("DB.SERVER not defined\n")
    return(NULL)
  }
  if(!exists("DB.USER")) {
    cat("DB.USER not defined\n")
    return(NULL)
  }
  if(!exists("DB.PASSWORD")) {
    cat("DB.PASSWORD not defined\n")
    return(NULL)
  }
  if(verbose) {
    printCurrentFunction()
    cat("query: ",query,"\n")
    cat("db: ",db,"\n")
  }
  tryCatch({
    if(DB.PORT<0) con <- RMySQL::dbConnect(drv=RMySQL::MySQL(),user=DB.USER,password=DB.PASSWORD,host=DB.SERVER,dbname=db)
    else con <- RMySQL::dbConnect(drv=RMySQL::MySQL(),user=DB.USER,password=DB.PASSWORD,host=DB.SERVER,port=DB.PORT,dbname=db)
    rs <- suppressWarnings(RMySQL::dbSendQuery(con, query))
    d1 <- RMySQL::dbFetch(rs, n = -1)
    if(verbose) {
      print(d1)
      utils::flush.console()
    }
    RMySQL::dbHasCompleted(rs)
    RMySQL::dbClearResult(rs)
    RMySQL::dbDisconnect(con)
    return(d1)
  }, warning = function(w) {
    cat("WARNING:",query,"\n")
    RMySQL::dbDisconnect(con)
    if(do.halt) browser()
    return(NULL)
  }, error = function(e) {
    cat("ERROR:",query,"\n")
    print(e)
    RMySQL::dbDisconnect(con)
    if(do.halt) browser()
    return(NULL)
  })
}


