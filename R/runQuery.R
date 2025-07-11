#' @title runQuery
#' @description Runs a database query and returns a result set.
#' @param query A properly formatted SQL query as a string.
#' @param db The name of the database.
#' @param do.halt If TRUE, halt on errors or warnings, default TRUE.
#' @param verbose If TRUE, print diagnostic information, default FALSE.
#' @param db.type String of what kind of database connection to use, default "mysql. If "sqlite", workflow with use .Renv defined "sqlite_file" file path.
#' @return Dataframe of query results.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  runQuery(query = "SELECT * FROM toxval LIMIT 1", db = "res_toxval_v96_1")
#'  }
#' }
#' @seealso
#'  \code{\link[RMySQL]{character(0)}}, \code{\link[RMySQL]{MySQLDriver-class}}
#'  \code{\link[utils]{flush.console}}
#' @importFrom RMySQL MySQL
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbHasCompleted dbClearResult dbDisconnect
#' @importFrom utils flush.console
#' @export
#' @rdname runQuery
runQuery <- function(query=NULL, db, do.halt=TRUE, verbose=FALSE, db.type = "mysql") {

  if(is.null(query)){
    cat("No query provided...\n")
    return(NULL)
  }

  # Check environment variables for database credentials are set
  credentials = c("db_user", "db_pass", "db_server", "db_port")
  for(cred in credentials){
    if(Sys.getenv(cred) == ""){
      cat(paste0("'", cred, "' environment variable not defined\n"))
      return(NULL)
    }
  }

  if(verbose) {
    printCurrentFunction()
    cat("query: ",query,"\n")
    cat("db: ",db,"\n")
  }

  tryCatch({
    if(db.type == "mysql"){
      message("Running using MySQL database connection...")
      con <- DBI::dbConnect(drv=RMySQL::MySQL(),
                            user=Sys.getenv("db_user"),
                            password=Sys.getenv("db_pass"),
                            host=Sys.getenv("db_server"),
                            dbname=db,
                            port=as.numeric(Sys.getenv("db_port"))
      )
    } else if(db.type == "sqlite"){
      message("Running using SQLite file...")
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            dbname = paste0(Sys.getenv("sqlite_file")))
    }

    rs <- suppressWarnings(DBI::dbSendQuery(con, query))
    d1 <- DBI::dbFetch(rs, n = -1)
    if(verbose) {
      print(d1)
      utils::flush.console()
    }
    DBI::dbHasCompleted(rs)
    DBI::dbClearResult(rs)
    DBI::dbDisconnect(con)
    return(d1)
  }, warning = function(w) {
    cat("WARNING:",query,"\n")
    DBI::dbDisconnect(con)
    if(do.halt) browser()
    return(NULL)
  }, error = function(e) {
    #cat("ERROR:",query,"\n")
    cat("Error message: ",paste0(e, collapse=" | "), "\n")
    DBI::dbDisconnect(con)
    if(do.halt) browser()
    return(NULL)
  })
}


