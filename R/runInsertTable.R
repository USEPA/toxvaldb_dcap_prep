#--------------------------------------------------------------------------------------
#' @param mat data frame containing the data, with the column names corresponding
#' @param table name of the database table to which data will be inserted
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @title runInsertTable
#' @description Inserts multiple rows into a database table
#' @param get.id PARAM_DESCRIPTION, Default: T
#' @return None; inserts data into DB
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[RMySQL]{character(0)}}, \code{\link[RMySQL]{MySQLDriver-class}}
#' @rdname runInsertTable
#' @export
#' @importFrom RMySQL dbConnect MySQL dbWriteTable dbSendQuery dbFetch dbHasCompleted dbClearResult dbDisconnect
#--------------------------------------------------------------------------------------
runInsertTable <- function(mat,table,db,do.halt=T,verbose=F,get.id=T) {
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
    cat("mat: ",dim(mat),"\n")
    cat("table: ",table,"\n")
    cat("db: ",db,"\n")
  }
  tryCatch({
    #
    # make sure all charactersc are in UTF8
    #
    desc <- runQuery(paste0("desc ",table),db)
    desc <- desc[is.element(desc[,"Field"],names(mat)),]
    for(i in seq_len(dim(desc)[1])) {
      col <- desc[i,"Field"]
      type <- desc[i,"Type"]
      if(grepl("varchar|text", type)) {
        if(verbose) cat("   enc2utf8:",col,"\n")
        x <- as.character(mat[,col])
        x[is.na(x)] <- "-"
        x <- enc2native(x)
        x <- iconv(x,from="latin1",to="UTF-8")
        x <- iconv(x,from="LATIN1",to="UTF-8")
        x <- iconv(x,from="LATIN2",to="UTF-8")
        x <- iconv(x,from="latin-9",to="UTF-8")
        mat[,col] <- enc2utf8(x)
      }
    }
    if(DB.PORT<0) con <- RMySQL::dbConnect(drv=RMySQL::MySQL(),user=DB.USER,password=DB.PASSWORD,host=DB.SERVER,dbname=db)
    else con <- RMySQL::dbConnect(drv=RMySQL::MySQL(),user=DB.USER,password=DB.PASSWORD,host=DB.SERVER,port=DB.PORT,dbname=db)
    ret <- RMySQL::dbWriteTable(con,name=table,value=mat,row.names=F,overwrite=F,append=T)
    if(get.id) {
      rs2 <- RMySQL::dbSendQuery(con, "select LAST_INSERT_ID()")
      d2 <- RMySQL::dbFetch(rs2, n = -1)
      id <- d2[1,1]
      RMySQL::dbHasCompleted(rs2)
      RMySQL::dbClearResult(rs2)
    }
    cat(">>> runInsertTable finished writing ",table,":",dim(mat),"\n")
    RMySQL::dbDisconnect(con)
  }, warning = function(w) {
    cat("WARNING:",table," : [",db,"]\n",sep="")
    RMySQL::dbDisconnect(con)
    if(do.halt) browser()
  }, error = function(e) {
    cat("ERROR:",table," : [",db,"]\n",sep="")
    print(e)
    RMySQL::dbDisconnect(con)
    if(do.halt) browser()
  })
  if(get.id) return(id)
}
