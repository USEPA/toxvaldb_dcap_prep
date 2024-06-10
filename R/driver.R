#-------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @export
#' @title driver
#' @description Run all of the calculations to go from database export to calculation of final BMDh values
#' @return None
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname driver
#-------------------------------------------------------------------------------
driver <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date(),user="user",password="password") {
  printCurrentFunction()
  setDBConn(user=user, password=password)
  export.for.bmdh(toxval.db)
  filter.for.bmdh(toxval.db,sys.date)
  filter.for.lel(toxval.db,sys.date)
  filter.for.multi.noel(toxval.db,sys.date)
  filter.summary(toxval.db,sys.date,do.load=T)
  study_group.multichem(toxval.db,sys.date)
  dcap.counts(toxval.db,sys.date)
  studies.per.chemical(toxval.db,sys.date)
  toxvaldb.statplots(to.file=T,toxval.db,sys.date)
  bmdh.per.study(toxval.db,sys.date)
  bmdh.per.chemical(toxval.db,sys.date)
  bmdh.percentile.plot(T,toxval.db,sys.date,minstudies=3,cutoff.logsd=2)
  bmdh.aurisano.check.plot(T,toxval.db,sys.date)
}
