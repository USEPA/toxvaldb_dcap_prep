#-------------------------------------------------------------------------------
#' Run all of the calculations to go from database export to calculation of final BMDh values
#'
#' `driver` Run all of the calculations to go from database export to calculation of final BMDh values
#'
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @param user The username for the MySQL database. The database instance is
#' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @export
#-------------------------------------------------------------------------------
driver <- function(toxval.db="res_toxval_v95",sys.date="2024-05-20",user="cweiteka",password="!QAZxsw2asdf") {
  toxvaldbBMDh::printCurrentFunction()
  toxvaldbBMDh::export.for.bmdh(toxval.db,user,password)
  toxvaldbBMDh::filter.for.bmdh(toxval.db,sys.date)
  toxvaldbBMDh::filter.for.lel(toxval.db,sys.date)
  toxvaldbBMDh::filter.for.multi.noel(toxval.db,sys.date)
  toxvaldbBMDh::filter.summary(toxval.db,sys.date,do.load=T)
  toxvaldbBMDh::study_group.multichem(toxval.db,sys.date)
  toxvaldbBMDh::dcap.counts(toxval.db,sys.date)
  toxvaldbBMDh::studies.per.chemical(toxval.db,sys.date)
  toxvaldbBMDh::toxvaldb.statplots(to.file=T,toxval.db,sys.date)
  #bmdh.per.study(toxval.db,sys.date)
  #bmdh.per.chemical(toxval.db,sys.date)
  #bmdh.percentile.plot(T,toxval.db,sys.date,minstudies=3,cutoff.logsd=2)
  #bmdh.aurisano.check.plot(T,toxval.db,sys.date)
}
