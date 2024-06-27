#-------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
#' @param run.export Whether to run the export.for.bmdh function (Default: TRUE)
#' @param include.pesticides Flag to include pesticides in output or not
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
driver <- function(toxval.db="res_toxval_v95", run_name=Sys.Date(), run.export=TRUE, include.pesticides=FALSE) {
  printCurrentFunction()
  if(include.pesticides) run_name = paste0(run_name, "_pesticides")

  if(run.export) {
    init.current.run.directory(run_name)
    export.for.bmdh(toxval.db, run_name=run_name, include.pesticides=include.pesticides)
  }
  # Skip filter.for.bmdh() with improved JSON storage of record_source entries
  # filter.for.bmdh(toxval.db,sys.date)
  filter.for.lel(toxval.db,run_name=run_name)
  filter.for.multi.noel(toxval.db,run_name=run_name)
  filter.summary(toxval.db,do.load=T,run_name=run_name)
  study_group.multichem(toxval.db,run_name=run_name)
  dcap.counts(toxval.db,run_name=run_name)
  studies.per.chemical(toxval.db,run_name=run_name)
  toxvaldb.statplots(to.file=T,toxval.db,run_name=run_name)
  bmdh.per.study(toxval.db,run_name=run_name)
  bmdh.per.chemical(toxval.db,run_name=run_name)
  bmdh.percentile.plot(T,toxval.db,minstudies=3,cutoff.logsd=2,run_name=run_name)
  # No longer used. Aurisano logic removed from bmdh.per.study
  # bmdh.aurisano.check.plot(T,toxval.db,sys.date)
}
