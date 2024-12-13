#-------------------------------------------------------------------------------
#' @param toxval.db Database name
#' @param run_name The desired name for the output directory (Default: current date)
#' @param run.export Whether to run the export.for.bmdh function (Default: TRUE)
#' @param include.pesticides Flag to include pesticides in output or not
#' @param include.drugs Flag to include drugs in output or not
#' @param include.epa_dws Flag to include EPA DWS in output or not
#' @param include.food_add Flag to include food additives in output or not
#' @param reset.study_group Flag to reset study_group
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
driver <- function(toxval.db, run_name=Sys.Date(), run.export=TRUE,
                   include.pesticides=FALSE, include.drugs=FALSE, include.epa_dws=TRUE,
                   include.food_add=FALSE, reset.study_group=FALSE) {
  printCurrentFunction()
  if(include.pesticides) run_name = paste0(run_name, "_pesticides")
  if(include.drugs) run_name = paste0(run_name, "_drugs")
  if(include.epa_dws) run_name = paste0(run_name, "_epa_dws")
  if(include.food_add) run_name = paste0(run_name, "_food_add")

  if(run.export) {
    init.current.run.directory(run_name)
    export.for.bmdh(toxval.db,
                    run_name=run_name,
                    include.pesticides=include.pesticides,
                    include.drugs=include.drugs,
                    include.epa_dws=include.epa_dws,
                    include.food_add=include.food_add,
                    reset.study_group = reset.study_group)
  }
  filter.pods(toxval.db, run_name=run_name)
  filter.summary(toxval.db, run_name=run_name, do.load=TRUE)
  study_group.multichem(toxval.db, run_name=run_name)
  dcap.counts(toxval.db, run_name=run_name)
  studies.per.chemical(toxval.db, run_name=run_name)
  toxvaldb.statplots(to.file=TRUE, toxval.db, run_name=run_name)
  bmdh.per.study(toxval.db, run_name=run_name)
  bmdh.per.chemical(toxval.db, run_name=run_name)
  bmdh.percentile.plot(TRUE, toxval.db, minstudies=3, cutoff.logsd=2, run_name=run_name)
  # No longer used. Aurisano logic removed from bmdh.per.study
  # bmdh.aurisano.check.plot(T,toxval.db,sys.date)
}
