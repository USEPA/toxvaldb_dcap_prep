#' @title run_toxvaldb_dcap_prep
#' @description Run all functions to pull records from ToxValDB and prep for DCAP analysis.
#' @param toxval.db Database name.
#' @param run_name The desired name for the output directory, default current date.
#' @param run.export Whether to run the export.for.dcap function, default TRUE.
#' @param include.pesticides Flag to include pesticides in output or not
#' @param include.drugs Flag to include drugs in output or not
#' @param include.epa_dws Flag to include EPA DWS in output or not
#' @param include.food_add Flag to include food additives in output or not
#' @return None, functions are run in sequence to generate output files.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  run_toxvaldb_dcap_prep(toxval.db = "res_toxval_v96_1")
#'  }
#' }
#' @export
#' @rdname driver
run_toxvaldb_dcap_prep <- function(toxval.db, run_name=Sys.Date(), run.export=TRUE,
                   include.pesticides=FALSE, include.drugs=FALSE, include.epa_dws=TRUE,
                   include.food_add=FALSE) {

  printCurrentFunction()
  # Update run_name depending on use of chemical list filtering
  if(include.pesticides) run_name = paste0(run_name, "_pesticides")
  if(include.drugs) run_name = paste0(run_name, "_drugs")
  if(include.epa_dws) run_name = paste0(run_name, "_epa_dws")
  if(include.food_add) run_name = paste0(run_name, "_food_add")
  # Run fresh export
  if(run.export) {
    init.current.run.directory(run_name)
    export.for.dcap(toxval.db,
                    run_name=run_name,
                    include.pesticides=include.pesticides,
                    include.drugs=include.drugs,
                    include.epa_dws=include.epa_dws,
                    include.food_add=include.food_add)
  }
  # Filter export using POD selection logic
  filter.pods(toxval.db, run_name=run_name)
}
