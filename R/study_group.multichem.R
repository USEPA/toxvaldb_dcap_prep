#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
#' @return Write a file with the filtered results:ToxValDB for BMDh LEL NEL multiNOEL filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title study_group.multichem
#' @description Find study groups that span multiple chemicals
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname study_group.multichem
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
study_group.multichem <- function(toxval.db, run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = paste0("data/results/", run_name, "/")

  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db,".xlsx")
  print(file)
  res = readxl::read_xlsx(file)

  # Check for study_groups that span multiple chemicals
  num_study_group = res %>%
    dplyr::select(source, dtxsid, name, study_group) %>%
    dplyr::distinct() %>%
    dplyr::group_by(study_group) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)

  # If study_groups are identified, prepare relevant information
  if(nrow(num_study_group)) {
    res = num_study_group %>%
      dplyr::left_join(res, by=c("study_group")) %>%
      tidyr::drop_na(n) %>%
      dplyr::select(source, dtxsid, name, study_group) %>%
      dplyr::distinct()
  }

  file = paste0(dir,"results/ToxValDB study_group.multichem ",toxval.db,".xlsx")
  writexl::write_xlsx(res, file)
}
