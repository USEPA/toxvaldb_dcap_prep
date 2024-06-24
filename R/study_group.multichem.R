#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @param include.pesticides Flag to include pesticides in output or not
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
study_group.multichem <- function(toxval.db="res_toxval_v95", sys.date=Sys.Date(), include.pesticides=FALSE) {
  printCurrentFunction(toxval.db)
  dir = "data/"

  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",sys.date,".xlsx")
  if(include.pesticides) file = paste0(dir,"results/ToxValDB for BMDh WITH PESTICIDES ",
                                       toxval.db," ",sys.date,".xlsx")
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
  res = data.frame()
  if(nrow(num_study_group)) {
    res = mat %>%
      dplyr::left_join(res, by=c("study_group")) %>%
      tidyr::drop_na(n) %>%
      dplyr::select(source, dtxsid, name, study_group) %>%
      dplyr::distinct()
  }

  file = paste0(dir,"results/ToxValDB study_group.multichem ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(res, file)
}
