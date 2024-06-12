#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export 
#' @title filter.for.lel
#' @description Filter the exported records for redundancy
#' @details Filters LEL, NEL values where a LOAEL/NOAEL value exists
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname filter.for.lel
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
filter.for.lel <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = readxl::read_xlsx(file)

  # Get study_groups to perform filtering on
  loael_study_groups = res %>%
    dplyr::filter(toxval_type == "LOAEL") %>%
    dplyr::pull(study_group) %>%
    unique()
  noael_study_groups = res %>%
    dplyr::filter(toxval_type == "NOAEL") %>%
    dplyr::pull(study_group) %>%
    unique()

  # Perform filtering
  no_changes = res %>%
    dplyr::filter(!(study_group %in% loael_study_groups | study_group %in% noael_study_groups))
  loael_filtered = res %>%
    dplyr::filter(study_group %in% loael_study_groups,
                  !(study_group %in% noael_study_groups),
                  !(toxval_type %in% c("LEL","LOEL")))
  noael_filtered = res %>%
    dplyr::filter(study_group %in% noael_study_groups,
                  !(study_group %in% loael_study_groups),
                  !(toxval_type %in% c("NEL","NOEL")))
  both_filtered = res %>%
    dplyr::filter(study_group %in% noael_study_groups,
                  study_group %in% loael_study_groups,
                  !(toxval_type %in% c("NEL","NOEL","LEL","LOEL")))

  # Combine final data
  output = dplyr::bind_rows(no_changes, loael_filtered, noael_filtered, both_filtered) %>%
    dplyr::distinct()

  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL filtered ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(output, file)
}
