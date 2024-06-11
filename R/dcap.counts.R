#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title dcap.counts
#' @description Calculate some stats for DCAP
#' @details Gets some statistics for the DCAP project off of the current ToxValDB export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname dcap.counts
#' @importFrom openxlsx read.xlsx write.xlsx
#-----------------------------------------------------------------------------------
dcap.counts <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = readxl::read_xlsx(file)
  cat("Find all combinations of toxval_types per study\n")

  # Record study_group counts
  counts = res %>%
    dplyr::select(study_group) %>%
    dplyr::group_by(study_group) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 2) %>%
    dplyr::arrange(dplyr::desc(count))

  t2 = res %>%
    dplyr::left_join(counts, by=c("study_group")) %>%
    tidyr::drop_na(count) %>%
    dplyr::group_by(study_group) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("dtxsid", "name", "source", "study_group", "count")) %>%
    dplyr::distinct()

  file = paste0(dir,"DCAP/big_study_group.xlsx")
  writexl::write_xlsx(t2,file)

  t3 = res %>%
    dplyr::group_by(study_group) %>%
    dplyr::summarize(
      pod.combination = paste0(unique(source), ": ", paste(sort(toxval_type),collapse="|"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-study_group) %>%
    dplyr::group_by(pod.combination) %>%
    dplyr::summarize(studies = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(studies))

  file = paste0(dir,"results/pod_combinations.xlsx")
  writexl::write_xlsx(t3,file)
}
