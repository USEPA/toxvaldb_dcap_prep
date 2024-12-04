#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title studies.per.chemical
#' @description Calculate some stats for DCAP
#' @details Gets the number of studies per chemical from the current ToxValDB export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_xlsx}}, \code{\link[writexl]{write_xlsx}}
#' @rdname studies.per.chemical
#' @importFrom readxl read_xlsx
#' @importFrom writexl write_xlsx
#-----------------------------------------------------------------------------------
studies.per.chemical <- function(toxval.db, run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = paste0(Sys.getenv("datapath"), "data/results/", run_name, "/")
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," POD filtered.xlsx")
  print(file)

  res = readxl::read_xlsx(file) %>%
    dplyr::select(dtxsid, casrn, name, study_group, filter_pod_group)

  mat = res %>%
    dplyr::select(dtxsid, casrn, name) %>%
    # Duplicates due to slight differences in name
    dplyr::filter(!duplicated(dtxsid)) %>%
    dplyr::distinct()

  n_rec = res %>%
    dplyr::group_by(dtxsid) %>%
    dplyr::summarise(records = dplyr::n())

  n_study = res %>%
    dplyr::select(dtxsid, study_group) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dtxsid) %>%
    dplyr::summarise(studies = dplyr::n())

  mat = mat %>%
    dplyr::left_join(n_rec,
                     by = "dtxsid") %>%
    dplyr::left_join(n_study,
                     by = "dtxsid")

  # Summarise chemicals with at least 1 study_group from auth source, and
  # has 5+ non-auth study_group entries
  auth_n = res %>%
    dplyr::select(dtxsid, study_group, filter_pod_group) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dtxsid, filter_pod_group) %>%
    dplyr::summarise(study_auth_type=dplyr::n()) %>%
    tidyr::pivot_wider(id_cols = c(dtxsid),
                       names_from = filter_pod_group,
                       values_from = study_auth_type) %>%
    dplyr::mutate(dplyr::across(c("non_auth", "auth_key", "auth_not_key"),
                                ~ tidyr::replace_na(., 0)),
                  auth_chem = dplyr::case_when(
                    auth_key > 0 | auth_not_key > 0 ~ 1,
                    TRUE ~ 0
                  ),
                  study_threshold = dplyr::case_when(
                    sum(non_auth, auth_key, auth_not_key, na.rm=TRUE) >= 5 ~ 1,
                    # non_auth >= 5 ~ 1,
                    TRUE ~ 0
                  )) %>%
    dplyr::filter(auth_chem == 1, study_threshold == 1)

  # Export files
  writexl::write_xlsx(mat, paste0(dir,"DCAP/study_x_chemical.xlsx"))
  writexl::write_xlsx(auth_n, paste0(dir, "results/authoritative_chemicals.xlsx"))

  t2 = as.data.frame(table(mat$records))
  names(t2) = c("records","chemicals")
  writexl::write_xlsx(t2,
                       paste0(dir,"DCAP/record count table.xlsx"))

  t2 = as.data.frame(table(mat$studies))
  names(t2) = c("studies","chemicals")
  writexl::write_xlsx(t2,
                       paste0(dir,"results/study count table.xlsx"))


}
