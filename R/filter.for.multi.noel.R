#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results
#' @export
#' @title filter.for.multi.noel
#' @description Filter the exported records for redundancy of NO(A)EL / LO(A)EL PODs in a study group
#' @details Filters where multiple NOEL/NOEL etc. exist. For each study_group
#' this will select the highest NO(A)EL below the lowest LO(A)EL and the lowest LO(A)EL.
#' In all cases, all BMDx values are included
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#' @rdname filter.for.multi.noel
#' @importFrom readxl read_xlsx
#' @importFrom dplyr group_by mutate ungroup filter slice_min slice_max bind_rows distinct case_when across select
#' @importFrom tidyr fill replace_na
#' @importFrom writexl write_xlsx
#-----------------------------------------------------------------------------------
filter.for.multi.noel <- function(toxval.db="res_toxval_v95", sys.date=Sys.Date()) {
  input_file = paste0("data/results/ToxValDB for BMDh LEL NEL filtered ", toxval.db, " ", sys.date, ".xlsx")
  if(!exists("T3")) T3 = readxl::read_xlsx(input_file)

  T4 = T3 %>%
    dplyr::group_by(study_group) %>%
    dplyr::mutate(
      nb_L = sum(grepl("^[L]",toxval_type)),
      nb_N = sum(grepl("^[N]",toxval_type))
    ) %>%
    dplyr::ungroup()

  ###create 3 dataframes that respect conditions
  a = T4 %>%
    dplyr::group_by(study_group) %>%
    dplyr::filter(nb_L>1 & grepl("^[L]", toxval_type)) %>%
    dplyr::slice_min(toxval_numeric, n=1) %>%
    dplyr::ungroup()
  b = T4 %>%
    dplyr::group_by(study_group) %>%
    dplyr::filter(nb_N>1 & grepl("^[N]", toxval_type)) %>%
    dplyr::slice_max(toxval_numeric, n=1) %>%
    dplyr::ungroup()
  c = T4 %>% dplyr::group_by(study_group) %>%
    dplyr::filter(grepl("^[B]", toxval_type) | (nb_L<=1 & grepl("^[L]", toxval_type)) | (nb_N<=1 & grepl("^[N]", toxval_type))) %>%
    dplyr::ungroup()

  ### stack the dataframes
  T4 = dplyr::bind_rows(a, b, c) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  ###add the value of L and N for each group
  T4 = T4 %>%
    dplyr::group_by(study_group) %>%
    dplyr::mutate(
      val_L = dplyr::case_when(
        grepl("^[L]", toxval_type) ~ toxval_numeric,
        TRUE ~ NA_real_
      ),
      val_N = dplyr::case_when(
        grepl("^[N]", toxval_type) ~ toxval_numeric,
        TRUE ~ NA_real_
      )
    ) %>%
    tidyr::fill(c(val_L, val_N), .direction = "downup") %>%
    dplyr::mutate(
      dplyr::across(c(val_L,val_N), ~tidyr::replace_na(.x,0)),
      keep = dplyr::case_when(
        nb_L > 0 & grepl("^[N]", toxval_type) & val_N > val_L ~ "remove",
        TRUE ~ "keep"
      )
    ) %>%
    dplyr::filter(keep == "keep") %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("keep", "nb_L", "nb_N", "val_L", "val_N")) %>%
    dplyr::distinct()

  # Write output
  output_file = paste0("data/results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(T4, output_file)
}
