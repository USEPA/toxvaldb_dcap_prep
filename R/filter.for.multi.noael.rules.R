#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @return Write a file with the filtered results
#' @export
#' @title filter.for.multi.noael.rules
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
#' @rdname filter.for.multi.noel
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
filter.for.multi.noael.rules <- function(toxval.db="res_toxval_v95", sys.date="2024-04-10") {

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
    dplyr::ungroup()

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
    dplyr::ungroup()
    #select(study_group,toxval_type,toxval_numeric) %>%
    #arrange(study_group,toxval_type) %>%
    #ungroup()
}
