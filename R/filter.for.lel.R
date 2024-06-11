#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title filter.for.lel
#' @description Filter the exported records for redundancy
#' @details Filters LEL, NEL values wihere a LOAEL/NOAEL value exists
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
  sgin = res %>%
    dplyr::filter(toxval_type %in% c("NEL","LEL","LOEL","NOEL")) %>%
    dplyr::pull(study_group) %>%
    unique()

  t2a = res %>%
    dplyr::filter(study_group %in% sgin)
  t2b = res %>%
    dplyr::filter(!(study_group %in% sgin))
  t2c = NULL

  for(sg in sgin) {
    t3 = t2a %>%
      dplyr::filter(study_group == !!sg)
    ttlist = t3 %>%
      dplyr::pull(toxval_type) %>%
      unique()

    if(is.element("LOAEL",ttlist) || is.element("NOAEL",ttlist)) t2c = dplyr::bind_rows(t2c, t3)
    else {
      if(is.element("LOAEL",ttlist)) {
        t2c = t2c %>%
          dplyr::bind_rows(t3 %>% dplyr::filter(!(toxval_type %in% c("LEL","LOEL"))))
      }
      if(is.element("NOAEL",ttlist)) {
        t2c = t2c %>%
          dplyr::bind_rows(t3 %>% dplyr::filter(!(toxval_type %in% c("NEL","NOEL"))))
      }
    }
  }

  t4 = dplyr::bind_rows(t2b, t2c)

  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL filtered ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(t4, file)
}
