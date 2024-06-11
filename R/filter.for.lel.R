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
  res = openxlsx::read.xlsx(file)
  t1 = res[is.element(res$toxval_type,c("NEL","LEL","LOEL","NOEL")),]

  sgin = unique(t1$study_group)
  t2a = res[is.element(res$study_group,sgin),]
  t2b = res[!is.element(res$study_group,sgin),]

  t2c = NULL
  for(sg in sgin) {
    t3 = t2a[is.element(t2a$study_group,sg),]
    ttlist = unique(t3$toxval_type)
    #cat(t3[1,"source"],":",paste(ttlist,collapse="|"),"\n")
    if(is.element("LOAEL",ttlist) || is.element("NOAEL",ttlist)) {
      if(is.element("LOAEL",ttlist)) t3 = t3[!is.element(t3$toxval_type,c("LEL","LOEL")),]
      if(is.element("NOAEL",ttlist)) t3 = t3[!is.element(t3$toxval_type,c("NEL","NOEL")),]
      t2c = rbind(t2c,t3)
    }
    else t2c = rbind(t2c,t3)
  }

  t4 = rbind(t2b,t2c)
  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL filtered ",toxval.db," ",sys.date,".xlsx")
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(t4,file,firstRow=T,headerStyle=sty)
}
