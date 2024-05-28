#-----------------------------------------------------------------------------------
#' @#' Find study groups that span multiple chemicals
#'
#' `filter.for.multi.noel` Filters where multiple NOEL/NOEL etc. exist. For each study_group
#' this will select the highest NO(A)EL below the lowest LO(A)EL and the lowest LO(A)EL.
#' In all cases, all BMDx values are included
#'
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh LEL NEL multiNOEL filtered {toxval.db} {sys.date}.xlsx
#' @export 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
study_group.multichem <- function(toxval.db="res_toxval_v95",sys.date="2024-04-10") {
  printCurrentFunction(toxval.db)
  dir = "data/"

  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",sys.date,".xlsx")
  print(file)
  t1 = openxlsx::read.xlsx(file)
  sglist = unique(t1$study_group)
  nlist = c("source","dtxsid","name","study_group")
  row = as.data.frame(matrix(nrow=1,ncol=length(nlist)))
  names(row) = nlist
  res = NULL
  for(i in 1:length(sglist)) {
    sg =sglist[i]
    t2 = t1[is.element(t1$study_group,sg),]
    dlist = unique(t2$dtxsid)
    if(length(dlist)>1) {
      for(dtxsid in dlist) {
        t3 = t2[is.element(t2$dtxsid,dtxsid),]
        row[1,"study_group"] = sg
        row[1,"source"] = t3[1,"source"]
        row[1,"dtxsid"] = dtxsid
        row[1,"name"] = t3[1,"name"]
        res = rbind(res,row)
        print(row)
      }
    }
    if(i%%100==0) cat("finished",i,"out of",length(sglist),"\n")
  }

  file = paste0(dir,"results/ToxValDB study_group.multichem ",toxval.db," ",sys.date,".xlsx")
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
