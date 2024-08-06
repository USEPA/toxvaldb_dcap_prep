#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
#' @return Write a file with the filtered results:ToxValDB for BMDh LEL NEL multiNOEL filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title filter.summary
#' @description Summarize the filtering steps
#' @param do.load Whether to load data from Excel, Default: TRUE
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
#' @rdname filter.summary
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
filter.summary <- function(toxval.db="res_toxval_v95",run_name=Sys.Date(),do.load=TRUE) {
  printCurrentFunction(toxval.db)
  dir = paste0("data/results/", run_name, "/")

  if(do.load) {
    file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db,".xlsx")
    print(file)
    T1 = openxlsx::read.xlsx(file)

    file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," POD filtered.xlsx")
    print(file)
    T2 = openxlsx::read.xlsx(file)
  }

  chems = unique(T1[,c("dtxsid","casrn","name")])
  chems$sg1 = NA
  chems$sg2 = NA

  chems$pod1 = NA
  chems$pod2 = NA

  for(i in seq_len(nrow(chems))) {
    dtxsid = chems[i,"dtxsid"]
    t1 = T1[is.element(T1$dtxsid,dtxsid),]
    chems[i,"sg1"] = length(unique(t1$study_group))
    chems[i,"pod1"] = nrow(t1)

    t2 = T2[is.element(T2$dtxsid,dtxsid),]
    chems[i,"sg2"] = length(unique(t2$study_group))
    chems[i,"pod2"] = nrow(t2)

    if(i%%100==0) cat("finished",i,"out of",nrow(chems),"\n")
  }

  file = paste0(dir,"results/ToxValDB summary stats ",toxval.db,".xlsx")
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(chems,file,firstRow=T,headerStyle=sty)
}
