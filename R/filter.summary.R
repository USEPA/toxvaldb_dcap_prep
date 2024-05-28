#-----------------------------------------------------------------------------------
#' Summarize the filtering steps
#'
#' `filter.for.multi.noel` Filters where multiple NOEL/NOEL etc. exist. For each study_group
#' this will select the highest NO(A)EL below the lowest LO(A)EL and the lowest LO(A)EL.
#' In all cases, all BMDx values are included
#'
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh LEL NEL multiNOEL filtered {toxval.db} {sys.date}.xlsx
#' @export
#-----------------------------------------------------------------------------------
filter.summary <- function(toxval.db="res_toxval_v95",sys.date="2024-04-10",do.load=T) {
  toxvaldbBMDh::printCurrentFunction(toxval.db)
  dir = "data/"

  if(do.load) {
    file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",sys.date,".xlsx")
    print(file)
    t1 = openxlsx::read.xlsx(file)

    file = paste0(dir,"results/ToxValDB for BMDh filtered ",toxval.db," ",sys.date,".xlsx")
    print(file)
    t2 = openxlsx::read.xlsx(file)

    file = paste0(dir,"results/ToxValDB for BMDh LEL NEL filtered ",toxval.db," ",sys.date,".xlsx")
    print(file)
    t3 = openxlsx::read.xlsx(file)

    file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
    print(file)
    t4 = openxlsx::read.xlsx(file)
    T1 <<- t1
    T2 <<- t2
    T3 <<- t3
    T4 <<- t4
  }

  chems = unique(T1[,c("dtxsid","casrn","name")])
  chems$sg1 = NA
  chems$sg2 = NA
  chems$sg3 = NA
  chems$sg4 = NA

  chems$pod1 = NA
  chems$pod1 = NA
  chems$pod1 = NA
  chems$pod1 = NA

  for(i in 1:nrow(chems)) {
    dtxsid = chems[i,"dtxsid"]
    t1 = T1[is.element(T1$dtxsid,dtxsid),]
    chems[i,"sg1"] = length(unique(t1$study_group))
    chems[i,"pod1"] = nrow(t1)

    t2 = T2[is.element(T2$dtxsid,dtxsid),]
    chems[i,"sg2"] = length(unique(t2$study_group))
    chems[i,"pod2"] = nrow(t2)

    t3 = T3[is.element(T3$dtxsid,dtxsid),]
    chems[i,"sg3"] = length(unique(t3$study_group))
    chems[i,"pod3"] = nrow(t3)

    t4 = T4[is.element(T4$dtxsid,dtxsid),]
    chems[i,"sg4"] = length(unique(t4$study_group))
    chems[i,"pod4"] = nrow(t4)
    if(i%%100==0) cat("finished",i,"out of",nrow(chems),"\n")
  }

  file = paste0(dir,"results/ToxValDB summary stats ",toxval.db," ",sys.date,".xlsx")
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(chems,file,firstRow=T,headerStyle=sty)
}
