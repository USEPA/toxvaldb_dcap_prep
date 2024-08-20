#-------------------------------------------------------------------------------
#' @param toxval.db The version of ToxValDB to use
#' @param toxval.db The version of ToxValDB to use
#' @param sys.date Date of the most recent data export #' Make the rule #' filter LOELs when NOELs are present for the same study #' filter out redundant values for the same study group #' Perform the allometric scaling
#' @param sys.date Date of the most recent data export #' Make the rule #' filter LOELs when NOELs are present for the same study #' filter out redundant values for the same study group #' Perform the allometric scaling
#' @title pod.per.chemical
#' @description Explore different methods for calculating PODs
#' @return Writes output to file
#' @details Explore different methods for calculating PODs
#' This uses the same input as the function bmdh.perstudy / bmdh.per.chemical
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pod.per.chemical
#' @export
#' @param t2 PARAM_DESCRIPTION
#' @param scale.mat PARAM_DESCRIPTION
#-------------------------------------------------------------------------------
pod.per.chemical <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction()

  cat("read in ToxValDB data\n")
   dir = "data/results/"
   file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
   print(file)
   toxval = openxlsx::read.xlsx(file)

  toxval = TOXVAL
  toxval[is.na(toxval$study_group),"study_group"] = toxval[is.na(toxval$study_group),"source_hash"]
  dir = "data/humanqsarpods/"

  print(nrow(toxval))
  nlist = c("dtxsid","name","source","toxval_type","toxval_numeric","toxval_units","study_type",
            "common_name","exposure_route","study_group","source_hash")
  toxval = unique(toxval[,nlist])
  toxval = toxval[toxval$toxval_numeric>0,]
  print(nrow(toxval))

  toxval= toxval[!is.na(toxval$dtxsid),]
  toxval = toxval[!is.element(toxval$source,c("PPRTV (NCEA)","ATSDR MRLs 2022","EFSA","COSMOS","Chiu")),]
  print(nrow(toxval))
  toxval = toxval[toxval$exposure_route=="oral",]
  print(nrow(toxval))
  toxval = toxval[toxval$toxval_units=="mg/kg-day",]
  print(nrow(toxval))
  toxval = toxval[is.element(toxval$common_name,c("Rat","Mouse","Dog","Rabbit","Human")),]
  humanized.pod.types = NULL
  tlist = unique(toxval$toxval_type)
  for(tt in tlist) {
    if(grepl("HED|HEC", tt)) humanized.pod.types = c(humanized.pod.types,tt)
  }
  toxval[is.element(toxval$toxval_type,humanized.pod.types),"common_name"] = "Human"

  x = unique(toxval[,c("dtxsid","name")])
  dlist = x$dtxsid
  dups = dlist[duplicated(dlist)]
  for(dtxsid in dups) {
    y = x[x$dtxsid==dtxsid,]
    goodname = x[1,"name"]
    toxval[toxval$dtxsid==dtxsid,"name"] = goodname
  }
  x = unique(toxval[,c("dtxsid","name")])
  cat(nrow(x),length(unique(x$dtxsid)),"\n")

  noel.types <<- c("NOAEC","NOAEL","NOEC","NOEL")
  loel.types <<- c("LOAEC","LOAEL","LOEC","LOEL","LOAL")
  bmdl.types = NULL
  tlist = unique(toxval$toxval_type)
  for(tt in tlist) {
    if(grepl("BMD|BMC", tt)) bmdl.types = c(bmdl.types,tt)
  }
  bmdl.types <<- bmdl.types
  repdose = c("chronic","developmental","neurotoxicity chronic","neurotoxicity subchronic",
              "reproduction","reproduction developmental","subchronic")
  repdose_plus = c(repdose,"28-day","neurotoxicity 28-day")
  res = NULL

  scale.mat = as.data.frame(matrix(nrow=5,ncol=2))
  names(scale.mat) = c("species","factor")
  scale.mat[,"species"] = c("Rat","Mouse","Dog","Rabbit","Human")
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4804402/
  # scale.mat[,"factor"] = c(6.2,12.3,1.8,3.1)
  # Appendix B of the BW3/4 allometric scaling guideline doc
  # rat     0.24
  # mouse   0.14
  # dog     0.63
  scale.mat[,"factor"] = c(0.24, 0.14, 0.63, 0.41, 1)
  rownames(scale.mat) = scale.mat$species
  scale.mat <<- scale.mat
  hra.sources = c("ATSDR PFAS 2021","HEAST","IRIS","PPRTV (CPHEA)")
  # res1 = pod.rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 1",rule.stype="chronic",
  #                   rule.ttype ="NO(A)EL",
  #                   stype.list=c("chronic"),
  #                   ttype.list=c(noel.types))
  # res = rbind(res,res1)
  # res1 = pod.rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 2",rule.stype="chronic",
  #                   rule.ttype ="LO(A)EL",
  #                   stype.list=c("chronic"),
  #                   ttype.list=c(loel.types))
  # res = rbind(res,res1)
  # res1 = pod.rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 3",rule.stype="chronic",
  #                   rule.ttype ="LO(A)EL, NO(A)EL, BMD",
  #                   stype.list=c("chronic"),
  #                   ttype.list=c(loel.types,noel.types,bmdl.types))
  # res = rbind(res,res1)
  # res1 = pod.rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 4",rule.stype="repeat dose",
  #                   rule.ttype ="LO(A)EL, NO(A)EL, BMD",
  #                   stype.list=repdose,
  #                   ttype.list=c(loel.types,noel.types,bmdl.types))
  # res = rbind(res,res1)
  res1 = pod.rule.maker(toxval,hra.sources,
                    rule.name="Rule 5",rule.stype="repeat dose+28 day",
                    rule.ttype ="LO(A)EL, NO(A)EL, BMD",
                    stype.list=repdose_plus,
                    ttype.list=c(loel.types,noel.types,bmdl.types))
  res = rbind(res,res1)

  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"toxval pods by rule.xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
