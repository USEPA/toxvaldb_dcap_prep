#-------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @param regulatory.sources This is the list of sources that will be used to select the #' optimal quantile to use for selecting the final chemical-level BMDh.
#' @return Write a file with the results: toxval_PODs_for_BMDh chemical level {toxval.db} {sys.date}.xlsx
#' @export 
#' @title bmdh.per.chemical
#' @description Calculate BMDh values one per chemical
#' @details Calculates one BMDh value per chemical. This is done by taking
#' various percentiles of the distribution of the BMDh values and building a table
#' with one column per percentile per chemical. The values are calibrated
#' against regulatory values. The list of high-quality, regulator sources is given
#' as one of the calling arguments.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[stats]{sd}}, \code{\link[stats]{quantile}}
#' @rdname bmdh.per.chemical
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#' @importFrom stats sd quantile
#-------------------------------------------------------------------------------
bmdh.per.chemical <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date(),
                              regulatory.sources=c("IRIS",
                                                   "PPRTV (CPHEA)",
                                                   "ATSDR MRLs 2022",
                                                   "ATSDR MRLs",
                                                   "ATSDR PFAS 2021",
                                                   "EPA OPP",
                                                   "HEAST")) {
  printCurrentFunction()
  dir = "data/"

  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db," ",sys.date,".xlsx")
  print(file)
  mat = openxlsx::read.xlsx(file)
  mat = mat[!is.na(mat$bmdh),]
  res = unique(mat[,c("dtxsid","casrn","name")])
  res$casrn = NA
  res$name = NA
  res = unique(res)
  res = res[!is.na(res$dtxsid),]
  rownames(res) = res$dtxsid

  res$studies = NA
  res$pod_01 = NA
  res$pod_02 = NA
  res$pod_05 = NA
  res$pod_10 = NA
  res$pod_15 = NA
  res$pod_20 = NA
  res$pod_25 = NA
  res$pod_30 = NA
  res$pod_35 = NA
  res$pod_hra = NA
  res$pod_hra_source = NA
  res$units = "mg/kg-day"
  res$log.sd = NA
  res$range = NA
  res$variance = NA

  for(i in seq_len(nrow(res))) {
    dtxsid = res[i,"dtxsid"]
    temp0 = mat[is.element(mat$dtxsid,dtxsid),c("dtxsid","casrn","name","bmdh","study_group","source","common_name")]
    temp = NULL

    #---------------------------------------------------------------------------
    # this piece of code will take the minimum BMDh from a study
    # This may be the right thing to do, but the studY_group field is
    # not properly populated now, so it is commented out
    #---------------------------------------------------------------------------
    # for(sg in unique(temp0$study_group)) {
    #   x = temp0[is.element(temp0$study_group,sg),]
    #   x = x[order(x$bmdh),]
    #   temp = rbind(temp,x[1,])
    # }
    temp = temp0
    res[i,"name"] = temp[1,"name"]
    res[i,"casrn"] = temp[1,"casrn"]

    temp1 =  temp[is.element(temp$source,regulatory.sources),]
     if(nrow(temp1)>0) {
      temp1 = temp1[order(temp1$bmdh),]
      res[i,"pod_hra"] = temp1[1,"bmdh"]
      temp1 = temp1[temp1$bmdh<=temp1[1,"bmdh"],]
      res[i,"pod_hra_source"] = paste(unique(sort(temp1[,"source"])),collapse="|")
    }
    vals = temp$bmdh
    lvals = sort(log10(vals))
    lsd = stats::sd(lvals)
    rng = max(lvals)-min(lvals)
    qq = stats::quantile(lvals,probs=seq(0,1,0.01))
    n01 = 10**(qq[2])
    n02 = 10**(qq[3])
    n05 = 10**(qq[6])
    n10 = 10**(qq[11])
    n15 = 10**(qq[16])
    n20 = 10**(qq[21])
    n25 = 10**(qq[26])
    n30 = 10**(qq[31])
    n35 = 10**(qq[36])
    res[i,"log.sd"] = lsd
    res[i,"variance"] = lsd**2
    res[i,"range"] = rng
    res[i,"studies"] = length(vals)
    res[i,"pod_01"] = n01
    res[i,"pod_02"] = n02
    res[i,"pod_05"] = n05
    res[i,"pod_10"] = n10
    res[i,"pod_15"] = n15
    res[i,"pod_20"] = n20
    res[i,"pod_25"] = n25
    res[i,"pod_30"] = n30
    res[i,"pod_35"] = n35

    if(!is.na(res[i,"pod_hra"])) res[i,"pod"] = res[i,"pod_hra"]
    if(i%%1000==0) cat("finished",i,"out of",nrow(res),"\n")
  }
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"results/ToxValDB BMDh per chemical ",toxval.db," ",sys.date,".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
