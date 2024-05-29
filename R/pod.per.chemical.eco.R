#-------------------------------------------------------------------------------
#' @param toxval.db The version fo the ToxValDB to use
#' @param sys.date The date of the database export
#' @title pod.per.chemical.eco
#' @description Calculate the eco PODs
#' @return Writes output to file
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[stats]{quantile}}, \code{\link[stats]{sd}}
#' @rdname toxvaldbPodsEco
#' @export
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom stats quantile sd
#-------------------------------------------------------------------------------
pod.per.chemical.eco <- function(toxval.db="res_toxval_v95",sys.date="2024-03-05") {
  printCurrentFunction()
  dir = "data/ecoqsarpods/"
  if(!exists("TOXVALECO")) {
    cat("read in ToxValDB data\n")
    file = paste0(dir,"ToxValDB Eco ",toxval.db," ",sys.date,".xlsx")
    print(file)
    res = openxlsx::read.xlsx(file)
    TOXVALECO <<- res
  }
  toxval = TOXVALECO

  chems = unique(toxval[,c("dtxsid","casrn","name")])

  print(nrow(toxval))
  nlist = c("dtxsid","casrn","name","source","toxval_type","toxval_numeric","toxval_units","study_type",
            "common_name","ecotox_group","study_group","source_hash")
  toxval = unique(toxval[,nlist])
  toxval = toxval[toxval$toxval_numeric>0,]
  print(nrow(toxval))

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

  res = unique(toxval[,c("dtxsid","casrn","name")])
  res = res[order(res$name),]
  res$vertebrate_pod = NA
  res$vertebrate_npod = NA
  res$vertebrate_sd = NA
  res$vertebrate_range = NA
  res$vertebrate_source = NA

  res$invertebrate_pod = NA
  res$invertebrate_npod = NA
  res$invertebrate_sd = NA
  res$invertebrate_range = NA
  res$invertebrate_source = NA

  res$plant_pod = NA
  res$plant_npod = NA
  res$plant_sd = NA
  res$plant_range = NA
  res$plant_source = NA

  for(i in seq_len(nrow(res))) {
    dtxsid = res[i,"dtxsid"]
    t1 = toxval[toxval$dtxsid==dtxsid,]
    # vertebrate
    t2 = t1[is.element(t1$ecotox_group,c("Amphibians","Fish")),]
    if(nrow(t2)>0) {
      x = log10(t2$toxval_numeric)
      val = stats::quantile(x,probs=seq(0,1,0.1))[2]
      res[i,"vertebrate_pod"] = 10**val
      res[i,"vertebrate_npod"] = length(x)
      res[i,"vertebrate_sd"] = stats::sd(x)
      res[i,"vertebrate_range"] = max(x)-min(x)
      res[i,"vertebrate_source"] = "ToxValDB"
    }

    # invertebrate
    t2 = t1[is.element(t1$ecotox_group,c("Invertebrates","Crustaceans","Molluscs")),]
    if(nrow(t2)>0) {
      x = log10(t2$toxval_numeric)
      val = stats::quantile(x,probs=seq(0,1,0.1))[2]
      res[i,"invertebrate_pod"] = 10**val
      res[i,"invertebrate_npod"] = length(x)
      res[i,"invertebrate_sd"] = stats::sd(x)
      res[i,"invertebrate_range"] = max(x)-min(x)
      res[i,"invertebrate_source"] = "ToxValDB"
    }

    # plant
    t2 = t1[is.element(t1$ecotox_group,c("Algae","Moss, Hornworts","Flowers, Trees, Shrubs, Ferns")),]
    if(nrow(t2)>0) {
      x = log10(t2$toxval_numeric)
      val = stats::quantile(x,probs=seq(0,1,0.1))[2]
      res[i,"plant_pod"] = 10**val
      res[i,"plant_npod"] = length(x)
      res[i,"plant_sd"] = stats::sd(x)
      res[i,"plant_range"] = max(x)-min(x)
      res[i,"plant_source"] = "ToxValDB"
    }
  }

  file = paste0(dir,"toxval eco pods.xlsx")
  openxlsx::write.xlsx(res,file)
}
