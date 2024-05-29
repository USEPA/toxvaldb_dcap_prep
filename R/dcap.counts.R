#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title dcap.counts
#' @description Calculate some stats for DCAP
#' @details Gets some statistics for the DCAP project off of the current ToxValDB export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname dcap.counts
#' @importFrom openxlsx read.xlsx write.xlsx
#-----------------------------------------------------------------------------------
dcap.counts <- function(toxval.db="res_toxval_v95",sys.date="2024-04-09") {
  printCurrentFunction(toxval.db)
  dir = "data/"
  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)
  cat("Find all combinations of toxval_types per study\n")

  # Record study_group counts
  t1 = res$study_group
  t2 = as.data.frame(table(t1))
  names(t2) = c("study_group","count")
  t2$dtxsid = NA
  t2$name = NA
  t2$source = NA
  t2 = t2[,c("dtxsid","name","source","study_group","count")]
  t2 = t2[t2$count>2,]
  t2 = t2[order(t2$count,decreasing=T),]
  for(i in seq_len(nrow(t2))) {
    sg = t2[i,"study_group"]
    x = res[is.element(res$study_group,sg),]
    t2[i,"source"] = x[1,"source"]
    t2[i,"dtxsid"] = x[1,"dtxsid"]
    t2[i,"name"] = x[1,"name"]
  }
  file = paste0(dir,"DCAP/big_study_group.xlsx")
  writexl::write_xlsx(t2,file)

  # Record POD combination counts
  sglist = unique(res$study_group)
  ttlist = NULL
  for(sg in sglist) {
    t1 = res[is.element(res$study_group,sg),]
    t2 = paste0(t1[1,"source"],": ",paste(sort(t1$toxval_type),collapse="|"))
    ttlist = c(ttlist,t2)
  }
  t3 = as.data.frame(table(ttlist))
  names(t3) = c("pod.combination","studies")
  t3 = t3[order(t3$studies,decreasing=T),]
  file = paste0(dir,"results/pod_combinations.xlsx")
  writexl::write_xlsx(t3,file)
}
