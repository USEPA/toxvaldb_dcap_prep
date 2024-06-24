#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export
#' @title studies.per.chemical
#' @description Calculate some stats for DCAP
#' @details Gets the number of studies per chemical from the current ToxValDB export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname studies.per.chemical
#' @importFrom openxlsx read.xlsx write.xlsx
#-----------------------------------------------------------------------------------
studies.per.chemical <- function(toxval.db="res_toxval_v95",run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = paste0("data/results/", run_name, "/")
  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db,".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)

  dlist = unique(res$dtxsid)
  nlist = c("dtxsid","casrn","name","records","studies")
  mat = as.data.frame(matrix(nrow=length(dlist),ncol=length(nlist)))
  names(mat) = nlist
  for(i in seq_len(length(dlist))) {
    dtxsid = dlist[i]
    t1 = res[is.element(res$dtxsid,dtxsid),]
    mat[i,"dtxsid"] = dtxsid
    mat[i,"casrn"] = t1[1,"casrn"]
    mat[i,"name"] = t1[1,"name"]
    mat[i,"records"] = nrow(t1)
    mat[i,"studies"] = length(unique(t1$study_group))
  }

  file = paste0(dir,"DCAP/study_x_chemical.xlsx")
  openxlsx::write.xlsx(mat,file)

  t2 = as.data.frame(table(mat$records))
  names(t2) = c("records","chemicals")
  file = paste0(dir,"DCAP/record count table.xlsx")
  openxlsx::write.xlsx(t2,file)
  t2 = as.data.frame(table(mat$studies))
  names(t2) = c("studies","chemicals")
  file = paste0(dir,"results/study count table.xlsx")
  openxlsx::write.xlsx(t2,file)
}
