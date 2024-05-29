#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results
#' @export
#' @title toxval.redundancies
#' @description Detect potential redundancies in ToxValDB.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname toxval.redundancies
#' @importFrom openxlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
toxval.redundancies <- function(toxval.db="res_toxval_v95",user="rjudson",password) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  setDBConn(user=user,password=password)
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  slist = sort(slist)
  nlist = c("source","total_records","total_unique_source_hash","total_unique_records","perc_redundant")
  res = as.data.frame(matrix(nrow=length(slist),ncol=length(nlist)))
  names(res) = nlist
  rownames(res) = slist
  for(src in slist) {
    n = runQuery(paste0("select count(*) from toxval where source='",src,"'"),toxval.db)[1,1]
    cat(src,":",n,"\n")
    query = paste0("select * from toxval where source='",src,"'")
    mat = runQuery(query,toxval.db,T,F)
    t1 = unique(mat)
    exclude = c("source_hash","toxval_id")
    t2 = t1[,!(names(t1) %in% exclude)]
    res[src,"source"] = src
    res[src,"total_records"] = nrow(t1)
    res[src,"total_unique_source_hash"] = length(unique(t1$source_hash))
    res[src,"total_unique_records"] = nrow(unique(t2))
    res[src,"perc_redundant"] = (res[src,"total_records"]-res[src,"total_unique_records"])/max(1,res[src,"total_records"])
  }
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"ToxValDB redundancies ",toxval.db," ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
