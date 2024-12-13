#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @export
#' @title property.counter
#' @description Count proerties of records
#' @param sys.date The date of the database export
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}, \code{\link[openxlsx]{createStyle}}
#' @rdname property.counter
#' @importFrom openxlsx read.xlsx write.xlsx createStyle
#-----------------------------------------------------------------------------------
property.counter <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = "data/"

  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",Sys.Date(),".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)
  nlist = c("dtxsid","casrn","name","source","toxval_type","toxval_subtype",
            "toxval_numeric_qualifier","toxval_numeric","toxval_units","study_type",
            "study_duration_value","study_duration_units","common_name","sex",
            "exposure_route","toxicological_effect","year","long_ref")
  res = unique(res[,nlist])

  x = as.data.frame(table(res$common_name))
  names(x) = c("species","records")
  x = x[(order(x$records,decreasing=T)),]
  file = paste0(dir,"counts/species counts.xlsx")
  openxlsx::write.xlsx(x,file)

  x = as.data.frame(table(res$toxval_type))
  names(x) = c("toxval_type","records")
  x = x[(order(x$records,decreasing=T)),]
  file = paste0(dir,"counts/toxval_type counts.xlsx")
  openxlsx::write.xlsx(x,file)

  x = as.data.frame(table(res$study_type))
  names(x) = c("study_type","records")
  x = x[(order(x$records,decreasing=T)),]
  file = paste0(dir,"counts/study_type counts.xlsx")
  openxlsx::write.xlsx(x,file)

  x = res[res$common_name=="Human",]
  nlist = c("dtxsid","casrn","name","source","toxval_type","toxval_subtype",
            "toxval_numeric_qualifier","toxval_numeric","toxval_units","study_type",
            "study_duration_value","study_duration_units","common_name","sex",
            "exposure_route","toxicological_effect","year","long_ref")
  x = x[,nlist]
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"counts/human studies.xlsx")
  openxlsx::write.xlsx(x,file,firstRow=T,headerStyle=sty)
}
