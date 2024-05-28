#-----------------------------------------------------------------------------------
#' @#' Count proerties of records
#'
#' `export.for.bmdh` Exports all of the data required for the BMDh calculations.
#' The main query may need to be modified to extract more columns if needed for
#' the final application. Certain sources have been excluded because they have a high
#' percentage of read-across values. Species are filtered to only include Human,
#' Dog, Mouse, Rat and Rabbit. If more species are to be included, then allometric
#' scaling factors for those need to added to the function bmd.per.study().
#'
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @export 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sys.date PARAM_DESCRIPTION, Default: '2024-03-26'
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
property.counter <- function(toxval.db="res_toxval_v95",sys.date="2024-03-26") {
  printCurrentFunction(toxval.db)
  dir = "data/"

  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",Sys.Date(),".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)
  nlist = c("dtxsid","casrn","name","source","toxval_type","toxval_subtype",
            "toxval_numeric_qualifier","toxval_numeric","toxval_units","study_type",
            "study_duration_value","study_duration_units","common_name","sex",
            "exposure_route","critical_effect","year","long_ref")
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
            "exposure_route","critical_effect","year","long_ref")
  x = x[,nlist]
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"counts/human studies.xlsx")
  openxlsx::write.xlsx(x,file,firstRow=T,headerStyle=sty)
}
