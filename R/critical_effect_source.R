#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @export
#' @title critical_effect_source
#' @description Find the source for odd critical effect chunks.
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
#' @rdname critical_effect_source
#' @importFrom openxlsx read.xlsx write.xlsx
#-----------------------------------------------------------------------------------
critical_effect_source <- function(toxval.db="res_toxval_v95",user, password) {
  printCurrentFunction(toxval.db)
  dir = "data/critical_effects/"
  setDBConn(user=user,password=password)
  file = paste0(dir,"devito other calls with comments.xlsx")
  print(file)
  input = openxlsx::read.xlsx(file)
  res = NULL
  for(i in seq_len(nrow(input))) {
    term = input[i,"term"]
    comment = input[i,"comment"]
    cat(i,term,"\n")
    query = paste0("SELECT
                    a.dtxsid,a.name,
                    b.source,
                    b.critical_effect_original,
                    b.critical_effect,
                    b.study_type,
                    b.toxval_type,
                    f.long_ref,
                    f.url,
                    f.pmid
                    FROM
                    toxval b
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    LEFT JOIN species d on b.species_id=d.species_id
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    WHERE
                    b.critical_effect_original like '%",term,"%'")
    temp = runQuery(query,toxval.db)
    if(nrow(temp)>0) {
      temp$term = term
      temp$comment = comment
      res = rbind(res,temp)
    }
  }
  file = paste0(dir,"critical_effect_source.xlsx")
  openxlsx::write.xlsx(res,file)
}
