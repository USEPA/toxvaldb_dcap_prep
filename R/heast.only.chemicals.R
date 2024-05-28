#-----------------------------------------------------------------------------------
#' Get the unique HEAST chemicals
#'
#' `heast.only.chemicals` Exports The unique chemicals in HEAST that are not
#' in IRIS or PPRTV
#'
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is
#' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: HEAST/HEAST unique chemicals.xlsx
#' @export
#-----------------------------------------------------------------------------------
heast.only.chemicals <- function(toxval.db="res_toxval_v95",user="_dataminer",password="pass") {
  toxvaldbBMDh::printCurrentFunction(toxval.db)
  dir = "data/HEAST/"
  toxvaldbBMDh::setDBConn(user=user,password=password)

  query = paste0("SELECT
                  a.dtxsid,a.casrn,a.name
                  FROM
                  toxval b
                  INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                  WHERE
                  b.source='HEAST'
                  and human_eco='human health'
                ")
  chems = toxvaldbBMDh::runQuery(query,toxval.db)
  chems = unique(chems)
  cat(nrow(chems),"\n")
  exclude = toxvaldbBMDh::runQuery("select distinct dtxsid from toxval where source in ('IRIS','PPRTV (CPHEA)')",toxval.db)[,1]
  chems2 = chems[!is.element(chems$dtxsid,exclude),]
  cat(nrow(chems2),"\n")
  file = paste0(dir,"HEAST-only chemicals.xlsx")
  openxlsx::write.xlsx(chems2,file)
}
