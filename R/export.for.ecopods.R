#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param do.load If TRUE, read from the database into a global
#' @return Write a file with the results
#' @title export.for.ecopods
#' @description Export records required for calculating POD values for ecotox QSAR models.
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @details The data is exported as a xlsx and csv file because some records may cause the Excel format
#' to break. You can then open the csv (always works) and save as Excel
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[utils]{write.table}}
#' @rdname export.for.ecopods
#' @export 
#' @importFrom openxlsx createStyle write.xlsx
#' @importFrom utils write.csv
#-----------------------------------------------------------------------------------
export.for.ecopods <- function(toxval.db="res_toxval_v95",do.load=T,user="rjudson",password) {
  printCurrentFunction(toxval.db)
  dir = "data/ecoqsarpods/"
  setDBConn(user=user,password=password)
  if(do.load) {
    slist = runQuery("select distinct source from toxval",toxval.db)[,1]
    exclude.list = c("COSMOS","EFSA","PPRTV (NCEA)","Chiu","DOE Wildlife Benchmarks")
    slist = slist[!is.element(slist,exclude.list)]
    res = NULL
    for(src in slist) {
      n = runQuery(paste0("select count(*) from toxval where source='",src,"'"),toxval.db)[1,1]
      cat(src,":",n,"\n")
      query = paste0("SELECT
                      a.dtxsid,a.casrn,a.name,
                      b.source,
                      b.toxval_type,
                      b.toxval_numeric_qualifier,
                      b.toxval_numeric,
                      b.toxval_units,
                      b.study_type,
                      b.study_duration_value,
                      b.study_duration_units,
                      b.study_duration_class,
                      d.common_name,
                      d.ecotox_group,
                      b.exposure_route,
                      b.critical_effect,
                      b.critical_effect_original,
                      b.year,
                      f.long_ref,
                      f.url,
                      b.source_hash,
                      b.study_group
                       FROM
                      toxval b
                      INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                      LEFT JOIN species d on b.species_id=d.species_id
                      INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                      INNER JOIN record_source f on b.toxval_id=f.toxval_id
                      WHERE
                      b.source='",src,"'
                      and b.human_eco='eco'
                      and b.toxval_units='mg/L'
                      and b.qc_status='pass'
                      and d.ecotox_group in  ('Fish','Crustaceans','Algae','Molluscs','Invertebrates','Amphibians','Moss, Hornworts','Flowers, Trees, Shrubs, Ferns','Fungi','Archea','Aquatic community')
                    ")
      #and e.toxval_type_supercategory in ('Point of Departure')

      mat = runQuery(query,toxval.db,T,F)
      mat = unique(mat)
      mat[is.na(mat$toxval_numeric_qualifier),"toxval_numeric_qualifier"] = "="
      mat[is.element(mat$toxval_numeric_qualifier,c("~","<","<=")),"toxval_numeric_qualifier"] = "="
      mat = mat[mat$toxval_numeric_qualifier=="=",]

      cat(src,nrow(mat),"\n")
      res = rbind(res,mat)
    }
    RES <<- res
  }
  res = RES
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"ToxValDB Eco ",toxval.db," ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
  file = paste0(dir,"ToxValDB Eco ",toxval.db," ",Sys.Date(),".csv")
  utils::write.csv(res,file=file,row.names=F)
}
