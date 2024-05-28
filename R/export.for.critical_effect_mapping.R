#-----------------------------------------------------------------------------------
#' @#' Export records required for managing the critical effect categories for the BMD calculations.
#'
#' `export.for.critical_effect_mapping` Exports all of the data required for
#' performing teh criticla effect mapping for the BMDh calculations
#'
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @export 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname export.for.critical_effect_mapping
#' @importFrom openxlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
export.for.critical_effect_mapping <- function(toxval.db="res_toxval_v95",user="rjudson",password) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  setDBConn(user=user,password=password)
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  exclude.sources = c("COSMOS","EFSA","PPRTV (NCEA)","ECHA IUCLID")
  slist = slist[!is.element(slist,exclude.sources)]
  res = NULL
  for(src in slist) {
    n = runQuery(paste0("select count(*) from toxval where source='",src,"'"),toxval.db)[1,1]
    cat(src,":",n,"\n")
    query = paste0("SELECT
                      a.dtxsid,a.casrn,a.name,
                      b.source,
                      b.toxval_type,
                      b.toxval_subtype,
                      b.study_type,
                      d.common_name,
                      b.critical_effect_original,
                      b.critical_effect,
                      a.cleaned_casrn,a.cleaned_name
                      FROM
                      toxval b
                      INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                      LEFT JOIN species d on b.species_id=d.species_id
                      INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                      WHERE
                      b.source='",src,"'
                      and b.human_eco='human health'
                      and e.toxval_type_supercategory in ('Point of Departure')
                      and b.toxval_units='mg/kg-day'
                      and b.exposure_route='oral'
                     ")

    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)

    ttlist = c( "NOAEL (HED)","NOAEL (HEC)","NOAEL","NOEL",
              "LOAEL (HED)","LOAEL (HEC)","LOAEL","LOEL",
              "BMD","BMD (10 LED)","BMD (1SD)","BMD (2.5)","BMD (2X)","BMD (50)","BMDL","BMDL (0.5 SD)","BMDL (0.5)","BMDL (01 HEC)",
              "BMDL (01 HED)","BMDL (01)","BMDL (05 HED)","BMDL (05)","BMDL (10 ADJ)","BMDL (10 HEC)","BMDL (10 HED)",
              "BMDL (10)","BMDL (1SD ADJ)","BMDL (1SD HEC)","BMDL (1SD HED)","BMDL (1SD)","BMDL (2X ADJ)",
              "BMDL (2X)","BMDL (5 RD)","BMDL (50)","BMDL (5RD HED)","BMDL (ADJ)","BMDL (HEC)","BMDL (HED)")
    mat = mat[is.element(mat$toxval_type,ttlist),]

    stlist = c("Not specified","epidemiology","genetics","human","occupational","repeat dose other")
    mat = mat[!is.element(mat$study_type,stlist),]


    exclude.list = c("-","Hershberger","acute","clinical","dose selection","in vitro","neurotoxicity acute","uterotrophic")
    mat = mat[!is.element(mat$study_type,exclude.list),]

    mat[mat$common_name=="European Rabbit","common_name"] = "Rabbit"
    mat[mat$common_name=="House Mouse","common_name"] = "Mouse"
    mat[mat$common_name=="Rabbit Family","common_name"] = "Rabbit"
    mat = mat[is.element(mat$common_name,c("Rat","Mouse","Dog","Rabbit","Human")),]

    elist = c("endpoint species LOAEL food","endpoint species LOAEL food, water",
              "endpoint species NOAEL food, water","endpoint species NOAEL food, water, piscivore")
    mat = mat[!is.element(mat$toxval_subtype,elist),]

    mat[is.na(mat$casrn),"casrn"] = mat[is.na(mat$casrn),"cleaned_casrn"]
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[is.na(mat$name),"name"] = mat[is.na(mat$name),"cleaned_name"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
    cat(src,nrow(mat),"\n\n")
    cremove = c("cleaned_name","cleaned_casrn","casrn","toxval_subtype")
    mat = mat[ , !(names(mat) %in% cremove)]
    res = rbind(res,mat)
  }
  res$adverse = "Y"
  res$standard_effect_1 = NA
  res$standard_effect_2 = NA
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"results/ToxValDB for critical effect ",toxval.db," ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
