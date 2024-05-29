#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is #' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @export
#' @title export.for.dcap.qc
#' @description Export records required for performing QC on the ToxValDB DCAP sources.
#' @details Exports all of the data required for the doing DCAP QC calculations.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname export.for.dcap.qc
#' @importFrom openxlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
export.for.dcap.qc <- function(toxval.db="res_toxval_v95",user="_dataminer",password="pass") {
  printCurrentFunction(toxval.db)
  dir = "data/"
  setDBConn(user=user,password=password)
  slist = c("ATSDR MRLs","ATSDR PFAS 2021","Copper Manufacturers",
            "ECHA IUCLID","ECOTOX","EFSA","HAWC PFAS 150","HAWC PFAS 430",
            "HAWC Project","HEAST","HESS","HPVIS","IRIS","NTP PFAS",
            "PFAS 150 SEM v2","PPRTV (CPHEA)","ToxRefDB","WHO JECFA Tox Studies")
  res = NULL
  for(src in slist) {
    n = runQuery(paste0("select count(*) from toxval where source='",src,"'"),toxval.db)[1,1]
    cat(src,":",n,"\n")

    # Query for source data from ToxVal
    query = paste0("SELECT
                      a.dtxsid,a.casrn,a.name,
                      b.source,
                      b.toxval_type,b.toxval_type_original,
                      b.toxval_subtype,b.toxval_subtype_original,
                      b.toxval_numeric_qualifier,b.toxval_numeric_qualifier_original,
                      b.toxval_numeric,b.toxval_numeric_original,
                      b.toxval_units,b.toxval_units_original,
                      b.study_type,b.study_type_original,
                      b.study_duration_value,b.study_duration_value_original,
                      b.study_duration_units,b.study_duration_units_original,
                      b.study_duration_class,b.study_duration_class_original,
                      d.common_name,b.species_original,
                      b.strain, b.strain_original, b.strain_group,
                      b.sex,b.sex_original,
                      b.lifestage,b.lifestage_original,
                      b.generation,b.generation_original,
                      b.exposure_route,b.exposure_route_original,
                      b.critical_effect,b.critical_effect_original,
                      b.year,
                      f.long_ref,
                      f.title,
                      f.pmid,
                      f.guideline,
                      f.record_source_level,
                      f.record_source_type,
                      f.priority,
                      b.source_hash,
                      b.study_group,
                      b.qc_status,
                      b.experimental_record,
                      a.cleaned_casrn,a.cleaned_name
                      FROM
                      toxval b
                      INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                      LEFT JOIN species d on b.species_id=d.species_id
                      INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                      INNER JOIN record_source f on b.toxval_id=f.toxval_id
                      WHERE
                      b.source='",src,"'
                      and b.human_eco='human health'
                      and e.toxval_type_supercategory in ('Point of Departure')
                      and b.toxval_units='mg/kg-day'
                      and b.exposure_route='oral'
                     ")


    mat = runQuery(query,toxval.db,T,F)

    # Prepare data
    if(src!="ECOTOX") mat = mat[mat$priority==1,]
    mat = unique(mat)
    #cat("[1]",nrow(mat),"\n")
    #mat[is.na(mat$toxval_numeric_qualifier),"toxval_numeric_qualifier"] = "="
    #mat[is.element(mat$toxval_numeric_qualifier,c("~","<","<=")),"toxval_numeric_qualifier"] = "="
    mat = mat[mat$toxval_numeric_qualifier=="=",]
    #cat("[3]",nrow(mat),"\n")

    exclude.list = c("HNEL","LEC","NOTEL","POD (HE)","POD (HEC)","POD (surrogate)","critical value",
                     "LOEC","NOEC","LOAEC","NOAEC","BMD (HEC)","BMDL (HEC)","POD (01 HED)","POD (HED)","LED")
    mat = mat[!is.element(mat$toxval_type,exclude.list),]
    #cat("[3]",nrow(mat),"\n")

    exclude.list = c("Hershberger","acute","clinical","dose selection","in vitro","neurotoxicity acute","uterotrophic")
    mat = mat[!is.element(mat$study_type,exclude.list),]
    #cat("[4]",nrow(mat),"\n")

    mat[mat$common_name=="European Rabbit","common_name"] = "Rabbit"
    mat[mat$common_name=="House Mouse","common_name"] = "Mouse"
    mat[mat$common_name=="Rabbit Family","common_name"] = "Rabbit"
    mat = mat[is.element(mat$common_name,c("Rat","Mouse","Dog","Rabbit","Human")),]

    mat[is.na(mat$casrn),"casrn"] = mat[is.na(mat$casrn),"cleaned_casrn"]
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[is.na(mat$name),"name"] = mat[is.na(mat$name),"cleaned_name"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
    cat(src,nrow(mat),"\n\n")
    cremove = c("cleaned_name","cleaned_casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    res = rbind(res,mat)
  }
  # Write data to file
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"DCAP/ToxValDB DCAP QC ",toxval.db," ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
