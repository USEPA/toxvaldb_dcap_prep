#-----------------------------------------------------------------------------------
#' Export records required for calculating BMDh values.
#'
#' `export.for.bmdh` Exports all of the data required for the BMDh calculations.
#' The main query may need to be modified to extract more columns if needed for
#' the final application. Certain sources have been excluded because they have a high
#' percentage of read-across values. Species are filtered to only include Human,
#' Dog, Mouse, Rat and Rabbit. If more species are to be included, then allometric
#' scaling factors for those need to added to the function bmd.per.study().
#'
#' @param toxval.db Database name
#' @param user The username for the MySQL database. The database instance is
#' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @export
#-----------------------------------------------------------------------------------
export.for.bmdh <- function(toxval.db="res_toxval_v95",user, password) {
  toxvaldbBMDh::printCurrentFunction(toxval.db)
  dir = "data/"
  toxvaldbBMDh::setDBConn(user=user,password=password)

  slist = c("ATSDR PFAS 2021","ATSDR MRLs","Copper Manufacturers",
           "ECHA IUCLID","ECOTOX","EFSA","HAWC PFAS 430",
            "HAWC Project","HEAST","HESS","HPVIS","IRIS",
            "NTP PFAS","PFAS 150 SEM v2","PPRTV (CPHEA)",
            "ToxRefDB","WHO JECFA Tox Studies")

  #slist = "ECOTOX"
  plist = vector(mode="integer",length=length(slist))
  plist[] = 1
  for(i in 1:length(slist)) {
    src = slist[i]
    query = paste0("select distinct priority from record_source where source='",src,"' and long_ref!='-'")
    vals = toxvaldbBMDh::runQuery(query,toxval.db)[,1]
    cat(src,paste(vals,collapse="|"),"\n")
    if(length(vals)>0) plist[i] = vals[1]
    else {
      if(src=="HEAST") plist[i] = 1
      if(src=="HESS") plist[i] = 1
      if(src=="ECHA IUCLID") plist[i] = 1
    }
  }
  res = NULL
  for(i in 1:length(slist)) {
    src = slist[i]
    priority = plist[i]
    query = paste0("SELECT
                      a.dtxsid,a.casrn,a.name,
                      b.source,
                      b.toxval_type,
                      b.toxval_subtype,
                      b.toxval_numeric_qualifier,
                      b.toxval_numeric,
                      b.toxval_units,
                      b.study_type,
                      b.study_duration_value,
                      b.study_duration_units,
                      b.study_duration_class,
                      d.common_name,
                      b.strain,
                      b.sex,
                      b.lifestage,
                      b.generation,
                      b.exposure_route,
                      b.exposure_method,
                      b.exposure_form,
                      b.critical_effect,
                      b.year,
                      f.long_ref,
                      f.url,
                      f.title,
                      f.pmid,
                      f.guideline,
                      f.record_source_level,
                      f.record_source_type,
                      f.priority,
                      f.clowder_doc_id,
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
                      #!and b.human_eco='human health'
                      and e.toxval_type_supercategory in ('Point of Departure')
                      and b.toxval_units='mg/kg-day'
                      and b.exposure_route='oral'
                      #!and f.priority=",priority,"
                     ")


    mat = toxvaldbBMDh::runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    cat("[1]",src,":",nrow(mat),"\n")
    mat[is.na(mat$toxval_numeric_qualifier),"toxval_numeric_qualifier"] = "ns"
    #mat[is.element(mat$toxval_numeric_qualifier,c("~","<","<=")),"toxval_numeric_qualifier"] = "="
    #mat = mat[mat$toxval_numeric_qualifier=="=",]

    exclude.list = c("HNEL","LEC","NOTEL","POD (HE)","POD (HEC)","POD (surrogate)","critical value",
                     "LOEC","NOEC","LOAEC","NOAEC","BMD (HEC)","BMDL (HEC)","POD (01 HED)","POD (HED)","LED","POD (99 HED)")
    mat = mat[!is.element(mat$toxval_type,exclude.list),]
    cat("[2]",src,":",nrow(mat),"\n")

    stlist = c(
      "short-term",
      "subchronic",
      "28-day",
      "chronic",
      "repeat dose other",
      "developmental",
      "reproduction",
      "reproduction developmental",
      "immunotoxicity",
      "immunotoxicity 28-day",
      "immunotoxicity chronic",
      "immunotoxicity short-term",
      "immunotoxicity subchronic",
      "intermediate",
      "neurotoxicity",
      "neurotoxicity 28-day",
      "neurotoxicity chronic",
      "neurotoxicity short-term",
      "neurotoxicity subchronic"
     )

    if(!is.element(src,c("HEAST"))) mat = mat[is.element(mat$study_type,stlist),]
    cat("[3]",src,":",nrow(mat),"\n")

    mat[mat$common_name=="European Rabbit","common_name"] = "Rabbit"
    mat[mat$common_name=="Norway Rat","common_name"] = "Rat"
    mat[mat$common_name=="Domestic Dog","common_name"] = "Dog"
    mat[mat$common_name=="House Mouse","common_name"] = "Mouse"
    #mat[mat$common_name=="Rabbit Family","common_name"] = "Rabbit"
    mat = mat[is.element(mat$common_name,c("Rat","Mouse","Dog","Rabbit","Human")),]

    mat[is.na(mat$casrn),"casrn"] = mat[is.na(mat$casrn),"cleaned_casrn"]
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[is.na(mat$name),"name"] = mat[is.na(mat$name),"cleaned_name"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
    cremove = c("cleaned_name","cleaned_casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    cat("[4]",src,":",nrow(mat),"\n\n")
    res = rbind(res,mat)
  }
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
