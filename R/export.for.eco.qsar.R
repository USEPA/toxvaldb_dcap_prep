#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param source The source to be updated
#' @return Write a file with the results
#' @title export.for.eco.qsar
#' @description Build a data frame of the data for the toxval manuscript
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{write.table}}
#'  \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname export.for.eco.qsar
#' @export
#' @importFrom utils write.csv
#' @importFrom openxlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
export.for.eco.qsar <- function(toxval.db,source=NULL) {
  printCurrentFunction(toxval.db)
  dir = "data/eco_qsar/"
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]

  slist = c("ECOTOX","EnviroTox_v2")

  res = NULL
  if(!is.null(source)) slist = source

  for(src in slist) {
    n = runQuery(paste0("select count(*) from toxval where human_eco='eco' and source='",src,"'"),toxval.db)[1,1]
    cat(src,":",n,"\n")
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,
                    b.source,
                    b.toxval_type,
                    e.toxval_type_supercategory,
                    b.toxval_numeric_qualifier,
                    b.toxval_numeric,
                    b.toxval_units,
                    b.study_type,
                    b.study_duration_value,
                    b.study_duration_units,
                    d.species_id,d.common_name,d.latin_name,d.ecotox_group,
                    b.exposure_route,
                    b.year,
                    f.long_ref,
                    f.title,
                    f.author,
                    b.source_hash,
                    a.cleaned_casrn,a.cleaned_name
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
                    #and b.exposure_route='aqueous'
                    and toxval_type_supercategory in (
                    'Point of Departure',
                    'Lethality Effect Level',
                    'Exposure Limit',
                    'Effect Level',
                    'Effect Concentration',
                    'Inhibition Concentration'
                    )
                    #limit 10000
                   ")
    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    mat[is.na(mat$casrn),"casrn"] = mat[is.na(mat$casrn),"cleaned_casrn"]
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[is.na(mat$name),"name"] = mat[is.na(mat$name),"cleaned_name"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
    cat(source,nrow(mat),"\n")
    cremove = c("cleaned_name","cleaned_casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    if(nrow(mat)>0) {
      # mat0 = mat
      # nlist = c("dtxsid","source","subsource","risk_assessment_class","toxval_units","study_type","study_duration_class",
      #           "study_duration_value","study_duration_units","common_name","strain","sex","exposure_route",
      #           "exposure_method","year","long_ref","ref_year","title","author","journal","volume","issue")
      # temp = mat[,nlist]
      # mat$hashkey = NA
      # mat$study_group = NA
      # for(i in 1:nrow(mat)) mat[i,"hashkey"] = digest(paste0(temp[i,],collapse=""), serialize = FALSE)
      # hlist = unique(mat$hashkey)
      # for(i in 1:length(hlist)) {
      #   sg = paste0(src,"_",i)
      #   mat[mat$hashkey==hlist[i],"study_group"] = sg
      # }
      # mat = mat[!is.element(mat$dtxsid,c("-","none","NODTXSID")),]
      res = rbind(res,mat)
      #cat("   ",nrow(mat),length(hlist),nrow(res),"\n")
    }
  }

  res = res[!is.element(res$study_duration_units,c("generations","brood or litter")),]
  # minutes
  x1 = res[res$study_duration_units=="minutes",]
  if(nrow(x1)>0) {
    x2 = res[res$study_duration_units!="minutes",]
    x1$study_duration_units = "days"
    x1$study_duration_value = x1$study_duration_value/1440
    res = rbind(x1,x2)
  }

  # hours
  x1 = res[res$study_duration_units=="hours",]
  if(nrow(x1)>0) {
    x2 = res[res$study_duration_units!="hours",]
    x1$study_duration_units = "days"
    x1$study_duration_value = x1$study_duration_value/24
    res = rbind(x1,x2)
  }

  # weeks
  x1 = res[res$study_duration_units=="weeks",]
  if(nrow(x1)>0) {
    x2 = res[res$study_duration_units!="weeks",]
    x1$study_duration_units = "days"
    x1$study_duration_value = x1$study_duration_value*7
    res = rbind(x1,x2)
  }

  # months
  x1 = res[res$study_duration_units=="months",]
  if(nrow(x1)>0) {
    x2 = res[res$study_duration_units!="months",]
    x1$study_duration_units = "days"
    x1$study_duration_value = x1$study_duration_value*30
    res = rbind(x1,x2)
  }

  # years
  x1 = res[res$study_duration_units=="years",]
  if(nrow(x1)>0) {
    x2 = res[res$study_duration_units!="years",]
    x1$study_duration_units = "days"
    x1$study_duration_value = x1$study_duration_value*365
    res = rbind(x1,x2)
  }

  x1 = res[res$source=="ECOTOX",]
  x2 = res[res$source=="EnviroTox_v2",]
  slist = sort(unique(x2$common_name))
  for(species in slist) {
    if(is.element(species,x1$common_name)) {
      route = unique(x1[x1$common_name==species,"exposure_route"])
      if(is.element("aqueous",route)) x2[x2$common_name==species,"exposure_route"] = "aqueous"
    }
  }

  res = rbind(x1,x2)

  res = res[res$exposure_route=="aqueous",]
  res = res[res$study_duration_value>0,]
  res = res[res$study_duration_units=="days",]
  exclude.list = c("ATCN","LETC","MCIG")
  res = res[!is.element(res$toxval_type,exclude.list),]
  res = res[res$toxval_numeric_qualifier=="=",]

  # res = subset(res,select= -c(hashkey))
  if(is.null(source)) {
    file = paste0(dir,"/toxval_eco_qsar_",toxval.db," ",Sys.Date(),".RData")
    save(res,file=file)
    file = paste0(dir,"/toxval_eco_qsar_",toxval.db," ",Sys.Date(),".csv")
    utils::write.csv(res,file,row.names=F)
  }
  else {
    sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
    file = paste0(dir,"/toxval_eco_qsar ",toxval.db," ",Sys.Date()," ",source,".xlsx")
    openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
  }
}
