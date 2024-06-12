#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh filtered {toxval.db} {sys.date}.xlsx
#' @export 
#' @title filter.for.bmdh
#' @description Filter the exported records for redundancy
#' @details Filters redundant rows in the raw database export. There are
#' two kinds of redundancy. The first filters extra reference rows from the record_source table.
#' The main data is in the toxval tables, and references are linked through the toxval_id to
#' the record_source table. During the curation process, these references get
#' cleaned and hence repeated, so a single (and the final or best) is selected.
#' Then, there are redundancies in the toxval table itself, likely caused by upstream
#' processing issues. These will be solved there, but the filtering
#' in this function takes care of this issue
#' for the moment.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[digest]{digest}}
#' @rdname filter.for.bmdh
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#' @importFrom digest digest
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when

#-----------------------------------------------------------------------------------
filter.for.bmdh <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",sys.date,".xlsx")
  print(file)

  raw = file %>%
    readxl::read_xlsx() %>%
    dplyr::mutate(level = dplyr::case_when(
      record_source_level=="primary (risk assessment values)" ~ 4,
      record_source_level=="-" ~ 3,
      record_source_level=="extraction" ~ 2,
      record_source_level=="origin" ~ 1,
      TRUE ~ 0
    ))

  cat("nrow raw:",nrow(raw),"\n")
  final = NULL
  slist = sort(unique(raw$source))
  # TODO Update hashing and reference selection logic
  for(source in slist) {
    t1 = raw[raw$source==source,]
    t2 = NULL
    cat(source,nrow(t1),"\n")
    shlist = unique(t1$source_hash)
    for(sh in shlist) {
      x1 = t1[t1$source_hash==sh,]
      x1 = x1[order(x1$level,decreasing=T),]
      t2 = rbind(t2,x1[1,])
    }
    cat(source,nrow(t2),"\n")
    exclude = c("long_ref","url","record_source_level","record_source_type","level","source_hash")
    t3 = t2[,!(names(t2) %in% exclude)]
    for(i in seq_len(nrow(t2))) t2[i,"hashkey"] = digest::digest(paste0(t3[i,],collapse=""), serialize = FALSE)
    if(length(unique(t2$hashkey))<nrow(t2)) {
      cat(">>>>>>> ",source," has redundancies <<<<<<<<<: ")
      t4 = NULL
      for(hk in unique(t2$hashkey)) {
        x1 = t2[t2$hashkey==hk,]
        t4 = rbind(t4,x1[1,])
      }
      cat(nrow(t4),"\n")
      final = rbind(final,t4)
    }
    else {
      final = rbind(final,t2)
    }
  }
  file = paste0(dir,"results/ToxValDB for BMDh filtered ",toxval.db," ",sys.date,".xlsx")
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(final,file,firstRow=T,headerStyle=sty)
}
