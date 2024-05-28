#-----------------------------------------------------------------------------------
#' @#' Filter the exported records for redundancy of NO(A)EL / LO(A)EL PODs in a study group
#'
#' `filter.for.multi.noel` Filters where multiple NOEL/NOEL etc. exist. For each study_group
#' this will select the highest NO(A)EL below the lowest LO(A)EL and the lowest LO(A)EL.
#' In all cases, all BMDx values are included
#'
#' @param toxval.db Database version
#' @param sys.date The date of the export
#' @return Write a file with the filtered results:ToxValDB for BMDh LEL NEL multiNOEL filtered {toxval.db} {sys.date}.xlsx
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
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname filter.for.multi.noel
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#-----------------------------------------------------------------------------------
filter.for.multi.noel <- function(toxval.db="res_toxval_v95",sys.date="2024-05-20") {
  printCurrentFunction(toxval.db)
  dir = "data/"
  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL filtered ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res0 = openxlsx::read.xlsx(file)

  slist = unique(res0$source)
  res = NULL
  for(i in 1:length(slist)) {
    src = slist[i]
    t0 = res0[is.element(res0$source,src),]
    #if(is.element(src,c("ATSDR PFAS 2021","ECHA IUCLID","EFSA","HAWC PFAS 430",
    #                    "HAWC Project","HESS","HPVIS","NTP PFAS","PFAS 150 SEM v2"))) {
    #if(is.element(src,c("HESS","NTP PFAS","PFAS 150 SEM v2"))) {
      #cat(src,"\n")
      t1 = NULL
      sglist = unique(t0$study_group)


      for(sg in sglist) {
        t2 = t0[is.element(t0$study_group,sg),]
        #---------------------------------------------------
        # Only a single POD for the study group
        #---------------------------------------------------
        if(nrow(t2)==1) {
          t1 = rbind(t1,t2)
        }
        #---------------------------------------------------
        # Multiple PODs for the study group
        #---------------------------------------------------
        else {
          #------------------------------------------------------------------
          # Single NOEL/LOEL or NOAEL/LOAEL pair of PODs for the study group
          #------------------------------------------------------------------
          if(paste(sort(t2$toxval_type),collapse="|")==paste(c("LOEL","NOEL"),collapse="|") || paste(sort(t2$toxval_type),collapse="|")==paste(c("LOAEL","NOAEL"),collapse="|")) {
            t1 = rbind(t1,t2)
          }
          #------------------------------------------------------------------
          # More than one NO(A)EL or LO(A)EL for the study group
          #------------------------------------------------------------------
          else {
            t2$tttype = t2$toxval_type
            t2$tttype = substr(t2$tttype,1,1)
            nset = t2[t2$tttype=="N",]
            lset = t2[t2$tttype=="L",]
            bset = t2[t2$tttype=="B",]

            nset = nset[order(nset$toxval_numeric,decreasing=T),]
            lset = lset[order(lset$toxval_numeric),]
            nset0 = nset
            lset0 = lset
            #------------------------------------------------------------------
            # At least one LO(A)EL for the study group
            #------------------------------------------------------------------
            if(nrow(lset)>0) {
              t3 = lset[1,]
              nset = nset[nset$toxval_numeric<=lset[1,"toxval_numeric"],]
              if(nrow(nset)>0) t3 = rbind(t3,nset[1,])
              if(nrow(bset)>0) t3 = rbind(t3,bset)
              t3 = t3[,names(t0)]
            }
            #------------------------------------------------------------------
            # Only NO(A)ELs for the study group
            #------------------------------------------------------------------
            else if(nrow(nset)>0) {
              t3 = nset[1,]
              if(nrow(bset)>0) t3 = rbind(t3,bset)
              t3 = t3[,names(t0)]
            }
            #------------------------------------------------------------------
            # Only BMDs for the study group
            #------------------------------------------------------------------
            else {
              t3 = bset
              t3 = t3[,names(t0)]
            }
            t1 = rbind(t1,t3)
          }
        }
      }
      res = rbind(res,t1)
      cat(src,nrow(t0),nrow(t1),length(unique(t0$dtxsid)),length(unique(t1$dtxsid)),"\n")
    #}
    #else res = rbind(res,t0)
  }

  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
  sty = openxlsx::createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
