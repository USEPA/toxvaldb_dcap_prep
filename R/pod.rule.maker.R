#' @title pod.rule.maker
#' @description Function to create/choose rule based on POD inputs
#' @param toxval Input dataframe of ToxVal data.
#' @param hra.sources Human Risk Assessment source list
#' @param rule.name PARAM_DESCRIPTION, Default: 'Rule 3'
#' @param rule.stype PARAM_DESCRIPTION, Default: 'chronic'
#' @param rule.ttype PARAM_DESCRIPTION, Default: 'LO(A)EL, NO(A)EL, BMD'
#' @param stype.list PARAM_DESCRIPTION, Default: c("chronic")
#' @param ttype.list PARAM_DESCRIPTION, Default: c(loel.types, noel.types, bmdl.types)
#' @return Modified version of input toxval dataframe.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{quantile}}, \code{\link[stats]{sd}}
#' @rdname pod.rule.maker
#' @export
#' @importFrom stats quantile sd
pod.rule.maker <- function(toxval,hra.sources,
                       rule.name="Rule 3",
                       rule.stype="chronic",
                       rule.ttype ="LO(A)EL, NO(A)EL, BMD",
                       stype.list=c("chronic"),
                       ttype.list=c(loel.types,noel.types,bmdl.types)) {
  printCurrentFunction(rule.name)

  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  filter.vals <- function(t2) {
    t3 = NULL
    sg.list = unique(t2$study_group)
    for(sg in sg.list) {
      t4 = t2[t2$study_group==sg,]
      t4.noel = t4[is.element(t4$toxval_type,noel.types),]
      t4.loel = t4[is.element(t4$toxval_type,loel.types),]
      t4.bmdl = t4[is.element(t4$toxval_type,bmdl.types),]
      noel = F
      if(nrow(t4.noel)>0) {
        t4.noel = t4.noel[order(t4.noel$toxval_numeric,decreasing=FALSE),]
        t4.noel = t4.noel[1,]
        noel = T
      }
      loel = F
      if(nrow(t4.loel)>0) {
        t4.loel = t4.loel[order(t4.loel$toxval_numeric),]
        t4.loel = t4.loel[1,]
        loel = T
      }
      bmdl = F
      if(nrow(t4.bmdl)>0) {
        t4.bmdl = t4.bmdl[order(t4.bmdl$toxval_numeric),]
        t4.bmdl = t4.bmdl[1,]
        bmdl = T
      }
      if(noel) t3 = rbind(t3,t4.noel)
      else {
        if(loel) t3 = rbind(t3,t4.loel)
      }
      if(bmdl) t3 = rbind(t3,t4.bmdl)
    }
    return(t3)
  }
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  allometric.scaling <- function(t2,scale.mat) {
    t2$scaled_pod = NA
    for(j in seq_len(nrow(t2))) {
      species = t2[j,"common_name"]
      tv = t2[j,"toxval_numeric"]
      tv = tv * scale.mat[species,"factor"]
      t2[j,"scaled_pod"] = tv
    }
    return(t2)
  }
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  t1 = toxval[is.element(toxval$study_type,stype.list),]
  t1 = t1[is.element(t1$toxval_type,ttype.list),]

  res1 = unique(t1[,c("dtxsid","name")])
  res1$rule = rule.name
  res1$stype = rule.stype
  res1$ttype = rule.ttype
  res1$npod = NA
  res1$pod_sd = NA
  res1$pod_range = NA
  res1$pod10_dist = NA
  res1$pod10_experimental = NA

  res1$source = NA
  res1$toxval_type = NA
  res1$toxval_numeric = NA
  res1$toxval_units = NA
  res1$study_type = NA
  res1$common_name = NA
  res1$exposure_route = NA
  res1$study_group = NA
  res1$source_hash = NA

  res1$pod_hra = NA
  res1$pod_hra_source = NA
  for(i in seq_len(nrow(res1))) {
    dtxsid = res1[i,"dtxsid"]
    t2 = t1[t1$dtxsid==dtxsid,]
    t3 = filter.vals(t2)
    if(nrow(t3)>0) {
      t3 = allometric.scaling(t3,scale.mat)
      vals = log10(t3$scaled_pod)
      qv = stats::quantile(vals,probs=seq(0,1,0.1))[2]
      t3$delta = abs(log10(t3$scaled_pod)-qv)
      t3 = t3[order(t3$delta),]

      res1[i,"pod10_dist"] = 10**qv
      res1[i,"npod"] = length(vals)
      res1[i,"pod_sd"] = stats::sd(vals)
      res1[i,"pod_range"] = max(vals)-min(vals)
      res1[i,"source"] = t3[1,"source"]
      res1[i,"toxval_type"] = t3[1,"toxval_type"]
      res1[i,"toxval_numeric"] = t3[1,"toxval_numeric"]
      res1[i,"toxval_units"] = t3[1,"toxval_units"]
      res1[i,"study_type"] = t3[1,"study_type"]
      res1[i,"common_name"] = t3[1,"common_name"]
      res1[i,"exposure_route"] = t3[1,"exposure_route"]
      res1[i,"study_group"] = t3[1,"study_group"]
      res1[i,"source_hash"] = t3[1,"source_hash"]
      res1[i,"pod10_experimental"] = t3[1,"scaled_pod"]
      t4 = t3[is.element(t3$source,hra.sources),]
      if(nrow(t4)>0) {
        t4 = t4[order(t4$scaled_pod),]
        res1[i,"pod_hra"] = t4[1,"scaled_pod"]
        res1[i,"pod_hra_source"] = t4[1,"source"]
      }
    }
  }
  return(res1)
}
