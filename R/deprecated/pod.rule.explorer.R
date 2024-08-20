library(openxlsx)
#-------------------------------------------------------------------------------
#' Explore different methods for calculating PODs
#' run pod.calc.v2() first
#' @param dir The directory where the lists are stored
#-------------------------------------------------------------------------------
pod.rule.explorer <- function(toxval.db="res_toxval_v95",sys.date="2023-10-24") {
  printCurrentFunction()
  setDBConn(user=user,password=password)
  dir = "data/input/pods/"
  if(!exists("TOXVAL")) {
    cat("read in ToxValDB data\n")
    file = paste0(dir,"toxval repeat dose plus ",toxval.db," ",sys.date,".xlsx")
    print(file)
    res = read.xlsx(file)
    TOXVAL <<- res
  }
  toxval = TOXVAL

  print(nrow(toxval))
  nlist = c("dtxsid","name","source","toxval_type","toxval_numeric","toxval_units","study_type",
            "common_name","exposure_route","study_group","source_hash")
  toxval = unique(toxval[,nlist])
  toxval = toxval[toxval$toxval_numeric>0,]
  print(nrow(toxval))

  toxval = toxval[!is.element(toxval$source,c("PPRTV (NCEA)","ATSDR MRLs 2022","EFSA","COSMOS","Chiu")),]
  print(nrow(toxval))
  toxval = toxval[toxval$exposure_route=="oral",]
  print(nrow(toxval))
  toxval = toxval[toxval$toxval_units=="mg/kg-day",]
  print(nrow(toxval))
  toxval = toxval[is.element(toxval$common_name,c("Rat","Mouse","Dog","Rabbit","Human")),]
  humanized.pod.types=c("BMCL (10 HEC)","BMCL (1SD HEC)","BMCL (20 HED)","BMCL (Adjusted HE)","BMCL (HEC)","BMDL (05 HED)","BMDL (10 HED)","BMDL (1SD HED)")
  toxval[is.element(toxval$toxval_type,humanized.pod.types),"common_name"] = "Human"

  x = unique(toxval[,c("dtxsid","name")])
  dlist = x$dtxsid
  dups = dlist[duplicated(dlist)]
  for(dtxsid in dups) {
    y = x[x$dtxsid==dtxsid,]
    goodname = x[1,"name"]
    toxval[toxval$dtxsid==dtxsid,"name"] = goodname
  }
  x = unique(toxval[,c("dtxsid","name")])
  cat(nrow(x),length(unique(x$dtxsid)),"\n")

  noel.types <<- c("NOAEC","NOAEL","NOEC","NOEL")
  loel.types <<- c("LOAEC","LOAEL","LOEC","LOEL","LOAL")
  bmdl.types <<-  c("BMC","BMCL","BMCL (05 HEC)","BMCL (05)","BMCL (10 HEC)","BMCL (10 surrogate)",
                 "BMCL (10)","BMCL (1SD HEC)","BMCL (1SD)","BMCL (20 HED)","BMCL (Adjusted HE)",
                 "BMCL (HEC)","BMD","BMDL","BMDL (0.5 SD)","BMDL (01)","BMDL (05 HED)",
                 "BMDL (05)","BMDL (10 HED)","BMDL (10)","BMDL (1SD HED)","BMDL (1SD)",
                 "BMDL (50)","BMDL (HED)")
  repdose = c("chronic","developmental","neurotoxicity chronic","neurotoxicity subchronic",
              "reproduction","reproduction developmental","subchronic")
  repdose_plus = c(repdose,"28-day","neurotoxicity 28-day")
  res = NULL

  scale.mat = as.data.frame(matrix(nrow=5,ncol=2))
  names(scale.mat) = c("species","factor")
  scale.mat[,"species"] = c("Rat","Mouse","Dog","Rabbit","Human")
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4804402/
  # scale.mat[,"factor"] = c(6.2,12.3,1.8,3.1)
  # Appendix B of the BW3/4 allometric scaling guideline doc
  # rat     0.24
  # mouse   0.14
  # dog     0.63
  scale.mat[,"factor"] = c(0.24, 0.14, 0.63, 0.41, 1)
  rownames(scale.mat) = scale.mat$species
  scale.mat <<- scale.mat
  hra.sources = c("ATSDR PFAS 2021","HEAST","IRIS","PPRTV (CPHEA)")
  # res1 = rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 1",rule.stype="chronic",
  #                   rule.ttype ="NO(A)EL",
  #                   stype.list=c("chronic"),
  #                   ttype.list=c(noel.types))
  # res = rbind(res,res1)
  # res1 = rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 2",rule.stype="chronic",
  #                   rule.ttype ="LO(A)EL",
  #                   stype.list=c("chronic"),
  #                   ttype.list=c(loel.types))
  # res = rbind(res,res1)
  # res1 = rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 3",rule.stype="chronic",
  #                   rule.ttype ="LO(A)EL, NO(A)EL, BMD",
  #                   stype.list=c("chronic"),
  #                   ttype.list=c(loel.types,noel.types,bmdl.types))
  # res = rbind(res,res1)
  # res1 = rule.maker(toxval,hra.sources,
  #                   rule.name="Rule 4",rule.stype="repeat dose",
  #                   rule.ttype ="LO(A)EL, NO(A)EL, BMD",
  #                   stype.list=repdose,
  #                   ttype.list=c(loel.types,noel.types,bmdl.types))
  # res = rbind(res,res1)
  res1 = rule.maker(toxval,hra.sources,
                    rule.name="Rule 5",rule.stype="repeat dose+28 day",
                    rule.ttype ="LO(A)EL, NO(A)EL, BMD",
                    stype.list=repdose_plus,
                    ttype.list=c(loel.types,noel.types,bmdl.types))
  res = rbind(res,res1)

  file = paste0(dir,"toxval pods by rule.xlsx")
  write.xlsx(res,file)
}
#-------------------------------------------------------------------------------
#' Make the rule
#-------------------------------------------------------------------------------
rule.maker <- function(toxval,hra.sources,
                       rule.name="Rule 3",
                       rule.stype="chronic",
                       rule.ttype ="LO(A)EL, NO(A)EL, BMD",
                       stype.list=c("chronic"),
                       ttype.list=c(loel.types,noel.types,bmdl.types)) {
  printCurrentFunction(rule.name)
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
  for(i in 1:nrow(res1)) {
    dtxsid = res1[i,"dtxsid"]
    t2 = t1[t1$dtxsid==dtxsid,]
    t2 = filter.vals(t2)
    t2 = allometric.scaling(t2,scale.mat)

    vals = log10(t2$scaled_pod)
    qv = quantile(vals,probs=seq(0,1,0.1))[2]
    t2$delta = abs(log10(t2$scaled_pod)-qv)
    t2 = t2[order(t2$delta),]

    res1[i,"pod10_dist"] = 10**qv
    res1[i,"npod"] = length(vals)
    res1[i,"pod_sd"] = sd(vals)
    res1[i,"pod_range"] = max(vals)-min(vals)
    res1[i,"source"] = t2[1,"source"]
    res1[i,"toxval_type"] = t2[1,"toxval_type"]
    res1[i,"toxval_numeric"] = t2[1,"toxval_numeric"]
    res1[i,"toxval_units"] = t2[1,"toxval_units"]
    res1[i,"study_type"] = t2[1,"study_type"]
    res1[i,"common_name"] = t2[1,"common_name"]
    res1[i,"exposure_route"] = t2[1,"exposure_route"]
    res1[i,"study_group"] = t2[1,"study_group"]
    res1[i,"source_hash"] = t2[1,"source_hash"]
    res1[i,"pod10_experimental"] = t2[1,"scaled_pod"]
    t3 = t2[is.element(t2$source,hra.sources),]
    if(nrow(t3)>0) {
      t3 = t3[order(t3$scaled_pod),]
      res1[i,"pod_hra"] = t3[1,"scaled_pod"]
      res1[i,"pod_hra_source"] = t3[1,"source"]
    }
  }
  return(res1)
}
#-------------------------------------------------------------------------------
#' filter LOELs when NOELs are present for the same study
#' filter out redundant values for the same study group
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
#' Perform the allometric scaling
#-------------------------------------------------------------------------------
allometric.scaling <- function(t2,scale.mat) {
  t2$scaled_pod = NA
  for(j in 1:nrow(t2)) {
    species = t2[j,"common_name"]
    tv = t2[j,"toxval_numeric"]
    tv = tv * scale.mat[species,"factor"]
    t2[j,"scaled_pod"] = tv
  }
  return(t2)
}
