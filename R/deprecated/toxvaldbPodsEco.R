#-------------------------------------------------------------------------------
#' Calculate the eco PODs
#'
#' run toxvaldbExportHumanHealth first
#'
#' @param toxvaldb The version fo the ToxValDB to use
#' @param sys.date The date of the database export
#-------------------------------------------------------------------------------
toxvaldbPodsEco <- function(toxval.db="res_toxval_v95",sys.date="2024-02-14") {
  printCurrentFunction()
  if(!exists("TOXVALECO")) {
    dir = "data/ToxValDB/"
    cat("read in ToxValDB data\n")
    file = paste0(dir,"ToxValDB Eco ",toxval.db," ",sys.date,".xlsx")
    print(file)
    res = read.xlsx(file)
    TOXVALECO <<- res
  }
  toxval = TOXVALECO
  dir = "data/Biosolids/"

  file = paste0(dir,"Biosolids chemicals.xlsx")
  print(file)
  chems = read.xlsx(file)
  rownames(chems) = chems$dtxsid
  file = paste0(dir,"Biosolids Chemicals for PICS Screening.xlsx")
  print(file)
  chemselect = read.xlsx(file)
  chems = chems[is.element(chems$casrn,chemselect$CASRN),]
  #toxval = toxval[is.element(toxval$dtxsid,chems$dtxsid),]

  print(nrow(toxval))
  nlist = c("dtxsid","casrn","name","source","toxval_type","toxval_numeric","toxval_units","study_type",
            "common_name","ecotox_group","study_group","source_hash")
  toxval = unique(toxval[,nlist])
  toxval = toxval[toxval$toxval_numeric>0,]
  print(nrow(toxval))

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

  file = paste0(dir,"Biosolids ecosar.xlsx")
  print(file)
  ecosar = read.xlsx(file)
  ecosar.vertebrate = ecosar[is.element(ecosar$Organism,c("Fish (SW)","Fish")),]
  ecosar.invertebrate = ecosar[is.element(ecosar$Organism,c("Earthworm","Mysid (SW)","Mysid","Daphnid")),]
  ecosar.plant = ecosar[is.element(ecosar$Organism,c("Green Algae (SW)","Green Algae","Lemna gibba")),]

  file = paste0(dir,"Biosolids TEST.xlsx")
  print(file)
  test = read.xlsx(file)
  rownames(test) = test$dtxsid

  res = unique(toxval[,c("dtxsid","casrn","name")])
  res = res[order(res$name),]
  res$vertebrate_pod = NA
  res$vertebrate_npod = NA
  res$vertebrate_sd = NA
  res$vertebrate_range = NA
  res$vertebrate_source = NA

  res$invertebrate_pod = NA
  res$invertebrate_npod = NA
  res$invertebrate_sd = NA
  res$invertebrate_range = NA
  res$invertebrate_source = NA

  res$plant_pod = NA
  res$plant_npod = NA
  res$plant_sd = NA
  res$plant_range = NA
  res$plant_source = NA

  for(i in 1:nrow(res)) {
    dtxsid = res[i,"dtxsid"]
    t1 = toxval[toxval$dtxsid==dtxsid,]
    # vertebrate
    t2 = t1[is.element(t1$ecotox_group,c("Amphibians","Fish")),]
    if(nrow(t2)>0) {
      x = log10(t2$toxval_numeric)
      val = quantile(x,probs=seq(0,1,0.1))[2]
      res[i,"vertebrate_pod"] = 10**val
      res[i,"vertebrate_npod"] = length(x)
      res[i,"vertebrate_sd"] = sd(x)
      res[i,"vertebrate_range"] = max(x)-min(x)
      res[i,"vertebrate_source"] = "ToxValDB"
    }
    else if(is.element(dtxsid,ecosar.vertebrate$dtxsid)) {
      temp = ecosar.vertebrate[ecosar.vertebrate$dtxsid==dtxsid,]
      res[i,"vertebrate_pod"] = min(temp[is.element(temp$dtxsid,dtxsid),"Concentration.(mg/L)"])
      res[i,"vertebrate_source"] = "ECOSAR"
    }
    else if(is.element(dtxsid,test$dtxsid)) {
      val = test[dtxsid,"96HR_FATHEAD_MINNOW_MOL/L_TEST_PRED"]
      if(nchar(val)>2) {
        res[i,"vertebrate_pod"] = as.numeric(val)
        res[i,"vertebrate_source"] = "TEST"
      }
    }

    # invertebrate
    t2 = t1[is.element(t1$ecotox_group,c("Invertebrates","Crustaceans","Molluscs")),]
    if(nrow(t2)>0) {
      x = log10(t2$toxval_numeric)
      val = quantile(x,probs=seq(0,1,0.1))[2]
      res[i,"invertebrate_pod"] = 10**val
      res[i,"invertebrate_npod"] = length(x)
      res[i,"invertebrate_sd"] = sd(x)
      res[i,"invertebrate_range"] = max(x)-min(x)
      res[i,"invertebrate_source"] = "ToxValDB"
    }
    else if(is.element(dtxsid,ecosar.invertebrate$dtxsid)) {
      temp = ecosar.invertebrate[ecosar.invertebrate$dtxsid==dtxsid,]
      res[i,"invertebrate_pod"] = min(temp[is.element(temp$dtxsid,dtxsid),"Concentration.(mg/L)"])
      res[i,"invertebrate_source"] = "ECOSAR"
    }
    else if(is.element(dtxsid,test$dtxsid)) {
      val = test[dtxsid,"48HR_DAPHNIA_LC50_MOL/L_TEST_PRED"]
      if(nchar(val)>2) {
        res[i,"invertebrate_pod"] = as.numeric(val)
        res[i,"invertebrate_source"] = "TEST"
      }
    }

    # plant
    t2 = t1[is.element(t1$ecotox_group,c("Algae","Moss, Hornworts","Flowers, Trees, Shrubs, Ferns")),]
    if(nrow(t2)>0) {
      x = log10(t2$toxval_numeric)
      val = quantile(x,probs=seq(0,1,0.1))[2]
      res[i,"plant_pod"] = 10**val
      res[i,"plant_npod"] = length(x)
      res[i,"plant_sd"] = sd(x)
      res[i,"plant_range"] = max(x)-min(x)
      res[i,"plant_source"] = "ToxValDB"
    }
    else if(is.element(dtxsid,ecosar.plant$dtxsid)) {
      temp = ecosar.plant[ecosar.plant$dtxsid==dtxsid,]
      res[i,"plant_pod"] = min(temp[is.element(temp$dtxsid,dtxsid),"Concentration.(mg/L)"])
      res[i,"plant_source"] = "ECOSAR"
    }
  }

  file = paste0(dir,"Biosolids Eco PODs.xlsx")
  write.xlsx(res,file)
}
