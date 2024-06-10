#-----------------------------------------------------------------------------------
#' @title export.for.bmdh
#' @description Export records required for calculating BMDh values.
#' @param toxval.db Database version
#' @param include.pesticides Flag to include pesticides in output or not
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @details Exports all of the data required for the BMDh calculations.
#' The main query may need to be modified to extract more columns if needed for
#' the final application. Certain sources have been excluded because they have a high
#' percentage of read-across values. Species are filtered to only include Human,
#' Dog, Mouse, Rat and Rabbit. If more species are to be included, then allometric
#' scaling factors for those need to added to the function bmd.per.study().
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @rdname export.for.bmdh
#' @importFrom openxlsx createStyle write.xlsx
#' @importFrom toxvaldbBMDh printCurrentFunction runQuery
#' @importFrom dplyr distinct filter mutate case_when select
#' @importFrom tidyr replace_na
#' @importFrom writexl write_xlsx
#-----------------------------------------------------------------------------------
export.for.bmdh <- function(toxval.db="res_toxval_v95", include.pesticides=FALSE) {
  printCurrentFunction(toxval.db)
  dir = "data/"

  slist =  c("ATSDR MRLs", "ATSDR PFAS 2021", "Cal OEHHA", "Copper Manufacturers",
             "ECHA IUCLID", "ECOTOX", "EFSA", "EPA HHTV", "HAWC PFAS 150", "HAWC PFAS 430",
             "HAWC Project", "Health Canada", "HEAST", "HESS", "HPVIS", "IRIS",
             "NTP PFAS", "PFAS 150 SEM v2", "PPRTV (CPHEA)", "ToxRefDB","WHO JECFA Tox Studies")

  # Read in pesticide DTXSID values to exclude
  # List of pesticides found at: https://ccte-res-ncd.epa.gov/dashboard/chemical_lists/PESTCHELSEA
  pesticide_file = paste0(dir, "input/list_chemicals-2024-06-07-08-25-08.xls")
  pesticide_dtxsid = readxl::read_xls(pesticide_file) %>%
    dplyr::pull(DTXSID) %>%
    unique() %>%
    paste0(., collapse="', '")

  # Set pesticide addition according to parameter
  if(include.pesticides) {
    pesticide_addition = ""
  } else {
    pesticide_addition = paste0(" and b.dtxsid NOT IN ('", pesticide_dtxsid, "')")
  }

  # Get priority values for each specified source
  plist = vector(mode="integer",length=length(slist))
  plist[] = 1
  for(i in seq_len(length(slist))) {
    src = slist[i]
    query = paste0("select distinct priority from record_source where source='",src,"' and long_ref!='-'")
    vals = runQuery(query,toxval.db)[,1]
    cat(src,paste(vals,collapse="|"),"\n")
    if(length(vals)>0) plist[i] = vals[1]
    else {
      if(src %in% c("HEAST", "HESS", "ECHA IUCLID")) plist[i] = 1
    }
  }

  # Query ToxVal for source data
  res = data.frame()

  for(i in seq_len(length(slist))) {
    src = slist[i]
    priority = plist[i]
    cat("Pulling ", src, " (", i, " of ", length(slist), ")\n")

    # Handle inclusion of only specified IUCLID OHTs
    iuclid_addition = NULL
    if(src == "ECHA IUCLID") {
      iuclid_addition = paste0(" AND b.source_table in ",
                               "('source_iuclid_repeateddosetoxicityoral', ",
                               "'source_iuclid_developmentaltoxicityteratogenicity', ",
                               "'source_iuclid_carcinogenicity', ",
                               "'source_iuclid_immunotoxicity', ",
                               "'source_iuclid_neurotoxicity')")
    }

    query = paste0("SELECT ",
                      "a.dtxsid, a.casrn, a.name, ",
                      "b.source, ",
                      "b.toxval_type, ",
                      "b.toxval_subtype, ",
                      "b.toxval_numeric_qualifier, ",
                      "b.toxval_numeric, ",
                      "b.toxval_units, ",
                      "b.study_type, ",
                      "b.study_duration_value, ",
                      "b.study_duration_units, ",
                      "b.study_duration_class, ",
                      "b.supersource, ",
                      "d.common_name, ",
                      "b.strain, ",
                      "b.sex, ",
                      "b.lifestage, ",
                      "b.generation, ",
                      "b.exposure_route, ",
                      "b.exposure_method, ",
                      "b.exposure_form, ",
                      "b.critical_effect, ",
                      "b.year, ",
                      "f.long_ref, ",
                      "f.url, ",
                      "f.title, ",
                      "f.pmid, ",
                      "f.guideline, ",
                      "f.record_source_level, ",
                      "f.record_source_type, ",
                      "f.priority, ",
                      "f.clowder_doc_id, ",
                      "f.quality, ",
                      "b.source_hash, ",
                      "b.study_group, ",
                      "b.qc_status, ",
                      "b.experimental_record, ",
                      "a.cleaned_casrn, a.cleaned_name ",
                      "FROM ",
                      "toxval b ",
                      "INNER JOIN source_chemical a on a.chemical_id=b.chemical_id ",
                      "LEFT JOIN species d on b.species_id=d.species_id ",
                      "INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type ",
                      "INNER JOIN record_source f on b.toxval_id=f.toxval_id ",
                      "WHERE ",
                      "b.source='", src, "' ",
                      "and b.qc_status NOT LIKE '%fail%' ",
                      # "and b.human_eco='human health' ",
                      "and e.toxval_type_supercategory in ('Point of Departure') ",
                      "and b.toxval_units='mg/kg-day' ",
                      "and b.exposure_route='oral'",
                      pesticide_addition,
                      # " and f.priority='", priority, "'",
                      iuclid_addition
                     )

    # Get unique entries using query
    mat = runQuery(query, toxval.db) %>%
      dplyr::distinct()

    cat("[1]",src,":",nrow(mat),"\n")

    # Initialize list of toxval_type values to exclude
    exclude.list = c("HNEL","LEC","NOTEL","POD (HE)","POD (HEC)","POD (surrogate)","critical value",
                     "LOEC","NOEC","LOAEC","NOAEC","BMD (HEC)","BMDL (HEC)","POD (01 HED)","POD (HED)","LED","POD (99 HED)")
    mat = mat %>%
      # Remove entries with excluded toxval_type
      dplyr::filter(!toxval_type %in% !!exclude.list) %>%

      # Replace NA qualifier with "ns"
      dplyr::mutate(
        toxval_numeric_qualifier = toxval_numeric_qualifier %>%
          tidyr::replace_na("ns")
      )
    cat("[2]",src,":",nrow(mat),"\n")

    # Initialize list of study_type values to include
    stlist = c(
      "subchronic",
      "28-day",
      "chronic",
      "repeat dose other",
      "developmental",
      "reproduction",
      "reproduction developmental",
      "clinical"
    )

    # Remove entries with invalid study_types
    mat = mat %>%
      dplyr::filter(study_type %in% !!stlist)
    cat("[3]",src,":",nrow(mat),"\n")

    mat = mat %>%
      # Clean common_name values
      dplyr::mutate(
        common_name = dplyr::case_when(
          common_name == "European Rabbit" ~ "Rabbit",
          common_name == "Western European House Mouse" ~ "Mouse",
          common_name == "Norway Rat" ~ "Rat",
          common_name == "Domestic Dog" ~ "Dog",
          common_name == "House Mouse" ~ "Mouse",
          TRUE ~ common_name
        ),

        # Use cleaned name and casrn values when original values are missing
        casrn = dplyr::case_when(
          casrn %in% c(as.character(NA), "-", "") ~ cleaned_casrn,
          TRUE ~ casrn
        ),
        name = dplyr::case_when(
          name %in% c(as.character(NA), "-", "") ~ cleaned_name,
          TRUE ~ name
        ),

        # Add toxval_numeric_hed flag field
        toxval_numeric_hed = dplyr::case_when(
          grepl("HED", toxval_type) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      # Keep only entries with specified common_name
      dplyr::filter(common_name %in% c("Rat", "Mouse", "Dog", "Rabbit", "Human")) %>%
      # Remove unused "cleaned" columns
      dplyr::select(-c("cleaned_casrn", "cleaned_name")) %>%
      # Remove non-experimental records
      dplyr::filter(!experimental_record %in% c("no", "No", "not experimental", "Not experimental"))

    # Filter out critical_effect values with "accumulation" if source is ECOTOX
    if(src == "ECOTOX") {
      mat = mat %>%
        dplyr::filter(!grepl("accumulation", critical_effect, ignore.case=TRUE))
    } else if(src == "ECHA IUCLID") {
      # Filter out records with quality rating of "3 (not reliable)" for IUCLID
      mat = mat %>%
        dplyr::filter(!grepl("3 \\(not reliable\\)", quality, ignore.case=TRUE))
    } else if(src == "EFSA"){
      # Filter out undetermined experimental record for EFSA (concern for surrogate and read-across records)
      mat = mat %>%
        dplyr::filter(!experimental_record %in% c("undetermined", "Undetermined"))
    } else if(src == "ATSDR MRLs"){
      # Set toxval_subtype = "-" for all "Point of Departure" toxval_type entries
      # At time of this edit, filter was for only "Point of Departure" toxval_type entries
      mat$toxval_subtype = "-"
    }

    cat("[4]",src,":",nrow(mat),"\n\n")

    # Map critical_effect_category
    crit_cat_map <- runQuery(paste0("SELECT source_hash, critical_effect_category FROM critical_effect_terms ",
                                    "WHERE source_hash in ('",
                                    paste0(mat$source_hash, collapse = "', '")
                                    ,"') ",
                                    # Remove select terms
                                    "and term not in ('none')"),
                             toxval.db) %>%
      dplyr::group_by(source_hash) %>%
      dplyr::mutate(critical_effect_category = paste0(unique(critical_effect_category[!is.na(critical_effect_category)]),
                                                      collapse = "|")) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    mat = mat %>%
      dplyr::left_join(crit_cat_map,
                       by="source_hash")

    rm(crit_cat_map)

    if(src == "NTP PFAS"){
      # Special logic implemented for now to further collapse NTP PFAS records post-ToxVal
      mat = toxval.source.import.dedup(mat %>%
                                         dplyr::rename(source_hash_toxval=source_hash),
                                       hashing_cols=c("study_group", "toxval_type", "toxval_numeric")) %>%
        # Replace "|::|" in critical_effect with "|" delimiter
        dplyr::mutate(
          critical_effect = critical_effect %>%
            gsub(" \\|::\\| ", "|", .),
          source_hash = source_hash_toxval %>%
            gsub(" \\|::\\| ", ",", .),
          critical_effect_category = critical_effect_category %>%
            gsub(" \\|::\\| ", "|", .)
        ) %>%
        dplyr::select(-source_hash_toxval)
    }

    # Add current source data to running total
    res = res %>%
      dplyr::bind_rows(mat)
  }

  # Set critical_effect_category values for NOAEL/related toxval_type to none
  res = res %>%
    dplyr::mutate(
      critical_effect_category = dplyr::case_when(
        grepl("NO?A?EL", toxval_type) ~ "none",
        TRUE ~ critical_effect_category
      )
    )

  # Get conceptual model by critical_effect_category
  conceptual_model_map = get.conceptual_model.by.critical_effect_category(df = res) %>%
    dplyr::select(-study_type, -critical_effect_category)
  # Map conceptual models
  res = res %>%
    dplyr::left_join(conceptual_model_map,
                     by = "source_hash")

  cat("Exporting results...\n")
  # Write unique toxval_type values included in full data
  unique_toxval_type = res %>%
    dplyr::select(source, toxval_type) %>%
    dplyr::distinct()
  file = paste0(dir,"results/bmdh_export_toxval_type_",Sys.Date(),".xlsx")
  writexl::write_xlsx(unique_toxval_type, file)

  # Write full data to file
  if(include.pesticides) file = paste0(dir,"results/ToxValDB for BMDh WITH PESTICIDES ",
                                       toxval.db," ",Sys.Date(),".xlsx")
  else file = paste0(dir,"results/ToxValDB for BMDh ", toxval.db," ",Sys.Date(),".xlsx")
  writexl::write_xlsx(res, file)
}
