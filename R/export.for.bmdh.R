#-----------------------------------------------------------------------------------
#' @title export.for.bmdh
#' @description Export records required for calculating BMDh values.
#' @param toxval.db Database version
#' @param include.pesticides Flag to include pesticides in output or not
#' @param include.drugs Flag to include drugs in output or not
#' @param run_name The desired name for the output directory (Default: current date)
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
#' @importFrom readxl read_xls
#-----------------------------------------------------------------------------------
export.for.bmdh <- function(toxval.db="res_toxval_v95", include.pesticides=FALSE,
                            include.drugs=FALSE, run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)
  input_dir = "data/input/"
  output_dir = paste0("data/results/", run_name, "/")

  slist =  c("ATSDR MRLs", "HAWC Project", "ATSDR PFAS 2021", "Cal OEHHA", "Copper Manufacturers",
             "ECHA IUCLID", "ECOTOX", "EFSA", "EPA HHTV", "HAWC PFAS 150", "HAWC PFAS 430",
             "Health Canada", "HEAST", "HESS", "HPVIS", "IRIS",
             "NTP PFAS", "PFAS 150 SEM v2", "PPRTV (CPHEA)", "ToxRefDB","WHO JECFA Tox Studies")

  # Read in pesticide DTXSID values to exclude
  # List of pesticides found at: https://ccte-res-ncd.epa.gov/dashboard/chemical_lists/PESTCHELSEA
  # Updated list: https://ccte-res-ncd.epa.gov/dashboard/chemical_lists/BCPCPEST
  # Current approach combines original and updated lists
  pesticide_file = Sys.getenv("pesticide_file")
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

  # Read in drug DTXSID values to exclude
  # List of drugs found at: https://comptox.epa.gov/dashboard/chemical-lists/FDAORANGE
  drug_file = Sys.getenv("drug_file")
  drug_dtxsid = readxl::read_xlsx(drug_file) %>%
    dplyr::pull(DTXSID) %>%
    unique() %>%
    paste0(., collapse="', '")

  # Set drug addition according to parameter
  if(include.drugs) {
    drug_addition = ""
  } else {
    drug_addition = paste0(" and b.dtxsid NOT IN ('", drug_dtxsid, "')")
  }

  # Get priority values for each specified source
  plist = vector(mode="integer",length=length(slist))
  plist[] = 1
  for(i in seq_len(length(slist))) {
    src = slist[i]
    query = paste0("select distinct priority from record_source where source LIKE '%",src,"%' and long_ref!='-'")
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
    if(grepl("ECHA IUCLID", src)) {
      iuclid_addition = paste0(" AND b.source_table in ",
                               "('source_iuclid_repeateddosetoxicityoral', ",
                               "'source_iuclid_developmentaltoxicityteratogenicity', ",
                               "'source_iuclid_carcinogenicity', ",
                               "'source_iuclid_immunotoxicity', ",
                               "'source_iuclid_neurotoxicity', ",
                               "'source_iuclid_toxicityreproduction')")
    }

    query = paste0("SELECT ",
                   "b.toxval_id, ",
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
                   "b.subsource_url, ",
                   "d.common_name, ",
                   "b.species_original, ",
                   "b.strain, ",
                   "b.sex, ",
                   "b.lifestage, ",
                   "b.generation, ",
                   "b.exposure_route, ",
                   "b.exposure_method, ",
                   "b.exposure_form, ",
                   "b.critical_effect, ",
                   "b.year, ",
                   # "f.long_ref, ",
                   # "f.url, ",
                   # "f.title, ",
                   # "f.pmid, ",
                   # "f.guideline, ",
                   # "f.record_source_level, ",
                   # "f.record_source_type, ",
                   # "f.priority, ",
                   "f.clowder_doc_id, ",
                   # "f.quality, ",
                   "b.source_hash, ",
                   "b.study_group, ",
                   "b.qc_category, ",
                   "b.experimental_record, ",
                   "b.key_finding, ",
                   "a.cleaned_casrn, a.cleaned_name ",
                   "FROM ",
                   "toxval b ",
                   "INNER JOIN source_chemical a on a.chemical_id=b.chemical_id ",
                   "LEFT JOIN species d on b.species_id=d.species_id ",
                   "INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type ",
                   "LEFT JOIN record_source f on b.toxval_id=f.toxval_id ",
                   "WHERE ",
                   "b.source LIKE '%", src, "%' ",
                   "and b.qc_status NOT LIKE '%fail%' ",
                   # "and b.human_eco='human health' ",
                   "and e.toxval_type_supercategory in ('Dose Response Summary Value') ",
                   "and b.toxval_units='mg/kg-day' ",
                   "and b.toxval_type NOT LIKE '%TAD%' ",
                   "and b.toxval_units NOT LIKE '%TAD%'",
                   # "and b.exposure_route='oral'",
                   pesticide_addition,
                   drug_addition,
                   # " and f.priority='", priority, "'",
                   iuclid_addition
    )

    # Get unique entries using query
    mat = runQuery(query, toxval.db) %>%
      dplyr::distinct() %>%
      # Remove specific ECOTOX entry that should be QC failed
      dplyr::filter(
        !(grepl("ECOTOX",source) & dtxsid=="DTXSID2024246" & toxval_numeric==7621 & toxval_units=="mg/kg-day")
      )

    # Special source_hash fixes
    hash_specific_changes = readxl::read_xlsx("data/input/dictionary conversions for DCAP.xlsx") %>%
      dplyr::filter(source_hash %in% mat$source_hash)

    if(nrow(hash_specific_changes)){
      for(j in seq_len(nrow(hash_specific_changes))){
        field = hash_specific_changes$`corrected field`[j]
        hash = hash_specific_changes$source_hash[j]
        correction = hash_specific_changes$correction[j]
        # Apply special correction
        mat[mat$source_hash==hash, field] = correction
      }
    }

    mat = mat %>%
      # Special rule to convert toxval_type to correct type (see data/input/dictionary conversions for DCAP_20240614.xlsx)
      dplyr::mutate(
        toxval_type = dplyr::case_when(
          toxval_type %in% c('NOEC', 'LOEC', 'NOAEC', 'LOAEC') &
            exposure_route == 'oral' &
            # Replace ending "C" with "L" instead
            toxval_units == 'mg/kg-day' ~ gsub("C$", "L", toxval_type),
          TRUE ~ toxval_type
        ),
        # Special rule to set missing exposure_route to 'oral' (see data/input/dictionary conversions for DCAP_20240614.xlsx)
        exposure_route = dplyr::case_when(
          exposure_route %in% c(NA, "-") &
            toxval_units == 'mg/kg-day' &
            (toxval_type %in% c('NEL', 'LEL', 'LOEL', 'NOEL', 'NOAEL', 'LOAEL') |
               grepl('^BMD', toxval_type)) ~ "oral",
          TRUE ~ exposure_route
        )
      ) %>%
      dplyr::filter(exposure_route == "oral") %>%
      dplyr::distinct()

    if(!nrow(mat)){
      cat("No data pulled for: ", src, "\n")
      # browser()
      next
    }

    # Pull record_source records and collapse into JSON list
    # Expand reference information rowwise() with as.data.frame(jsonlite::fromJSON(mat$record_source_info[1]))
    mat_refs = runQuery(paste0("SELECT ",
                               "toxval_id, ",
                               "long_ref, ",
                               "url, ",
                               "title, ",
                               "external_source_id, ",
                               "external_source_id_desc, ",
                               "pmid, ",
                               "guideline, ",
                               "record_source_level, ",
                               "record_source_type, ",
                               "priority, ",
                               "clowder_doc_id, ",
                               "quality ",
                               "FROM record_source ",
                               "WHERE toxval_id in (", toString(mat$toxval_id), ")"),
                        toxval.db) %>%
      dplyr::mutate(record_source_info = convert.fields.to.json(dplyr::select(.,
                                                                              -tidyr::any_of(c("toxval_id"))))) %>%
      dplyr::select(toxval_id, record_source_info) %>%
      dplyr::group_by(toxval_id) %>%
      # Convert into JSON array/list
      dplyr::mutate(record_source_info = paste0(record_source_info %>% gsub("\\]|\\[", "", .),
                                                collapse=", ") %>%
                      paste0("[", ., "]")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    # Join to main ToxVal data
    mat = mat %>%
      dplyr::left_join(mat_refs,
                       by="toxval_id") %>%
      dplyr::select(-toxval_id)

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
      "chronic",
      "repeat dose other",
      "developmental",
      "reproduction developmental",
      "clinical",
      "short-term"
    )

    mat = mat %>%
      dplyr::mutate(
        # Add short-term study_type for entries with duration at least 14 days
        study_type = dplyr::case_when(
          grepl("minute", study_duration_units) & study_duration_value >= 20160 ~ "short-term",
          grepl("hour", study_duration_units) & study_duration_value >= 336 ~ "short-term",
          grepl("day", study_duration_units) & study_duration_value >= 14 ~ "short-term",
          grepl("week", study_duration_units) & study_duration_value >= 2 ~ "short-term",
          grepl("month", study_duration_units) & study_duration_value >= 0.5 ~ "short-term",
          grepl("year", study_duration_units) & study_duration_value >= 0.038356 ~ "short-term",
          TRUE ~ study_type

        )
      ) %>%
      # Remove entries with invalid study_types
      dplyr::filter(study_type %in% !!stlist)

    cat("[3]",src,":",nrow(mat),"\n")

    # Split species list if present
    mat = split.species.list(df=mat) %>%
      # Remove species_original used for species list splitting
      dplyr::select(-species_original)

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

    # Source specific filtering
    if(grepl("ECOTOX", src)) {
      # Filter out critical_effect values with "accumulation" if source is ECOTOX
      mat = mat %>%
        dplyr::filter(!grepl("accumulation", critical_effect, ignore.case=TRUE))
    } else if(grepl("ECHA IUCLID", src)) {
      # Filter out records with quality rating of "3 (not reliable)" for IUCLID
      mat = mat %>%
        dplyr::filter(!grepl('"quality":"3 (not reliable)"', record_source_info, fixed = TRUE))
    } else if(grepl("EFSA", src)){
      # Filter out undetermined experimental record for EFSA (concern for surrogate and read-across records)
      mat = mat %>%
        dplyr::filter(!experimental_record %in% c("undetermined", "Undetermined"))
    } else if(grepl("ATSDR MRLs", src)){
      # Set toxval_subtype = "-" for all "Point of Departure" toxval_type entries
      # At time of this edit, filter was for only "Point of Departure" toxval_type entries
      mat$toxval_subtype = "-"
    } else if(grepl("PFAS 150 SEM v2", src)){
      # Set toxval_subtype = "-" to remove duplicates due to system vs. study level designations
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
                       by="source_hash") %>%
      dplyr::mutate(critical_effect_category = dplyr::case_when(
        # Set to "none" if no critical_effect present
        critical_effect %in% c(NA, "-") ~ "none",
        TRUE ~ critical_effect_category
      ))

    rm(crit_cat_map)

    if(src %in% slist){#[!slist %in% c("ECHA IUCLID", "ECOTOX")]){#c("NTP PFAS", "ECHA IUCLID", "HAWC Project",  "PFAS 150 SEM v2")){
      # Special logic implemented for now to further collapse source records post-ToxVal
      mat = toxval.source.import.dedup(mat %>%
                                         dplyr::rename(source_hash_toxval=source_hash),
                                       hashing_cols=c("study_group", "toxval_type", "toxval_numeric", "record_source_info")) %>%
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
      dplyr::bind_rows(mat %>%
                         dplyr::mutate(across(c("study_duration_value",
                                                "toxval_numeric_hed"), ~as.numeric(.))))
  }

  res = res %>%
    dplyr::mutate(
      # Convert "<" N(OA)EL to L(OA)EL
      toxval_type = dplyr::case_when(
        grepl("NO?A?EL", toxval_type) & toxval_numeric_qualifier %in% c("<") ~ gsub("^N", "L", toxval_type),
        TRUE ~ toxval_type
      ),
      # Set critical_effect_category values for NOAEL/related toxval_type to none
      # critical_effect_category_original = critical_effect_category,
      critical_effect_category = dplyr::case_when(
        grepl("NO?A?EL", toxval_type) ~ "none",
        TRUE ~ critical_effect_category
      ),
      # Set all toxval_numeric_qualifier values to "=".
      toxval_numeric_qualifier = "=",

      # Create critical_effect_category field w/o "cancer" for conceptual model mapping
      critical_effect_category_temp = critical_effect_category %>%
        tidyr::replace_na("-"),
      critical_effect_category = critical_effect_category %>%
        gsub("\\|cancer\\|", "|", .) %>%
        gsub("\\|cancer|cancer\\|", "", .)
    ) %>%
    # Drop records with critical_effect_category "cancer"
    dplyr::filter(critical_effect_category_temp != "cancer") %>%
    dplyr::mutate(
      critical_effect_category_temp = critical_effect_category_temp %>%
        dplyr::na_if("-")
    )

  # Get conceptual model by critical_effect_category
  conceptual_model_map = get.conceptual_model.by.critical_effect_category(df = res) %>%
    dplyr::select(-study_type, -critical_effect_category)

  # Get grouped_dtxsid information
  grouped_dtxsid = readxl::read_xlsx(paste0(input_dir, "ToxVal_DTXSIDs_Grouped.xlsx")) %>%
    dplyr::rename(Grouped_DTXSID = Parent_DTXSID, dtxsid = DTXSID) %>%
    dplyr::select(-DCAP_INDEX)

  # Map conceptual models and DTXSID group
  res = res %>%
    dplyr::left_join(conceptual_model_map,
                     by = "source_hash") %>%
    dplyr::select(-critical_effect_category) %>%
    dplyr::rename(critical_effect_category = critical_effect_category_temp) %>%
    dplyr::left_join(grouped_dtxsid, by="dtxsid")

  # Dedup ECOTOX where all values are identical except study_duration (filter out lower duration entries)
  res_ecotox = res %>%
    dplyr::filter(grepl("ECOTOX", source))

  # Get all fields except study_duration and others that arbitrarily differ
  eco_hash_cols = names(mat)[!names(mat) %in% c("source_hash", "study_duration_value", "study_duration_units",
                                                "qc_status", "critical_effect", "critical_effect_category",
                                                "study_group", "study_duration_class")]
  # Ignore study_type for repeat dose and repro dev entries
  eco_hash_cols_type = c(eco_hash_cols, "type", "study_type")

  # Perform deduping on entries that are not repeat dose/repro dev
  ecotox_no_study_type = res_ecotox %>%
    dplyr::filter(!type %in% c("repeat dose", "repro dev")) %>%
    # Get groups of entries that are identical except for study_duration
    dplyr::group_by_at(eco_hash_cols) %>%
    # Select entry with maximum study_duration value
    dplyr::mutate(
      max_duration = max(study_duration_value),

      keep = dplyr::case_when(
        study_duration_value == max_duration ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::filter(keep == 1) %>%
    dplyr::select(-c("max_duration", "keep")) %>%
    # In case of ties, select first alphabetical entry by study_group
    dplyr::arrange(study_group) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Perform deduping on entries that are repeat dose/repro dev
  ecotox_study_type = res_ecotox %>%
    dplyr::filter(type %in% c("repeat dose", "repro dev")) %>%
    # Get groups of entries that are identical except for study_duration/study_type
    dplyr::group_by_at(eco_hash_cols_type) %>%
    # Select entry with maximum study_duration value
    dplyr::mutate(
      max_duration = max(study_duration_value),

      keep = dplyr::case_when(
        study_duration_value == max_duration ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::filter(keep == 1) %>%
    dplyr::select(-c("max_duration", "keep")) %>%
    # In case of ties, select first alphabetical entry by study_group
    dplyr::arrange(study_group) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Add filtered ECOTOX data back to res
  res = res %>%
    dplyr::filter(!grepl("ECOTOX", source)) %>%
    dplyr::bind_rows(ecotox_no_study_type, ecotox_study_type)

  cat("Exporting results...\n")
  # Write unique toxval_type values included in full data
  unique_toxval_type = res %>%
    dplyr::select(source, toxval_type) %>%
    dplyr::distinct()
  file = paste0(output_dir,"results/bmdh_export_toxval_type.xlsx")
  writexl::write_xlsx(unique_toxval_type, file)

  # Trim string size to fit Excel character limit
  res = res %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~stringr::str_sub(., 1, 32000)))

  # Write full data to file
  file = paste0(output_dir,"results/ToxValDB for BMDh ",toxval.db,".xlsx")
  writexl::write_xlsx(res, file)

  return(res)
}
