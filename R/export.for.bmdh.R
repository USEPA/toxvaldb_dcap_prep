#-----------------------------------------------------------------------------------
#' @title export.for.bmdh
#' @description Export records required for calculating BMDh values.
#' @param toxval.db Database version
#' @param include.pesticides Flag to include pesticides in output or not
#' @param include.drugs Flag to include drugs in output or not
#' @param include.epa_dws Flag to include EPA DWS in output or not
#' @param include.food_add Flag to include food additives in output or not
#' @param reset.study_group Flag to reset study_group
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
export.for.bmdh <- function(toxval.db,
                            include.pesticides=FALSE,
                            include.drugs=FALSE,
                            include.epa_dws=TRUE,
                            include.food_add=FALSE,
                            reset.study_group=FALSE,
                            run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)
  input_dir = paste0(Sys.getenv("datapath"), "data/input/")
  output_dir = paste0(Sys.getenv("datapath"), "data/results/", run_name, "/")

  slist =  c("EPA OPP", "ATSDR MRLs", "HAWC Project", "ATSDR PFAS 2021", "Cal OEHHA",
             "ECHA IUCLID", "ECOTOX", "EFSA", "EPA HHTV", "HAWC PFAS 150", "HAWC PFAS 430",
             "Health Canada", "HEAST", "HESS", "HPVIS", "IRIS",
             "NTP PFAS", "PFAS 150 SEM v2", "PPRTV (CPHEA)", "ToxRefDB", "WHO JECFA Tox Studies")
  # sources by supersource name
  # slist = c(
  #   "ATSDR", "EPA HAWC", "Cal OEHHA", "ECHA IUCLID", "EPA ECOTOX",
  #   "EFSA OpenFoodTox", "EPA HHTV", "Health Canada", "EPA HEAST", "NITE HESS", "EPA HPVIS",
  #   "EPA IRIS", "NTP PFAS", "EPA PPRTV", "EPA ToxRefDB", "WHO JECFA"
  # )

  # Read in pesticide DTXSID values to exclude
  # List of pesticides found at: https://ccte-res-ncd.epa.gov/dashboard/chemical_lists/PESTCHELSEA
  # Updated list: https://ccte-res-ncd.epa.gov/dashboard/chemical_lists/BCPCPEST
  # Current approach combines original and updated lists
  pesticide_file = paste0(Sys.getenv("datapath"), Sys.getenv("pesticide_file"))
  pesticide_dtxsid = switch(tools::file_ext(pesticide_file),
                            "xls" = {
                              readxl::read_xls(pesticide_file) %>%
                                dplyr::pull(DTXSID) %>%
                                unique() %>%
                                paste0(., collapse="', '")
                            },
                            "xlsx" = {
                              readxl::read_xlsx(pesticide_file,
                                                sheet = "Main Data") %>%
                                dplyr::pull(DTXSID) %>%
                                unique() %>%
                                paste0(., collapse="', '")
                            },
                            stop("Unhandled file type for pesticide file")
  )

  # Set pesticide addition according to parameter
  if(include.pesticides) {
    pesticide_addition = ""
  } else {
    pesticide_addition = paste0(" and b.dtxsid NOT IN ('", pesticide_dtxsid, "')")
  }

  # Read in drug DTXSID values to exclude
  # List of drugs found at: https://comptox.epa.gov/dashboard/chemical-lists/FDAORANGE
  drug_file = paste0(Sys.getenv("datapath"), Sys.getenv("drug_file"))
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

  # Read in EPA drinking water standards chemical list
  # List found at: https://comptox.epa.gov/dashboard/chemical-lists/EPADWS

  epa_dws_file = paste0(Sys.getenv("datapath"), Sys.getenv("epa_dws_file"))
  epa_dws_dtxsid = readxl::read_xlsx(epa_dws_file) %>%
    dplyr::pull(DTXSID) %>%
    unique() %>%
    paste0(., collapse="', '")

  # Set epa_dws addition according to parameter
  if(include.epa_dws) {
    epa_dws_addition = ""
  } else {
    epa_dws_addition = paste0(" and b.dtxsid NOT IN ('", epa_dws_dtxsid, "')")
  }

  # Read in Food Additives Chemical list

  food_add_file = paste0(Sys.getenv("datapath"), Sys.getenv("food_additives"))
  food_add_dtxsid = readxl::read_xlsx(food_add_file) %>%
    dplyr::pull(DTXSID) %>%
    unique() %>%
    paste0(., collapse="', '")

  # Set epa_dws addition according to parameter
  if(include.food_add) {
    food_add_addition = ""
  } else {
    food_add_addition = paste0(" and b.dtxsid NOT IN ('", food_add_dtxsid, "')")
  }

  # # Get priority values for each specified source
  # plist = vector(mode="integer",length=length(slist))
  # plist[] = 1
  # for(i in seq_len(length(slist))) {
  #   src = slist[i]
  #   query = paste0("select distinct priority from record_source where source LIKE '%",src,"%' and long_ref!='-'")
  #   vals = runQuery(query,toxval.db)[,1]
  #   cat(src,paste(vals,collapse="|"),"\n")
  #   if(length(vals)>0) plist[i] = vals[1]
  #   else {
  #     if(src %in% c("HEAST", "EPA HEAST",
  #                   "HESS", "NITE HESS",
  #                   "ECHA IUCLID")) plist[i] = 1
  #   }
  # }

  # Query ToxVal for source data
  res = data.frame()

  for(i in seq_len(length(slist))) {
    src = slist[i]
    # priority = plist[i]
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
                   "b.subsource, ",
                   "b.source_table, ",
                   "e.toxval_type_supercategory, ",
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
                   "f.clowder_doc_id, ",
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
                   epa_dws_addition,
                   food_add_addition,
                   # " and f.priority='", priority, "'",
                   iuclid_addition
    )

    # Get unique entries using query
    mat = runQuery(query, toxval.db) %>%
      dplyr::distinct() %>%
      # Remove specific ECOTOX entry that should be QC failed
      dplyr::filter(
        !(grepl("ECOTOX",source) & dtxsid=="DTXSID2024246" & toxval_numeric==7621 & toxval_units=="mg/kg-day"),
        !(grepl("ToxRefDB", source) & dtxsid == "DTXSID6040371")
      )

    if(reset.study_group){
      study_group_new = fix.study_group(df=mat)

      mat = mat %>%
        dplyr::rename(study_group_toxval = study_group) %>%
        dplyr::left_join(study_group_new,
                         by = "toxval_id")
    }

    # Special case for IRIS and HESS. toxval_type_supercategory DRSV set toxval_subtype to "-"
    if(grepl("IRIS$|HESS$", src)){
      mat = mat %>%
        dplyr::mutate(toxval_subtype = dplyr::case_when(
          toxval_type_supercategory %in% c("Dose Response Summary Value") ~ "-",
          TRUE ~ toxval_subtype
        ))
    }

    # Special source_hash fixes
    hash_specific_changes = readxl::read_xlsx(paste0(Sys.getenv("datapath"), "data/input/dictionary conversions for DCAP.xlsx")) %>%
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
               grepl('^BMD', toxval_type)) ~ "oral_fix",
          TRUE ~ exposure_route
        ),
        exposure_route_fix = dplyr::case_when(
          exposure_route == "oral_fix" ~ 1,
          TRUE ~ 0
        ),
        exposure_route = exposure_route %>%
          gsub("oral_fix", "oral", .)
      ) %>%
      dplyr::filter(exposure_route == "oral") %>%
      dplyr::distinct()

    # Filter to and export records that were manually assigned to oral
    exposure_route_fix = mat %>%
      dplyr::filter(exposure_route_fix == 1)

    if(nrow(exposure_route_fix)){
      expo_dir = paste0(output_dir, "exposure_route_fix")
      if(!dir.exists(expo_dir)) dir.create(expo_dir)
      writexl::write_xlsx(exposure_route_fix, paste0(expo_dir, src, "_oral.xlsx"))
    }

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
                               "document_name, ",
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
      # Remove entries with invalid study_types
      dplyr::filter(study_type %in% !!stlist) %>%
      # Keep only short-term entries with duration of at least 14 days
      dplyr::mutate(
        keep_short_term = dplyr::case_when(
          study_type != "short-term" ~ "1",
          grepl("minute", study_duration_units) & (study_duration_value >= 20160) ~ "1",
          grepl("hour", study_duration_units) & (study_duration_value >= 336) ~ "1",
          grepl("day", study_duration_units) & (study_duration_value >= 14) ~ "1",
          grepl("week", study_duration_units) & (study_duration_value >= 2) ~ "1",
          grepl("month", study_duration_units) & (study_duration_value >= 0.5) ~ "1",
          grepl("year", study_duration_units) & (study_duration_value >= 0.038356) ~ "1",
          TRUE ~ "0"
        )
      ) %>%
      dplyr::filter(keep_short_term == "1") %>%
      dplyr::select(-keep_short_term)

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

      # Get long_ref by source_hash
      ecotox_longref = mat %>%
        dplyr::select(source_hash, record_source_info) %>%
        dplyr::mutate(
          record_source_info = record_source_info %>%
            gsub("] |::| [", ",", ., fixed=TRUE),
          json_parsed = purrr::map(record_source_info, ~jsonlite::fromJSON(., flatten = TRUE))
        ) %>%
        dplyr::select(-record_source_info) %>%
        tidyr::unnest(json_parsed) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(., "-"))) %>%
        dplyr::filter(!record_source_level %in% c("origin")) %>%
        dplyr::select(source_hash, long_ref) %>%
        dplyr::distinct()

      # Alert to unhandled cases
      if(any(!unique(mat$toxval_type) %in% c("NOEL", "LOEL"))){
        cat("Need to handle missing ECOTOX toxval_type filtering cases:\n", paste0("- ", unique(mat$toxval_type)[!unique(mat$toxval_type) %in% c("NOEL", "LOEL")],
                                                                                   collapse="\n "),
            "\n")
        stop()
      }

      # Within study, drop NOEL > LOEL per chemical (constrained to match on species, duration, and sex)
      ecotox_noel_filter = mat %>%
        dplyr::left_join(ecotox_longref,
                         by="source_hash") %>%
        dplyr::group_by(dtxsid, long_ref, common_name, study_duration_value, study_duration_units, sex, lifestage, generation) %>%
        # Get minimum LOAEL/LEL values to initially filter out NOAEL/NEL > LOAEL/LEL
        dplyr::mutate(
          low_loel = dplyr::case_when(
            toxval_type == "LOEL" ~ toxval_numeric,
            TRUE ~ NA
          ),
          low_loel = suppressWarnings(min(low_loel, na.rm = TRUE)),
          # Replace Inf with NA from min with only NA values
          dplyr::across(c(low_loel), ~dplyr::na_if(., Inf)),
          remove_flag = dplyr::case_when(
            toxval_type == "NOEL" & toxval_numeric > low_loel & !is.na(low_loel) ~ 1,
            TRUE ~ 0
          )
        ) %>%
        dplyr::distinct() %>%
        dplyr::filter(remove_flag %in% c(1))

      if(nrow(ecotox_noel_filter)){
        # Export filtered out records for review
        writexl::write_xlsx(ecotox_noel_filter, paste0(output_dir,"results/ToxValDB for BMDh ",toxval.db," ECOTOX NOEL filtered.xlsx"))
        mat = mat %>%
          dplyr::filter(!source_hash %in% ecotox_noel_filter$source_hash)
      }

    } else if(grepl("ECHA IUCLID", src)) {
      # Filter out records with quality rating of "3 (not reliable)" for IUCLID
      mat = mat %>%
        dplyr::filter(!grepl('"quality":"3 (not reliable)"', record_source_info, fixed = TRUE))
    } else if(grepl("EFSA", src)){
      # Filter out undetermined experimental record for EFSA (concern for surrogate and read-across records)
      mat = mat %>%
        dplyr::filter(!experimental_record %in% c("undetermined", "Undetermined"))
    } else if(grepl("ATSDR", src)){
      # Specific to ATSDR MRLs
      # Set toxval_subtype = "-" for all "Point of Departure" toxval_type entries
      # At time of this edit, filter was for only "Point of Departure" toxval_type entries
      mat$toxval_subtype[mat$source_table == "source_atsdr_mrls"] = "-"
    } else if(grepl("PFAS 150 SEM v2", src) | src == "EPA HAWC"){
      # Specific to PFAS 150 SEM v2
      # Set toxval_subtype = "-" to remove duplicates due to system vs. study level designations
      mat$toxval_subtype[mat$source_table == "source_pfas_150_sem_v2"] = "-"
    }

    cat("[4]",src,":",nrow(mat),"\n\n")

    # Map critical_effect_category
    # https://stackoverflow.com/questions/5629111/how-can-i-make-sql-case-sensitive-string-comparison-on-mysql
    crit_cat_map <- runQuery(paste0("SELECT distinct source_hash, BINARY term as term, study_type, critical_effect_category, ",
                                    "CONCAT(source_hash, '_', term) as crit_key ",
                                    "FROM critical_effect_terms ",
                                    "WHERE source_hash in ('",
                                    paste0(mat$source_hash, collapse = "', '")
                                    ,"') "),
                             toxval.db)

    # Copy over study_type collapse lists from old Python script logic
    repeat_study_types = c('immunotoxicity','intermediate','repeat dose other','subchronic',
                           'neurotoxicity subchronic','neurotoxicity chronic',
                           'neurotoxicity 28-day', 'neurotoxicity','intermediate','1','104','14','2','24', #typo
                           'immunotoxicity subchronic','immunotoxicity chronic',
                           'immunotoxicity 28-day','immunotoxicity','growth','chronic','28-day', 'short-term')
    reprodev_study_types = c('reproduction developmental',
                             'extended one-generation reproductive toxicity - with F2 generation and developmental neurotoxicity (Cohorts 1A, 1B with extension, 2A and 2B)',
                             'extended one-generation reproductive toxicity - with F2 generation and both developmental neuro- and immunotoxicity (Cohorts 1A, 1B with extension, 2A, 2B, and 3)',
                             'extended one-generation reproductive toxicity - with F2 generation (Cohorts 1A, and 1B with extension)',
                             'developmental')

    # Unwrap critical_effect and export missing critical_effect_category mappings
    missing_crit_cat = mat %>%
      dplyr::select(source_hash, critical_effect, study_type) %>%
      tidyr::separate_rows(critical_effect, sep = "\\|") %>%
      dplyr::mutate(critical_effect = stringr::str_squish(critical_effect)) %>%
      dplyr::filter(!critical_effect %in% c("", "none", "None")) %>%
      tidyr::unite(col = "crit_key",
                   source_hash, critical_effect,
                   sep = "_",
                   remove = FALSE) %>%
      dplyr::distinct() %>%
      dplyr::filter(!crit_key %in% crit_cat_map$crit_key) %>%
      dplyr::select(-crit_key) %>%
      # Collapse source_hash to export unique cases to map
      dplyr::group_by(dplyr::across(c(-source_hash))) %>%
      dplyr::summarise(source_hash = toString(source_hash)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        study_type = dplyr::case_when(
          study_type %in% !!repeat_study_types ~ "repeat dose",
          study_type %in% !!reprodev_study_types ~ "reproductive developmental",
          TRUE ~ study_type
        )
      ) %>%
      dplyr::select(source_hash, term=critical_effect, dplyr::everything())

    if(nrow(missing_crit_cat)){
      # Handle case of long source_hash list XLSX character limit
      # Get hash length, add 2 characters for ", " delimiter
      hash_length = min(stringr::str_length(mat$source_hash)) + 2
      tmp = missing_crit_cat %>%
        dplyr::mutate(n_count = stringr::str_length(source_hash),
                      group_length = dplyr::case_when(
                        n_count < 10000 ~ n_count,
                        TRUE ~ floor(n_count / floor(n_count / 10000))
                      ),
                      n_group = floor(group_length / hash_length)
        )

      # Split large strings into smaller groups
      split = tmp %>%
        dplyr::filter(n_count > 10000)

      if(nrow(split)){
        split = split %>%
          dplyr::rowwise() %>%
          dplyr::mutate(out_groups = strsplit(source_hash, ", ") %>%
                          unlist() %>%
                          split(., rep(seq_along(.), each  = n_group, length.out = length(.))) %>%
                          lapply(., toString) %>%
                          paste0(collapse = ";")
          ) %>%
          dplyr::ungroup() %>%
          tidyr::separate_rows(out_groups, sep = ";") %>%
          dplyr::select(-n_count, -group_length, -n_group)

        # Filter out records with strings over limit, append split groups
        missing_crit_cat = missing_crit_cat %>%
          dplyr::filter(!source_hash %in% split$source_hash) %>%
          dplyr::bind_rows(split %>%
                             dplyr::select(-source_hash) %>%
                             dplyr::rename(source_hash = out_groups))
      }

      # max(stringr::str_length(missing_crit_cat$source_hash))

      # Try to provide suggestions
      crit_suggs <- runQuery(paste0("SELECT distinct term, study_type, critical_effect_category, CONCAT(term, study_type) as crit_key ",
                                    "FROM res_toxval_v96.critical_effect_terms ",
                                    "HAVING crit_key in ('", paste0(missing_crit_cat$term %>%
                                                                      # Escape "'" for query
                                                                      gsub("'", "''", .),
                                                                    missing_crit_cat$study_type, collapse="', '"), "')"),
                             toxval.db) %>%
        dplyr::select(-crit_key)

      tryCatch(
        {
          writexl::write_xlsx(missing_crit_cat %>%
                                dplyr::left_join(crit_suggs,
                                                 by = c("term", "study_type")),
                              paste0(output_dir, "missing_crit_cat/missing_crit_cat_", src, "_", Sys.Date(),".xlsx"))
        },
        error = function(e){
          message(e)
        }
      )
    }

    # Report duplicate/conflicting mappings
    dups = crit_cat_map %>%
      dplyr::distinct() %>%
      dplyr::group_by(crit_key, study_type) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(n > 1)

    if(nrow(dups)){
      writexl::write_xlsx(crit_cat_map %>%
                            dplyr::filter(crit_key %in% dups$crit_key),
                          paste0(output_dir, "missing_crit_cat/duplicate_crit_cat_mappings_", src, "_", Sys.Date(),".xlsx"))
    }

    crit_cat_map = crit_cat_map %>%
      # Remove select terms
      # term not in ('none') and critical_effect_category not in ('cancer')
      dplyr::filter(!term %in% c("None", "none")#,
                    #!critical_effect_category %in% c("cancer")
                    ) %>%
      dplyr::select(-term, -study_type, -crit_key) %>%
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

      # If reset study_group, ignoring year and reference information
      hashing_cols=c("study_group", "toxval_type", "toxval_numeric", "record_source_info")
      if(reset.study_group){
        hashing_cols = hashing_cols[!hashing_cols %in% c("record_source_info")]
      }

      mat = toxval.source.import.dedup(mat %>%
                                         dplyr::rename(source_hash_toxval=source_hash),
                                       hashing_cols=hashing_cols) %>%
        # Replace "|::|" in critical_effect with "|" delimiter
        dplyr::mutate(dplyr::across(dplyr::any_of(c("critical_effect", "critical_effect_category_original", "critical_effect_category")),
                                    ~gsub(" \\|::\\| ", "|", .)
        ),
        source_hash = source_hash_toxval %>%
          gsub(" \\|::\\| ", ",", .)

        ) %>%
        dplyr::select(-source_hash_toxval)
    }

    # Add current source data to running total
    res = res %>%
      dplyr::bind_rows(mat %>%
                         dplyr::mutate(across(c("study_duration_value",
                                                "toxval_numeric_hed",
                                                "exposure_route_fix"
                         ), ~as.numeric(.))))
  }

  res = res %>%
    dplyr::mutate(
      experimental_record = dplyr::case_when(
        # Hard-code experimental record change for select source_hash values
        source_hash %in% c(
          "ToxValhc_299a0fc3ac8d080494ab57ddd9fc591d", "ToxValhc_13afb685d768ebc7bd84cc6ac9fb14f2",
          "ToxValhc_14a2b9d4adabf1e116bd40853c481504", "ToxValhc_e42ae724eb6a28bcef618ba41e92291c",
          "ToxValhc_88f7cb5d118b846f6a37512b2e3b5164", "ToxValhc_91438f0c9c23a417c38d78ff4d553618"
        ) ~ "not experimental",
        TRUE ~ experimental_record
      ),
      # Convert "<" N(OA)EL to L(OA)EL
      toxval_type = dplyr::case_when(
        grepl("NO?A?EL", toxval_type) & toxval_numeric_qualifier %in% c("<") ~ gsub("^N", "L", toxval_type),
        TRUE ~ toxval_type
      ),
      # Set critical_effect_category values for NOAEL/related toxval_type to none
      critical_effect_category_original = critical_effect_category,
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
    )

  # Store cancer records removed (ones with only cancer as a critical_effect_category)
  writexl::write_xlsx(res %>%
                        dplyr::filter(critical_effect_category_temp == "cancer"),
                      paste0(output_dir, "results/ToxValDB for BMDh ", toxval.db, "_cancer_removed.xlsx")
                      )

  res = res %>%
  # Drop records with critical_effect_category "cancer"
    dplyr::filter(critical_effect_category_temp != "cancer") %>%
    # Remove non-experimental records
    dplyr::filter(!experimental_record %in% c("no", "No", "not experimental", "Not experimental")) %>%
    dplyr::mutate(
      critical_effect_category_temp = critical_effect_category_temp %>%
        dplyr::na_if("-")
    )

  # ECOTOX specific filtering
  if(any(grepl("ECOTOX", res$source))){
    message("Reminder to remove this logic following ECOTOX QC/curation of duration field")
    # Dedup ECOTOX where all values are identical except study_duration (filter out lower duration entries)
    res_ecotox = res %>%
      dplyr::filter(grepl("ECOTOX", source))

    # Get all fields except study_duration and others that arbitrarily differ
    eco_hash_cols = names(res)[!names(res) %in% c("source_hash", "study_duration_value", "study_duration_units",
                                                  "qc_status", "critical_effect", "critical_effect_category",
                                                  "critical_effect_category_original", "critical_effect_category_temp", "crit_key",
                                                  "study_group", "study_duration_class", "qc_category",
                                                  "final_model1", "final_model2")]

    if(reset.study_group){
      eco_hash_cols = eco_hash_cols[!eco_hash_cols %in% c("record_source_info")]
    }
    # # Ignore study_type for repeat dose and repro dev entries
    # eco_hash_cols_type = c(eco_hash_cols, "type", "study_type")

    # Perform deduping on entries that are not repeat dose/repro dev
    # ecotox_no_study_type = res_ecotox %>%
    #   dplyr::filter(!type %in% c("repeat dose", "repro dev"))

    ecotox_dedup = res_ecotox %>%
      dplyr::rowwise() %>%
      dplyr::mutate(study_duration_norm = convert_units(study_duration_value,
                                                        units=study_duration_units,
                                                        desired="days")
      ) %>%
      dplyr::ungroup() %>%
      # Get groups of entries that are identical except for study_duration/study_type
      dplyr::group_by(dplyr::across(dplyr::any_of(eco_hash_cols))) %>%
      # Select entry with maximum study_duration value
      # dplyr::summarise(max_duration = max(study_duration_norm))
      dplyr::mutate(
        max_duration = max(study_duration_norm),
        # Store original max duration selection
        max_duration_sel = paste0(study_duration_value[which(study_duration_value == max_duration)], "_",
                                  study_duration_units[which(study_duration_value == max_duration)])
      ) %>%
      dplyr::ungroup()

    # Collapse groups
    ecotox_dedup =
      toxval.source.import.dedup(ecotox_dedup %>%
                                   dplyr::rename(source_hash_toxval=source_hash),
                                 hashing_cols=eco_hash_cols) %>%
      # Replace "|::|" in critical_effect with "|" delimiter
      dplyr::mutate(dplyr::across(dplyr::any_of(c("critical_effect", "critical_effect_category_original",
                                                  "critical_effect_category", "critical_effect_category_temp")),
                                  ~gsub(" \\|::\\| ", "|", .)
      ),
      source_hash = source_hash_toxval %>%
        gsub(" \\|::\\| ", ",", .)
      ) %>%
      dplyr::select(-source_hash_toxval, -study_duration_value, -study_duration_units,
                    -max_duration, -study_duration_norm) %>%
      tidyr::separate(max_duration_sel, into=c("study_duration_value", "study_duration_units"), sep = "_") %>%
      dplyr::mutate(study_duration_value = as.numeric(study_duration_value))

    # Add filtered ECOTOX data back to res
    res = res %>%
      dplyr::filter(!grepl("ECOTOX", source)) %>%
      dplyr::bind_rows(ecotox_dedup)
  }

  # Get grouped_dtxsid information
  grouped_dtxsid = readxl::read_xlsx(paste0(input_dir, "ToxVal_DTXSIDs_Grouped.xlsx")) %>%
    dplyr::rename(Grouped_DTXSID = Parent_DTXSID, dtxsid = DTXSID) %>%
    dplyr::select(-DCAP_INDEX)

  # Map DTXSID group
  res = res %>%
    dplyr::select(-critical_effect_category) %>%
    dplyr::rename(critical_effect_category = critical_effect_category_temp) %>%
    dplyr::left_join(grouped_dtxsid, by="dtxsid")

  cat("Exporting results...\n")
  # Write unique toxval_type values included in full data
  unique_toxval_type = res %>%
    dplyr::select(source, toxval_type) %>%
    dplyr::distinct()

  file = paste0(output_dir,"results/bmdh_export_toxval_type.xlsx")
  writexl::write_xlsx(unique_toxval_type, file)

  # Trim string size to fit Excel character limit
  res = res %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~stringr::str_sub(., 1, 32000) %>%
                                  tidyr::replace_na("-")))

  # Write full data to file
  file = paste0(output_dir,"results/ToxValDB for BMDh ",toxval.db,".xlsx")
  writexl::write_xlsx(res, file)

  return(res)
}
