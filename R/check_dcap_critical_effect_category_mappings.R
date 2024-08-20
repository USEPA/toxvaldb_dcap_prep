#--------------------------------------------------------------------------------------
#' @title check_dcap_critical_effect_category_mappings
#' @description Attempts to remap previously-mapped critical_effect_category values to DCAP entries
#' @returns None; output is written to Excel files
#' @details
#' The output Excel files are as follows:
#' - dcap_mappings_identified.xlsx: All critical_effect_categories that could be confidently remapped
#' - dcap_mappings_still_missing.xlsx: Full data for entries missing categorizations
#' - dcap_missing_categorization.xlsx: Just critical_effect, study_type values missing categorizations
#' - dcap_mapping_suggestions.xlsx: Mapping suggestions based on close, but not exact, matches
#' @param toxval.db The version of ToxVal to use
#' @param get_suggestions Whether to provide mapping suggestions (Default: TRUE)
#' @param input_file The file to pull missing critical_effect_category from.
#' @param output_dir The folder used to write output to
#--------------------------------------------------------------------------------------
check_dcap_critical_effect_category_mappings <- function(toxval.db, get_suggestions=TRUE,
                                                         input_file="", output_dir=""){
  # Get list of DCAP tables to check
  iuclid_dcap = c('source_iuclid_repeateddosetoxicityoral',
                  'source_iuclid_developmentaltoxicityteratogenicity',
                  'source_iuclid_carcinogenicity',
                  'source_iuclid_immunotoxicity',
                  'source_iuclid_neurotoxicity',
                  'source_iuclid_toxicityreproduction')

  dcap_sources = c("ATSDR MRLs", "HAWC Project", "ATSDR PFAS 2021", "Cal OEHHA", "Copper Manufacturers",
                   "ECOTOX", "EFSA", "EPA HHTV", "HAWC PFAS 150", "HAWC PFAS 430", "Health Canada",
                   "HEAST", "HESS", "HPVIS", "IRIS", "NTP PFAS", "PFAS 150 SEM v2", "PPRTV (CPHEA)",
                   "ToxRefDB", "WHO JECFA Tox Studies")

  dcap_sources = c(
    "ATSDR", "EPA HAWC", "Cal OEHHA", "Copper Manufacturers", "ECHA IUCLID", "EPA ECOTOX",
    "EFSA OpenFoodTox", "EPA HHTV", "Health Canada", "EPA HEAST", "NITE HESS", "EPA HPVIS",
    "EPA IRIS", "NTP PFAS", "EPA PPRTV", "EPA ToxRefDB", "WHO JECFA"
  )

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

  # Get all existing critical_effect_terms and the source_hashes that they cover
  critical_effect_terms = runQuery("SELECT * FROM critical_effect_terms", toxval.db) %>%
    dplyr::rename(critical_effect = term)
  curr_source_hash = critical_effect_terms %>%
    dplyr::pull(source_hash) %>%
    unique()

  # Get source_hash values included in most recent filtered DCAP data
  dcap_hashes = readxl::read_xlsx(input_file) %>%
    tidyr::separate_rows(source_hash, sep=",") %>%
    tidyr::separate_rows(source_hash, sep=" \\|::\\| ") %>%
    dplyr::mutate(source_hash = stringr::str_squish(source_hash)) %>%
    dplyr::select(source_hash) %>%
    dplyr::distinct()

  # Get IUCLID DCAP entries whose source_hashes are not accounted for in critical_effect_terms
  missing_query = paste0("SELECT * FROM toxval WHERE (source_table IN ('", paste0(iuclid_dcap, collapse="', '"), "')",
                         " OR source IN ('", paste0(dcap_sources, collapse="', '"), "')) ",
                         " AND source_hash NOT IN ('", paste0(curr_source_hash, collapse="', '"), "')")
  missing_entries = runQuery(missing_query, toxval.db) %>%
    dplyr::inner_join(dcap_hashes, by=c("source_hash"))

  # Perform study_type collapsing and select relevant fields
  initial = missing_entries %>%
    dplyr::mutate(
      study_type = dplyr::case_when(
        study_type %in% !!repeat_study_types ~ "repeat dose",
        study_type %in% !!reprodev_study_types ~ "reproductive developmental",
        TRUE ~ study_type
      )
    ) %>%
    dplyr::select(source_hash, critical_effect, study_type, source_table)

  # Map values to missing data
  missing_mapped = initial %>%
    # Separate critical_effect on |
    tidyr::separate_rows(critical_effect, sep="\\|") %>%
    # Remove entries that do not have a critical_effect value
    dplyr::filter(!critical_effect %in% c("-", "", as.character(NA))) %>%
    # Remove whitespace from critical_effect values to improve mapping
    dplyr::mutate(critical_effect = stringr::str_trim(critical_effect)) %>%
    # Map terms to entries missing categorization by uncollapsed critical_effect and study_type
    dplyr::left_join(critical_effect_terms %>%
                       dplyr::select(critical_effect, study_type, critical_effect_category) %>%
                       dplyr::distinct(),
                     by=c("critical_effect", "study_type")) %>%
    dplyr::mutate(
      study_type = study_type %>%
        gsub("nonneoplastic", "non-neoplastic", .)
    )

  # Get source_hashes that are still missing a mapping
  na_hashes = missing_mapped %>%
    dplyr::filter(is.na(critical_effect_category)) %>%
    dplyr::pull(source_hash) %>%
    unique()

  # Get IUCLID DCAP entries that still do not have a mapping
  query = paste0("SELECT * FROM toxval WHERE source_hash IN ('",
                 paste0(na_hashes, collapse="', '"), "')")
  still_missing = runQuery(query, toxval.db)

  # Record results
  writexl::write_xlsx(missing_mapped %>% dplyr::filter(!is.na(critical_effect_category)),
                      paste0(output_dir, "dcap_mappings_identified.xlsx"))
  writexl::write_xlsx(still_missing,
                      paste0(output_dir, "dcap_mappings_still_missing.xlsx"))
  writexl::write_xlsx(missing_mapped %>%
                        dplyr::filter(is.na(critical_effect_category)) %>%
                        dplyr::select(-source_table) %>%
                        dplyr::group_by(critical_effect, study_type) %>%
                        dplyr::mutate(source_hash = source_hash %>% toString()) %>%
                        dplyr::ungroup() %>%
                        dplyr::distinct(),
                      paste0(output_dir, "dcap_missing_categorization.xlsx"))

  if(get_suggestions) {
    # Try to get suggested mappings
    remaining_categorizations = missing_mapped %>%
      dplyr::filter(is.na(critical_effect_category)) %>%
      dplyr::mutate(
        # Keep only first term in list of critical_effects
        critical_effect_original = critical_effect,
        critical_effect = critical_effect %>%
          gsub(",.*", "", .) %>%
          stringr::str_squish()
      ) %>%
      dplyr::select(-critical_effect_category) %>%
      # Try to match to individual critical_effect components
      dplyr::left_join(critical_effect_terms %>%
                         tidyr::separate_rows(critical_effect, sep=",\\s*") %>%
                         dplyr::select(critical_effect, study_type, critical_effect_category) %>%
                         dplyr::distinct(),
                       by=c("critical_effect", "study_type")) %>%
      dplyr::mutate(critical_effect = critical_effect_original) %>%
      dplyr::select(-critical_effect_original)

    # Get list of suggestions and any entries that are still missing mappings
    final_missing_categorizations = remaining_categorizations %>%
      dplyr::filter(is.na(critical_effect_category))
    mapping_suggestions = remaining_categorizations %>%
      dplyr::filter(!is.na(critical_effect_category)) %>%
      dplyr::mutate(s_temp = source_hash) %>%
      toxval.source.import.dedup(hashing_cols=c("source_hash", "s_temp", "critical_effect", "study_type", "source_table"),
                                 delim="|") %>%
      dplyr::rename(source_hash = s_temp)

    # Rewrite missing categorizations file, and write suggested mappings
    writexl::write_xlsx(final_missing_categorizations %>%
                          dplyr::select(-source_table) %>%
                          dplyr::group_by(critical_effect, study_type) %>%
                          dplyr::mutate(source_hash = source_hash %>% toString()) %>%
                          dplyr::ungroup() %>%
                          dplyr::distinct(),
                        paste0(output_dir, "dcap_missing_categorization.xlsx"))
    writexl::write_xlsx(mapping_suggestions,
                        paste0(output_dir, "dcap_mapping_suggestions.xlsx"))
  }
}
