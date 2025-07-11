#' @title check_dcap_critical_effect_category_mappings
#' @description Attempts to remap previously-mapped toxicological_effect_category values to DCAP entries.
#' @param toxval.db The version of ToxVal to use.
#' @param get_suggestions Whether to provide mapping suggestions (Default: TRUE).
#' @param input_file The file to pull missing toxicological_effect_category from (typically the filtered POD output file).
#' @param output_dir The directory to write the output file.
#' @param db.type String of what kind of database connection to use, default "mysql. If "sqlite", workflow with use .Renv defined "sqlite_file" file path.
#' @return None, output is written to Excel files for additional review.
#' @details
#' The output Excel files are as follows:
#' - dcap_mappings_identified.xlsx: All toxicological_effect_categories that could be confidently remapped.
#' - dcap_mappings_still_missing.xlsx: Full data for entries missing categorizations.
#' - dcap_missing_categorization.xlsx: Just toxicological_effect, study_type values missing categorizations.
#' - dcap_mapping_suggestions.xlsx: Mapping suggestions based on close, but not exact, matches.
#' @seealso
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname check_dcap_critical_effect_category_mappings
#' @export
#' @importFrom dplyr rename pull mutate select distinct inner_join case_when filter left_join group_by ungroup
#' @importFrom readxl read_xlsx
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_squish str_trim
#' @importFrom writexl write_xlsx
check_dcap_critical_effect_category_mappings <- function(toxval.db, get_suggestions=TRUE,
                                                         input_file="", output_dir="", db.type){
  # Get list of DCAP sources to check
  iuclid_dcap = global_vars()$iuclid_dcap
  dcap_sources = global_vars()$dcap_sources
  # List of types to overwrite as repeated dose or reprodev
  repeat_study_types = global_vars()$repeat_study_types
  reprodev_study_types = global_vars()$reprodev_study_types

  # Get all existing toxicological_effect_terms and the source_hashes that they cover
  toxicological_effect_terms = runQuery("SELECT * FROM toxicological_effect_terms ORDER BY id",
                                        toxval.db,
                                        db.type = db.type) %>%
    dplyr::rename(toxicological_effect = term)
  curr_source_hash = toxicological_effect_terms %>%
    dplyr::pull(source_hash) %>%
    unique()

  # Get source_hash values included in filtered DCAP input data
  dcap_hashes = readxl::read_xlsx(input_file) %>%
    tidyr::separate_rows(source_hash, sep=",") %>%
    tidyr::separate_rows(source_hash, sep=" \\|::\\| ") %>%
    dplyr::mutate(source_hash = stringr::str_squish(source_hash)) %>%
    dplyr::select(source_hash) %>%
    dplyr::distinct()

  # Get IUCLID DCAP entries whose source_hashes are not accounted for in toxicological_effect_terms
  missing_query = paste0("SELECT * FROM toxval WHERE (source_table IN ('", paste0(iuclid_dcap, collapse="', '"), "')",
                         " OR source IN ('", paste0(dcap_sources, collapse="', '"), "')) ",
                         " AND source_hash NOT IN ('", paste0(curr_source_hash, collapse="', '"), "') ",
                         "ORDER BY toxval_id")
  missing_entries = runQuery(missing_query,
                             toxval.db,
                             db.type = db.type) %>%
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
    dplyr::select(source_hash, toxicological_effect, study_type, source_table)

  # Map values to missing data
  missing_mapped = initial %>%
    # Separate toxicological_effect on |
    tidyr::separate_rows(toxicological_effect, sep="\\|") %>%
    # Remove entries that do not have a toxicological_effect value
    dplyr::filter(!toxicological_effect %in% c("-", "", as.character(NA))) %>%
    # Remove whitespace from toxicological_effect values to improve mapping
    dplyr::mutate(toxicological_effect = stringr::str_trim(toxicological_effect)) %>%
    # Map terms to entries missing categorization by uncollapsed toxicological_effect and study_type
    dplyr::left_join(toxicological_effect_terms %>%
                       dplyr::select(toxicological_effect, study_type, toxicological_effect_category) %>%
                       dplyr::distinct(),
                     by=c("toxicological_effect", "study_type")) %>%
    dplyr::mutate(
      study_type = study_type %>%
        gsub("nonneoplastic", "non-neoplastic", .)
    )

  # Get source_hashes that are still missing a mapping
  na_hashes = missing_mapped %>%
    dplyr::filter(is.na(toxicological_effect_category)) %>%
    dplyr::pull(source_hash) %>%
    unique()

  # Get IUCLID DCAP entries that still do not have a mapping
  query = paste0("SELECT * FROM toxval WHERE source_hash IN ('",
                 paste0(na_hashes, collapse="', '"), "') ORDER BY toxval_id")
  still_missing = runQuery(query, toxval.db, db.type = db.type)

  # Record results
  writexl::write_xlsx(missing_mapped %>% dplyr::filter(!is.na(toxicological_effect_category)),
                      paste0(output_dir, "dcap_mappings_identified.xlsx"))
  writexl::write_xlsx(still_missing,
                      paste0(output_dir, "dcap_mappings_still_missing.xlsx"))
  writexl::write_xlsx(missing_mapped %>%
                        dplyr::filter(is.na(toxicological_effect_category)) %>%
                        dplyr::select(-source_table) %>%
                        dplyr::group_by(toxicological_effect, study_type) %>%
                        dplyr::mutate(source_hash = source_hash %>% toString()) %>%
                        dplyr::ungroup() %>%
                        dplyr::distinct(),
                      paste0(output_dir, "dcap_missing_categorization.xlsx"))

  if(get_suggestions) {
    # Try to get suggested mappings
    remaining_categorizations = missing_mapped %>%
      dplyr::filter(is.na(toxicological_effect_category)) %>%
      dplyr::mutate(
        # Keep only first term in list of toxicological_effects
        toxicological_effect_original = toxicological_effect,
        toxicological_effect = toxicological_effect %>%
          gsub(",.*", "", .) %>%
          stringr::str_squish()
      ) %>%
      dplyr::select(-toxicological_effect_category) %>%
      # Try to match to individual toxicological_effect components
      dplyr::left_join(toxicological_effect_terms %>%
                         tidyr::separate_rows(toxicological_effect, sep=",\\s*") %>%
                         dplyr::select(toxicological_effect, study_type, toxicological_effect_category) %>%
                         dplyr::distinct(),
                       by=c("toxicological_effect", "study_type")) %>%
      dplyr::mutate(toxicological_effect = toxicological_effect_original) %>%
      dplyr::select(-toxicological_effect_original)

    # Get list of suggestions and any entries that are still missing mappings
    final_missing_categorizations = remaining_categorizations %>%
      dplyr::filter(is.na(toxicological_effect_category))
    mapping_suggestions = remaining_categorizations %>%
      dplyr::filter(!is.na(toxicological_effect_category)) %>%
      dplyr::mutate(s_temp = source_hash) %>%
      toxval.record.dedup(hashing_cols=c("source_hash", "s_temp", "toxicological_effect", "study_type", "source_table"),
                                 delim="|") %>%
      dplyr::rename(source_hash = s_temp)

    # Rewrite missing categorizations file, and write suggested mappings
    writexl::write_xlsx(final_missing_categorizations %>%
                          dplyr::select(-source_table) %>%
                          dplyr::group_by(toxicological_effect, study_type) %>%
                          dplyr::mutate(source_hash = source_hash %>% toString()) %>%
                          dplyr::ungroup() %>%
                          dplyr::distinct(),
                        paste0(output_dir, "dcap_missing_categorization.xlsx"))
    writexl::write_xlsx(mapping_suggestions,
                        paste0(output_dir, "dcap_mapping_suggestions.xlsx"))
  }
}
