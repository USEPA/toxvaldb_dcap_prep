#-----------------------------------------------------------------------------------
#' Set the study_group field
#'
#' @param df Input dataframe to set study_group
#' @return for each source writes an Excel file with the name
#'  ../export/export_by_source_{data}/toxval_all_{toxval.db}_{source}.xlsx
#' @export
#-----------------------------------------------------------------------------------
fix.study_group <- function(df) {
  # Query unique study fields
  group_vars = c("toxval_id", "source", "subsource", "dtxsid", "common_name", "toxval_units",
                 "study_type", "exposure_route", "exposure_method", "exposure_form",
                 "study_duration_value", "study_duration_units", "sex", "lifestage", "generation",
                 "strain", "long_ref", "title")

  df = df %>%
    dplyr::select(dplyr::any_of(group_vars)) %>%
    dplyr::distinct()

  report.out = data.frame()

  # Set non-hashing columns to ignore for study_group hashing
  non_hashing_cols = c("toxval_id")

  source = unique(df$source)

  # Do not group by record_source fields for EFSA and HPVIS
  if(source %in% c("EFSA", "HPVIS")){
    non_hashing_cols = c("toxval_id", "long_ref", "title")
  } else if (source %in% c("ECHA IUCLID")){
    non_hashing_cols = c("toxval_id")
  }

  # Hash to identify duplicate groups
  temp.temp = df %>%
    tidyr::unite(hash_col, dplyr::all_of(sort(names(.)[!names(.) %in% non_hashing_cols])), sep="") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(source_hash = paste0("ToxValhc_", digest::digest(hash_col, serialize = FALSE))) %>%
    dplyr::ungroup()

  temp_sg = df %>%
    dplyr::mutate(source_hash = temp.temp$source_hash) %>%
    dplyr::select(toxval_id, source, subsource, source_hash) %>%
    # Collapse toxval_id for duplicate hashes
    dplyr::group_by(dplyr::across(c(-toxval_id))) %>%
    dplyr::summarise(toxval_id = toString(toxval_id)) %>%
    dplyr::ungroup() %>%
    # Only account for those with duplicates
    dplyr::filter(grepl(",", toxval_id))

  if(nrow(temp_sg)){
    temp_sg = temp_sg %>%
      # Assign study group
      dplyr::mutate(study_group = 1:n() %>%
                      paste0(source, "_dup_", subsource, "_", .)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-source_hash) %>%
      # Separate collapse toxval_id groups
      tidyr::separate_rows(toxval_id, sep=", ") %>%
      dplyr::mutate(toxval_id = as.numeric(toxval_id))
  } else {
    # Set up empty duplicate study_group dataframe
    temp_sg = df %>%
      dplyr::mutate(study_group = NA) %>%
      .[0,]
  }

  # Check/report if any duplicates present
  nr = nrow(df)
  nsg = length(unique(temp_sg$study_group)) + length(df$toxval_id[!df$toxval_id %in% temp_sg$toxval_id])
  cat("  nrow:",nr," unique values:",nsg,"\n")
  # Set default study group to toxval_id
  src.report.out = df %>%
    dplyr::mutate(study_group = paste0(source,':',toxval_id,':',sex,':',generation,lifestage)) %>%
    dplyr::filter(!toxval_id %in% temp_sg$toxval_id)

  # If duplicate groups, set to generated study group
  if(nsg!=nr) {
    cat("   Number of dups:", length(unique(temp_sg$study_group)),"\n")

    src.dups = df %>%
      dplyr::filter(toxval_id %in% temp_sg$toxval_id) %>%
      dplyr::mutate(study_group_addition = paste0(sex,':',generation,lifestage)) %>%
      dplyr::select(toxval_id, study_group_addition)

    src.report.out = temp_sg %>%
      dplyr::left_join(src.dups,
                       by="toxval_id") %>%
      tidyr::unite(col="study_group", study_group, study_group_addition, sep = ":", na.rm=TRUE) %>%
      # Add dups to report
      dplyr::bind_rows(src.report.out, .) %>%
      dplyr::distinct()
  }
  # Bind source reports
  report.out = report.out %>%
    dplyr::bind_rows(src.report.out) %>%
    dplyr::select(toxval_id, study_group)

  return(report.out)
}
