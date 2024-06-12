#-----------------------------------------------------------------------------------
#' @title write_dcap_summary
#' @description Output summary reports containing chemical and key field information
#' @param toxval.db The version of ToxVal to use
#' @param sys.date The date of the export to use, Default: Sys.Date()
#' @return None; writes summary reports to file
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx]
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [select][dplyr::select], [distinct][dplyr::distinct], [left_join][dplyr::left_join], [pull][dplyr::pull], [ensym][dplyr::ensym]
#'  [replace_na][tidyr::replace_na]
#'  [tibble][tibble::tibble]
#'  [write.xlsx][openxlsx::write.xlsx]
#' @rdname write_dcap_summary
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter select distinct left_join pull ensym
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @importFrom openxlsx write.xlsx
#-----------------------------------------------------------------------------------
write_dcap_summary <- function(toxval.db, sys.date=Sys.Date()) {
  # Read in previous DCAP export
  dcap_data = readxl::read_xlsx(paste0("data/results/ToxValDB for BMDh ",toxval.db," ",sys.date,".xlsx")) %>%
    dplyr::mutate(toxval_type_supercategory = "Point of Departure")

  # Write list containing all DCAP sources
  dcap_list = c("ATSDR PFAS 2021", "ATSDR MRLs", "Cal OEHHA", "Copper Manufacturers",
                "ECHA IUCLID", "ECOTOX", "EFSA", "HAWC PFAS 430",
                "HAWC Project", "Health Canada", "HESS", "HPVIS", "IRIS",
                "NTP PFAS", "PPRTV (CPHEA)", "ToxRefDB","WHO JECFA Tox Studies")

  # Write list of key fields
  key_fields = c("study_type", "common_name", "exposure_route", "toxval_units", "toxval_type_supercategory")

  # Track various metrics for aggregate report
  all_excl_chems = data.frame()
  all_chems = data.frame()
  all_excl_key_fields = list(data.frame(), data.frame(), data.frame(), data.frame(), data.frame())
  total_toxval_records = 0
  prop_missing_key_fields = data.frame()

  # Query ToxVal for records by source
  for(src in dcap_list) {
    cat("Generating report for source", src, "\n")
    # Filter DCAP data to current source
    current_source_dcap = dcap_data %>%
      dplyr::filter(source == !!src)

    # Handle inclusion of only specified IUCLID OHTs for later queries
    iuclid_addition = NULL
    if(src == "ECHA IUCLID") {
      iuclid_addition = paste0(" AND source_table in ",
                               "('source_iuclid_repeateddosetoxicityoral', ",
                               "'source_iuclid_developmentaltoxicityteratogenicity', ",
                               "'source_iuclid_carcinogenicity', ",
                               "'source_iuclid_immunotoxicity', ",
                               "'source_iuclid_neurotoxicity')")
    }

    # Get current source DCAP chemicals and ToxVal chemicals
    current_dpac_chems = current_source_dcap %>%
      dplyr::select(dtxsid) %>%
      dplyr::distinct() %>%
      dplyr::mutate(remove=1)
    current_toxval_chems = runQuery(paste0("SELECT DISTINCT dtxsid FROM toxval WHERE source='",
                                           src, "'", iuclid_addition),
                                    toxval.db)
    num_chems_included = nrow(current_dpac_chems)
    num_chems_excluded = nrow(current_toxval_chems) - num_chems_included
    prop_chems_excluded = num_chems_excluded / nrow(current_toxval_chems)
    excluded_chems = current_toxval_chems %>%
      dplyr::left_join(current_dpac_chems) %>%
      dplyr::mutate(remove = remove %>% tidyr::replace_na(0)) %>%
      dplyr::filter(remove != 1) %>%
      dplyr::select(dtxsid) %>%
      dplyr::distinct()
    all_excl_chems = rbind(all_excl_chems, excluded_chems) %>%
      dplyr::distinct()
    all_chems = rbind(all_chems, current_toxval_chems) %>%
      dplyr::distinct()

    # Get current source DCAP and ToxVal record counts
    current_dpac_count = nrow(current_source_dcap)
    current_toxval_count = runQuery(paste0("SELECT COUNT(*) FROM toxval WHERE source='",
                                           src, "'", iuclid_addition),
                                    toxval.db)[,1]
    num_records_excluded = current_toxval_count - current_dpac_count
    prop_records_excluded = num_records_excluded / current_toxval_count
    total_toxval_records = total_toxval_records + current_toxval_count

    # Create numeric summary
    report_summary = tibble::tibble(
      `Num Chems Included` = !!num_chems_included,
      `Num Chems Excluded` = !!num_chems_excluded,
      `Prop. Chems Excluded` = !!prop_chems_excluded,
      `Num Records Included` = !!current_dpac_count,
      `Num Records Excluded` = !!num_records_excluded,
      `Prop. Records Excluded` = !!prop_records_excluded
    )

    # Get key field information
    output_reports = list(report_summary, excluded_chems)
    for(i in seq_len(length(key_fields))) {
      field = key_fields[i]
      # Handle toxval_type_supercategory and common_name separately
      if(field == "toxval_type_supercategory") {
        key_vals = runQuery(paste0("SELECT b.toxval_type_supercategory ",
                                   "FROM toxval a ",
                                   "LEFT JOIN toxval_type_dictionary b on a.toxval_type=b.toxval_type ",
                                   "WHERE a.source='", src, "'", iuclid_addition),
                            toxval.db)
      } else if(field == "common_name") {
        key_vals = runQuery(paste0("SELECT b.common_name ",
                                   "FROM toxval a ",
                                   "LEFT JOIN species b on a.species_id=b.species_id ",
                                   "WHERE a.source='", src, "'", iuclid_addition),
                            toxval.db)
      } else {
        # Get current field from ToxVal for this source
        key_vals = runQuery(paste0("SELECT ", field, " FROM toxval WHERE source='",
                                   src, "'", iuclid_addition),
                            toxval.db)
      }

      # Get proportion of entries missing current field
      key_col_empty = key_vals %>%
        dplyr::pull(dplyr::ensym(field))
      key_col_empty = key_col_empty[key_col_empty %in% c("-", as.character(NA), "")]
      key_num_missing = length(key_col_empty)
      if(is.null(key_num_missing)) {
        key_num_missing = 0
        key_prop_missing = 0
      } else key_prop_missing = key_num_missing / current_toxval_count

      # Store results
      curr_prop_missing_key_fields = tibble::tibble(
        Source = !!src,
        Field = !!field,
        `Entries Missing Value` = !!key_num_missing,
        `Proportion Missing Value` = !!key_prop_missing
      )
      prop_missing_key_fields = prop_missing_key_fields %>%
        rbind(curr_prop_missing_key_fields)

      # Get list of key field values that are left out
      dcap_key_vals = dcap_data %>%
        dplyr::select(dplyr::ensym(field)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(remove=1)
      dropped_key_vals = key_vals %>%
        dplyr::distinct() %>%
        dplyr::left_join(dcap_key_vals) %>%
        dplyr::mutate(remove = remove %>% tidyr::replace_na(0)) %>%
        dplyr::filter(remove != 1) %>%
        dplyr::select(-remove)

      # Add findings to report
      output_reports = append(output_reports, dropped_key_vals)

      all_excl_key_fields[i] = rbind(all_excl_key_fields[i], dropped_key_vals) %>%
        unique()
    }

    # Add proportion missing information to output
    output_reports = append(output_reports, list(prop_missing_key_fields %>%
                                                   dplyr::filter(Source == !!src)))

    # Name output reports
    names(output_reports) = c("general_summary", "excl_chemicals",
                              lapply(key_fields, function(x) paste0("excl_", x)),
                              "prop_missing_key_fields")

    # Write source-specific report to file
    openxlsx::write.xlsx(output_reports, paste0("data/results/dcap_source_summaries/dcap_summary_",
                                                src, "_", sys.date, ".xlsx"))
    cat("\n")
  }

  # Generate aggregate report
  num_chems_included = length(unique(dcap_data$dtxsid))
  num_chems_excluded = nrow(all_excl_chems)
  prop_chems_excluded = num_chems_excluded / nrow(all_chems)

  num_records_included = nrow(dcap_data)
  num_records_excluded = total_toxval_records - num_records_included
  prop_records_excluded = num_records_excluded / total_toxval_records

  agg_report_summary = tibble::tibble(
    `Num Chems Included` = !!num_chems_included,
    `Num Chems Excluded` = !!num_chems_excluded,
    `Prop. Chems Excluded` = !!prop_chems_excluded,
    `Num Records Included` = !!num_records_included,
    `Num Records Excluded` = !!num_records_excluded,
    `Prop. Records Excluded` = !!prop_records_excluded
  )

  aggregate_report = list(agg_report_summary, all_excl_chems)
  aggregate_report = append(aggregate_report, all_excl_key_fields)
  aggregate_report = append(aggregate_report, list(prop_missing_key_fields %>%
                                                     dplyr::filter(`Entries Missing Value` > 0)))
  names(aggregate_report) = c("general_summary", "excl_chemicals", "excl_study_type", "excl_common_name",
                              "excl_exposure_route", "excl_toxval_units", "excl_toxval_type_supercategory",
                              "prop_missing_key_fields")

  # Write aggregate report to file
  openxlsx::write.xlsx(aggregate_report,paste0("data/results/dcap_source_summaries/dcap_summary_ALL SOURCES_",
                                               sys.date, ".xlsx"))
}
