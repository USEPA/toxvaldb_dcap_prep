#' @title filter.pods
#' @description Filter values for DCAP according to specified POD rules.
#' @param toxval.db Database name.
#' @param run_name The desired name for the output directory, default current date.
#' @param db.type String of what kind of database connection to use, default "mysql. If "sqlite", workflow with use .Renv defined "sqlite_file" file path.
#' @return None, filtered results are recorded in Excel file.
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  filter.pods(toxval.db = "res_toxval_v96_1")
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#' @rdname filter.pods
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter case_when group_by ungroup select bind_rows
#' @importFrom stringr str_trim str_extract
#' @importFrom writexl write_xlsx
#' @importFrom tidyr replace_na unite separate_rows
#' @importFrom tidyselect contains
#' @importFrom digest digest
filter.pods <- function(toxval.db, run_name=Sys.Date(), db.type) {
  printCurrentFunction(toxval.db)

  # Read in initial export data
  dir = paste0(Sys.getenv("datapath"), "data/results/", run_name, "/")
  file = paste0(dir,"results/ToxValDB for DCAP ",toxval.db,".xlsx")
  res0 = readxl::read_xlsx(file) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-")))

  # Additional check to remove "cancer" form toxicological_effect_category since export.for.dcap()
  res0 = res0 %>%
    dplyr::mutate(
      toxicological_effect_category = toxicological_effect_category %>%
        gsub("\\|cancer\\|", "|", .) %>%
        gsub("\\|cancer|cancer\\|", "", .) %>%
        gsub("cancer\\|", "", .) %>%
        stringr::str_squish()
    )

  # Establish list of authoritative sources
  auth_sources = c(
    # "ATSDR MRLs"
    "source_atsdr_mrls",
    #"EPA HHTV",
    "source_epa_hhtv",
    #"Health Canada",
    "source_health_canada",
    #"IRIS",
    "source_iris",
    #"HEAST",
    "source_heast",
    #"PPRTV (CPHEA)"
    "source_pprtv_cphea"
  )

  cat("Filtering authoritative sources:\n", paste0("- ", sort(unique(res0$source[res0$source_table %in% auth_sources])),
                                                   collapse="\n "),
      "\n")

  # Get key_finding PODs from authoritative sources
  res_auth = res0 %>%
    dplyr::filter(
      source_table %in% auth_sources,
      grepl("key|yes", key_finding, ignore.case=TRUE)
    ) %>%
    dplyr::mutate(filter_pod_group = "auth_key")

  # Get list of key auth study_groups
  key_finding_study_groups = res_auth %>%
    dplyr::pull(study_group) %>%
    unique()

  # Filter out non-key auth records in the key-auth study_groups
  filtered_out_auth_non_key_study_group = res0 %>%
    dplyr::filter(study_group %in% key_finding_study_groups,
                  !key_finding %in% c("yes", "key")) %>%
    dplyr::mutate(
      reason_for_filtering = "authoritative non-key_finding record from authoritative study_group with key_finding"
    )

  res_auth_not_key = res0 %>%
    dplyr::filter(
      source_table %in% auth_sources,
      !grepl("key|yes", key_finding, ignore.case=TRUE),
      !study_group %in% key_finding_study_groups
    ) %>%
    dplyr::mutate(filter_pod_group = "auth_not_key")

  cat("Filtering non-authoritative sources\n")
  # Extract entries with key_finding for non_authoritative sources
  # non_auth_key_findings = res0 %>%
  #   dplyr::filter(!source_table %in% auth_sources,
  #                 key_finding %in% c("yes", "key"))
  #
  # non_auth_key_finding_study_groups = non_auth_key_findings %>%
  #   dplyr::pull(study_group) %>%
  #   unique()
  #
  # non_auth_non_key_findings_filtered = res0 %>%
  #   dplyr::filter(study_group %in% non_auth_key_finding_study_groups,
  #                 !key_finding %in% c("yes", "key")) %>%
  #   dplyr::mutate(
  #     reason_for_filtering = "from non-auth study group that were not key_finding"
  #   )

  # Filter non-authoritative sources
  res_init = res0 %>%
    dplyr::filter(!source_table %in% auth_sources,
                  !study_group %in% key_finding_study_groups
    ) %>%
    dplyr::mutate(filter_pod_group = "non_auth") %>%
    dplyr::bind_rows(res_auth, res_auth_not_key#, non_auth_key_findings
    ) %>%
    dplyr::mutate(
      # Standardize toxval_type values to "{TYPE} {MODIFIER}"
      tts = dplyr::case_when(
        grepl("BMDL?", toxval_type) & grepl("HED", toxval_type) ~ "BMDL HED",
        grepl("BMDL?", toxval_type) & grepl("ADJ", toxval_type) ~ "BMDL ADJ",
        grepl("BMDL?", toxval_type) ~ "BMDL",
        grepl("NOAEL", toxval_type) & grepl("HED", toxval_type) ~ "NOAEL HED",
        grepl("NOAEL", toxval_type) & grepl("ADJ", toxval_type) ~ "NOAEL ADJ",
        grepl("NOAEL", toxval_type) ~ "NOAEL",
        grepl("LOAEL", toxval_type) & grepl("HED", toxval_type) ~ "LOAEL HED",
        grepl("LOAEL", toxval_type) & grepl("ADJ", toxval_type) ~ "LOAEL ADJ",
        grepl("LOAEL", toxval_type) ~ "LOAEL",
        grepl("NO?EL", toxval_type) & grepl("HED", toxval_type) ~ "NEL HED",
        grepl("NO?EL", toxval_type) & grepl("ADJ", toxval_type) ~ "NEL ADJ",
        grepl("NO?EL", toxval_type) ~ "NEL",
        grepl("LO?EL", toxval_type) & grepl("HED", toxval_type) ~ "LEL HED",
        grepl("LO?EL", toxval_type) & grepl("ADJ", toxval_type) ~ "LEL ADJ",
        grepl("LO?EL", toxval_type) ~ "LEL",
        grepl("FO?EL", toxval_type) & grepl("HED", toxval_type) ~ "FEL HED",
        grepl("FO?EL", toxval_type) & grepl("ADJ", toxval_type) ~ "FEL ADJ",
        grepl("FO?EL", toxval_type) ~ "FEL",
        TRUE ~ as.character(NA)
      ) %>%
        stringr::str_trim(),
      # Get underlying root of toxval_type (no HED/ADJ/etc.)
      ttr = tts %>%
        stringr::str_extract("(BMDL|[NLF]OAEL|[NLF]EL)", group=1)
    ) %>%
    dplyr::group_by(study_group, filter_pod_group) %>%
    # Get minimum LOAEL/LEL values to initially filter out NOAEL/NEL > LOAEL/LEL
    dplyr::mutate(
      low_loael = dplyr::case_when(
        ttr == "LOAEL" ~ toxval_numeric,
        TRUE ~ NA
      ),
      low_loael = suppressWarnings(min(low_loael, na.rm = TRUE)),

      low_lel = dplyr::case_when(
        ttr == "LEL" ~ toxval_numeric,
        TRUE ~ NA
      ),
      low_lel = suppressWarnings(min(low_lel, na.rm = TRUE)),

      max_noael = dplyr::case_when(
        ttr == "NOAEL" & toxval_numeric < low_loael ~ toxval_numeric,
        TRUE ~ NA
      ),
      max_noael = suppressWarnings(max(max_noael, na.rm = TRUE)),

      max_nel = dplyr::case_when(
        ttr == "NEL" & toxval_numeric < low_lel ~ toxval_numeric,
        TRUE ~ NA
      ),
      max_nel = suppressWarnings(max(max_nel, na.rm = TRUE)),
      # Replace Inf with NA from min with only NA values
      dplyr::across(c(low_loael, low_lel, max_noael, max_nel), ~dplyr::na_if(., Inf))
    ) %>%
    dplyr::ungroup() %>%
    # Remove NOAEL/NEL greater than minimum LOAEL/LEL
    dplyr::mutate(
      remove_flag = dplyr::case_when(
        ttr == "NOAEL" & toxval_numeric >= low_loael & !is.na(low_loael) ~ 1,
        ttr == "NEL" & toxval_numeric >= low_lel & !is.na(low_lel) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    # Get maximum/minumum values for each toxval_type within study groups (ignoring entries to remove)
    dplyr::group_by(study_group, filter_pod_group, tts, remove_flag) %>%
    dplyr::mutate(
      min_val = min(toxval_numeric, na.rm = TRUE),
      max_val = max(toxval_numeric, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(study_group, filter_pod_group, remove_flag) %>%
    dplyr::mutate(
      # Tag each entry in every study_group with filter step it corresponds to
      keep_flag = dplyr::case_when(
        # Tag appropriate number based on priority
        tts == "BMDL HED" & toxval_numeric == min_val ~ 1,
        tts == "BMDL ADJ" & toxval_numeric == min_val ~ 2,
        tts == "BMDL" & toxval_numeric == min_val ~ 3,
        tts == "NOAEL HED" & toxval_numeric == max_val & remove_flag != 1 ~ 4,
        tts == "NOAEL ADJ" & toxval_numeric == max_val & remove_flag != 1 ~ 5,
        tts == "NOAEL" & toxval_numeric == max_val & remove_flag != 1 ~ 6,
        tts == "LOAEL HED" & toxval_numeric == min_val ~ 7,
        tts == "LOAEL ADJ" & toxval_numeric == min_val ~ 8,
        tts == "LOAEL" & toxval_numeric == min_val ~ 9,
        tts == "NEL HED" & toxval_numeric == max_val & remove_flag != 1 ~ 10,
        tts == "NEL ADJ" & toxval_numeric == max_val & remove_flag != 1 ~ 11,
        tts == "NEL" & toxval_numeric == max_val & remove_flag != 1 ~ 12,
        tts == "LEL HED" & toxval_numeric == min_val ~ 13,
        tts == "LEL ADJ" & toxval_numeric == min_val ~ 14,
        tts == "LEL" & toxval_numeric == min_val ~ 15,
        tts == "FEL HED" & toxval_numeric == min_val ~ 16,
        tts == "FEL ADJ" & toxval_numeric == min_val ~ 17,
        tts == "FEL" & toxval_numeric == min_val ~ 18,
        TRUE ~ 999
      ),

      # Commenting out for now to troubleshoot use (2024-10-16)
      # repro_dev_fix = dplyr::case_when(
      #   ttr %in% c("BMDL", "NOAEL", "LOAEL", "NEL", "LEL", "FEL") & study_type == "reproduction developmental" ~ 1,
      #   TRUE ~ 0
      # ),

      # Pick row with highest priority
      selected_row = min(keep_flag, na.rm = TRUE),

      # Special case to check where NOAEL/LOAEL, NOEL/LOEL, or NEL/LEL may have the same dose, keep the L form
      keep_flag_tie = dplyr::case_when(
        all(c(4, 7) %in% keep_flag) & all(remove_flag != 1) & any(low_loael == max_noael) & any(c(4, 7) %in% selected_row) ~ 7,
        all(c(5, 8) %in% keep_flag) & all(remove_flag != 1) & any(low_loael == max_noael) & any(c(5, 8) %in% selected_row) ~ 8,
        all(c(6, 9) %in% keep_flag) & all(remove_flag != 1) & any(low_loael == max_noael) & any(c(6, 9) %in% selected_row) ~ 9,
        all(c(10, 13) %in% keep_flag) & all(remove_flag != 1) & any(low_lel == max_nel) & any(c(10, 13) %in% selected_row) ~ 13,
        all(c(11, 14) %in% keep_flag) & all(remove_flag != 1) & any(low_lel == max_nel) & any(c(11, 14) %in% selected_row) ~ 14,
        all(c(12, 15) %in% keep_flag) & all(remove_flag != 1) & any(low_lel == max_nel) & any(c(12, 15) %in% selected_row) ~ 15,
        TRUE ~ NA
      ),

      selected_row = dplyr::case_when(
        !is.na(keep_flag_tie) ~ keep_flag_tie,
        TRUE ~ selected_row
      ),

      # Set reason for filtering row out
      reason_for_filtering = dplyr::case_when(
        remove_flag == 1 ~ "NEL/NOAEL greater than minimum LEL/LOAEL",
        selected_row == 1 ~ "BMDL (HED) selected for this group",
        selected_row == 2 ~ "BMDL (ADJ) selected for this group",
        selected_row == 3 ~ "BMDL selected for this group",
        selected_row == 4 ~ "NOAEL (HED) selected for this group",
        selected_row == 5 ~ "NOAEL (ADJ) selected for this group",
        selected_row == 6 ~ "NOAEL selected for this group",
        selected_row == 7 ~ "LOAEL (HED) selected for this group",
        selected_row == 8 ~ "LOAEL (ADJ) selected for this group",
        selected_row == 9 ~ "LOAEL selected for this group",
        selected_row == 10 ~ "NEL (HED) selected for this group",
        selected_row == 11 ~ "NEL (ADJ) selected for this group",
        selected_row == 12 ~ "NEL selected for this group",
        selected_row == 13 ~ "LEL (HED) selected for this group",
        selected_row == 14 ~ "LEL (ADJ) selected for this group",
        selected_row == 15 ~ "LEL selected for this group",
        selected_row == 16 ~ "FEL (HED) selected for this group",
        selected_row == 17 ~ "FEL (ADJ) selected for this group",
        selected_row == 18 ~ "FEL selected for this group",
        TRUE ~ "no selection from this group"
      ),

      # Add reason based on special tie case
      reason_for_filtering = dplyr::case_when(
        keep_flag_tie == 7 ~ paste0("LOAEL (HED) selected for this group with NOAEL (HED) dose tie"),
        keep_flag_tie == 8 ~ paste0("LOAEL (ADJ) selected for this group with NOAEL (ADJ) dose tie"),
        keep_flag_tie == 9 ~ paste0("LOAEL selected for this group with NOAEL dose tie"),
        keep_flag_tie == 13 ~ paste0("LEL (HED) selected for this group with NEL (HED) dose tie"),
        keep_flag_tie == 14 ~ paste0("LEL (ADJ) selected for this group with NEL (ADJ) dose tie"),
        keep_flag_tie == 15 ~ paste0("LEL selected for this group with NEL dose tie"),
        TRUE ~ reason_for_filtering
      ),
    ) %>%
    dplyr::ungroup()

  # Check assignments
  # View(res_init %>% select(dplyr::any_of(c("study_group", "toxval_type", "toxval_numeric", "toxval_units", "tts", "ttr", "low_loael", "low_lel", "max_noael", "max_nel", "remove_flag", "min_val", "max_val", "selected_row", "keep_flag", "keep_flag_tie", "reason_for_filtering"))) %>% distinct())

  # Select chosen rows
  res = res_init %>%
    dplyr::filter(selected_row == keep_flag,
                  remove_flag != 1) %>%
    # # Commenting out for now to troubleshoot use (2024-10-16)
    # # Handle case where repro dev causes multiple selections per study_group
    # dplyr::group_by(study_group, filter_pod_group) %>%
    # dplyr::mutate(
    #   n = dplyr::n(),
    #
    #   drop = dplyr::case_when(
    #     n > 1 & repro_dev_fix == 0 ~ 1,
    #     TRUE ~ 0
    #   )
    # ) %>%
    # dplyr::ungroup() %>%
    # dplyr::filter(drop == 0) %>%
    dplyr::select(-dplyr::any_of(c("tts", "ttr", "low_loael", "low_lel", "max_noael", "max_nel", "min_val", "max_val", "remove_flag",
                                   "keep_flag", "selected_row", "reason_for_filtering", "repro_dev_fix", "n", "drop",
                                   "keep_flag_tie")))

  # Select rows that were filtered out
  filtered_out_non_auth = res_init %>%
    dplyr::filter(!source_hash %in% res$source_hash) %>%
    # dplyr::filter(remove_flag == 1 | keep_flag != selected_row) %>%
    dplyr::select(-c("tts", "ttr", "low_loael", "low_lel", "min_val", "max_val",
                     "remove_flag", "keep_flag", "selected_row"))

  # Combine filtered out data from authoritative and non-authoritative sources
  filtered_out = dplyr::bind_rows(
    filtered_out_non_auth, filtered_out_auth_non_key_study_group
  )

  if(nrow(filtered_out) + nrow(res) != nrow(res0)){
    # Check for any accidental overlap between selected and filtered groups
    combined = res %>%
      dplyr::select(source_hash) %>%
      dplyr::bind_rows(filtered_out %>%
                         dplyr::select(source_hash))

    dups = combined %>%
      dplyr::group_by(source_hash) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(n > 1)
    if(nrow(dups)){
      browser("...Records found in filter and not filtered datasets...")
    }

    # Check if any missing from filtering
    missing = res0 %>%
      dplyr::filter(!source_hash %in% combined$source_hash)

    if(nrow(missing)){
      browser("...Records missing from filtering process...")
    }
  }

  # Flag case where an entire study_group was removed
  if(any(!unique(filtered_out$study_group) %in% unique(res$study_group))){
    tmp = unique(filtered_out$study_group)[!unique(filtered_out$study_group) %in% unique(res$study_group)]
    message("study_groups completely filtered out: ", length(tmp))
    browser()
  }

  # Perform deduping on identical records with different source_hash values
  non_hashing_cols = c("source_hash", "record_source_info", "toxicological_effect", "name",
                       "toxval_subtype", "toxicological_effect_category", "multiple_flag",
                       "toxicological_effect_category_original")
  # Add model and record_source fields to non_hashing_cols
  record_source_cols = runQuery("SELECT * FROM record_source LIMIT 1",
                                toxval.db,
                                db.type = db.type) %>%
    names()

  model_non_hash_cols = names(res %>% dplyr::select(tidyselect::contains("model")))
  non_hashing_cols = c(non_hashing_cols, model_non_hash_cols, record_source_cols) %>%
    unique()
  hashing_cols = names(res %>% dplyr::select(-dplyr::any_of(!!non_hashing_cols)))

  # Manually handle LOEL > LEL edge case for ECHA IUCLID_dup_Repeated Dose Toxicity Oral_11183:M/F:--
  res = res %>%
    dplyr::mutate(
      toxval_type = dplyr::case_when(
        study_group == "ECHA IUCLID_dup_Repeated Dose Toxicity Oral_11183:M/F:--" ~ "LOEL",
        TRUE ~ toxval_type
      )
    )

  # Hash to identify duplicate groups
  res.temp = res %>%
    tidyr::unite(hash_col, dplyr::all_of(sort(names(.)[!names(.) %in% non_hashing_cols])), sep="") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(source_hash_temp = paste0("ToxValhc_", digest::digest(hash_col, serialize = FALSE))) %>%
    dplyr::ungroup()
  res$source_hash_temp = res.temp$source_hash_temp

  # Collapse deduping fields
  res = res %>%
    dplyr::mutate(
      # Replace previously set | delimeters to help with deduping
      dplyr::across(dplyr::any_of(!!non_hashing_cols), ~gsub("\\|", " |::| ", .))
    ) %>%
    dplyr::group_by(source_hash_temp, study_group) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(!!non_hashing_cols),
                                # Ensure unique entries in alphabetic order
                                ~paste0(sort(unique(.[!is.na(.)])), collapse=" |::| ") %>%
                                  dplyr::na_if("NA") %>%
                                  dplyr::na_if("") %>%
                                  dplyr::na_if("-")
    )) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    # Create unique list of entries, then collapse
    dplyr::mutate(dplyr::across(dplyr::any_of(!!non_hashing_cols), ~stringr::str_split(., " \\|::\\| ") %>%
                                  unlist() %>%
                                  unique() %>%
                                  paste0(collapse=" |::| "))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Replace |::| with | where appropriate
      toxicological_effect = gsub(" \\|::\\| ", "|", toxicological_effect),
      toxicological_effect_category = gsub(" \\|::\\| ", "|", toxicological_effect_category),
      toxicological_effect_category_original = gsub(" \\|::\\| ", "|", toxicological_effect_category_original),
      dplyr::across(!!model_non_hash_cols, ~gsub(" \\|::\\| ", "|", .))
    ) %>%
    dplyr::distinct() %>%
    dplyr::select(-source_hash_temp)

  # Flag and collapse case where multiple records chosen per study_group
  # Use special delimiter
  if(any(duplicated(res$study_group))){
    tmp = res %>%
      dplyr::filter(duplicated(study_group)) %>%
      dplyr::pull(study_group)
    message("study_groups with multiple records selected: ", length(tmp))
    message("Collapsing...")
    res = toxval.record.dedup(res %>%
                                       dplyr::rename(source_hash_toxval=source_hash),
                                     hashing_cols=c("study_group", "toxval_type", "toxval_numeric"),
                                     delim=" <::> ") %>%
      # Replace "|::|" in toxicological_effect with "|" delimiter
      dplyr::mutate(dplyr::across(dplyr::any_of(c("toxicological_effect", "toxicological_effect_category_original", "toxicological_effect_category")),
                                  ~gsub(" <::> ", "|", ., fixed = TRUE)
      ),
      source_hash = source_hash_toxval %>%
        gsub(" <::> ", ",", ., fixed = TRUE)

      ) %>%
      dplyr::select(-source_hash_toxval)
  }

  # res$toxicological_effect_category[grepl("ToxValhc_588aba8b214220f10302d03dfdaab1c9|ToxValhc_b67010d01feae0e8ec9023ea570ae651", res$source_hash)]

  # Add duration_adjustment field to POD filtered output
  dedup_fields = c("study_type", "duration_adjustment")
  hashing_fields = names(res)[!names(res) %in% dedup_fields]
  res = res %>%
    # Temporarily separate study_types to get unique duration_adjustment assignment
    tidyr::separate_rows(study_type, sep=" \\|::\\| ") %>%
    # Use rules to assign correct duration_adjustment
    dplyr::mutate(
      duration_adjustment = dplyr::case_when(
        grepl("development|reproduction", toxicological_effect_category_original) ~ "no adjustment",
        study_type == "developmental" ~ "no adjustment",
        study_type %in% c("short-term", "subchronic", "chronic") ~ study_type,
        study_type %in% c("clinical", "repeat dose other") ~ "subchronic",

        study_type == "reproduction developmental" & study_duration_class %in% c(NA, "-") ~ "subchronic",
        study_type == "reproduction developmental" ~ study_duration_class %>%
          gsub("\\(.+", "", .) %>%
          stringr::str_squish(),
        TRUE ~ as.character(NA)
      ),
      # toxval.source.import.dedup removes the locally generated source_hash, so preserve here
      source_hash_old = source_hash
    ) %>%
    # Recombine rows
    toxval.record.dedup(dedup_fields=dedup_fields, hashing_cols=hashing_fields) %>%
    # Translate key_finding to boolean
    dplyr::mutate(key_finding = dplyr::case_when(
      !source_table %in% auth_sources ~ NA,
      grepl("key|yes", key_finding, ignore.case=TRUE) ~ 1,
      # Only display key_finding for authoritative sources
      TRUE ~ 0
    )) %>%
    # Rename key_finding and source_hash fields
    dplyr::rename(calibration_flag = key_finding, source_hash = source_hash_old)

  # Load calibration dictionary
  calibration_dict = readxl::read_xlsx(paste0(Sys.getenv("datapath"), Sys.getenv("calibration_dict")),
                                       guess_max = 21474836) %>%
    dplyr::distinct()

  # Remove and export duplicate mappings
  dup_calib_dict = calibration_dict %>%
    dplyr::group_by(source, dtxsid, toxval_type, toxval_numeric, study_duration_class, study_duration_value, study_duration_units) %>%
    dplyr::filter(dplyr::n()>1)

  if(nrow(dup_calib_dict)){
    writexl::write_xlsx(missing_calibration_dict,
                        paste0(dir, "dup_calibration_dict.xlsx"))
    stop("Duplicate calibration dictionary entries found...see ", paste0(dir, "dup_calibration_dict.xlsx"))
  }

  # Export missing calibration dictionary
  missing_calibration_dict = res %>%
    dplyr::mutate(calibration_string = paste(source, dtxsid, toxval_type, toxval_numeric, study_duration_class, study_duration_value, study_duration_units,
                                             sep = "_")) %>%
    dplyr::filter(calibration_flag %in% c(1),
                  !calibration_string %in% calibration_dict$calibration_string) %>%
    dplyr::select(calibration_string, source, dtxsid, toxval_type, toxval_numeric, study_duration_class, study_duration_value, study_duration_units)

  if(nrow(missing_calibration_dict)){
    writexl::write_xlsx(missing_calibration_dict,
                        paste0(dir, "missing_calibration_dict.xlsx"))
  }

  # Add calibration_class field for calibration_flag records
  res = res %>%
    dplyr::left_join(calibration_dict,
                     by = c("source", "dtxsid", "toxval_type", "toxval_numeric",
                            "study_duration_class", "study_duration_value", "study_duration_units"))
  # dplyr::mutate(calibration_class = dplyr::case_when(
  #   study_type %in% c("chronic", "subchronic") & calibration_flag %in% c(1) ~ study_type,
  #   TRUE ~ NA
  # ))

  res = res %>%
    # Select calibration_record by chemical based on calibration_class
    dplyr::group_by(dtxsid) %>%
    # chronic > subchronic > NA
    dplyr::mutate(rank_calibration_class = dplyr::case_when(
      grepl("\\(|\\)", calibration_class) ~ NA,
      grepl("\\bchronic\\b", calibration_class) ~ 1,
      grepl("\\bsubchronic\\b|\\bintermediate\\b", calibration_class) ~ 2,
      TRUE ~ NA
    ),
    # Select calibration rank within group
    calibration_record = suppressWarnings(min(rank_calibration_class, na.rm = TRUE))
    ) %>%
    dplyr::ungroup() %>%
    # Set calibration_record flag
    dplyr::mutate(calibration_record = dplyr::case_when(
      calibration_record == rank_calibration_class ~ 1,
      TRUE ~ 0
    )) %>%
    # Remove intermediate fields
    dplyr::select(-rank_calibration_class)

  # Get conceptual model by toxicological_effect_category
  conceptual_model_map = get.conceptual_model.by.toxicological_effect_category(df = res, run_name=run_name) %>%
    dplyr::select(-study_type, -toxicological_effect_category)

  res = res %>%
    dplyr::left_join(conceptual_model_map,
                     by = "source_hash")

  # Renaming toxicological_effect_category for reporting purposes/clarity
  res = res %>%
    dplyr::mutate(toxicological_effect_category_fix = dplyr::case_when(
      grepl("\\|", toxicological_effect_category) ~ "multiple",
      toxicological_effect_category == "none" & !grepl("NO?A?EL", toxval_type) ~ "other",
      TRUE ~ toxicological_effect_category
    ))

  # Write results to Excel
  writexl::write_xlsx(res, paste0(dir,"results/ToxValDB for DCAP ",toxval.db," POD filtered.xlsx"))
  writexl::write_xlsx(filtered_out, paste0(dir,"results/ToxValDB for DCAP ",toxval.db," removed entries.xlsx"))
}
