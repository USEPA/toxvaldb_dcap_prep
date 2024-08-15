#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
#' @return None; filtered results are recorded in Excel file
#' @export
#' @title filter.pods
#' @description Filter values for BMDh according to specified POD rules
#' @details Filtering steps differ between authoritative and non-authoritative sources
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
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
#-----------------------------------------------------------------------------------
filter.pods <- function(toxval.db="res_toxval_v95", run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)

  # Read in initial export data
  dir = paste0("data/results/", run_name, "/")
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db,".xlsx")
  res0 = readxl::read_xlsx(file)

  # Establish list of authoritative sources
  auth_sources = c(
    "ATSDR MRLs",
    "Cal OEHHA",
    "EPA HHTV",
    "Health Canada",
    "IRIS",
    "HEAST",
    "PPRTV (CPHEA)"
  )
  cat("Filtering authoritative sources:", paste0(auth_sources, collapse=", "), "\n")

  # Get key_finding PODs from authoritative sources
  res_auth = res0 %>%
    dplyr::filter(
      source %in% auth_sources,
      grepl("key|yes", key_finding, ignore.case=TRUE)
    )

  # Track filtered out entries (with reason for filtering)
  filtered_out_auth = res0 %>%
    dplyr::filter(
      source %in% auth_sources,
      !grepl("key|yes", key_finding, ignore.case=TRUE)
    ) %>%
    dplyr::mutate(
      reason_for_filtering = "from authoritative source but not key finding"
    )

  cat("Filtering non-authoritative sources\n")
  # Extract entries with key_finding for non_authoritative sources
  non_auth_key_findings = res0 %>%
    dplyr::filter(!source %in% auth_sources,
                  key_finding %in% c("yes", "key"))

  key_finding_study_groups = non_auth_key_findings %>%
    dplyr::pull(study_group) %>%
    unique()

  non_auth_key_findings_filtered = res0 %>%
    dplyr::filter(study_group %in% key_finding_study_groups,
                  !key_finding %in% c("yes", "key")) %>%
    dplyr::mutate(
      reason_for_filtering = "from non-auth study group that had key_finding"
    )

  # Filter non-authoritative sources
  res_init = res0 %>%
    dplyr::filter(!source %in% auth_sources,
                  !study_group %in% key_finding_study_groups) %>%
    dplyr::bind_rows(res_auth) %>%
    dplyr::bind_rows(non_auth_key_findings) %>%
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
        TRUE ~ as.character(NA)
      ) %>%
        stringr::str_trim(),
      # Get underlying root of toxval_type (no HED/ADJ/etc.)
      ttr = tts %>%
        stringr::str_extract("(BMDL|[NL]OAEL|[NL]EL)", group=1)
    ) %>%
    dplyr::group_by(study_group) %>%
    # Get minimum LOAEL/LEL values to initially filter out NOAEL/NEL > LOAEL/LEL
    dplyr::mutate(
      low_loael = dplyr::case_when(
        ttr == "LOAEL" ~ toxval_numeric,
        TRUE ~ 999
      ),
      low_loael = min(low_loael),

      low_lel = dplyr::case_when(
        ttr == "LEL" ~ toxval_numeric,
        TRUE ~ 999
      ),
      low_lel = min(low_lel),
    ) %>%
    dplyr::ungroup() %>%
    # Remove NOAEL/NEL greater than minimum LOAEL/LEL
    dplyr::mutate(
      remove_flag = dplyr::case_when(
        ttr == "NOAEL" & toxval_numeric > low_loael & low_loael != 999 ~ 1,
        ttr == "NEL" & toxval_numeric > low_lel & low_lel != 999 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    # Get maximum/minumum values for each toxval_type within study groups (ignoring entries to remove)
    dplyr::group_by(study_group, tts, remove_flag) %>%
    dplyr::mutate(
      min_val = min(toxval_numeric),
      max_val = max(toxval_numeric)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(study_group, remove_flag) %>%
    dplyr::mutate(
      # Tag each entry in every study_group with filter step it corresponds to
      keep_flag = dplyr::case_when(
        # Tag appropriate number based on priority
        tts == "BMDL HED" & toxval_numeric == min_val ~ 1,
        tts == "BMDL ADJ" & toxval_numeric == min_val ~ 2,
        tts == "BMDL" & toxval_numeric == min_val ~ 3,
        tts == "NOAEL HED" & toxval_numeric == max_val ~ 4,
        tts == "NOAEL ADJ" & toxval_numeric == max_val ~ 5,
        tts == "NOAEL" & toxval_numeric == max_val ~ 6,
        tts == "LOAEL HED" & toxval_numeric == min_val ~ 7,
        tts == "LOAEL ADJ" & toxval_numeric == min_val ~ 8,
        tts == "LOAEL" & toxval_numeric == min_val ~ 9,
        tts == "NEL HED" & toxval_numeric == max_val ~ 10,
        tts == "NEL ADJ" & toxval_numeric == max_val ~ 11,
        tts == "NEL" & toxval_numeric == max_val ~ 12,
        tts == "LEL HED" & toxval_numeric == min_val ~ 13,
        tts == "LEL ADJ" & toxval_numeric == min_val ~ 14,
        tts == "LEL" & toxval_numeric == min_val ~ 15,
        TRUE ~ 999
      ),

      repro_dev_fix = dplyr::case_when(
        ttr %in% c("BMDL", "NOAEL", "LOAEL", "NEL", "LEL") & study_type == "reproduction developmental" ~ 1,
        TRUE ~ 0
      ),

      # Pick row with highest priority
      selected_row = min(keep_flag),

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
        TRUE ~ "no selection from this group"
      )
    ) %>%
    dplyr::ungroup()

  # Select chosen rows
  res = res_init %>%
    dplyr::filter(selected_row == keep_flag,
                  remove_flag != 1) %>%
    # Handle case where repro dev causes multiple selections per study_group
    dplyr::group_by(study_group) %>%
    dplyr::mutate(
      n = dplyr::n(),

      drop = dplyr::case_when(
        n > 1 & repro_dev_fix == 0 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(drop == 0) %>%
    dplyr::select(-c("tts", "ttr", "low_loael", "low_lel", "min_val", "max_val", "remove_flag",
                     "keep_flag", "selected_row", "reason_for_filtering", "repro_dev_fix", "n", "drop"))

  # Select rows that were filtered out
  filtered_out_non_auth = res_init %>%
    dplyr::filter(remove_flag == 1 | keep_flag != selected_row) %>%
    dplyr::select(-c("tts", "ttr", "low_loael", "low_lel", "min_val", "max_val",
                     "remove_flag", "keep_flag", "selected_row"))

  # Combine filtered out data from authoritative and non-authoritative sources
  filtered_out = dplyr::bind_rows(filtered_out_auth, filtered_out_non_auth, non_auth_key_findings_filtered)

  # Perform deduping on identical records with different source_hash values
  non_hashing_cols = c("source_hash", "record_source_info", "critical_effect", "name",
                       "toxval_subtype", "critical_effect_category", "multiple_flag")
  # Add model and record_source fields to non_hashing_cols
  record_source_cols = runQuery("DESC record_source", toxval.db) %>%
    dplyr::pull(Field)
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
      critical_effect = gsub(" \\|::\\| ", "|", critical_effect),
      critical_effect_category = gsub(" \\|::\\| ", "|", critical_effect_category),
      dplyr::across(!!model_non_hash_cols, ~gsub(" \\|::\\| ", "|", .))
    ) %>%
    dplyr::distinct() %>%
    dplyr::select(-source_hash_temp)

  # Add duration_adjustment field to POD filtered output
  res = res %>%
    dplyr::mutate(
      duration_adjustment = dplyr::case_when(
        study_type == "developmental" | grepl("development", critical_effect_category) ~ "developmental",
        study_type == "reproduction developmental" & !is.na(study_duration_class) ~ study_duration_class %>%
          gsub("\\(.+", "", .) %>%
          stringr::str_squish(),
        study_type == "reproduction developmental" & study_duration_value %in% c(-999, NA) ~ "subchronic",
        study_type %in% c("short-term", "subchronic", "chronic") ~ "study_type",
        TRUE ~ as.character(NA)
      )
    )

  # Write results to Excel
  writexl::write_xlsx(res, paste0(dir,"results/ToxValDB for BMDh ",toxval.db," POD filtered.xlsx"))
  writexl::write_xlsx(filtered_out, paste0(dir,"results/ToxValDB for BMDh ",toxval.db," removed entries.xlsx"))
}
