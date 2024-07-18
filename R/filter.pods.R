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
  res0 = readxl::read_xlsx(file) %>%
    dplyr::mutate(key_finding = as.character(NA))

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
      grepl("key", key_finding, ignore.case=TRUE)
    )

  # Track filtered out entries (with reason for filtering)
  filtered_out_auth = res0 %>%
    dplyr::filter(
      source %in% auth_sources,
      !grepl("key", key_finding, ignore.case=TRUE)
    ) %>%
    dplyr::mutate(
      reason_for_filtering = "from authoritative source but not key finding"
    )

  cat("Filtering non-authoritative sources\n")
  # Filter non-authoritative sources
  res0_non_auth = res0 %>%
    dplyr::filter(!source %in% auth_sources) %>%
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
      # Get minimum LOAEL/LEL values for comparison
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
  res_non_auth = res0_non_auth %>%
    dplyr::filter(selected_row == keep_flag,
                  remove_flag != 1) %>%
    dplyr::select(-c("tts", "ttr", "low_loael", "low_lel", "min_val", "max_val",
                     "remove_flag", "keep_flag", "selected_row", "reason_for_filtering"))

  # Select rows that were filtered out
  filtered_out_non_auth = res0_non_auth %>%
    dplyr::filter(remove_flag == 1 | keep_flag != selected_row) %>%
    dplyr::select(-c("tts", "ttr", "low_loael", "low_lel", "min_val", "max_val",
                     "remove_flag", "keep_flag", "selected_row"))

  # Combine data from authoritative and non-authoritative sources
  res = dplyr::bind_rows(res_auth, res_non_auth)
  filtered_out = dplyr::bind_rows(filtered_out_auth, filtered_out_non_auth)

  # Write results to Excel
  writexl::write_xlsx(res, paste0(dir,"results/ToxValDB for BMDh ",toxval.db," POD filtered.xlsx"))
  writexl::write_xlsx(filtered_out, paste0(dir,"results/ToxValDB for BMDh ",toxval.db," removed entries.xlsx"))
}
