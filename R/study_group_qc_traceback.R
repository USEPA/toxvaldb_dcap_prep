library(fuzzyjoin)


load_dcap_study_groups <- function(toxval.db, run_name, source){

  filter_pods = readxl::read_xlsx(paste0(Sys.getenv("datapath"),
                                         "data/results/", run_name,
                                         "/results/ToxValDB for BMDh ",
                                         toxval.db," POD filtered.xlsx")) %>%
    dplyr::filter(source == !!source) %>%
    dplyr::select(source_hash) %>%
    dplyr::mutate(filter_pod_hash_group = 1:dplyr::n()) %>%
    tidyr::separate_longer_delim(source_hash, delim = ",") %>%
    tidyr::separate_longer_delim(source_hash, delim = "|::|") %>%
    dplyr::mutate(source_hash = source_hash %>%
                    stringr::str_squish())

  DCAP_full = readxl::read_xlsx(paste0(Sys.getenv("datapath"),
                                       "data/results/", run_name,
                                       "/results/ToxValDB for BMDh ",
                                       toxval.db,".xlsx")) %>%
    dplyr::filter(source == !!source) %>%
    dplyr::select(source_hash, study_group, source, source_table) %>%
    dplyr::mutate(full_hash_group = 1:dplyr::n()) %>%
    tidyr::separate_longer_delim(source_hash, delim = ",") %>%
    tidyr::separate_longer_delim(source_hash, delim = "|::|") %>%
    dplyr::mutate(source_hash = source_hash %>%
                    stringr::str_squish()) %>%
    dplyr::left_join(filter_pods,
                     by = "source_hash") %>%
    dplyr::mutate(DCAP_filter_pod = dplyr::case_when(
      !is.na(filter_pod_hash_group) ~ 1,
      TRUE ~ 0
    ))

  # Check record counts
  # DCAP_full %>% group_by(DCAP_filter_pod) %>% summarise(n=n())

  return(DCAP_full)
}

get_record_audit_map <- function(source.db, df){

  # Prep query filter of all hash values
  rec_list = df %>%
    dplyr::select(source_hash, parent_hash) %>%
    unlist() %>%
    unique()

  rec_list = paste0("'", paste0(rec_list, collapse = "', '"), "'")

  # Pull audit information
  audit = runQuery(paste0("SELECT DISTINCT fk_source_hash, parent_hash FROM source_audit WHERE fk_source_hash in (",
                          rec_list, ") or parent_hash in (", rec_list, ")"),
                   source.db) %>%
    dplyr::filter(!fk_source_hash == parent_hash) %>%
    dplyr::mutate(root = 0) %>%
    # Append the latest in the record lineage
    dplyr::bind_rows(., df %>%
                       dplyr::select(fk_source_hash = source_hash, parent_hash) %>%
                       dplyr::mutate(live_record = 1)) %>%
    dplyr::distinct()

  # Prep while-loop inputs and outputs
  result_df = data.frame()
  tmp = audit %>%
    dplyr::mutate(row_id = 1:dplyr::n())
  # Filter to top level live cases to start
  root = audit %>%
    dplyr::filter(live_record == 1) %>%
    dplyr::pull(fk_source_hash)

  while(nrow(tmp)){
    message("Records left: ", nrow(tmp))
    # Filter to matches
    parents = tmp %>%
      dplyr::filter(fk_source_hash %in% !!root)

    # Check if any matched
    if(!nrow(parents)){
      if(nrow(tmp)){
        message("Error, missing lineage matches...")
        browser()
      } else {
        # If all accounted for, break the loop
        break
      }
    }

    # Remove already pulled records
    tmp = tmp %>%
      dplyr::filter(!row_id %in% parents$row_id)

    # Prep for join
    parents = parents %>%
      dplyr::select(fk_source_hash, parent_hash) %>%
      dplyr::group_by(fk_source_hash) %>%
      dplyr::mutate(parent_hash = paste0(unique(parent_hash[!parent_hash %in% c("-", "NA")]),
                                         collapse = ", ") %>%
                      dplyr::na_if("")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    if(!nrow(result_df)){
      # Initial case
      result_df = parents %>%
        dplyr::select(fk_source_hash, parent_hash) %>%
        tidyr::unite(col="hash_family", fk_source_hash, parent_hash,
                     sep = ", ", remove = FALSE, na.rm = TRUE) %>%
        dplyr::select(-fk_source_hash)
    } else {
      # Add parent_hash connection
      result_df = result_df %>%
        dplyr::left_join(parents %>%
                           dplyr::rename(new_parent_hash = parent_hash),
                         by = c("parent_hash"="fk_source_hash"))

      if(nrow(tmp)){
        result_df = result_df %>%
          tidyr::unite(col="hash_family", hash_family, new_parent_hash,
                       sep = ", ", remove = FALSE, na.rm = TRUE) %>%
          dplyr::select(-parent_hash) %>%
          dplyr::rename(fk_source_hash = new_parent_hash)
      } else {
        # End case combine since no more tmp data left
        result_df = result_df %>%
          tidyr::unite(col = "hash_family", sep = ", ", na.rm = TRUE)
      }
    }

    # Set the new root filter
    root = unique(parents$parent_hash[!parents$parent_hash %in% c("-", "")])
  }

  result_df$hash_family_unique = sapply(strsplit(result_df$hash_family, ", "),
                                        function(x) hash_family = paste(unique(x), collapse = ", "))

  rec_lineage = paste0(result_df$hash_family_unique, collapse = ", ") %>%
    strsplit(split = ", ") %>%
    unlist()

  audit_hashes = audit %>%
    dplyr::select(fk_source_hash, parent_hash) %>%
    unlist() %>%
    unique() %>%
    .[!. %in% c("-")]

  # Check if all fk_source_hash and parent_hash values from audit are present
  # in the result_df hash_family column
  if(!all(rec_lineage %in% audit_hashes)){
    stop("Missing lineage from audit...")
  }

  return(result_df %>% dplyr::select(hash_family = hash_family_unique))
}

run_qc_traceback <- function(){
  old_run_name = "2024-11-12_epa_dws"
  new_run_name = "2024-11-21_epa_dws"
  src_name = "ATSDR PFAS 2021"

  # Get source table to add back in parent_hash and qc_notes
  src_tbl = runQuery("SELECT source_hash, parent_hash, qc_notes FROM source_atsdr_pfas_2021", source.db)

  # New DCAP
  old_DCAP = load_dcap_study_groups(toxval.db = toxval.db,
                                    run_name = old_run_name,
                                    source = src_name) %>%
    dplyr::rename(full_hash_group_old=full_hash_group,
                  filter_pod_hash_group_old=filter_pod_hash_group,
                  DCAP_filter_pod_old=DCAP_filter_pod,
                  study_group_old = study_group) %>%
    dplyr::left_join(src_tbl,
                     by = "source_hash")

  new_DCAP = load_dcap_study_groups(toxval.db = toxval.db,
                                    run_name = new_run_name,
                                    source = src_name) %>%
    dplyr::rename(full_hash_group_new=full_hash_group,
                  filter_pod_hash_group_new=filter_pod_hash_group,
                  DCAP_filter_pod_new=DCAP_filter_pod,
                  study_group_new = study_group) %>%
    dplyr::left_join(src_tbl,
                     by = "source_hash") %>%
    dplyr::mutate(expert_reviewed = dplyr::case_when(
      grepl("expert reviewed", qc_notes, ignore.case=TRUE) ~ 1,
      TRUE ~ 0
    ))

  # Get record_lineage audit map for how records are related
  rec_lineage = get_record_audit_map(source.db = source.db, df = new_DCAP)

  # Compare record counts by source (should be the same unless new source_hash records were
  # excluded or collapsed more)
  old_missing = old_DCAP %>%
    dplyr::filter(!source_hash %in% new_DCAP$source_hash) %>%
    dplyr::select(source_hash, source) %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(n = dplyr::n())

  new_missing = new_DCAP %>%
    dplyr::filter(!source_hash %in% old_DCAP$source_hash) %>%
    dplyr::select(source_hash, source) %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(n = dplyr::n())

  src_tbl_cols = c("source", "source_table", "qc_notes", "parent_hash")

  # Convert to long_form to join
  DCAP_full = old_DCAP %>%
    dplyr::select(-dplyr::all_of(src_tbl_cols)) %>%
    tidyr::pivot_longer(cols = -source_hash,
                        values_transform = as.character) %>%
    dplyr::mutate(run_name = old_run_name) %>%
    dplyr::bind_rows(
      new_DCAP %>%
        dplyr::select(-dplyr::all_of(src_tbl_cols)) %>%
        tidyr::pivot_longer(cols = -source_hash,
                            values_transform = as.character) %>%
        dplyr::mutate(run_name = new_run_name)
    ) %>%
    tidyr::pivot_wider(id_cols = c("source_hash", "run_name")) %>%
    # Collapse by source_hash
    dplyr::group_by(source_hash) %>%
    dplyr::mutate(dplyr::across(
      names(.)[!names(.) %in% c("source_hash")],
      ~paste0(.[!is.na(.)], collapse = " | ")
    )) %>%
    dplyr::distinct()

  compare_DCAP = DCAP_full %>%
    # Filter only to cases where the filter_pod selection is different
    dplyr::filter(DCAP_filter_pod_old != DCAP_filter_pod_new)

  report = list(
    general_summary = list()
  )

  ##############################################################################
  ### Test 0: New hashes in new DCAP not present in old and not explained by the audit
  ##############################################################################

  ### Check case where new records in new, not present in old. Check audit
  missing_hash = compare_DCAP %>%
    dplyr::filter(DCAP_filter_pod_old == "") %>%
    dplyr::select(source_hash)

  new_hash = rec_lineage %>%
    fuzzyjoin::regex_left_join(missing_hash,
                               by = c("hash_family"="source_hash")) %>%
    dplyr::filter(!is.na(source_hash)) %>%
    dplyr::mutate(hash_family = hash_family %>%
                    sub(', .*', '', .) %>%
                    stringr::str_squish()) %>%
    dplyr::pull(hash_family)

  if(!all(new_hash %in% old_DCAP$source_hash)){
    message("Unexpected new source_hash record values not previously pulled and not in record audit check")
  }

  report$general_summary = append(report$general_summary,
                                  data.frame(report="new_missing_in_old",
                                             value = length(new_hash),
                                             hash_list = paste0(new_hash, collapse = ", "))
                                  )

  ##############################################################################
  ### Test 1: Missing hashes between old and new, despite audit check
  ##############################################################################

  # Check lineage to ensure records previously included are still included,
  # just under a different source_hash
  missing_hash = compare_DCAP %>%
    dplyr::filter(DCAP_filter_pod_new == "") %>%
    dplyr::select(source_hash)

  new_hash = rec_lineage %>%
    fuzzyjoin::regex_left_join(missing_hash,
                               by = c("hash_family"="source_hash")) %>%
    dplyr::filter(!is.na(source_hash)) %>%
    dplyr::mutate(hash_family = hash_family %>%
                    sub(', .*', '', .) %>%
                    stringr::str_squish()) %>%
    dplyr::pull(hash_family)

  if(!all(new_hash %in% new_DCAP$source_hash)){
    message("Old source_hash record values missing from new DCAP after record audit check")
  }

  report$general_summary = append(report$general_summary,
                                  data.frame(report="old_missing_in_new",
                                             value = length(new_hash[!new_hash %in% new_DCAP$source_hash]),
                                             hash_list = paste0(new_hash[!new_hash %in% new_DCAP$source_hash], collapse = ", "))
  )

  # Filter out accounted for missing hashes
  compare_DCAP = compare_DCAP %>%
    dplyr::filter(!source_hash %in% new_hash)

  ##############################################################################
  ### Test 2: Old selected, but new is not. Check if replaced by QC'd record
  ##############################################################################

  # Case of old was filtered POD, but is now not selected (could be Qc'd data update)
  old_pod_now_rm = compare_DCAP %>%
    dplyr::filter(DCAP_filter_pod_old == 1, DCAP_filter_pod_new %in% c(0))

  # Check if was replaced by a QC'd record "Expert Reviewed"
  old_pod_not_selected = new_DCAP %>%
    dplyr::filter(study_group_new %in% old_pod_now_rm$study_group_new,
                  # Find those that were selected instead
                  DCAP_filter_pod_new == 1) %>%
    dplyr::group_by(study_group_new) %>%
    dplyr::filter(expert_reviewed == 0)

  if(nrow(old_pod_not_selected)){
    message("Old filtered POD record no longer selected and not replaced by expert reviewed record.")
  }

  report$general_summary = append(report$general_summary,
                                  data.frame(report="old_not_selected_not_er_replaced",
                                             value = nrow(old_pod_not_selected),
                                             hash_list = ifelse(nrow(old_pod_not_selected),
                                                                paste0(old_pod_not_selected$source_hash, collapse = ", "),
                                                                "")
                                             )
  )


  ##############################################################################
  ### Test 3: Not selected before, now selected. Check study_group lineage and
  ###         record lineage to see if change was due to QC'd expert reviewed record
  ###         changing study_group values
  ##############################################################################

  # Present in old output that was filtered out, but is now filter POD selected
  old_rm_now_pod = compare_DCAP %>%
    dplyr::filter(DCAP_filter_pod_old == 0, DCAP_filter_pod_new == 1)

  # Get study_group lineage where study_group was different
  study_group_lineage_dff = DCAP_full %>%
    dplyr::select(source_hash, study_group_old, study_group_new) %>%
    tidyr::pivot_longer(-source_hash) %>%
    dplyr::group_by(name, value) %>%
    dplyr::mutate(study_group_family = paste0(sort(unique(source_hash[!source_hash %in% c("")])), collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(id_cols = c("source_hash"),
                       names_from = "name",
                       values_from = "study_group_family") %>%
    dplyr::distinct() %>%
    dplyr::filter(study_group_old != study_group_new)

  rm_now_pod_study_group = study_group_lineage_dff %>%
    dplyr::filter(source_hash %in% old_rm_now_pod$source_hash) %>%
    tidyr::pivot_longer(-source_hash,
                        names_to = "study_group_label",
                        values_to = "study_group_hashes") %>%
    tidyr::separate_longer_delim(study_group_hashes, delim = ", ")

  # Join to record lineage
  rm_now_pod_study_group = rec_lineage %>%
    fuzzyjoin::regex_left_join(rm_now_pod_study_group,
                               by = c("hash_family"="study_group_hashes")) %>%
    dplyr::filter(!is.na(source_hash)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(hash_family) %>%
    dplyr::mutate(hash_family_index = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::separate_longer_delim(hash_family, delim = ", ") %>%
    dplyr::left_join(new_DCAP %>%
                       dplyr::select(hash_family = source_hash,
                                     expert_reviewed),
                     by = "hash_family") %>%
    # Roll back up
    dplyr::group_by(source_hash, study_group_label, study_group_hashes, hash_family_index) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("hash_family", "expert_reviewed")),
                                ~paste0(unique(.[!. %in% c(NA)]), collapse = ", "))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(-hash_family_index) %>%
    dplyr::group_by(source_hash, study_group_label) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("hash_family", "expert_reviewed", "study_group_hashes")),
                                ~paste0(unique(.[!. %in% c(NA)]), collapse = ", "))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(id_cols = source_hash,
                       names_from = "study_group_label",
                       values_from = c("study_group_hashes", "expert_reviewed", "hash_family"))

  # Flag cases where no expert reviewed (er) change, er record lost, er record gained
  rm_now_pod_study_group_summ = rm_now_pod_study_group %>%
    dplyr::mutate(
      er_no_change = dplyr::case_when(
        expert_reviewed_study_group_old == expert_reviewed_study_group_new ~ 1,
        TRUE ~ 0
      ),
      er_lost = dplyr::case_when(
        grepl("1", expert_reviewed_study_group_old) &
          !grepl("1", expert_reviewed_study_group_new) ~ 1,
        TRUE ~ 0
      ),
      er_gained = dplyr::case_when(
        grepl("1", expert_reviewed_study_group_new) &
          !grepl("1", expert_reviewed_study_group_old) ~ 1,
        TRUE ~ 0
      )
    )

  report$general_summary = append(report$general_summary,
                                  data.frame(report="old_not_selected_er_lost",
                                             value = nrow(rm_now_pod_study_group_summ),
                                             hash_list = paste0(rm_now_pod_study_group_summ$source_hash[rm_now_pod_study_group_summ$er_lost == 1], collapse = ", ")

                                  )
  )

  report$general_summary = report$general_summary %>%
    dplyr::bind_rows()
}
