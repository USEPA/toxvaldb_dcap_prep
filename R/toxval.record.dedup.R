#' @title toxval.record.dedup
#' @description Perform deduplication on data based on select identifier fields. Non-identifier fields will be collapsed based on the input delimiter.
#' @param res Dataframe to process and collapse duplicate record fields.
#' @param dedup_fields vector containing field names to deduplicate, default NULL (all fields but hashing cols).
#' @param hashing_cols vector containing field names of hashing columns, default NULL.
#' @param delim String used to separate collapsed values, default ' |::| '.
#' @return Dataframe containing deduplicated data, with duplicate records collapsed by delimiter in non-identifier fields.
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{distinct}}
#' @rdname toxval.record.dedup
#' @export
#' @importFrom dplyr select group_by summarise n filter mutate across any_of na_if ungroup distinct
#' @importFrom tidyr replace_na
toxval.record.dedup <- function(res,
                                dedup_fields=NULL,
                                hashing_cols=NULL,
                                delim=" |::| ") {
  cat("Deduping data\n")

  # If no hashing_cols provided, use toxval.config()$hashing_cols
  if(is.null(hashing_cols)) {
    hashing_cols = toxval.config()$hashing_cols
  }

  # If no dedup fields provided, set dedup_fields to be all cols but source_hash and hashing_cols
  if(is.null(dedup_fields)) {
    dedup_fields = names(res %>% dplyr::select(-dplyr::any_of(c("source_hash", "toxval_id", hashing_cols))))
  }

  # Add source_hash column
  res.temp = source_hash_vectorized(res, hashing_cols)
  res$source_hash = res.temp$source_hash

  # Check for immediate duplicate hashes
  dup_hashes = res %>%
    dplyr::group_by(source_hash) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)

  # Perform deduping only if there are duplicate entries
  if(nrow(dup_hashes)) {
    cat(paste0("Duplicate records identified (", sum(dup_hashes$n) - nrow(dup_hashes), " records are duplicates)\n"))
    cat("Performing deduping...\n")
    # Dedup by collapsing non hashing columns to dedup
    res = res %>%
      dplyr::group_by(source_hash) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(!!dedup_fields),
                                  # Ensure unique entries in alphabetic order
                                  ~paste0(sort(unique(.[!is.na(.)])), collapse=!!delim) %>%
                                    dplyr::na_if("NA") %>%
                                    dplyr::na_if("") %>%
                                    # Replace NA with "-"
                                    tidyr::replace_na("-")
      )) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    # Check if success
    dup_hashes = res %>%
      dplyr::group_by(source_hash) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(n > 1)

    if(nrow(dup_hashes)) {
      cat("Deduping failed. Duplicate records still present.\n")
      browser()
    } else {
      cat("Deduping was successful. Returning...\n")
    }
  } else {
    cat("No duplicate records found.\n")
  }

  res = res %>% dplyr::select(-source_hash)
  return(res)
}
