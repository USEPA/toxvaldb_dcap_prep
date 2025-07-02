#' @title source_hash_vectorized
#' @description Generate the hash key for an input table based on hashing columns. The hash key helps identify duplicate records in a dataframe.
#' @param res The data frame to receive a hash key.
#' @param hashing_cols Optional list of columns to use for generating hash key field.
#' @return Modified input dataframe with new source_hash field.
#' @seealso
#'  \code{\link[digest]{digest}}
#'  \code{\link[dplyr]{distinct}}
#' @importFrom digest digest
#' @importFrom dplyr distinct mutate ungroup rowwise
#' @importFrom tidyr unite
#' @importFrom tidyselect any_of
#' @rdname toxval_source.hash.and.load
#' @export
source_hash_vectorized <- function(res, hashing_cols){
  if(is.null(hashing_cols)){
    hashing_cols = names(res)
  }

  res %>%
    tidyr::unite(hash_col, tidyselect::any_of(sort(names(.)[names(.) %in% hashing_cols])), sep="") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(source_hash = paste0("ToxValhc_", digest::digest(hash_col, serialize = FALSE))) %>%
    dplyr::ungroup() %>%
    return()
}
