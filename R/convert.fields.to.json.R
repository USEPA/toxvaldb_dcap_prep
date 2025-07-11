#' @title convert.fields.to.json
#' @description Generate a new "record" field as a JSON key-value dictionary of row field values.
#' @param in_dat Dataframe to use to generate the new "record" JSON field.
#' @return Modified dataframe with new "record" field in JSON format.
#' @seealso
#'  \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname convert.fields.to.json
#' @export
#' @importFrom dplyr summarise select bind_rows
#' @importFrom jsonlite toJSON
convert.fields.to.json <- function(in_dat){
  # Iterate through rows of data
  lapply(seq_len(nrow(in_dat)), function(row){
    # Convert rows to JSON format
    in_dat[row, ] %>%
      dplyr::summarise(record = jsonlite::toJSON(.)) %>%
      dplyr::select(record)
  }) %>%
    dplyr::bind_rows() %>%
    unlist() %>%
    unname() %>%
    return()
}
