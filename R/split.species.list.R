#' @title Split species list
#' @description Split species lists into multiple rows.
#' @param df Input dataframe with species lists to split.
#' @return Modified input "df" with split species lists into multiple rows.
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{case}}
#' @export
#' @rdname split.species.list
#' @importFrom dplyr mutate case_when filter bind_rows group_by summarise
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_squish str_to_title
split.species.list <- function(df){

  species_list = df %>%
    # Standardize species lists
    dplyr::mutate(species_original = dplyr::case_when(
      species_original == "rats/mice" ~ "rats, mice",
      grepl(" and | or ", species_original) ~ species_original %>%
        gsub(" and ", ", ", .) %>%
        gsub(" or ", ", ", .),
      TRUE ~ species_original
    )) %>%
    # Filter to lists
    dplyr::filter(grepl(",", species_original)) %>%
    # Split lists
    tidyr::separate_rows(species_original, sep = ",") %>%
    # Standardize species
    dplyr::mutate(species_original = species_original %>%
                    stringr::str_squish() %>%
                    stringr::str_to_title(),
                  species_original = dplyr::case_when(
                    species_original == "Rats" ~ "Rat",
                    species_original == "Mice" ~ "Mouse",
                    species_original == "Guinea Pigs" ~ "Guinea Pig",
                    species_original == "Dogs" ~ "Dog",
                    species_original == "Rabbits" ~ "Rabbit",
                    TRUE ~ species_original
                  ),
                  # Set to common_name for use
                  common_name = species_original)

  # Filter out old records and recombine new
  out = df %>%
    dplyr::filter(!source_hash %in% species_list$source_hash) %>%
    dplyr::bind_rows(species_list)

  # Check changes
  tmp = out %>%
    dplyr::group_by(source_hash) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)

  # Report changes
  if(nrow(tmp)){
    cat(paste0("...", nrow(tmp), " species lists split into ", sum(tmp$n), " records\n"))
  }

  # Return changes
  return(out)
}
