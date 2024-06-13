#' @title get.conceptual_model.by.critical_effect_category
#' @description Get the conceptual model based on critical_effect_category
#' @param df Input dataframe of study_type and critical_effect data.
#' @export
#' @return DataFrame map of models by critical_effect and study_type
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{na_if}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{replace_na}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[readr]{read_delim}}, \code{\link[readr]{cols}}
#' @rdname set.conceptual_model.by.critical_effect_category
#' @importFrom dplyr select distinct mutate n case_when left_join rename across where group_by any_of na_if ungroup
#' @importFrom tidyr separate_rows replace_na
#' @importFrom stringr str_squish
#' @importFrom readr read_csv cols
get.conceptual_model.by.critical_effect_category <- function(df){
  dir = "data/"

  df_dcap <- df %>%
    dplyr::select(source_hash, study_type, critical_effect_category) %>%
    dplyr::mutate(critical_effect_category = fix.replace.unicode(critical_effect_category)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(hash_group = 1:dplyr::n()) %>%
    # Spread out collapsed source_hash
    tidyr::separate_rows(source_hash, sep = ",") %>%
    dplyr::mutate(source_hash = source_hash %>%
                    stringr::str_squish())

  df2_dcap <- df_dcap %>%
    tidyr::separate_rows(critical_effect_category, sep = "\\|") %>%
    dplyr::distinct() %>%
    dplyr::mutate(type_map = dplyr::case_when(
      grepl("chronic", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("subchronic", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("28-day", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("clinical", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("repeat dose other", study_type, ignore.case=TRUE) ~ "repeat dose",
      study_type=="developmental"~"repro dev",
      study_type=="reproduction"~"repro dev",
      study_type=="reproduction developmental"~"repro dev",
      TRUE ~ NA_character_
    ))%>%
    dplyr::mutate(type = dplyr::case_when(
      !is.na(type_map) ~ type_map,
      TRUE ~ study_type)) %>%
    dplyr::select(-type_map) %>%
    # dplyr::select(source_hash, study_type, type, critical_effect_category) %>%
    dplyr::distinct()

  # Read in map for standard term to conceptual model
  model_map <- readr::read_csv(paste0(dir, "input/conceptual_model_map.csv"), col_types = readr::cols())

  df3 <- df2_dcap %>%
    dplyr::left_join(model_map %>%
                       dplyr::rename(model1_all = model1,
                                     model2_all = model2),
              # map terms to matching conceptual model
              by=c("critical_effect_category" = "standard")) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-"))) %>%
    # Recollapse category and conceptual models
    dplyr::group_by(source_hash) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(names(.)[!names(.) %in% c("source_hash", "study_type", "type")]),
                                ~paste0(., collapse="|") %>%
                                  dplyr::na_if("NA") %>%
                                  dplyr::na_if("") %>%
                                  dplyr::na_if("-")
    )) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df4 = df3 %>%
    dplyr::mutate(final_model1 = dplyr::case_when(
      # If both multiple models are within string, assign continuous model (for multiple)
      (grepl("cont", model1_all, ignore.case=TRUE) & grepl("det", model1_all, ignore.case=TRUE)) ~ "continuous",
      (grepl("cont", model1_all, ignore.case=TRUE) & grepl("stoch", model1_all, ignore.case=TRUE)) ~ "continuous",
      (grepl("det", model1_all, ignore.case=TRUE) & grepl("stoch", model1_all, ignore.case=TRUE)) ~ "continuous",
      grepl("det", model1_all, ignore.case=TRUE) ~ "quantal-deterministic",
      grepl("stoch", model1_all, ignore.case=TRUE) ~ "quantal-stochastic",
      grepl("cont", model1_all, ignore.case=TRUE) ~ "continuous",
      TRUE ~ NA_character_)
    ) %>%
    dplyr::mutate(multiple_flag = dplyr::case_when(
      # Add a flag for multiples to use in model2
      (grepl("cont", model1_all, ignore.case=TRUE) & grepl("det", model1_all, ignore.case=TRUE)) ~ "multiple",
      (grepl("cont", model1_all, ignore.case=TRUE) & grepl("stoch", model1_all, ignore.case=TRUE)) ~ "multiple",
      (grepl("det", model1_all, ignore.case=TRUE) & grepl("stoch", model1_all, ignore.case=TRUE)) ~ "multiple",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(model2 = dplyr::case_when(
      (grepl("det", model2_all, ignore.case=TRUE) & grepl("stoch", model2_all) & type == "repeat dose") ~ "quantal-deterministic",
      (grepl("det", model2_all, ignore.case=TRUE) & grepl("stoch", model2_all) & type == "repro dev") ~ "quantal-stochastic",
      grepl("det", model2_all, ignore.case=TRUE) ~ "quantal-deterministic",
      grepl("stoch", model2_all, ignore.case=TRUE) ~ "quantal-stochastic",
      TRUE ~ NA_character_)
    )

  final <- df4 %>%
    dplyr::mutate(final_model2 = dplyr::case_when(
      model2 == "quantal-stochastic" ~ "quantal-stochastic",
      model2 == "quantal-deterministic" ~ "quantal-deterministic",
      multiple_flag == "multiple" & type == "repeat dose" ~ "quantal-deterministic",
      multiple_flag == "multiple" & type == "repro dev" ~ "quantal-stochastic",
      TRUE ~ NA_character_)
    ) %>%
    distinct()

  # Collapse source_hash again
  final = final %>%
    dplyr::group_by(hash_group) %>%
    dplyr::mutate(source_hash = paste0(source_hash, collapse=",")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(-hash_group)

  return(final)
}
