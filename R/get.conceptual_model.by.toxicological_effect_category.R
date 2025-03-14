#' @title get.conceptual_model.by.toxicological_effect_category
#' @description Get the conceptual model based on toxicological_effect_category
#' @param df Input dataframe of study_type and toxicological_effect data.
#' @param run_name The desired name for the output directory (Default: current date)
#' @export
#' @return DataFrame map of models by toxicological_effect and study_type
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
#' @rdname get.conceptual_model.by.toxicological_effect_category
#' @importFrom dplyr select distinct mutate n case_when left_join rename across where group_by any_of na_if ungroup
#' @importFrom tidyr separate_rows replace_na
#' @importFrom stringr str_squish
#' @importFrom readr read_csv cols
get.conceptual_model.by.toxicological_effect_category <- function(df, run_name){
  dir = paste0(Sys.getenv("datapath"), "data/")

  df_dcap <- df %>%
    dplyr::mutate(
      piped_toxicological_effect = dplyr::case_when(
        grepl("\\|", toxicological_effect_category) ~ "1",
        TRUE ~ "0"
      )
    ) %>%
    dplyr::select(source_hash, study_type, toxicological_effect_category, toxicological_effect_category_original, piped_toxicological_effect) %>%
    dplyr::mutate(toxicological_effect_category = fix.replace.unicode(toxicological_effect_category)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(hash_group = 1:dplyr::n()) %>%
    # Spread out collapsed source_hash
    tidyr::separate_rows(source_hash, sep = ",") %>%
    dplyr::mutate(source_hash = source_hash %>%
                    stringr::str_squish())

  df2_dcap <- df_dcap %>%
    tidyr::separate_rows(toxicological_effect_category, sep = "\\|") %>%
    dplyr::distinct() %>%
    dplyr::mutate(type_map = dplyr::case_when(
      study_type == "reproduction developmental" & !grepl("development|reproduction", toxicological_effect_category_original) ~ "repeat dose",
      grepl("development|reproduction", toxicological_effect_category_original) ~ "repro dev",
      grepl("chronic", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("subchronic", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("28-day", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("clinical", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("repeat dose other", study_type, ignore.case=TRUE) ~ "repeat dose",
      grepl("short-term", study_type, ignore.case=TRUE) ~ "repeat dose",
      study_type=="developmental"~"repro dev",
      study_type=="reproduction"~"repro dev",
      study_type=="reproduction developmental" ~ "repro dev",
      TRUE ~ NA_character_
    ))%>%
    dplyr::mutate(type = dplyr::case_when(
      !is.na(type_map) ~ type_map,
      TRUE ~ study_type)) %>%
    dplyr::select(-type_map) %>%
    # dplyr::select(source_hash, study_type, type, toxicological_effect_category) %>%
    dplyr::distinct()

  ##############################################################################
  # Reassign "type" based on input dictionary
  type_remap_dict <- readxl::read_xlsx(paste0(dir, "input/toxicological_effect_cat_remap_type_dict.xlsx")) %>%
    dplyr::select(source_hash,
                  toxicological_effect_category_original = toxicological_effect_category,
                  type_remap) %>%
    tidyr::separate_longer_delim(source_hash, delim = ", ") %>%
    tidyr::separate_longer_delim(source_hash, delim = "|::|") %>%
    dplyr::mutate(source_hash = source_hash %>%
                    stringr::str_squish())

  df2_dcap = df2_dcap %>%
    # Spread out source_hash matches
    dplyr::mutate(type_remap_index = source_hash) %>%
    tidyr::separate_longer_delim(source_hash, delim = ", ") %>%
    tidyr::separate_longer_delim(source_hash, delim = "|::|") %>%
    dplyr::left_join(type_remap_dict,
                     by = c("source_hash", "toxicological_effect_category_original"))

  missing_type_remap = df2_dcap %>%
    dplyr::filter(grepl("multiple", toxicological_effect_category_original),
                  is.na(type_remap)) %>%
    dplyr::select(source_hash, type_remap) %>%
    dplyr::left_join(df,
                     by = "source_hash") %>%
    dplyr::select(source_hash,
                  term = toxicological_effect,
                  study_type,
                  toxicological_effect_category=toxicological_effect_category_original,
                  type_remap) %>%
    # Collapse to unique entries
    dplyr::group_by(dplyr::across(c(-source_hash))) %>%
    dplyr::summarise(source_hash = toString(source_hash)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(source_hash, dplyr::everything())

  if(nrow(missing_type_remap)){
    writexl::write_xlsx(missing_type_remap,
                        paste0(dir, "results/", run_name, "/missing_toxicological_effect_cat_remap_type_dict.xlsx"))
  }

  df2_dcap = df2_dcap %>%
    # Collapse back
    dplyr::group_by(type_remap_index) %>%
    dplyr::mutate(
      dplyr::across(c("type_remap", "source_hash"), ~ toString(unique(.[!is.na(.)])))
      ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  type_remap_check = df2_dcap %>%
    dplyr::filter(!is.na(type_remap))

  # Export reassignments to check if they were correct
  if(nrow(type_remap_check)){
    writexl::write_xlsx(type_remap_check %>%
                          dplyr::filter(!is.na(type_remap)),
                        paste0(dir, "results/", run_name, "/toxicological_effect_cat_type_remap_check.xlsx"))
  }

  df2_dcap = df2_dcap %>%
    dplyr::mutate(type = dplyr::case_when(
      !is.na(type_remap) ~ type_remap,
      TRUE ~ type
    )) %>%
    dplyr::select(-type_remap, -source_hash) %>%
    dplyr::rename(source_hash = type_remap_index)

  ##############################################################################
  # Read in map for standard term to conceptual model
  model_map <- readr::read_csv(paste0(dir, "input/conceptual_model_map.csv"), col_types = readr::cols())

  df3 <- df2_dcap %>%
    dplyr::left_join(model_map %>%
                       dplyr::rename(model1_all = model1,
                                     model2_all = model2),
              # map terms to matching conceptual model
              by=c("toxicological_effect_category" = "standard")) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-"))) %>%
    # Recollapse category and conceptual models
    dplyr::group_by(source_hash) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(names(.)[!names(.) %in% c("source_hash", "study_type",
                                                                        "type", "piped_toxicological_effect")]),
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
      grepl("1", piped_toxicological_effect) ~ "multiple",
      (grepl("cont", model1_all, ignore.case=TRUE) & grepl("det", model1_all, ignore.case=TRUE)) ~ "multiple",
      (grepl("cont", model1_all, ignore.case=TRUE) & grepl("stoch", model1_all, ignore.case=TRUE)) ~ "multiple",
      (grepl("det", model1_all, ignore.case=TRUE) & grepl("stoch", model1_all, ignore.case=TRUE)) ~ "multiple",
      TRUE ~ NA_character_
    )) %>%
    dplyr::select(-piped_toxicological_effect) %>%
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
    dplyr::select(-dplyr::any_of(c("hash_group", "multiple_flag", "model1", "model2",
                                   "model1_all", "model2_all", "toxicological_effect_category_original")))

  return(final)
}
