#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @return Write a file with the results: toxval_PODs_for_BMDh {toxval.db} {sys.date}.xlsx
#' @export 
#' @title bmdh.per.study
#' @description Calculate the BMDh values per study
#' @details Calculates one BMDh value per study using the Aurisano algorithm.
#' Because EPA has not fully developed the mapping from critical effects in ToxValDB
#' to standardized effects, teh values from Aurisano are used where records match.
#' Aurisano used ToxValDB 9.1, whereas 9.5 is used here. There is also code here to do the other
#' required mappings, and thos may need to be updated.For records in both the old and new
#' databases, an on-the-fly plot is produced to show the corresponded between study-level
#' BMDh values.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{createStyle}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[graphics]{plot.default}}
#' @rdname bmdh.per.study
#' @importFrom openxlsx read.xlsx createStyle write.xlsx
#' @importFrom graphics plot
#' @importFrom dplyr mutate
#-----------------------------------------------------------------------------------
bmdh.per.study <- function(toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  file = paste0(dir,"results/ToxValDB for BMDh LEL NEL multiNOEL filtered ",toxval.db," ",sys.date,".xlsx")
  print(file)

  res = readxl::read_xlsx(file) %>%
    # Filter out entries with invalid study_type or toxval_numeric
    dplyr::filter(toxval_numeric > 0,
                  !(study_type %in% c("epidemiology","human","genetics","occupational"))) %>%
    # Handle humanized pod types
    dplyr::mutate(
      common_name = dplyr::case_when(
        grepl("HED", toxval_type) ~ "Human",
        TRUE ~ common_name
      )
    )

  file = paste0(dir,"Aurisano S1.xlsx")
  print(file)
  s1 = readxl::read_xlsx(file)
  file = paste0(dir,"Aurisano S2.xlsx")
  print(file)
  s2 = readxl::read_xlsx(file)

  # Fix test species/study type values, get key
  s1 = s1 %>%
    dplyr::mutate(
      tested_species_curated = dplyr::case_when(
        tested_species_curated %in% c("rat", "rat*") ~ "Rat",
        tested_species_curated == "mouse" ~ "Mouse",
        tested_species_curated == "human" ~ "Human",
        tested_species_curated == "dog" ~ "Dog",
        tested_species_curated == "rabbit" ~ "Rabbit",
        TRUE ~ tested_species_curated
      ),
      study_type_curated = dplyr::case_when(
        study_type_curated == "subacute" ~ "short-term",
        TRUE ~ study_type_curated
      ),
      key = paste(dtxsid, source, toxval_numeric, tested_species_curated, toxval_type_curated, study_type_curated)
    ) %>%
    dplyr::select(c("key", "log_BMDh_nrd_1 [mg/kg-d]", "log_BMDh_nrd_2 [mg/kg-d]", "log_BMDh_nrd_avg [mg/kg-d]",
                    "conceptual_model_nrd_1", "conceptual_model_nrd_2", "standardized_effect_categories")) %>%
    dplyr::rename(s1_standardized_effect_categories = standardized_effect_categories) %>%
    dplyr::distinct() %>%
    dplyr::group_by(key) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(has_s1=1)
  s2 = s2 %>%
    dplyr::mutate(
      tested_species_curated = dplyr::case_when(
        tested_species_curated %in% c("rat", "rat*") ~ "Rat",
        tested_species_curated == "mouse" ~ "Mouse",
        tested_species_curated == "human" ~ "Human",
        tested_species_curated == "dog" ~ "Dog",
        tested_species_curated == "rabbit" ~ "Rabbit",
        TRUE ~ tested_species_curated
      ),
      key = paste(dtxsid, source, toxval_numeric, tested_species_curated, toxval_type_curated, study_type_curated)
    ) %>%
    dplyr::select(c("key", "log_BMDh_rd_1 [mg/kg-d]", "log_BMDh_rd_2 [mg/kg-d]", "log_BMDh_rd_avg [mg/kg-d]",
                    "conceptual_model_rd_1", "conceptual_model_rd_2", "standardized_effect_categories")) %>%
    dplyr::rename(s2_standardized_effect_categories = standardized_effect_categories) %>%
    dplyr::distinct() %>%
    dplyr::group_by(key) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(has_s2=1)

  file = paste0(dir,"effect category dictionary.xlsx")
  print(file)
  dict = readxl::read_xlsx(file)

  file = paste0(dir,"study type dictionary.xlsx")
  print(file)
  sdict = readxl::read_xlsx(file) %>%
    dplyr::rename(study_type=study_type_original, sdict_study_type=study_type)

  file = paste0(dir,"conceptual model dictionary 2.xlsx")
  print(file)
  cdict = readxl::read_xlsx(file) %>%
    dplyr::rename(sts2=study_type_standard)

  res = res %>%
    dplyr::select(
      tidyselect::any_of(
        c("dtxsid","casrn","name","source","toxval_type","toxval_type_standard","study_type","study_type_standard",
          "critical_effect",
          "effect_category_standard","conceptual_model_1","conceptual_model_2",
          "conceptual_model_1_aurisano","conceptual_model_2_aurisano",
          "bmdh1","bmdh2","bmdh",
          "bmdh1_aurisano","bmdh2_aurisano","bmdh_aurisano","bmdh_ratio",
          "F1","F2","F31","F32","F4","F5",
          "common_name","toxval_numeric","toxval_units",
          "toxval_numeric_qualifier",
          "study_duration_value","study_duration_units","study_duration_class",
          "exposure_route",
          "year", "record_source_info","source_hash",
          "study_group","key",
          "toxval_numeric_hed", "final_model1", "final_model2")
      )
    ) %>%
    dplyr::left_join(sdict, by=c("study_type")) %>%
    dplyr::left_join(dict, by=c("critical_effect")) %>%
    dplyr::mutate(
      study_type = dplyr::case_when(
        !sdict_study_type %in% c(as.character(NA), "", "-") ~ sdict_study_type,
        TRUE ~ study_type
      ),

      toxval_type_standard = dplyr::case_when(
        substr(toxval_type,1,1) == "N" ~ "NOAEL",
        substr(toxval_type,1,1) == "L" ~ "LOAEL",
        substr(toxval_type,1,4) == "BMDL" ~ "BMDL",
        substr(toxval_type,1,3) == "BMD" | substr(toxval_type,1,3) == "POD" ~ "BMD",
        substr(toxval_type,1,4) == "BMCL" ~ "BMCL",
        substr(toxval_type,1,3) == "BMC" ~ "BMC",
        TRUE ~ as.character(NA)
      ),

      study_type_standard = dplyr::case_when(
        study_type == "chronic" ~ "chronic",
        study_type == "subchronic" | study_type == "28-day" ~ "subchronic",
        study_type == "short-term" ~ "short-term",
        study_type %in% c("developmental", "reproduction", "reproduction developmental") ~ "reproductive developmental",
        study_type == "repeat dose other" ~ "subchronic",
        TRUE ~ as.character(NA)
      ),

      effect_category_standard = dplyr::case_when(
        !standardized_effect_category %in% c(as.character(NA), "", "-") ~ standardized_effect_category,
        TRUE ~ "other"
      ),

      key = paste(dtxsid, source, toxval_numeric, common_name, toxval_type, study_type_standard)
    ) %>%
    dplyr::left_join(s1, by=c("key")) %>%
    dplyr::left_join(s2, by=c("key")) %>%
    dplyr::mutate(
      # Assign values from s1 and s2 (checked - no overlap)
      bmdh1_aurisano = dplyr::case_when(
        has_s1==1 ~ 10**`log_BMDh_nrd_1 [mg/kg-d]`,
        has_s2==1 ~ 10**`log_BMDh_rd_1 [mg/kg-d]`,
        TRUE ~ NA
      ),
      bmdh2_aurisano = dplyr::case_when(
        has_s1==1 ~ 10**`log_BMDh_nrd_2 [mg/kg-d]`,
        has_s2==1 ~ 10**`log_BMDh_rd_2 [mg/kg-d]`,
        TRUE ~ NA
      ),
      bmdh_aurisano = dplyr::case_when(
        has_s1==1 ~ 10**`log_BMDh_nrd_avg [mg/kg-d]`,
        has_s2==1 ~ 10**`log_BMDh_rd_avg [mg/kg-d]`,
        TRUE ~ NA
      ),
      conceptual_model_1_aurisano = dplyr::case_when(
        has_s1==1 ~ conceptual_model_nrd_1,
        has_s2==1 ~ conceptual_model_rd_1,
        TRUE ~ "-"
      ),
      conceptual_model_2_aurisano = dplyr::case_when(
        has_s1==1 ~ conceptual_model_nrd_2,
        has_s2==1 ~ conceptual_model_rd_2,
        TRUE ~ "-"
      ),
      effect_category_standard = dplyr::case_when(
        has_s1==1 ~ s1_standardized_effect_categories,
        has_s2==1 ~ s2_standardized_effect_categories,
        TRUE ~ "-"
      )
    )

  # Perform calculations on appropriate entries
  calc_res = res %>%
    dplyr::filter(!is.na(effect_category_standard) & !is.na(study_type_standard)) %>%
    dplyr::mutate(
      sts2 = dplyr::case_when(
        study_type_standard %in% c("chronic", "subchronic", "short-term") ~ "repeat dose",
        TRUE ~ "reproductive developmental"
      )
    ) %>%
    dplyr::left_join(cdict, by=c("sts2", "effect_category_standard")) %>%
    dplyr::mutate(
      F1 = dplyr::case_when(
        study_type_standard == "subchronic" ~ 2,
        study_type_standard == "short-term" ~ 5,
        TRUE ~ 1
      ),

      F2 = dplyr::case_when(
        toxval_type_standard == "LOAEL" ~ 3,
        study_type_standard == "BMDL" & sts2 == "repeat dose" ~ 0.5,
        TRUE ~ 1
      ),

      F31 = dplyr::case_when(
        is.na(final_model1) | is.na(final_model2) ~ NA,
        final_model1 == "Continuous" & toxval_type_standard == "BMDL" & sts2 == "repeat dose" ~ 2/3,
        final_model1 == "Continuous" ~ 1/3,
        final_model1 == "Quantal-Deterministic" ~ 2/9,
        final_model1 == "Quantal-Stochastic" ~ 2/3,
        TRUE ~ 1
      ),

      F32 = dplyr::case_when(
        is.na(final_model1) | is.na(final_model2) ~ NA,
        final_model1 == "Continuous" ~ 1/3,
        final_model1 == "Quantal-Deterministic" ~ 2/9,
        final_model1 == "Quantal-Stochastic" & toxval_type_standard == "BMDL" ~ 1/3,
        final_model1 == "Quantal-Stochastic" ~ 2/3,
        TRUE ~ 1
      ),

      F4 = dplyr::case_when(
        is.na(final_model1) | is.na(final_model2) ~ NA,
        toxval_numeric_hed == 1 ~ 1,
        common_name == "Rat" ~ 4.1,
        common_name == "Mouse" ~ 7.3,
        common_name == "Rabbit" ~ 2.4,
        common_name == "Dog" ~ 1.5,
        TRUE ~ 1
      ),

      F5 = 1,

      denom1 = F1*F2*F31*F4*F5,
      denom2 = F1*F2*F32*F4*F5,

      bmdh1 = toxval_numeric / denom1,
      bmdh2 = toxval_numeric / denom2,

      bmdh = dplyr::case_when(
        final_model2 != "-" ~ 10**(0.5*(log10(bmdh1)+log10(bmdh2))),
        TRUE ~ bmdh1
      ),

      bmdh2 = dplyr::case_when(
        final_model2 == "-" ~ NA,
        TRUE ~ bmdh2
      )
    )

  res = res %>%
    dplyr::filter(is.na(effect_category_standard) | is.na(study_type_standard)) %>%
    dplyr::bind_rows(calc_res) %>%
    dplyr::mutate(
      bmdh_ratio = dplyr::case_when(
        !is.na(bmdh_aurisano) ~ bmdh / bmdh_aurisano,
        TRUE ~ NA
      )
    ) %>%
    dplyr::filter(study_type != "acute") %>%
    dplyr::select(
      tidyselect::any_of(
        c("dtxsid","casrn","name","source","toxval_type","toxval_type_standard","study_type","study_type_standard",
          "critical_effect",
          "effect_category_standard","conceptual_model_1","conceptual_model_2",
          "conceptual_model_1_aurisano","conceptual_model_2_aurisano",
          "bmdh1","bmdh2","bmdh",
          "bmdh1_aurisano","bmdh2_aurisano","bmdh_aurisano","bmdh_ratio",
          "F1","F2","F31","F32","F4","F5",
          "common_name","toxval_numeric","toxval_units",
          "toxval_numeric_qualifier",
          "study_duration_value","study_duration_units","study_duration_class",
          "exposure_route",
          "year", "record_source_info","source_hash",
          "study_group","key",
          "toxval_numeric_hed", "final_model1", "final_model2")
      )
    )

  plot_res = res %>%
    dplyr::select(bmdh, bmdh_aurisano) %>%
    tidyr::drop_na(bmdh_aurisano)
  p = ggplot2::ggplot(data=plot_res, ggplot2::aes(x=bmdh, y=bmdh_aurisano))
  fname = paste0("data/results/toxvaldb.bmdh.per.study.plot.pdf")
  ggplot2::ggsave(plot = p, width = 5, height = 6, dpi = 300, filename =fname)

  # Write output to file
  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(res,file)
}
