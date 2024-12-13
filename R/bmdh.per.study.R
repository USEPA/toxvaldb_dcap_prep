#-----------------------------------------------------------------------------------
#' @param toxval.db Database version
#' @param run_name The desired name for the output directory (Default: current date)
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
bmdh.per.study <- function(toxval.db, run_name=Sys.Date()) {
  printCurrentFunction(toxval.db)
  dir = paste0(Sys.getenv("datapath"), "data/results/", run_name, "/")

  # Read in initial data
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," POD filtered.xlsx")
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
    ) %>%
    dplyr::mutate(
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
        study_type %in% c("developmental", "reproduction",
                          "reproduction developmental") ~ "reproductive developmental",
        study_type == "repeat dose other" ~ "subchronic",
        TRUE ~ as.character(NA)
      )
    )

  # Perform calculations on appropriate entries
  calc_res = res %>%
    dplyr::filter(!is.na(study_type_standard)) %>%
    dplyr::mutate(
      sts2 = dplyr::case_when(
        study_type_standard %in% c("chronic", "subchronic", "short-term") ~ "repeat dose",
        TRUE ~ "reproductive developmental"
      )
    ) %>%
    dplyr::mutate(
      F1 = dplyr::case_when(
        study_type_standard == "subchronic" ~ 2,
        study_type_standard == "short-term" ~ 5,
        TRUE ~ 1
      ),

      F2 = dplyr::case_when(
        toxval_type_standard == "LOAEL" ~ 3,
        toxval_type_standard == "BMDL" & sts2 == "repeat dose" ~ 0.5,
        TRUE ~ 1
      ),

      F31 = dplyr::case_when(
        is.na(final_model1) ~ NA,
        final_model1 == "continuous" & toxval_type_standard == "BMDL" & sts2 == "repeat dose" ~ 2/3,
        final_model1 == "continuous" ~ 1/3,
        final_model1 == "quantal-deterministic" ~ 2/9,
        final_model1 == "quantal-stochastic" ~ 2/3,
        TRUE ~ 1
      ),

      F32 = dplyr::case_when(
        is.na(final_model1) | is.na(final_model2) ~ NA,
        final_model2 == "continuous" ~ 1/3,
        final_model2 == "quantal-deterministic" ~ 2/9,
        final_model2 == "quantal-stochastic" & toxval_type_standard == "BMDL" ~ 1/3,
        final_model2 == "quantal-stochastic" ~ 2/3,
        TRUE ~ 1
      ),

      F4 = dplyr::case_when(
        is.na(final_model1) ~ NA,
        toxval_numeric_hed == 1 ~ 1,
        common_name == "Rat" ~ 4.1,
        common_name == "Mouse" ~ 7.3,
        common_name == "Rabbit" ~ 2.4,
        common_name == "Dog" ~ 1.5,
        TRUE ~ 1
      ),

      F5 = 1,

      denom1 = F1*F2*F31*F4*F5,
      denom2 = dplyr::case_when(
        is.na(final_model2) ~ NA,
        TRUE ~ F1*F2*F32*F4*F5
      ),

      bmdh1 = toxval_numeric / denom1,
      bmdh2 = dplyr::case_when(
        is.na(final_model2) ~ NA,
        TRUE ~ toxval_numeric / denom2
      ),

      bmdh = dplyr::case_when(
        !(final_model2 %in% c("-", "", as.character(NA))) ~ 10**(0.5*(log10(bmdh1)+log10(bmdh2))),
        TRUE ~ bmdh1
      ),

      bmdh2 = dplyr::case_when(
        final_model2 %in% c("-", "", as.character(NA)) ~ NA,
        TRUE ~ bmdh2
      )
    )

  res = res %>%
    dplyr::filter(is.na(study_type_standard)) %>%
    dplyr::bind_rows(calc_res) %>%
    dplyr::filter(study_type != "acute") %>%
    dplyr::select(
      tidyselect::any_of(
        c("dtxsid","casrn","name","source","source_table","toxval_type","toxval_type_standard","study_type",
          "study_type_standard", "sts2", "toxicological_effect",
          "bmdh1","bmdh2","bmdh",
          "F1","F2","F31","F32","F4","F5",
          "common_name","toxval_numeric","toxval_units",
          "toxval_numeric_qualifier",
          "study_duration_value","study_duration_units","study_duration_class",
          "exposure_route",
          "year", "record_source_info","source_hash",
          "study_group",
          "toxval_numeric_hed", "final_model1", "final_model2")
      )
    )

  # Write output to file
  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db,".xlsx")
  writexl::write_xlsx(res,file)
}
