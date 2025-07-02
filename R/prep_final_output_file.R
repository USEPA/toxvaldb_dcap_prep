#' @title prep_final_output_file
#' @description Function to finalize prepped DCAP input file to remove intermediate fields and update field assignments.
#' @param toxval.db Database version.
#' @param run_name The desired name for the output directory, default current date.
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname prep_final_output_file
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate case_when rename select all_of
#' @importFrom writexl write_xlsx
prep_final_output_file <- function(toxval.db, run_name=Sys.Date()){
  # Rename select fields for final output
  field_map = c("dtxsid_group"="Grouped_DTXSID", "chemical_name"="name", "standardized_drsv_type"="toxval_type",
                "drsv_numeric"="toxval_numeric", "drsv_units"="toxval_units", "standardized_study_type"="type",
                "study_species"="common_name", "conceptual_model_1"="final_model1", "conceptual_model_2"="final_model2")

  # Read in POD filtered file
  output = readxl::read_xlsx(paste0(Sys.getenv("datapath"), "data/results/",
                                        run_name, "/results/ToxValDB for DCAP ",
                                        toxval.db," POD filtered.xlsx")) %>%
    # Filter out "acute" duration_adjustments
    dplyr::filter(duration_adjustment != "acute") %>%
    # Recode standardized study type
    dplyr::mutate(type = dplyr::case_when(
      type == "repro dev" ~ "reproductive developmental",
      TRUE ~ type
    ),
    # Set grouped dtxsid to dtxsid if blank
    Grouped_DTXSID = dplyr::case_when(
      is.na(Grouped_DTXSID) | Grouped_DTXSID == "-" ~ dtxsid,
      TRUE ~ Grouped_DTXSID
    )
    ) %>%
    # Rename fields
    dplyr::rename(field_map) %>%
    # Remove extraneous intermediate fields not needed for DCAP
    dplyr::select(-dplyr::all_of(c('source_table', 'toxval_type_supercategory', 'toxval_subtype', 'toxval_numeric_qualifier_original',
                                   'toxval_numeric_qualifier', 'experimental_record', 'toxval_type_orig_dcap',
                                   'exposure_route_fix', 'toxicological_effect_category_original', 'filter_pod_group', 'toxicological_effect_category_fix',
                                   'duration_adjustment', 'toxval_numeric_hed', 'calibration_flag',
                                   'calibration_string', 'calibration_class', 'calibration_record'
                                   )))

  # Write final prepped DCAP input file
  writexl::write_xlsx(output, paste0(Sys.getenv("datapath"), "data/results/",
                                     run_name, "/results/DCAP_ToxVal_", toxval.db ,"_input.xlsx"))
}
