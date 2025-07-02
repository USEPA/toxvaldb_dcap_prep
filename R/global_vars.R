#' @title global_vars
#' @description Global variable list to use acros scripts.
#' @return Named list of global variables.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Get vector of DCAP sources
#'  global_vars()$dcap_sources
#'  }
#' }
#' @rdname global_vars
#' @export
global_vars <- function(){
  list(
    # List of types to overwrite as repeated dose or reprodev
    repeat_study_types = c('immunotoxicity','intermediate','repeat dose other','subchronic',
                           'neurotoxicity subchronic','neurotoxicity chronic',
                           'neurotoxicity 28-day', 'neurotoxicity','intermediate','1','104','14','2','24',
                           'immunotoxicity subchronic','immunotoxicity chronic',
                           'immunotoxicity 28-day','immunotoxicity','growth','chronic','28-day', 'short-term'),
    reprodev_study_types = c('reproduction developmental',
                             'extended one-generation reproductive toxicity - with F2 generation and developmental neurotoxicity (Cohorts 1A, 1B with extension, 2A and 2B)',
                             'extended one-generation reproductive toxicity - with F2 generation and both developmental neuro- and immunotoxicity (Cohorts 1A, 1B with extension, 2A, 2B, and 3)',
                             'extended one-generation reproductive toxicity - with F2 generation (Cohorts 1A, and 1B with extension)',
                             'developmental'),
    iuclid_dcap = c('source_iuclid_repeateddosetoxicityoral',
                    'source_iuclid_developmentaltoxicityteratogenicity',
                    'source_iuclid_carcinogenicity',
                    'source_iuclid_immunotoxicity',
                    'source_iuclid_neurotoxicity',
                    'source_iuclid_toxicityreproduction'),
    dcap_sources = c("ATSDR MRLs", "HAWC Project", "ATSDR PFAS 2021", "Cal OEHHA",
                     "ECHA IUCLID", "ECOTOX", "EFSA", "EPA HHTV", "HAWC PFAS 150", "HAWC PFAS 430",
                     "Health Canada", "HEAST", "HESS", "HPVIS", "IRIS",
                     "NTP PFAS", "PFAS 150 SEM v2", "PPRTV (CPHEA)", "ToxRefDB", "WHO JECFA Tox Studies")
  ) %>%
    return()
}
