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
                             'developmental')
  ) %>%
    return()
}
