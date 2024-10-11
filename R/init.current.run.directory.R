#-------------------------------------------------------------------------------
#' @param run_name The desired name for the output directory (Default: current date)
#' @export
#' @title init.current.run.directory
#' @description Initialize output directory for current driver run
#' @return None; new directory is created
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#-----------------------------------------------------------------------------------
init.current.run.directory <- function(run_name=Sys.Date()) {
  printCurrentFunction()
  new_directory = paste0("data/results/", run_name)
  if(dir.exists(new_directory)) {
    cat(paste0("Directory '", new_directory, "' already exists. Continuing will overwrite previous data.\n"))
    browser()
    do.call(file.remove, list(list.files(new_directory, full.names = TRUE)))
    do.call(file.remove, list(list.files(paste0(new_directory, "/results"), full.names = TRUE)))
    do.call(file.remove, list(list.files(paste0(new_directory, "/DCAP"), full.names = TRUE)))
  } else {
    cat(paste0("Initializing directory: ", new_directory, "\n"))
    dir.create(new_directory)
    dir.create(paste0(new_directory, "/results"))
    dir.create(paste0(new_directory, "/DCAP"))
    dir.create(paste0(new_directory, "/missing_crit_cat"))
  }
}
