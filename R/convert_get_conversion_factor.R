#' @description A helper function to convert input values to desired units.
#' @param conv_factor Conversion factor to use (such as Molecular weight, tissue Density, etc.)
#' @title convert_get_conversion_factor
#' @return List of conversion factors
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_get_conversion_factor
#' @export
convert_get_conversion_factor <- function(conv_factor=1){
  # Map of input units to desired output units equation
  list(
    minutes = list(minutes="/1", hours="/60", days="/60/24", weeks="/60/24/7", months="/60/24/30", years="/60/24/365"),
    hours = list(minutes="*60", hours="/1", days="/24", weeks="/24/7", months="/24/30", years="/24/365"),
    days = list(minutes="*60*24", hours="*24", days="/1", weeks="/7", months="/30", years="/365"),
    weeks = list(minutes="*60*24*7", hours="*24*7", days="*7", weeks="/1", months="/4", years="/52"),
    months = list(minutes="*60*30*24", hours="*30*24", days="*30", weeks="*4", months="/1", years="/52"),
    years = list(minutes="*60*24*365", hours="*24*365", days="*365", weeks="*52", months="*12", years="/1")
  ) %>%
    return()
}
