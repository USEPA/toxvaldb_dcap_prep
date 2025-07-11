#' @title convert_units
#' @description Function to convert input values to desired units.
#' @param x Numeric to be converted.
#' @param units Original units to be converted.
#' @param desired Desired units to convert the input value into.
#' @param conv_factor Conversion factor to use (such as Molecular weight, tissue Density, etc.).
#' @param overwrite_units Boolean to overwrite the 'units' with desired units.
#' @return Modified input "x" parameter converted to desired units.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Convert input "24" days into hours - expected result of "576" hours
#'  convert_units(x=24, units = "days", desired = "hours")
#'  }
#' }
#' @rdname convert_units
#' @export
convert_units <- function(x, units, desired, conv_factor=NA){
  # Get conversion factor
  conv = convert_get_conversion_factor(conv_factor)

  # Convert units based on input string equation
  if(is.null(conv[[units]][[desired]])){
    # No matching desired output
    x = NA
  } else {
    # Get the conversion equation (e.g. 20 days to weeks is '20/7')
    equ = paste0(x, conv[[units]][[desired]])
    x = parse(text=equ) %>% # parse the string
      eval() %>% # evaluate the string equation
      round(., 5) # round to 5 decimal places
  }
  return(x)
}
