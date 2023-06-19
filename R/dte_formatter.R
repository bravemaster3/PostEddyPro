#' Date formatter for use in other ggplot related functions
#'
#' @param x : a date
#' @param mode : one of "month_only" or "month_year"
#'
#' @return formatted dates
#' @export
#'
dte_formatter <- function(x,
                          mode = "month_only"#could also be "month_year"
                          ){
  #formatter for axis labels: J12, F12, M12, etc...
  mth <- substr(format(x, "%b"),1,1)
  if(mode == "month_year") {
    yr <- format(x, "%y")
    formatted <- paste0(mth, yr)
  }
  if(mode == "month_only"){
    formatted <- toupper(mth)
  }

  return(formatted)
}
