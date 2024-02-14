#' Wrapper for as.POSIXct to make conversion simpler
#'
#' @param str string regresenting a timestamp
#' @param format format of the timestamp, default is "%Y-%m-%d %H:%M:%S"
#' @param tz time zone, default is "UTC"
#'
#' @return a POSIXct timestamp object
#' @export
#'
#' @examples
#' str_posix("2024-01-01 12:30:00")
str_posix <- function(str, format="%Y-%m-%d %H:%M:%S",tz="UTC"){
  as.POSIXct(str,origin=format,tz=tz)
}
