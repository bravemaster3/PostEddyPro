#' Caculates and appends sine, cosine and time delta to a dataframe
#'
#' @param df dataframe containing the timestamp column
#' @param datetime timestamp column name, which should already be a POSIXct object
#'
#' @return a dataframe to which the sine, cosine and delta (decimal timestamp from beginning of the year)
#' @export
#'
#' @examples
#' temporal_calculators(data.frame(datetime=as.POSIXct(c("2020-01-01 00:30:00",
#' "2020-01-01 01:00:00", "2020-01-01 01:30:00", "2020-01-01 02:00:00"))))
temporal_calculators <- function(df, datetime="datetime"){
  df$doy <- lubridate::yday(df[[datetime]])
  df$yearly_sin <- sin(2 * pi * (df$doy - 1) / 365)
  df$yearly_cos <- cos(2 * pi * (df$doy - 1) / 365)
  df$delta <- lubridate::decimal_date(df[[datetime]]) - lubridate::year(df[[datetime]])
  return(df)
}
