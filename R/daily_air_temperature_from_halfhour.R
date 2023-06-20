#' Aggregates halfhourly air temperature data to average daily temperatures. Useful for defining the growing season even before aggregating biomet and fluxes
#'
#' @param df a dataframe containing a datetime column as POSIXct object, and an air temperature column in °C
#' @param time_col the name of the datetime column
#' @param Ta_col the name of the air temperature column
#'
#' @return a dataframe containing daily values of average air temperature with 2 columns: date, and Ta.
#' @export
#'
daily_air_temperature_from_halfhour <- function(df,
                                  time_col,
                                  Ta_col){
  df_sel <- df %>% dplyr::select(dplyr::one_of(c(time_col, Ta_col)))
  names(df_sel) = c("datetime", "Ta")
  df_aggr <- df_sel %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Ta = mean(Ta))

  return(df_aggr)

}
