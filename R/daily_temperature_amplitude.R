#' Computes temperature (soil temperature, but can also be air temperature) daily amplitudes from halfhourly data
#'
#' @param df a dataframe containing a datetime column as POSIXct object, and temperature column in °C
#' @param time_col the name of the datetime column
#' @param T_col the name of the temperature column
#'
#' @return a dataframe containing daily values of daily temperature amplitude, and the mean soil temperature#' @export
#' @export
#'
daily_temperature_amplitude <- function(df,
                                        time_col,
                                        T_col){
    df_sel <- df %>% dplyr::select(dplyr::one_of(c(time_col, T_col)))
    names(df_sel) = c("datetime", "Ts")
    df_aggr <- df_sel %>%
      dplyr::mutate(date=as.Date(datetime)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(Ts_min = min(Ts, na.rm = TRUE),
                Ts_max = max(Ts, na.rm = TRUE),
                Ts_ampl = Ts_max - Ts_min,
                Ts_mean = mean(Ts, na.rm = TRUE))

    return(df_aggr)

  }
