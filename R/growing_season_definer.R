#' Growing season definition from date or timestamp and air temperature
#'
#' @param df dataframe containing a timestamp or date, and air temperature
#' @param time_col name of the timestamp column. It can be a Date column, or a POSIXct object
#' @param Ta_col name of the air temperature (in °C) column
#' @param Ta_day_threshold threshold of air temperature above or below which the growing season starts or ends
#' @param number_consec_days number of consecutive days when Ta is above or below Ta_day_threshold for start/stop counting the growing season
#'
#' @return a dataframe with 3 columns: the year, the date of start of the growing season, the date of end of the growing season
#' @export
#'
#'
growing_season_definer <- function(df,
                                   time_col,
                                   Ta_col,
                                   Ta_day_threshold=5,
                                   number_consec_days=5){
  df$counter <- 0
  df$counter[df[[Ta_col]] > Ta_day_threshold] <- 1
  df$antiCounter <- 1 - df$counter
  df$cumCount <- NA
  df$antCumCount <- NA

  for(i in 1:nrow(df)){
    if(i == 1 | df$counter[i] == 0){
      df$cumCount[i] <- df$counter[i]
    }
    if(i > 1 & df$counter[i] != 0){
      df$cumCount[i] <- df$cumCount[i-1] + df$counter[i]
    }
  }

  for(i in 1:nrow(df)){
    if(i == 1 | df$antiCounter[i] == 0){
      df$antCumCount[i] <- df$antiCounter[i]
    }
    if(i > 1 & df$antiCounter[i] != 0){
      df$antCumCount[i] <- df$antCumCount[i-1] + df$antiCounter[i]
    }
  }


  df$year <- lubridate::year(df[[time_col]])

  df_gs_info <- data.frame(year=unique(df$year), start=as.Date(NA), end = as.Date(NA))

  for(year in df_gs_info$year){
    df_sub <- df[df$year == year,]
    all_5 <- which(df_sub$cumCount == (number_consec_days+1))
    date_start <- df_sub$date[all_5[1]-number_consec_days]

    all_5_end <-which(df_sub$antCumCount == (number_consec_days+1))
    date_end <- df_sub$date[all_5_end[which(all_5_end > all_5[1])][1]-number_consec_days]

    df_gs_info[df_gs_info$year==year, "start"] <- as.Date(date_start)
    df_gs_info[df_gs_info$year==year, "end"] <- as.Date(date_end)
  }

  return(df_gs_info)
}
