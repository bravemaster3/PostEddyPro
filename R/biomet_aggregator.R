#' Biomet variables aggregator, from halfhourly to either hourly, daily, weekly, monthly yearly and growing_season timescales
#'
#' @param aggr_var Type of aggregation, either of "hour","day","week","month","year","growing_season"
#' @param df dataframe containing the variables to aggregate and the timestamp column
#' @param datetime timestamp column name already in a POSIXct format and UTC timezone to avoid summertime problems
#' @param biomet_vars Vector of Biomet variables to aggregate
#' @param aggr_fun Vector of aggregation functions to apply to each biomet variable. This should be the same lendth and same order as biomet_vars
#' @param month_start_growing_season When aggr_var is "growing_seaon", this is the month considered as the start of the growing season
#' @param month_end_growing_season When aggr_var is "growing_seaon", this is the month considered as the end of the growing season
#' @param na.rm_val logical, TRUE if you want NAs to be ignored. Note that this means that sums may be calculated even when there are missing values, which is misleading.
#' @param growing_season_definition The way growing season is definied. "fixed_months" <default> will use the months between month_start_growing_season and month_end_growing_season.
#' "meteorological" will use air temperature data to calculate the growing season start and end dates. This option requires to provide a dataframe containing date and air temperature data.
#' @param df_Ta a dataframe containing 2 columns: a column named "date" containing dates, and a column named "Ta" containing daily average air temperatures
#' @param df_Ts a dataframe containing 2 columns: a PosiXct column with timestamps in the format "%Y-%m-%d %H:%M:%S" and a column representing soil temperatures <e.g. Ts 15 cm>
#' @param number_consec_days Minimum number of days used to define the growing season, for which air temperature, soil temperature or soil temperature amplitude remains above a set threshold
#'
#' @return a summarized dataframe, containing the aggregation timescale variables and the aggregated values
#' @export
biomet_aggregator <- function(aggr_var=c("hour","day","week","month","year","growing_season"),#only one of these
                              df=gapfilling_list_hals$site_df,
                              datetime="datetime",
                              aggr_fun=c("mean","mean","mean","sum","mean"),
                              biomet_vars=c("Ta_f", "Ts_f", "WTD_f", "PARin_f", "SNOWd"),
                              month_start_growing_season=5,
                              month_end_growing_season=10,
                              na.rm_val=TRUE,
                              growing_season_definition="fixed_months", #or"meteorological", or "soil_temp", or "soil_temp_mean"
                              df_Ta = NULL,
                              df_Ts = NULL,
                              number_consec_days=7
                              ){

  #First, lets identify the growing season periods if the growing_season_definition is "meteorological"

  if(growing_season_definition == "meteorological" & !is.null(df_Ta)){
    gs_info <- growing_season_definer(df=df_Ta, time_col="date", Ta_col="Ta", number_consec_days=number_consec_days)
  }

  if(growing_season_definition == "soil_temp" & !is.null(df_Ts)){
    gs_info <- growing_season_definer(df=df_Ts, time_col="date", Ta_col="Ts_ampl", Ta_day_threshold=0.1, number_consec_days=number_consec_days)
  }

  if(growing_season_definition == "soil_temp_mean" & !is.null(df_Ts)){
    gs_info <- growing_season_definer(df=df_Ts, time_col="date", Ta_col="Ts_mean", Ta_day_threshold=2, number_consec_days=number_consec_days)
  }


  output_list <- list()
  aggr_var_real <- NULL
  if(aggr_var == "hour"){
    df[,datetime] <- as.POSIXct(round(df[,datetime], units="hours"), tz="UTC")
    aggr_var_real <- "datetime"
    list_aggr_var_real <- list(df$datetime)
  }else if(aggr_var == "day"){
    df$date <- as.Date(df[,datetime])
    aggr_var_real <- "date"
    list_aggr_var_real <- list(df$date)
  } else if(aggr_var == "week") {
    df$year <- lubridate::year(df[,datetime])
    df$week <- lubridate::week(df[,datetime])
    aggr_var_real <- c("year","week")
    list_aggr_var_real <- list(df$year,df$week)
  } else if(aggr_var == "month") {
    df$year <- lubridate::year(df[,datetime])
    df$month <- lubridate::month(df[,datetime])
    aggr_var_real <- c("year","month")
    list_aggr_var_real <- list(df$year,df$month)
  } else if(aggr_var == "year") {
    df$year <- lubridate::year(df[,datetime])
    aggr_var_real <- "year"
    list_aggr_var_real <- list(df$year)
  } else if(aggr_var == "growing_season") {
    df$year <- lubridate::year(df[,datetime])
    df$month <- lubridate::month(df[,datetime])
    df$growing_season <- NA

    if (growing_season_definition == "fixed_months"){
      for(yr in unique(df$year)){
        g_s <- NA
        g_s <- yr - min(unique(df$year)) + 1
        df$growing_season[which(df$month %in% c(month_start_growing_season:month_end_growing_season) & df$year==yr)] <- paste("Growing season", yr, sep= " ")
        #df$growing_season[which(!(df$month %in% c(month_start_growing_season:month_end_growing_season)) & df$year==yr)] <-  paste("Non growing season", g_s, sep= " ")
        df$growing_season[which((df$month < month_start_growing_season) & df$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
        df$growing_season[which((df$month > month_end_growing_season) & df$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
      }
    }

    if(growing_season_definition %in% c("meteorological", 'soil_temp', 'soil_temp_mean')){
      for(yr in unique(df$year)){
        gs_start <- gs_info[gs_info$year == yr, "start"]
        gs_end <- gs_info[gs_info$year == yr, "end"]


        # g_s <- NA
        # g_s <- yr - min(unique(df$year)) + 1
        #df$growing_season[which(df$month %in% c(month_start_growing_season:month_end_growing_season) & df$year==yr)] <- paste("Growing season", yr, sep= " ")
        ###########################df$growing_season[which(!(df$month %in% c(month_start_growing_season:month_end_growing_season)) & df$year==yr)] <-  paste("Non growing season", g_s, sep= " ")
        #df$growing_season[which((df$month < month_start_growing_season) & df$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
        #df$growing_season[which((df$month > month_end_growing_season) & df$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
        df$growing_season[which(df$date >= gs_start & df$date <= gs_end & df$year==yr)] <- paste("Growing season", yr, sep= " ")
        df$growing_season[which(df$date < gs_start & df$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
        df$growing_season[which(df$date > gs_end & df$year==yr)] <-  paste("Non growing season2", yr, sep= " ")

        }
    }


    aggr_var_real <- c("year","growing_season")
    list_aggr_var_real <- list(df$year,df$growing_season)
  }

for(fun in unique(aggr_fun)){
  print(fun)
  ind <- which(aggr_fun == fun)
  biomet_sub <- biomet_vars[ind]
  df_fun_name <- paste0("df","_",fun)
    #df[,biomet_sub]

  output_list[[df_fun_name]] <- stats::aggregate(list(df[,biomet_sub]), by = list_aggr_var_real, fun, na.rm=na.rm_val) %>% `colnames<-` (c(aggr_var_real,biomet_sub))
}

#return(purrr::reduce(output_list,  by=aggr_var))
  df <- Reduce(function(...) merge(..., all = TRUE, by=aggr_var_real), output_list)

  if(growing_season_definition %in% c("meteorological", "soil_temp", "soil_temp_mean")) list_df <- list(df=df, growing_season_info=gs_info)

  return(list_df)
}
