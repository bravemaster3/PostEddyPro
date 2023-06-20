
#' Aggregate methane fluxes and uncertainties <measurement and gapfilling related> to 30min, daily, monthly and yearly
#'
#' @param flux_df flux dataframe output from the random forest gapfilling
#' @param df_mc dataframe of merged montecarlo simulation already gapfilled
#' @param rd_err_df dataframe containing random errors already imputed
#' @param month_start_growing_season Month of the start of growing season. Default is 5
#' @param month_end_growing_season Month of the end of growing season. Default is 10
#' @param growing_season_definition The way growing season is definied. "fixed_months" <default> will use the months between month_start_growing_season and month_end_growing_season.
#' "meteorological" will use air temperature data to calculate the growing season start and end dates. This option requires to provide a dataframe containing date and air temperature data.
#' @param df_Ta a dataframe containing 2 columns: a column named "date" containing dates, and a column named "Ta" containing daily average air temperatures
#' @param df_Ts a dataframe containing 2 columns: a PosiXct column with timestamps in the format "%Y-%m-%d %H:%M:%S" and a column representing soil temperatures <e.g. Ts 15 cm>
#' @param number_consec_days Minimum number of days used to define the growing season, for which air temperature, soil temperature or soil temperature amplitude remains above a set threshold
#'
#' @return a list of half-hourly, hourly, daily, weekly, monthly, growing season and yearly aggregated flux dataframes
#' @export
ch4_flux_aggregator <- function(flux_df,
                                df_mc,
                                rd_err_df,
                                month_start_growing_season = 5,
                                month_end_growing_season = 10,
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



  #testing
  # flux_df = gapfilling_list_hals$site_df
  # df_mc = df_mc
  # rd_err_df = hals_err_df_imp

  #initializing or "nulifying" global variables
  rand_err_ch4_flux <- datetime <- meas_err <- ch4_flux_final_filled <- FCH4_f <- iteration <- FCH4_sum <- year <- month <- FCH4_sd <- quality <- gapf_err <- tot_err <- FCH4 <- NULL
  aggr_list <- list()
  #converting the errors to mg...
  rd_err_df <- rd_err_df %>%
    dplyr::mutate(rand_err_ch4_flux = PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(rand_err_ch4_flux))%>%
    dplyr::rename(meas_err = rand_err_ch4_flux) %>%
    dplyr::select(c(datetime,meas_err))

  #gapfilling errors and measurement uncertainties together
  #30 min
  df_30min <- df_mc %>%
    dplyr::mutate(ch4_flux_final_filled=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(ch4_flux_final_filled)) %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(FCH4_sd = stats::sd(ch4_flux_final_filled)) %>%
    merge(rd_err_df, all=TRUE)

  #hour

  rd_err_df_hour <- rd_err_df %>%
    dplyr::mutate(hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_hour <- df_mc %>%
    dplyr::rename(FCH4_f=ch4_flux_final_filled) %>%
    dplyr::mutate(FCH4_f=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(FCH4_f),
                  hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour,iteration) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4_f)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(FCH4_sd = stats::sd(FCH4_sum)) %>%
    merge(rd_err_df_hour, all=TRUE)

  #day
  rd_err_df_day <- rd_err_df %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_day <- df_mc %>%
    dplyr::rename(FCH4_f=ch4_flux_final_filled) %>%
    dplyr::mutate(FCH4_f=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(FCH4_f),
           date=as.Date(datetime)) %>%
    dplyr::group_by(date,iteration) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4_f)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(FCH4_sd = stats::sd(FCH4_sum)) %>%
    merge(rd_err_df_day, all=TRUE)

  #week
  rd_err_df_week <- rd_err_df %>%
    dplyr::mutate(year=lubridate::year(datetime),
                  week=lubridate::week(datetime)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_week <- df_mc %>%
    dplyr::rename(FCH4_f=ch4_flux_final_filled) %>%
    dplyr::mutate(FCH4_f=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(FCH4_f),
                  year=lubridate::year(datetime),
                  week=lubridate::week(datetime)) %>%
    dplyr::group_by(year,week,iteration) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4_f)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(FCH4_sd = stats::sd(FCH4_sum)) %>%
    merge(rd_err_df_week, all=TRUE)

  #month
  rd_err_df_month <- rd_err_df %>%
    dplyr::mutate(year=lubridate::year(datetime),
           month=lubridate::month(datetime)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_month <- df_mc %>%
    dplyr::rename(FCH4_f=ch4_flux_final_filled) %>%
    dplyr::mutate(FCH4_f=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(FCH4_f),
           year=lubridate::year(datetime),
           month=lubridate::month(datetime)) %>%
    dplyr::group_by(year,month,iteration) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4_f)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(FCH4_sd = stats::sd(FCH4_sum)) %>%
    merge(rd_err_df_month, all=TRUE)


  #growing season
  rd_err_df$year <- lubridate::year(rd_err_df[,"datetime"])
  rd_err_df$month <- lubridate::month(rd_err_df[,"datetime"])
  rd_err_df$date <- as.Date(rd_err_df$datetime)
  rd_err_df$growing_season <- NA

  if(growing_season_definition == "fixed_months"){
    for(yr in unique(rd_err_df$year)){
      # gs_start <- gs_info[gs_info$year == yr, "start"]
      # gs_end <- gs_info[gs_info$year == yr, "end"]

      rd_err_df$growing_season[which(rd_err_df$month %in% c(month_start_growing_season:month_end_growing_season) & rd_err_df$year==yr)] <- paste("Growing season", yr, sep= " ")
      #rd_err_df$growing_season[which(!(rd_err_df$month %in% c(month_start_growing_season:month_end_growing_season)) & rd_err_df$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      rd_err_df$growing_season[which((rd_err_df$month < month_start_growing_season) & rd_err_df$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      rd_err_df$growing_season[which((rd_err_df$month > month_end_growing_season) & rd_err_df$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }
  }
  if (growing_season_definition %in% c("meteorological", 'soil_temp', 'soil_temp_mean')){
    for(yr in unique(rd_err_df$year)){
      gs_start <- gs_info[gs_info$year == yr, "start"]
      gs_end <- gs_info[gs_info$year == yr, "end"]
      rd_err_df[which(rd_err_df$date >= gs_start & rd_err_df$date <= gs_end &  rd_err_df$year==yr), "growing_season"] <- paste("Growing season", yr, sep= " ")
      #rd_err_df$growing_season[which(!(rd_err_df$month %in% c(month_start_growing_season:month_end_growing_season)) & rd_err_df$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      rd_err_df[which(rd_err_df$date < gs_start & rd_err_df$year==yr),"growing_season"] <-  paste("Non growing season1", yr, sep= " ")
      rd_err_df[which((rd_err_df$date > gs_end) & rd_err_df$year==yr), "growing_season"] <-  paste("Non growing season2", yr, sep= " ")
    }
  }

  #rd_err_df$growing_season <- as.factor(rd_err_df$growing_season)


  df_mc$year <- lubridate::year(df_mc$datetime)
  df_mc$month <- lubridate::month(df_mc$datetime)
  df_mc$date <- as.Date(df_mc$datetime)
  df_mc$growing_season <- NA

  if (growing_season_definition == "fixed_months"){
    for(yr in unique(df_mc$year)){
      # gs_start <- gs_info[gs_info$year == yr, "start"]
      # gs_end <- gs_info[gs_info$year == yr, "end"]
      df_mc$growing_season[which(df_mc$month %in% c(month_start_growing_season:month_end_growing_season) & df_mc$year==yr)] <- paste("Growing season", yr, sep= " ")
      #df_mc$growing_season[which(!(df_mc$month %in% c(month_start_growing_season:month_end_growing_season)) & df_mc$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      df_mc$growing_season[which((df_mc$month < month_start_growing_season) & df_mc$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      df_mc$growing_season[which((df_mc$month > month_end_growing_season) & df_mc$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }
  }
  if(growing_season_definition %in% c("meteorological", 'soil_temp', 'soil_temp_mean')){
    for(yr in unique(df_mc$year)){
      gs_start <- gs_info[gs_info$year == yr, "start"]
      gs_end <- gs_info[gs_info$year == yr, "end"]
      df_mc$growing_season[which(df_mc$date >= gs_start & df_mc$date <= gs_end & df_mc$year==yr)] <- paste("Growing season", yr, sep= " ")
      #df_mc$growing_season[which(!(df_mc$month %in% c(month_start_growing_season:month_end_growing_season)) & df_mc$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      df_mc$growing_season[which(df_mc$date < gs_start & df_mc$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      df_mc$growing_season[which(df_mc$date > gs_end & df_mc$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }
  }



  #df_mc$growing_season <- as.factor(df_mc$growing_season)

  rd_err_df_gs <- rd_err_df %>%
    dplyr::group_by(year,growing_season) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_gs <- df_mc %>%
    dplyr::mutate(FCH4_f=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(ch4_flux_final_filled)) %>%
    dplyr::group_by(year,growing_season,iteration) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4_f)) %>%
    dplyr::group_by(year,growing_season) %>%
    dplyr::summarise(FCH4_sd = stats::sd(FCH4_sum)) %>%
    merge(rd_err_df_gs[,c("year","growing_season","meas_err")], all=TRUE)



  #year
  rd_err_df_year <- rd_err_df %>%
    dplyr::mutate(year=lubridate::year(datetime)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_year <- df_mc %>%
    dplyr::rename(FCH4_f=ch4_flux_final_filled) %>%
    dplyr::mutate(FCH4_f=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(FCH4_f),
                  year=lubridate::year(datetime)) %>%
    dplyr::group_by(year,iteration) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4_f)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(FCH4_sd = stats::sd(FCH4_sum)) %>%
    merge(rd_err_df_year, all=TRUE)

  ##########################
  #AGGREGATING THE FLUXES

  flux_df <- flux_df %>%
    dplyr::mutate(ch4_flux_final_filled=PostEddyPro::umolCH4m2s_to_mgCH4m2_30min(ch4_flux_final_filled))

  flux_df_30min <- flux_df %>%
    dplyr::select(c("datetime","ch4_flux_final_filled","quality"))%>%
    merge(df_30min, by="datetime", all=TRUE)%>%
    dplyr::rename(gapf_err=FCH4_sd) %>%
    dplyr::mutate(tot_err = ifelse(quality=="original",
                            meas_err,
                            ifelse(quality=="gapfilled",
                                   sqrt(meas_err^2+gapf_err^2),
                                   NA)
    )) %>%
    dplyr::select(c(datetime, ch4_flux_final_filled,quality,meas_err,gapf_err,tot_err)) %>%
    dplyr::rename(FCH4=ch4_flux_final_filled)


  #hourly

  flux_df_hour <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::mutate(hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_hour, by="hour", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2))

  #daily

  flux_df_day <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_day, by="date", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2))

  #weekly

  flux_df_week <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::mutate(year = lubridate::year(datetime),
                  week = lubridate::week(datetime)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_week, by=c("year","week"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2)) %>%
    dplyr::arrange(year,week)

  #monthly

  flux_df_month <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::mutate(year = lubridate::year(datetime),
           month = lubridate::month(datetime)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_month, by=c("year","month"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2)) %>%
    dplyr::arrange(year,month)



  #Growing season
  if(growing_season_definition =="fixed_months"){
    flux_df$year <- lubridate::year(flux_df[,"datetime"])
    flux_df$month <- lubridate::month(flux_df[,"datetime"])
    flux_df$date <- as.Date(flux_df$datetime)
    flux_df$growing_season <- NA
    for(yr in unique(flux_df$year)){
      flux_df$growing_season[which(flux_df$month %in% c(month_start_growing_season:month_end_growing_season) & flux_df$year==yr)] <- paste("Growing season", yr, sep= " ")
      #flux_df$growing_season[which(!(flux_df$month %in% c(month_start_growing_season:month_end_growing_season)) & flux_df$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      flux_df$growing_season[which((flux_df$month < month_start_growing_season) & flux_df$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      flux_df$growing_season[which((flux_df$month > month_end_growing_season) & flux_df$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }
  }
  if(growing_season_definition %in% c("meteorological", "soil_temp", "soil_temp_mean")){
    flux_df$year <- lubridate::year(flux_df[,"datetime"])
    flux_df$month <- lubridate::month(flux_df[,"datetime"])
    flux_df$date <- as.Date(flux_df$datetime)
    flux_df$growing_season <- NA
    for(yr in unique(flux_df$year)){
      gs_start <- gs_info[gs_info$year == yr, "start"]
      gs_end <- gs_info[gs_info$year == yr, "end"]
      flux_df[which(flux_df$date >= gs_start & flux_df$date <= gs_end  & flux_df$year==yr), "growing_season"] <- paste("Growing season", yr, sep= " ")
      #flux_df$growing_season[which(!(flux_df$month %in% c(month_start_growing_season:month_end_growing_season)) & flux_df$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      flux_df[which((flux_df$date < gs_start) & flux_df$year==yr), "growing_season"] <-  paste("Non growing season1", yr, sep= " ")
      flux_df[which((flux_df$date > gs_end) & flux_df$year==yr), "growing_season"] <-  paste("Non growing season2", yr, sep= " ")
    }
  }


  #flux_df$growing_season <- as.factor(flux_df$growing_season)

  flux_df_gs <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::group_by(year, growing_season) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_gs, by=c("year","growing_season"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2))



  #yearly

  flux_df_year <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::mutate(year = lubridate::year(datetime)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_year, by=c("year"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2)) %>%
    dplyr::arrange(year)

  aggr_list$flux_df_30min <- flux_df_30min
  aggr_list$flux_df_hour <- flux_df_hour
  aggr_list$flux_df_day <- flux_df_day
  aggr_list$flux_df_week <- flux_df_week
  aggr_list$flux_df_month <- flux_df_month
  aggr_list$flux_df_growing_season <- flux_df_gs
  aggr_list$flux_df_year <- flux_df_year
  if(growing_season_definition %in% c("meteorological", "soil_temp", "soil_temp_mean")) aggr_list$growing_season_info <- gs_info

  return(aggr_list)

}
