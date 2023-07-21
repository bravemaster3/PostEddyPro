
#' Aggregate CO2 fluxes and uncertainties <measurement and gapfilling related> to 30min, daily, monthly and yearly
#'
#' @param path_to_df_f path to flux dataframe REddyProc-Gapfilled
#' @param df_mc dataframe of merged montecarlo simulation already gapfilled
#' @param rd_err_df dataframe containing random errors already imputed
#' @param month_start_growing_season Month of the start of growing season. Default is 5
#' @param month_end_growing_season Month of the end of growing season. Default is 10
#' @param growing_season_definition The way growing season is definied. "fixed_months" <default> will use the months between month_start_growing_season and month_end_growing_season.
#' "meteorological" will use air temperature data to calculate the growing season start and end dates. This option requires to provide a dataframe containing date and air temperature data.
#' @param df_Ta a dataframe containing 2 columns: a column named "date" containing dates, and a column named "Ta" containing daily average air temperatures
#' @param df_Ts a dataframe containing 2 columns: a PosiXct column with timestamps in the format "\%Y-\%m-\%d \%H:\%M:\%S" and a column representing soil temperatures <e.g. Ts 15 cm>
#' @param number_consec_days Minimum number of days used to define the growing season, for which air temperature, soil temperature or soil temperature amplitude remains above a set threshold
#' @param Ts_ampl_thresh threshold of daily soil temperature amplitude when using that option for defining the growing season. default is 0.1°C
#' @param Ts_mean_thresh threshold of daily mean soil temperature when using that option for defining the growing season. default is 2°C
#' @param datetime datetime column in both of your data.frames
#'
#' @return a list of half-hourly, hourly, daily, weekly, monthly, growing season and yearly aggregated flux dataframes
#' @export
co2_flux_aggregator <- function(path_to_df_f,
                                df_mc,
                                rd_err_df,
                                month_start_growing_season=5,
                                month_end_growing_season=10,
                                growing_season_definition="fixed_months", #or"meteorological", or "soil_temp", or "soil_temp_mean"
                                df_Ta = NULL,
                                df_Ts = NULL,
                                datetime = "datetime",
                                number_consec_days=7,
                                Ts_ampl_thresh = 0.1,
                                Ts_mean_thresh = 2
                                ){


  #First, lets identify the growing season periods if the growing_season_definition is "meteorological"

  if(growing_season_definition == "meteorological" & !is.null(df_Ta)){
    gs_info <- growing_season_definer(df=df_Ta, time_col="date", Ta_col="Ta", number_consec_days=number_consec_days)
  }

  if(growing_season_definition == "soil_temp" & !is.null(df_Ts)){
    gs_info <- growing_season_definer(df=df_Ts, time_col="date", Ta_col="Ts_ampl", Ta_day_threshold=Ts_ampl_thresh, number_consec_days=number_consec_days)
  }

  if(growing_season_definition == "soil_temp_mean" & !is.null(df_Ts)){
    gs_info <- growing_season_definer(df=df_Ts, time_col="date", Ta_col="Ts_mean", Ta_day_threshold=Ts_mean_thresh, number_consec_days=number_consec_days)
  }


#creating important columns that are missing
  rd_err_df <- rd_err_df %>%
    mutate(date = as.Date(datetime),
           year = lubridate::year(datetime))

  df_mc <- df_mc %>%
    mutate(date = as.Date(datetime),
           year = lubridate::year(datetime))

  ####################Aggregating random measurement error

  rd_err_co2_final <- rd_err_df %>%
    dplyr::select(c(datetime, "rand_err_co2_flux")) %>%
    dplyr::mutate(rand_err_co2_flux=PostEddyPro::umolCO2m2s_to_gCm2_30min(rand_err_co2_flux)) %>%
    dplyr::rename(meas_err = rand_err_co2_flux)

  plot(meas_err~datetime, data=rd_err_co2_final)

  ###Now back to the gapfilling_errors


  #30 min...
  df_30min <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
           GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
           Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco)) %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_f),
              GPP_sd = stats::sd(GPP_f),
              Reco_sd = stats::sd(Reco)) %>%
    merge(rd_err_co2_final[,c(datetime,"meas_err")], all=TRUE)

  #fwrite(df_30min, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_CO2_30min_SD.csv", dateTimeAs = "write.csv")



  #hour...

  rd_err_co2_final_hour <- rd_err_co2_final %>%
    dplyr::mutate(hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))



  df_hour <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
                  GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
                  Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco),
                  hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour,iteration) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
                     GPP_sum = sum(GPP_f),
                     Reco_sum = sum(Reco)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_sum),
                     GPP_sd = stats::sd(GPP_sum),
                     Reco_sd = stats::sd(Reco_sum)) %>%
    merge(rd_err_co2_final_hour[,c("hour","meas_err")], all=TRUE)

  #day...

  rd_err_co2_final_day <- rd_err_co2_final %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))



  df_day <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
           GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
           Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco),
           date=as.Date(datetime)) %>%
    dplyr::group_by(date,iteration) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
              GPP_sum = sum(GPP_f),
              Reco_sum = sum(Reco)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_sum),
              GPP_sd = stats::sd(GPP_sum),
              Reco_sd = stats::sd(Reco_sum)) %>%
    merge(rd_err_co2_final_day[,c("date","meas_err")], all=TRUE)




  #fwrite(df_day, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_CO2_day_sd.csv", dateTimeAs = "write.csv")


  #week...

  rd_err_co2_final_week <- rd_err_co2_final %>%
    dplyr::mutate(year=lubridate::year(datetime),
                  week=lubridate::week(datetime)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))


  df_week <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
                  GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
                  Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco),
                  year = lubridate::year(datetime),
                  week = lubridate::week(datetime)) %>%
    dplyr::group_by(year,week,iteration) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
                     GPP_sum = sum(GPP_f),
                     Reco_sum = sum(Reco)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_sum),
                     GPP_sd = stats::sd(GPP_sum),
                     Reco_sd = stats::sd(Reco_sum)) %>%
    merge(rd_err_co2_final_week[,c("year","week","meas_err")], all=TRUE)


  #month...

  rd_err_co2_final_month <- rd_err_co2_final %>%
    dplyr::mutate(year=lubridate::year(datetime),
           month=lubridate::month(datetime)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))


  df_month <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
           GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
           Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco),
           year = lubridate::year(datetime),
           month = lubridate::month(datetime)) %>%
    dplyr::group_by(year,month,iteration) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
              GPP_sum = sum(GPP_f),
              Reco_sum = sum(Reco)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_sum),
              GPP_sd = stats::sd(GPP_sum),
              Reco_sd = stats::sd(Reco_sum)) %>%
    merge(rd_err_co2_final_month[,c("year","month","meas_err")], all=TRUE)

  #fwrite(df_month, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_CO2_month_sd.csv", dateTimeAs = "write.csv")


  #growing season
  rd_err_co2_final$year <- lubridate::year(rd_err_co2_final[,datetime])
  rd_err_co2_final$month <- lubridate::month(rd_err_co2_final[,datetime])
  rd_err_co2_final$growing_season <- NA

  df_mc$year <- lubridate::year(df_mc[[datetime]])
  df_mc$month <- lubridate::month(df_mc[[datetime]])
  df_mc$growing_season <- NA

  if(growing_season_definition == "fixed_months"){
    for(yr in unique(rd_err_co2_final$year)){
      rd_err_co2_final$growing_season[which(rd_err_co2_final$month %in% c(month_start_growing_season:month_end_growing_season) & rd_err_co2_final$year==yr)] <- paste("Growing season", yr, sep= " ")
      #rd_err_co2_final$growing_season[which(!(rd_err_co2_final$month %in% c(month_start_growing_season:month_end_growing_season)) & rd_err_co2_final$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      rd_err_co2_final$growing_season[which((rd_err_co2_final$month < month_start_growing_season) & rd_err_co2_final$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      rd_err_co2_final$growing_season[which((rd_err_co2_final$month > month_end_growing_season) & rd_err_co2_final$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }

    for(yr in unique(df_mc$year)){
      df_mc$growing_season[which(df_mc$month %in% c(month_start_growing_season:month_end_growing_season) & df_mc$year==yr)] <- paste("Growing season", yr, sep= " ")
      #df_mc$growing_season[which(!(df_mc$month %in% c(month_start_growing_season:month_end_growing_season)) & df_mc$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      df_mc$growing_season[which((df_mc$month < month_start_growing_season) & df_mc$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      df_mc$growing_season[which((df_mc$month > month_end_growing_season) & df_mc$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }

  }


  ######
  if(growing_season_definition %in% c("meteorological", 'soil_temp', 'soil_temp_mean')){

    for(yr in unique(rd_err_df$year)){
      gs_start <- gs_info[gs_info$year == yr, "start"]
      gs_end <- gs_info[gs_info$year == yr, "end"]
      rd_err_co2_final[which(rd_err_co2_final$date >= gs_start & rd_err_co2_final$date <= gs_end &  rd_err_co2_final$year==yr), "growing_season"] <- paste("Growing season", yr, sep= " ")
      #rd_err_df$growing_season[which(!(rd_err_df$month %in% c(month_start_growing_season:month_end_growing_season)) & rd_err_df$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      rd_err_co2_final[which(rd_err_co2_final$date < gs_start & rd_err_co2_final$year==yr),"growing_season"] <-  paste("Non growing season1", yr, sep= " ")
      rd_err_co2_final[which((rd_err_co2_final$date > gs_end) & rd_err_co2_final$year==yr), "growing_season"] <-  paste("Non growing season2", yr, sep= " ")
    }

    for(yr in unique(df_mc$year)){
      gs_start <- gs_info[gs_info$year == yr, "start"]
      gs_end <- gs_info[gs_info$year == yr, "end"]
      df_mc$growing_season[which(df_mc$date >= gs_start & df_mc$date <= gs_end & df_mc$year==yr)] <- paste("Growing season", yr, sep= " ")
      #df_mc$growing_season[which(!(df_mc$month %in% c(month_start_growing_season:month_end_growing_season)) & df_mc$year==yr)] <-  paste("Non growing season", yr, sep= " ")
      df_mc$growing_season[which(df_mc$date < gs_start & df_mc$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
      df_mc$growing_season[which(df_mc$date > gs_end & df_mc$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
    }
  }


  rd_err_co2_final_gs <- rd_err_co2_final %>%
    dplyr::group_by(year,growing_season) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_gs <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
                  GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
                  Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco)) %>%
    dplyr::group_by(year,growing_season,iteration) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
                     GPP_sum = sum(GPP_f),
                     Reco_sum = sum(Reco)) %>%
    dplyr::group_by(year,growing_season) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_sum),
                     GPP_sd = stats::sd(GPP_sum),
                     Reco_sd = stats::sd(Reco_sum)) %>%
    merge(rd_err_co2_final_gs[,c("year","growing_season","meas_err")], all=TRUE)



  #year...

  rd_err_co2_final_year <- rd_err_co2_final %>%
    dplyr::mutate(year=lubridate::year(datetime)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_year <- df_mc %>%
    dplyr::mutate(NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
           GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
           Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco),
           year = lubridate::year(datetime)) %>%
    dplyr::group_by(year,iteration) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
              GPP_sum = sum(GPP_f),
              Reco_sum = sum(Reco)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(NEE_sd = stats::sd(NEE_sum),
              GPP_sd = stats::sd(GPP_sum),
              Reco_sd = stats::sd(Reco_sum)) %>%
    merge(rd_err_co2_final_year[,c("year","meas_err")], all=TRUE)

  #fwrite(df_year, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_CO2_year_sd.csv", dateTimeAs = "write.csv")



  #################NOW LET'S AGGREGATE ALSO CO2 fluxes

  #reading the gapfilled file
  headers <- colnames(data.table::fread(path_to_df_f))
  #units <- scan(path_to_file, what = character(), skip=1, nlines = 1, sep="\t")
  flux_df_f <- data.table::fread(path_to_df_f, skip = 2, header = F)
  names(flux_df_f) <- headers

  flux_df_f <- as.data.frame(flux_df_f)
  flux_df_f[flux_df_f== -9999] <- NA

  flux_df_f$date <- as.Date(flux_df_f$DoY, origin = paste0(flux_df_f$Year-1,"-12-31"))
  flux_df_f$time <- hms::hms(hours=flux_df_f$Hour)

  flux_df_f[,datetime] <- as.POSIXct(paste(flux_df_f$date,flux_df_f$time), format="%Y-%m-%d %H:%M:%S",tz="UTC")


  flux_df_f <- flux_df_f %>%
    dplyr::mutate(NEE=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE),
           NEE_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(NEE_f),
           GPP_f=PostEddyPro::umolCO2m2s_to_gCm2_30min(GPP_f),
           Reco=PostEddyPro::umolCO2m2s_to_gCm2_30min(Reco))


  # #30min...
  # flux_df_f_30min <- flux_df_f %>%
  #   dplyr::select(c("datetime","NEE","NEE_f","GPP_f","Reco"))%>%
  #   merge(df_30min, by="datetime", all=TRUE)
  #
  # fwrite(flux_df_f_30min, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_30min.csv", dateTimeAs = "write.csv")



  flux_df_f_30min <- flux_df_f %>%
    dplyr::select(c(datetime,"NEE","NEE_f","GPP_f","Reco"))%>%
    merge(df_30min, by=datetime, all=TRUE)%>%
    dplyr::rename(gapf_err=NEE_sd) %>%
    dplyr::mutate(quality = ifelse(!is.na(NEE),
                            "original",
                            ifelse(!is.na(NEE_f),
                                   "gapfilled",
                                   NA)
    )) %>%
    dplyr::mutate(tot_err = ifelse(quality=="original",
                            meas_err,
                            ifelse(quality=="gapfilled",
                                   sqrt(meas_err^2+gapf_err^2),
                                   NA)
    )) %>%
    dplyr::select(c(datetime, "NEE", "NEE_f","quality","meas_err","gapf_err","tot_err","GPP_f","GPP_sd","Reco","Reco_sd"))

  #fwrite(flux_df_f_30min, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_CO2_30min_NEW.csv", dateTimeAs = "write.csv")



  #hour...
  flux_df_f_hour <- flux_df_f %>%
    dplyr::mutate(hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
                     GPP_sum = sum(GPP_f),
                     Reco_sum = sum(Reco))%>%
    merge(df_hour, by="hour", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+NEE_sd^2))

  #day...
  flux_df_f_day <- flux_df_f %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
              GPP_sum = sum(GPP_f),
              Reco_sum = sum(Reco))%>%
    merge(df_day, by="date", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+NEE_sd^2))

  #fwrite(flux_df_f_day, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_CO2_day_NEW.csv", dateTimeAs = "write.csv")

  plot(GPP_sum~date, data=flux_df_f_day, type="l")
  lines(flux_df_f_day$date,flux_df_f_day$GPP_sum-df_day$GPP_sd, col="red")
  lines(flux_df_f_day$date,flux_df_f_day$GPP_sum+df_day$GPP_sd, col="red")


  #week..
  flux_df_f_week <- flux_df_f %>%
    dplyr::mutate(year = lubridate::year(datetime),
                  week = lubridate::week(datetime)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
                     GPP_sum = sum(GPP_f),
                     Reco_sum = sum(Reco))%>%
    merge(df_week, by=c("year","week"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+NEE_sd^2)) %>%
    arrange(year,week)


    #month...
  flux_df_f_month <- flux_df_f %>%
    dplyr::mutate(year = lubridate::year(datetime),
           month = lubridate::month(datetime)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
              GPP_sum = sum(GPP_f),
              Reco_sum = sum(Reco))%>%
    merge(df_month, by=c("year","month"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+NEE_sd^2)) %>%
    arrange(year,month)

  #fwrite(flux_df_f_month, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_CO2_month_NEW.csv", dateTimeAs = "write.csv")

  flux_df_f_month$yearmon <- zoo::as.yearmon(paste(flux_df_f_month$year,flux_df_f_month$month,sep="-"))
  flux_df_f_month <- flux_df_f_month %>%
    arrange(yearmon)

  df_month$yearmon <- zoo::as.yearmon(paste(df_month$year,df_month$month,sep="-"))
  df_month <- df_month%>%
    arrange(yearmon)

  plot(NEE_sum~yearmon, data=flux_df_f_month, type="l")
  lines(flux_df_f_month$yearmon,flux_df_f_month$NEE_sum-df_month$NEE_sd, col="red")
  lines(flux_df_f_month$yearmon,flux_df_f_month$NEE_sum+df_month$NEE_sd, col="red")


  #Growing season
  flux_df_f$year <- lubridate::year(flux_df_f[,datetime])
  flux_df_f$month <- lubridate::month(flux_df_f[,datetime])
  flux_df_f$growing_season <- NA
  for(yr in unique(flux_df_f$year)){
    flux_df_f$growing_season[which(flux_df_f$month %in% c(month_start_growing_season:month_end_growing_season) & flux_df_f$year==yr)] <- paste("Growing season", yr, sep= " ")
    #flux_df_f$growing_season[which(!(flux_df_f$month %in% c(month_start_growing_season:month_end_growing_season)) & flux_df_f$year==yr)] <-  paste("Non growing season", yr, sep= " ")
    flux_df_f$growing_season[which((flux_df_f$month < month_start_growing_season) & flux_df_f$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
    flux_df_f$growing_season[which((flux_df_f$month > month_end_growing_season) & flux_df_f$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
  }

  flux_df_f_gs <- flux_df_f %>%
    dplyr::group_by(year, growing_season) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
                     GPP_sum = sum(GPP_f),
                     Reco_sum = sum(Reco))%>%
    merge(df_gs, by=c("year", "growing_season"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+NEE_sd^2))


  #year...
  flux_df_f_year <- flux_df_f %>%
    dplyr::mutate(year = lubridate::year(datetime)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(NEE_sum = sum(NEE_f),
              GPP_sum = sum(GPP_f),
              Reco_sum = sum(Reco))%>%
    merge(df_year, by=c("year"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+NEE_sd^2))

  #fwrite(flux_df_f_year, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_CO2_year_NEW.csv", dateTimeAs = "write.csv")

  aggr_list$flux_df_30min <- flux_df_f_30min
  aggr_list$flux_df_hour <- flux_df_f_hour
  aggr_list$flux_df_day <- flux_df_f_day
  aggr_list$flux_df_week <- flux_df_f_week
  aggr_list$flux_df_month <- flux_df_f_month
  aggr_list$flux_df_growing_season <- flux_df_f_gs
  aggr_list$flux_df_year <- flux_df_f_year
  if(growing_season_definition %in% c("meteorological", "soil_temp", "soil_temp_mean")) aggr_list$growing_season_info <- gs_info

  return(aggr_list)

}
