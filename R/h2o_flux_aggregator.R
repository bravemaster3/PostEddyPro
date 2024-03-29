
#' Aggregate h2o fluxes and uncertainties (measurement and gapfilling related) to 30min, daily, monthly and yearly
#'
#' @param path_to_df_f path to flux dataframe REddyProc-Gapfilled
#' @param df_mc dataframe of merged montecarlo simulation already gapfilled
#' @param rd_err_df dataframe containing random errors already imputed
#' @param month_start_growing_season Month of the start of growing season. Default is 5
#' @param month_end_growing_season Month of the end of growing season. Default is 10
#'
#' @return a list of half-hourly, hourly, daily, weekly, monthly and yearly aggregated flux dataframes
#' @export
h2o_flux_aggregator <- function(path_to_df_f,
                                df_mc,
                                rd_err_df,
                                month_start_growing_season=5,
                                month_end_growing_season=10){


  aggr_list <- list()

  ####################Aggregating random measurement error

  rd_err_h2o_final <- rd_err_df %>%
    dplyr::select(datetime, rand_err_h2o_flux) %>%
    dplyr::mutate(rand_err_h2o_flux=PostEddyPro::mmolH2Om2s_to_mm_30min(rand_err_h2o_flux)) %>%
    dplyr::rename(meas_err = rand_err_h2o_flux)

  plot(meas_err~datetime, data=rd_err_h2o_final)

  ###Now back to the gapfilling_errors


  #30 min...
  df_30min <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f)) %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_f)) %>%
    merge(rd_err_h2o_final[,c("datetime","meas_err")], all=TRUE)

  #fwrite(df_30min, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_h2o_30min_SD.csv", dateTimeAs = "write.csv")



  #hour...

  rd_err_h2o_final_hour <- rd_err_h2o_final %>%
    dplyr::mutate(hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))



  df_hour <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f),
                  hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour,iteration) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_sum)) %>%
    merge(rd_err_h2o_final_hour[,c("hour","meas_err")], all=TRUE)

  #day...

  rd_err_h2o_final_day <- rd_err_h2o_final %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))



  df_day <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f),
                  date=as.Date(datetime)) %>%
    dplyr::group_by(date,iteration) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_sum)) %>%
    merge(rd_err_h2o_final_day[,c("date","meas_err")], all=TRUE)




  #fwrite(df_day, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_h2o_day_sd.csv", dateTimeAs = "write.csv")


  #week...

  rd_err_h2o_final_week <- rd_err_h2o_final %>%
    dplyr::mutate(year=lubridate::year(datetime),
                  week=lubridate::week(datetime)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))


  df_week <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f),
                  year = lubridate::year(datetime),
                  week = lubridate::week(datetime)) %>%
    dplyr::group_by(year,week,iteration) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_sum)) %>%
    merge(rd_err_h2o_final_week[,c("year","week","meas_err")], all=TRUE)


  #month...

  rd_err_h2o_final_month <- rd_err_h2o_final %>%
    dplyr::mutate(year=lubridate::year(datetime),
                  month=lubridate::month(datetime)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))


  df_month <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f),
                  year = lubridate::year(datetime),
                  month = lubridate::month(datetime)) %>%
    dplyr::group_by(year,month,iteration) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_sum)) %>%
    merge(rd_err_h2o_final_month[,c("year","month","meas_err")], all=TRUE)

  #fwrite(df_month, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_h2o_month_sd.csv", dateTimeAs = "write.csv")


  #growing season
  rd_err_h2o_final$year <- lubridate::year(rd_err_h2o_final[,"datetime"])
  rd_err_h2o_final$month <- lubridate::month(rd_err_h2o_final[,"datetime"])
  rd_err_h2o_final$growing_season <- NA
  for(yr in unique(rd_err_h2o_final$year)){
    rd_err_h2o_final$growing_season[which(rd_err_h2o_final$month %in% c(month_start_growing_season:month_end_growing_season) & rd_err_h2o_final$year==yr)] <- paste("Growing season", yr, sep= " ")
    #rd_err_h2o_final$growing_season[which(!(rd_err_h2o_final$month %in% c(month_start_growing_season:month_end_growing_season)) & rd_err_h2o_final$year==yr)] <-  paste("Non growing season", yr, sep= " ")
    rd_err_h2o_final$growing_season[which((rd_err_h2o_final$month < month_start_growing_season) & rd_err_h2o_final$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
    rd_err_h2o_final$growing_season[which((rd_err_h2o_final$month > month_end_growing_season) & rd_err_h2o_final$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
  }

  #rd_err_h2o_final$growing_season <- as.factor(rd_err_h2o_final$growing_season)


  df_mc$year <- lubridate::year(df_mc[,"datetime"])
  df_mc$month <- lubridate::month(df_mc[,"datetime"])
  df_mc$growing_season <- NA
  for(yr in unique(df_mc$year)){
    df_mc$growing_season[which(df_mc$month %in% c(month_start_growing_season:month_end_growing_season) & df_mc$year==yr)] <- paste("Growing season", yr, sep= " ")
    #df_mc$growing_season[which(!(df_mc$month %in% c(month_start_growing_season:month_end_growing_season)) & df_mc$year==yr)] <-  paste("Non growing season", yr, sep= " ")
    df_mc$growing_season[which((df_mc$month < month_start_growing_season) & df_mc$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
    df_mc$growing_season[which((df_mc$month > month_end_growing_season) & df_mc$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
  }

  #df_mc$growing_season <- as.factor(df_mc$growing_season)

  rd_err_h2o_final_gs <- rd_err_h2o_final %>%
    dplyr::group_by(year,growing_season) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_gs <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f)) %>%
    dplyr::group_by(year,growing_season,iteration) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f)) %>%
    dplyr::group_by(year,growing_season) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_sum)) %>%
    merge(rd_err_h2o_final_gs[,c("year","growing_season","meas_err")], all=TRUE)



  #year...

  rd_err_h2o_final_year <- rd_err_h2o_final %>%
    dplyr::mutate(year=lubridate::year(datetime)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(meas_err=sqrt(sum((meas_err^2))))

  df_year <- df_mc %>%
    dplyr::mutate(H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f),
                  year = lubridate::year(datetime)) %>%
    dplyr::group_by(year,iteration) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(H2O_sd = stats::sd(H2O_sum)) %>%
    merge(rd_err_h2o_final_year[,c("year","meas_err")], all=TRUE)

  #fwrite(df_year, "C:/BRAVE/slu/eddy_covariance/myphd/R_codes/halsingfors_for_Marcus/new_flow/monte_carlo/final/monte_carlo_h2o_year_sd.csv", dateTimeAs = "write.csv")



  #################NOW LET'S AGGREGATE ALSO h2o fluxes

  #reading the gapfilled file
  headers <- colnames(data.table::fread(path_to_df_f))
  #units <- scan(path_to_file, what = character(), skip=1, nlines = 1, sep="\t")
  flux_df_f <- data.table::fread(path_to_df_f, skip = 2, header = F)
  names(flux_df_f) <- headers

  flux_df_f <- as.data.frame(flux_df_f)
  flux_df_f[flux_df_f== -9999] <- NA

  flux_df_f$date <- as.Date(flux_df_f$DoY, origin = paste0(flux_df_f$Year-1,"-12-31"))
  flux_df_f$time <- hms::hms(hours=flux_df_f$Hour)

  flux_df_f[,"datetime"] <- as.POSIXct(paste(flux_df_f$date,flux_df_f$time), format="%Y-%m-%d %H:%M:%S",tz="UTC")


  flux_df_f <- flux_df_f %>%
    dplyr::mutate(H2O=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O),
                  H2O_f=PostEddyPro::mmolH2Om2s_to_mm_30min(H2O_f))


  # #30min...
  # flux_df_f_30min <- flux_df_f %>%
  #   dplyr::select(c("datetime","H2O","H2O_f","GPP_f","Reco"))%>%
  #   merge(df_30min, by="datetime", all=TRUE)
  #
  # fwrite(flux_df_f_30min, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_30min.csv", dateTimeAs = "write.csv")



  flux_df_f_30min <- flux_df_f %>%
    dplyr::select(c("datetime","H2O","H2O_f"))%>%
    merge(df_30min, by="datetime", all=TRUE)%>%
    dplyr::rename(gapf_err=H2O_sd) %>%
    dplyr::mutate(quality = ifelse(!is.na(H2O),
                                   "original",
                                   ifelse(!is.na(H2O_f),
                                          "gapfilled",
                                          NA)
    )) %>%
    dplyr::mutate(tot_err = ifelse(quality=="original",
                                   meas_err,
                                   ifelse(quality=="gapfilled",
                                          sqrt(meas_err^2+gapf_err^2),
                                          NA)
    )) %>%
    dplyr::select(c(datetime, H2O, H2O_f,quality,meas_err,gapf_err,tot_err))

  #fwrite(flux_df_f_30min, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_h2o_30min_NEW.csv", dateTimeAs = "write.csv")



  #hour...
  flux_df_f_hour <- flux_df_f %>%
    dplyr::mutate(hour=as.POSIXct(round(datetime, units="hours"), tz="UTC")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f))%>%
    merge(df_hour, by="hour", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+H2O_sd^2))

  #day...
  flux_df_f_day <- flux_df_f %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f))%>%
    merge(df_day, by="date", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+H2O_sd^2))

  #fwrite(flux_df_f_day, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_h2o_day_NEW.csv", dateTimeAs = "write.csv")

  # plot(GPP_sum~date, data=flux_df_f_day, type="l")
  # lines(flux_df_f_day$date,flux_df_f_day$GPP_sum-df_day$GPP_sd, col="red")
  # lines(flux_df_f_day$date,flux_df_f_day$GPP_sum+df_day$GPP_sd, col="red")


  #week..
  flux_df_f_week <- flux_df_f %>%
    dplyr::mutate(year = lubridate::year(datetime),
                  week = lubridate::week(datetime)) %>%
    dplyr::group_by(year,week) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f))%>%
    merge(df_week, by=c("year","week"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+H2O_sd^2)) %>%
    arrange(year,week)


  #month...
  flux_df_f_month <- flux_df_f %>%
    dplyr::mutate(year = lubridate::year(datetime),
                  month = lubridate::month(datetime)) %>%
    dplyr::group_by(year,month) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f))%>%
    merge(df_month, by=c("year","month"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+H2O_sd^2)) %>%
    arrange(year,month)

  #fwrite(flux_df_f_month, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_h2o_month_NEW.csv", dateTimeAs = "write.csv")

  flux_df_f_month$yearmon <- zoo::as.yearmon(paste(flux_df_f_month$year,flux_df_f_month$month,sep="-"))
  flux_df_f_month <- flux_df_f_month %>%
    arrange(yearmon)

  df_month$yearmon <- zoo::as.yearmon(paste(df_month$year,df_month$month,sep="-"))
  df_month <- df_month%>%
    arrange(yearmon)

  plot(H2O_sum~yearmon, data=flux_df_f_month, type="l")
  lines(flux_df_f_month$yearmon,flux_df_f_month$H2O_sum-df_month$H2O_sd, col="red")
  lines(flux_df_f_month$yearmon,flux_df_f_month$H2O_sum+df_month$H2O_sd, col="red")


  #Growing season
  flux_df_f$year <- lubridate::year(flux_df_f[,"datetime"])
  flux_df_f$month <- lubridate::month(flux_df_f[,"datetime"])
  flux_df_f$growing_season <- NA
  for(yr in unique(flux_df_f$year)){
    flux_df_f$growing_season[which(flux_df_f$month %in% c(month_start_growing_season:month_end_growing_season) & flux_df_f$year==yr)] <- paste("Growing season", yr, sep= " ")
    #flux_df_f$growing_season[which(!(flux_df_f$month %in% c(month_start_growing_season:month_end_growing_season)) & flux_df_f$year==yr)] <-  paste("Non growing season", yr, sep= " ")
    flux_df_f$growing_season[which((flux_df_f$month < month_start_growing_season) & flux_df_f$year==yr)] <-  paste("Non growing season1", yr, sep= " ")
    flux_df_f$growing_season[which((flux_df_f$month > month_end_growing_season) & flux_df_f$year==yr)] <-  paste("Non growing season2", yr, sep= " ")
  }

  #flux_df_f$growing_season <- as.factor(flux_df_f$growing_season)

  flux_df_f_gs <- flux_df_f %>%
    dplyr::group_by(year, growing_season) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f))%>%
    merge(df_gs, by=c("year","growing_season"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+H2O_sd^2))


  #year...
  flux_df_f_year <- flux_df_f %>%
    dplyr::mutate(year = lubridate::year(datetime)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(H2O_sum = sum(H2O_f))%>%
    merge(df_year, by=c("year"), all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+H2O_sd^2))

  #fwrite(flux_df_f_year, "C:/BRAVE/slu/eddy_covariance/myphd/Data_organized/flux_df_f/Eddypro_output/flux_df_f_ReddyProc_GF_h2o_year_NEW.csv", dateTimeAs = "write.csv")

  aggr_list$flux_df_30min <- flux_df_f_30min
  aggr_list$flux_df_hour <- flux_df_f_hour
  aggr_list$flux_df_day <- flux_df_f_day
  aggr_list$flux_df_week <- flux_df_f_week
  aggr_list$flux_df_month <- flux_df_f_month
  aggr_list$flux_df_growing_season <- flux_df_f_gs
  aggr_list$flux_df_year <- flux_df_f_year

  return(aggr_list)

}
