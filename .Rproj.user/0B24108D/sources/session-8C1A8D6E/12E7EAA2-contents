
#' Aggregate methane fluxes and uncertainties (measurement and gapfilling related) to 30min, daily, monthly and yearly
#'
#' @param flux_df flux dataframe output from the random forest gapfilling
#' @param df_mc dataframe of merged montecarlo simulation already gapfilled
#' @param rd_err_df dataframe containing random errors already imputed
#'
#' @return a list of half-hourly, daily, monthly and yearly aggregated flux dataframes
#' @export
ch4_flux_aggregator <- function(flux_df,
                                df_mc,
                                rd_err_df){

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

  #daily

  flux_df_day <- flux_df %>%
    dplyr::rename(FCH4=ch4_flux_final_filled) %>%
    dplyr::mutate(date=as.Date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(FCH4_sum = sum(FCH4))%>%
    merge(df_day, by="date", all=TRUE) %>%
    dplyr::mutate(tot_err = sqrt(meas_err^2+FCH4_sd^2))

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
  aggr_list$flux_df_day <- flux_df_day
  aggr_list$flux_df_month <- flux_df_month
  aggr_list$flux_df_year <- flux_df_year

  return(aggr_list)

}
