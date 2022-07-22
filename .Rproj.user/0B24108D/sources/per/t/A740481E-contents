#' Title
#'
#' @param fluxes_meteo dataframe containing fluxes and biomet data
#' @param mauden_flag vector, default c(1,2) for qc values to flag out. Recommended practice is to keep 0 as good data, and flag out 2 only if dealing with annual budgets, and flag both 1 and 2 if looking at high frequency data
#' @param hard_flags boolean telling whether or not to apply a hard flag
#' @param limit_lower lower limit for the hard flags after Holl et al. 2020
#' @param limit_upper upper limit for the hard flags after Holl et al. 2020
#' @param RSSI boolean telling whether or not to do a RSSI filtering. Default is TRUE
#' @param RSSI_col name of the column that contains RSSI values.
#' @param ustar boolean telling whether or not to do u* filtering. Default is TRUE
#' @param Temp_col name of the temperature column to be used for u_star threshold calculation
#' @param datetime_start starting datetime as a posixct object (format \%Y-\%m-\%d \%H:\%M:\%S") for creating a complete dataset to ensure no missing values exist in the final dataset. If not provided (default NULL), the first/minimum datetime will be used.
#' @param datetime_end Ending datetime as a posixct object (format \%Y-\%m-\%d \%H:\%M:\%S") for creating a complete dataset to ensure no missing values exist in the final dataset. If not provided (default NULL), the last/maximum datetime will be used.
#'
#' @return a dataframe having some additional columns, with the flux column suffixed by "_final" being the quality checked flux values
#' @export
quality_control <- function(fluxes_meteo,
                            mauden_flag=c(1,2),
                            hard_flags=TRUE,
                            limit_lower = -0.1, #umol.m-2.s-1, Holl et al. 2020
                            limit_upper = 1, #umol.m-2.s-1, normally 1 in Holl et al. 2020
                            # threshold_ch4_uncertainty = 0.4, #umol.m-2.s-1, Holl et al. 2020
                            # standard_deviation_time = 15
                            RSSI=FALSE,
                            RSSI_col="diag_77_mean",
                            ustar=TRUE,
                            Temp_col="Ts",
                            datetime_start=NULL,
                            datetime_end=NULL
){

  if(is.null(datetime_start)) datetime_start <- min(fluxes_meteo$datetime)
  if(is.null(datetime_end)) datetime_end <- max(fluxes_meteo$datetime)
  #making a copy of fluxes
  fluxes_meteo$ch4_flux_ori <- fluxes_meteo$ch4_flux
  fluxes_meteo$ch4_strg_ori <- fluxes_meteo$ch4_strg


  #Adding the storage term to the ch4_flux
  fluxes_meteo$ch4_flux_final <- fluxes_meteo$ch4_flux+fluxes_meteo$ch4_strg

  #original gaps
  original_datacoverage <- 100 * length(fluxes_meteo$ch4_flux_final[which(!is.na(fluxes_meteo$ch4_flux_final))])/length(fluxes_meteo$ch4_flux_final)

  #Filtering with Mauden and Foken flags
  fluxes_meteo$ch4_flux_final[which(fluxes_meteo$qc_ch4_flux %in% mauden_flag)] <- NA
  fluxes_meteo$ch4_mixing_ratio[which(fluxes_meteo$qc_ch4_flux %in% mauden_flag)] <- NA

  if(hard_flags==TRUE){
    fluxes_meteo$ch4_flux_final[which(fluxes_meteo$ch4_flux_final < limit_lower | fluxes_meteo$ch4_flux_final > limit_upper)] <- NA
  }

  if(RSSI==TRUE){
    list_rssi<- PostEddyPro::rssi_filter(fluxes_meteo, RSSI_col=RSSI_col)
    print(paste0("the RSSI threshold is: ", list_rssi$rssi_thresh))
    fluxes_meteo$flag_rssi <- list_rssi$flag_rssi
    fluxes_meteo$ch4_flux_final[which(fluxes_meteo$flag_rssi==1)] <- NA
  }

  if(ustar==TRUE){
    list_ustar <- PostEddyPro::ustar_filter(fluxes_meteo, Temp_col = Temp_col)
    print(paste0("the ustar threshold is: ", list_ustar$ustar_thresh))
    fluxes_meteo$flag_ustar <- list_ustar$flag_ustar
    fluxes_meteo$ch4_flux_final[which(fluxes_meteo$flag_ustar==1)] <- NA
  }

  plot(ch4_flux_final~datetime, data=fluxes_meteo)

  #Percentage of gaps remaining
  remaining_good_datacoverage <- 100*length(fluxes_meteo$ch4_flux_final[which(!is.na(fluxes_meteo$ch4_flux_final))])/length(fluxes_meteo$ch4_flux_final)

  #Printing to the console all the indicators of quality control (gaps at start and remaining...)
  median <- median(fluxes_meteo$ch4_flux_final, na.rm = T)
  mean <- mean(fluxes_meteo$ch4_flux_final, na.rm = T)
  stdev <- stats::sd(fluxes_meteo$ch4_flux_final, na.rm = T)
  paste0("u* threshold: ", list_ustar$ustar_thresh)
  paste0("median: ", median)
  paste0("mean: ", mean)
  paste0("stdev: ", stdev)
  paste0("original_datacoverage: ", round(original_datacoverage,2), "%")
  paste0("remaining_good_datacoverage: ", round(remaining_good_datacoverage,2), "%")


  #creating a complete dataset with no missing timestamp before exporting
  complete_datetime <- seq(from=as.POSIXct(datetime_start,tz = "UTC"),to=as.POSIXct(datetime_end,tz = "UTC"),by="30 min")

  fluxes_meteo_final <- merge(data.frame(datetime=complete_datetime), fluxes_meteo, by="datetime", all = TRUE)

  return(fluxes_meteo_final)
}
