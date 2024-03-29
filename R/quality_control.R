#' Perform quality check and quality control (supported are Mauden and Foken flags filtering, RSSI and U star filtering)
#'
#' @param fluxes_meteo dataframe containing fluxes and biomet data
#' @param gas gas name as denoted in Eddypro (e.g. "ch4" or "co2",includes also non gas like LE)
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
#' @param ustar_SetThresh A given threshold for ustar filtering if that is known (e.g. 0.1). The unit is m/s
#'
#' @return a dataframe having some additional columns, with the flux column suffixed by "_final" being the quality checked flux values
#' @export
quality_control <- function(fluxes_meteo,
                            gas="ch4",
                            mauden_flag=c(1,2),
                            hard_flags=TRUE,
                            limit_lower = -0.1, #umol.m-2.s-1, Holl et al. 2020
                            limit_upper = 1, #umol.m-2.s-1, normally 1 in Holl et al. 2020
                            # threshold_ch4_uncertainty = 0.4, #umol.m-2.s-1, Holl et al. 2020
                            # standard_deviation_time = 15
                            RSSI=FALSE,
                            RSSI_col="diag_77_mean",
                            ustar=TRUE,
                            ustar_SetThresh=NULL,
                            Temp_col="Ts",
                            datetime_start=NULL,
                            datetime_end=NULL
){

  flux_col <- paste0(gas,"_flux")
  if(is.null(datetime_start)) datetime_start <- min(fluxes_meteo$datetime)
  if(is.null(datetime_end)) datetime_end <- max(fluxes_meteo$datetime)
  #making a copy of fluxes
  fluxes_meteo[,paste0(flux_col,"_ori")] <- fluxes_meteo[,flux_col]
  fluxes_meteo[,paste0(gas,"_strg_ori")] <- fluxes_meteo[,paste0(gas, "_strg")]


  #Adding the storage term to the ch4_flux
  fluxes_meteo[,paste0(flux_col,"_final")] <- fluxes_meteo[,flux_col]+fluxes_meteo[,paste0(gas, "_strg")]

  #original gaps
  original_datacoverage <- 100 * length(fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(flux_col,"_final")])),paste0(flux_col,"_final")])/length(fluxes_meteo[,paste0(flux_col,"_final")])

  #Filtering with Mauden and Foken flags
  fluxes_meteo[which(fluxes_meteo[,paste0("qc_", flux_col)] %in% mauden_flag), paste0(flux_col,"_final")] <- NA
  fluxes_meteo[which(fluxes_meteo[,paste0("qc_", flux_col)] %in% mauden_flag), paste0(gas,"_mixing_ratio")] <- NA

  if(hard_flags==TRUE){
    fluxes_meteo[which(fluxes_meteo[,paste0(flux_col,"_final")] < limit_lower | fluxes_meteo[,paste0(flux_col,"_final")] > limit_upper), paste0(flux_col,"_final")] <- NA
  }

  if(RSSI==TRUE){
    list_rssi<- PostEddyPro::rssi_filter(fluxes_meteo, gas=gas, RSSI_col=RSSI_col)
    print(paste0("the RSSI threshold is: ", list_rssi$rssi_thresh))
    fluxes_meteo$flag_rssi <- list_rssi$flag_rssi
    fluxes_meteo[which(fluxes_meteo$flag_rssi==1),paste0(flux_col,"_final")] <- NA
  }

  if(ustar==TRUE){
    list_ustar <- PostEddyPro::ustar_filter(fluxes_meteo, gas=gas, Temp_col = Temp_col, ustarSetThresh = ustar_SetThresh)
    print(paste0("the ustar threshold is: ", list_ustar$ustar_thresh))
    fluxes_meteo$flag_ustar <- list_ustar$flag_ustar
    fluxes_meteo$ch4_flux_final[fluxes_meteo$flag_ustar==1] <- NA
  }

  plot(fluxes_meteo[,paste0(flux_col,"_final")]~fluxes_meteo[,"datetime"])

  #Percentage of gaps remaining
  remaining_good_datacoverage <- 100 * length(fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(flux_col,"_final")])),paste0(flux_col,"_final")])/length(fluxes_meteo[,paste0(flux_col,"_final")])


  #Printing to the console all the indicators of quality control (gaps at start and remaining...)
  median <- median(fluxes_meteo[,paste0(flux_col,"_final")], na.rm = T)
  mean <- mean(fluxes_meteo[,paste0(flux_col,"_final")], na.rm = T)
  stdev <- stats::sd(fluxes_meteo[,paste0(flux_col,"_final")], na.rm = T)
  print(paste0("u* threshold: ", list_ustar$ustar_thresh))
  print(paste0("median: ", median))
  print(paste0("mean: ", mean))
  print(paste0("stdev: ", stdev))
  print(paste0("original_datacoverage: ", round(original_datacoverage,2), "%"))
  print(paste0("remaining_good_datacoverage: ", round(remaining_good_datacoverage,2), "%"))


  #creating a complete dataset with no missing timestamp before exporting
  complete_datetime <- seq(from=as.POSIXct(datetime_start,tz = "UTC"),to=as.POSIXct(datetime_end,tz = "UTC"),by="30 min")

  fluxes_meteo_final <- merge(data.frame(datetime=complete_datetime), fluxes_meteo, by="datetime", all = TRUE)

  return(fluxes_meteo_final)
}
