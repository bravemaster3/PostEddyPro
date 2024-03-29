

#' Filters for low residual signal strength indicator aka RSSI
#'
#' @param fluxes_meteo dataframe containing both fluxes and soil/air temperature data
#' @param gas gas name as denoted in Eddypro (e.g. "ch4" or "co2",includes also non gas like LE)
#' @param RSSI_col name of the column containing RSSI values
#'
#' @return a list containing the RSSI threshold decided by the user based on the graph (the visual inflection point), and a vector of flags (1 means low RSSI, and 0 means good RSSI) the same length as the number of rows of the input data.frame
#' @export
rssi_filter <- function(fluxes_meteo,
                        gas,
                        RSSI_col
){

  names(fluxes_meteo)[which(names(fluxes_meteo)==RSSI_col)] <- "RSSI"
  # plot(ch4_mixing_ratio~RSSI, data = fluxes_meteo, ylim=c(1, 4))
  # plot(rand_err_ch4_flux~RSSI, data = fluxes_meteo, ylim=c(0, 0.2))
  plot(fluxes_meteo[,paste0(gas,"_mixing_ratio")] ~ fluxes_meteo[,"RSSI"])
  plot(fluxes_meteo[,paste0("rand_err_", gas,"_flux")] ~ fluxes_meteo[,"RSSI"])


  breaks <- seq(0,85, by=5)
  labels <- seq(5,85, by=5)
  fluxes_meteo$RSSI_classes <- cut(fluxes_meteo$RSSI, breaks=breaks, labels=labels)
  plot(fluxes_meteo$RSSI_classes[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")]))], las=2, xlab="RSSI Classes (%)", ylab="frequency")
  graphics::boxplot(fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")])),paste0(gas,"_mixing_ratio")]~fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")])),"RSSI_classes"],
                    # las=2, xlab="RSSI Classes (%)", ylab="CH4 mixing ratio", ylim=c(1,4))
                    las=2, xlab="RSSI Classes (%)", ylab=paste(toupper(gas), "mixing ratio"))

  mean_gas <- stats::aggregate(fluxes_meteo[,paste0(gas,"_mixing_ratio")], list(fluxes_meteo$RSSI_classes), FUN = function(x) mean(x, na.rm = TRUE))
  median_gas <- stats::aggregate(fluxes_meteo[,paste0(gas,"_mixing_ratio")], list(fluxes_meteo$RSSI_classes), FUN = function(x) stats::median(x, na.rm = TRUE))
  sd_gas <- stats::aggregate(fluxes_meteo[,paste0(gas,"_mixing_ratio")], list(fluxes_meteo$RSSI_classes), FUN = function(x) stats::sd(x, na.rm = TRUE))

  names(mean_gas) <- c("RSSI_classes", paste0("mean_",gas))
  names(median_gas) <- c("RSSI_classes", paste0("median_", gas))
  names(sd_gas) <- c("RSSI_classes", paste0("sd_",gas))


  merge_temp <- merge(mean_gas,median_gas, by="RSSI_classes")
  gas_RSSI <- merge(merge_temp,sd_gas, by="RSSI_classes")

  plot(gas_RSSI[,paste0("mean_",gas)]~gas_RSSI[,"RSSI_classes"])
  plot(gas_RSSI[,paste0("median_",gas)]~gas_RSSI[,"RSSI_classes"])

  ##
  # plot(mean_ch4~RSSI_classes, data=ch4_RSSI)
  # plot(median_ch4~RSSI_classes, data=ch4_RSSI)

  # plot(gas_RSSI[,paste0("median_",gas)]~gas_RSSI[,"RSSI_classes"], ylim=c(1.5, 2.5))
  plot(gas_RSSI[,paste0("median_",gas)]~gas_RSSI[,"RSSI_classes"])
  graphics::arrows(as.numeric(gas_RSSI[,"RSSI_classes"]), gas_RSSI[,paste0("median_",gas)]-gas_RSSI[,paste0("sd_",gas)],
                   as.numeric(gas_RSSI[,"RSSI_classes"]), gas_RSSI[,paste0("median_",gas)]+gas_RSSI[,paste0("sd_",gas)], length=0.05, angle=90, code=3)


  # plot(gas_RSSI[,paste0("mean_",gas)]~gas_RSSI[,"RSSI_classes"], ylim=c(1.5, 2.5))
  plot(gas_RSSI[,paste0("mean_",gas)]~gas_RSSI[,"RSSI_classes"])
  graphics::arrows(as.numeric(gas_RSSI[,"RSSI_classes"]), gas_RSSI[,paste0("mean_",gas)]-gas_RSSI[,paste0("sd_",gas)],
                   as.numeric(gas_RSSI[,"RSSI_classes"]), gas_RSSI[,paste0("mean_",gas)]+gas_RSSI[,paste0("sd_",gas)], length=0.05, angle=90, code=3)

  plot(fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")])), paste0(gas,"_mixing_ratio")] ~ fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")])), "RSSI"],
       # xlab="RSSI(%)", ylab=paste(toupper(gas), "mixing ratio"), ylim=c(1,4))
       xlab="RSSI(%)", ylab=paste(toupper(gas), "mixing ratio"))

  graphics::boxplot(fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")])),paste0(gas,"_mixing_ratio")] ~ fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_mixing_ratio")])),"RSSI_classes"],
     #las=2, xlab="RSSI Classes (%)", ylab=paste(toupper(gas), "mixing ratio"), ylim=c(1,4))
     las=2, xlab="RSSI Classes (%)", ylab=paste(toupper(gas), "mixing ratio"))

  ##it looks from the previous graphs that Discarding data with RSSI less than 10% if not 5% is good.

  RSSI_threshold  <- readline(prompt = "From the graphs, enter the estimated RSSI value: ") #15
  #fluxes_meteo$ch4_flux_final[which(fluxes_meteo$RSSI<=RSSI_threshold)] <- NA
  fluxes_meteo$flag_rssi <- NA

  fluxes_meteo$flag_rssi <- ifelse(fluxes_meteo$RSSI <= RSSI_threshold, 1, 0)

  return(list(rssi_thresh=RSSI_threshold, flag_rssi=fluxes_meteo$flag_rssi))
}
