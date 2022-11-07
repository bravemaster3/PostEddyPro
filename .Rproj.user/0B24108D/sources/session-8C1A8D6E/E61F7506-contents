

#' Filters for low residual signal strength indicator aka RSSI
#'
#' @param fluxes_meteo dataframe containing both fluxes and soil/air temperature data
#' @param RSSI_col name of the column containing RSSI values
#'
#' @return a list containing the RSSI threshold decided by the user based on the graph (the visual inflection point), and a vector of flags (1 means low RSSI, and 0 means good RSSI) the same length as the number of rows of the input data.frame
#' @export
rssi_filter <- function(fluxes_meteo,
                        RSSI_col
){
  names(fluxes_meteo)[which(names(fluxes_meteo)==RSSI_col)] <- "RSSI"
  plot(ch4_mixing_ratio~RSSI, data = fluxes_meteo, ylim=c(1, 4))
  plot(rand_err_ch4_flux~RSSI, data = fluxes_meteo, ylim=c(0, 0.2))


  breaks <- seq(0,85, by=5)
  labels <- seq(5,85, by=5)
  fluxes_meteo$RSSI_classes <- cut(fluxes_meteo$RSSI, breaks=breaks, labels=labels)
  plot(fluxes_meteo$RSSI_classes[which(!is.na(fluxes_meteo$ch4_mixing_ratio))], las=2, xlab="RSSI Classes (%)", ylab="frequency")
  graphics::boxplot(ch4_mixing_ratio~RSSI_classes, data = fluxes_meteo[which(!is.na(fluxes_meteo$ch4_mixing_ratio)),], las=2, xlab="RSSI Classes (%)", ylab="CH4 mixing ratio", ylim=c(1,4))

  mean_ch4 <- stats::aggregate(fluxes_meteo[,"ch4_mixing_ratio"], list(fluxes_meteo$RSSI_classes), FUN = function(x) mean(x, na.rm = TRUE))
  median_ch4 <- stats::aggregate(fluxes_meteo[,"ch4_mixing_ratio"], list(fluxes_meteo$RSSI_classes), FUN = function(x) stats::median(x, na.rm = TRUE))
  sd_ch4 <- stats::aggregate(fluxes_meteo[,"ch4_mixing_ratio"], list(fluxes_meteo$RSSI_classes), FUN = function(x) stats::sd(x, na.rm = TRUE))

  names(mean_ch4) <- c("RSSI_classes", "mean_ch4")
  names(median_ch4) <- c("RSSI_classes", "median_ch4")
  names(sd_ch4) <- c("RSSI_classes", "sd_ch4")


  merge_temp <- merge(mean_ch4,median_ch4, by="RSSI_classes")
  ch4_RSSI <- merge(merge_temp,sd_ch4, by="RSSI_classes")

  plot(mean_ch4~RSSI_classes, data=ch4_RSSI)
  plot(median_ch4~RSSI_classes, data=ch4_RSSI)

  ##
  plot(mean_ch4~RSSI_classes, data=ch4_RSSI)
  plot(median_ch4~RSSI_classes, data=ch4_RSSI)

  plot(ch4_RSSI$RSSI_classes,ch4_RSSI$median_ch4, ylim=c(1.5, 2.5))
  graphics::arrows(as.numeric(ch4_RSSI$RSSI_classes), ch4_RSSI$median_ch4-ch4_RSSI$sd_ch4, as.numeric(ch4_RSSI$RSSI_classes), ch4_RSSI$median_ch4+ch4_RSSI$sd_ch4, length=0.05, angle=90, code=3)

  plot(ch4_RSSI$RSSI_classes,ch4_RSSI$mean_ch4, ylim=c(1.5, 2.5))
  graphics::arrows(as.numeric(ch4_RSSI$RSSI_classes), ch4_RSSI$mean_ch4-ch4_RSSI$sd_ch4, as.numeric(ch4_RSSI$RSSI_classes), ch4_RSSI$mean_ch4+ch4_RSSI$sd_ch4, length=0.05, angle=90, code=3)

  plot(ch4_mixing_ratio~RSSI, data = fluxes_meteo[which(!is.na(fluxes_meteo$ch4_mixing_ratio)),],xlab="RSSI(%)", ylab="CH4 mixing ratio", ylim=c(1,4))
  graphics::boxplot(ch4_mixing_ratio~RSSI_classes, data = fluxes_meteo[which(!is.na(fluxes_meteo$ch4_mixing_ratio)),], las=2, xlab="RSSI Classes (%)", ylab="CH4 mixing ratio", ylim=c(1,4))

  ##it looks from the previous graphs that Discarding data with RSSI less than 10% if not 5% is good.

  RSSI_threshold  <- readline(prompt = "From the graphs, enter the estimated RSSI value: ") #15
  #fluxes_meteo$ch4_flux_final[which(fluxes_meteo$RSSI<=RSSI_threshold)] <- NA
  fluxes_meteo$flag_rssi <- NA

  fluxes_meteo$flag_rssi <- ifelse(fluxes_meteo$RSSI <= RSSI_threshold, 1, 0)

  return(list(rssi_thresh=RSSI_threshold, flag_rssi=fluxes_meteo$flag_rssi))
}
