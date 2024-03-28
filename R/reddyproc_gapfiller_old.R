#' A wrapper for ReddyProc gapfiller for gapfilling NEE or H2O fluxes,and partitionning NEE. This old form has a problem. Due to using sMRFluxPartition instead of sMRFluxPartitionUStarScens, Reco was not as variable as it should have been.
#'
#' @param formatted_file_path path to the file ready for ReddyProc gapfilling. Typically the output from the "formatting_fluxes_REddyProc" function of the PostEddyPro package
#' @param saving_folder path to the folder where the gapfilled fluxes will be saved
#' @param file_name fine name with extension (only base name with extension, e.g. fluxes.txt)
#' @param FLUX flux column in the final output: NEE for CO2 flux OR H2O for water flux
#' @param longitude longitude of the site
#' @param latitude latitude of the site
#' @param timezone timezone as a signed integer (e.g. 1 for UTC+1)
#' @param gapfill_flux boolean. Default is TRUE. Set to false if gapfilling of NEE has been performed before and do only partitioning.
#'
#' @return No value returned, but a file saved to disk
#' @export
reddyproc_gapfiller_old <- function(formatted_file_path,
                                saving_folder,
                                file_name=NULL,
                                FLUX=c("NEE","H2O"), #either of them
                                longitude=19.556646,
                                latitude=64.181980,
                                timezone=1,
                                gapfill_flux=TRUE){
  #i=1 #turn on for testing
  EddyData.F <- REddyProc::fLoadTXTIntoDataframe(formatted_file_path)

  #+++ If not provided, calculate VPD from Tair and rH
  # EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))

  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- REddyProc::fConvertTimeToPosix(EddyData.F, 'YDH', Year='Year', Day='DoY', Hour='Hour')

  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later
  EddyProc.C <- REddyProc::sEddyProc$new('EQTP', EddyDataWithPosix.F, c(FLUX,'Rg','Tair','VPD')) #change 'NEE' to other variable names if gapfilling other fluxes (have to match the variable name in the input data file)
  EddyProc.C$sSetLocationInfo(LatDeg=latitude, LongDeg=longitude, TimeZoneHour=timezone)  #Location of Halsingfors

  #+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
  EddyProc.C$sMDSGapFill('Tair', FillAll=FALSE)  	# Gap-filled Tair needed for partitioning and gapfilling
  EddyProc.C$sMDSGapFill('VPD', FillAll=FALSE)  	# Gap-filled VPD needed for gapfilling
  EddyProc.C$sMDSGapFill('Rg', FillAll=FALSE)     #Fill only the gaps for the meteo condition, e.g. 'Rg'
  EddyProc.C$sMDSGapFill(FLUX, FillAll=gapfill_flux)     #Fill all values to estimate flux uncertainties

  #+++ Partition NEE into GPP and respiration
  if(FLUX == "NEE") EddyProc.C$sMRFluxPartition()	# night time partitioning -> Reco, GPP

  #+++ Export gap filled and partitioned data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportResults()

  #+++ Save results into (tab-delimited) text file in directory
  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)

  #+++ May rename variables to correspond to Ameriflux
  colnames(CombinedDataAmeriflux.F <- REddyProc::renameVariablesInDataframe(CombinedData.F, REddyProc::getBGC05ToAmerifluxVariableNameMapping() ))
  CombinedDataAmeriflux.F$TIMESTAMP_END <- REddyProc::POSIXctToBerkeleyJulianDate( EddyProc.C$sExportData()[[1]] )
  utils::head(tmp <- REddyProc::BerkeleyJulianDateToPOSIXct( CombinedDataAmeriflux.F$TIMESTAMP_END ))
  colnames(tmp <- REddyProc::renameVariablesInDataframe(CombinedData.F, REddyProc::getAmerifluxToBGC05VariableNameMapping() ))
  #opf <- paste0("EQTP_output_sim",toString(i),".txt") #update the output file name


  if(is.null(file_name)) file_name <- paste0("For_ReddyProc_", FLUX, "_GF.txt")
  REddyProc::fWriteDataframeToFile(CombinedData.F, file_name, saving_folder) #update the output file path

  # #+++ Example plots of filled data to screen or to directory /plots
  #EddyProc.C$sPlotFingerprintY('NEE_f', Year=2021) #gapfilled NEE fingerprint, update the year accordingly

}
