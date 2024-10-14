#' A wrapper for ReddyProc gapfiller for gapfilling NEE or H2O fluxes,and partitionning NEE. Each year is gapfilled/partitioned separately and merged
#'
#' @param formatted_file_path path to the file ready for ReddyProc gapfilling. Typically the output from the "formatting_fluxes_REddyProc" function of the PostEddyPro package
#' @param saving_folder path to the folder where the gapfilled fluxes will be saved
#' @param file_name fine name with extension (only base name with extension, e.g. fluxes.txt)
#' @param FLUX flux column in the final output: NEE for CO2 flux OR H2O for water flux
#' @param longitude longitude of the site
#' @param latitude latitude of the site
#' @param timezone timezone as a signed integer (e.g. 1 for UTC+1)
#' @param gapfill_flux boolean. Default is TRUE. Set to false if gapfilling of NEE has been performed before and do only partitioning. Keep to true even if gapfilling was performed before if you want to get a gapfilling uncertainty estimate as NEE_fsd
#' @param filter_ustar boolean. Default is FALSE. Set to true if you want to perform ustar filtering and then gapfill the new gaps created by uStar filtering using MDS. Note that in that case NEE_uStar_f instead of NEE_f will equal Reco-GPP_f. Reco and GPP_f are actually renamed in this function from Reco_uStar and GPP_uStar_f
#'
#' @return No value returned, but a file saved to disk
#' @export
reddyproc_gapfiller <- function(formatted_file_path,
                                saving_folder,
                                file_name=NULL,
                                FLUX=c("NEE","H2O"), #either of them
                                longitude=19.556646,
                                latitude=64.181980,
                                timezone=1,
                                gapfill_flux=TRUE,
                                filter_ustar=FALSE){
  #i=1 #turn on for testing
  all_df <- REddyProc::fLoadTXTIntoDataframe(formatted_file_path)

  year_index = 0
  for (year in unique(all_df$Year)){
    message(paste("Now gapfilling/partitionning Year:", year))
    year_index = year_index+1
    EddyData.F <- all_df %>%
      dplyr::filter(Year == year)

    tryCatch({

      #+++ If not provided, calculate VPD from Tair and rH
      # EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))

      #+++ Add time stamp in POSIX time format
      EddyDataWithPosix.F <- REddyProc::fConvertTimeToPosix(EddyData.F, 'YDH', Year='Year', Day='DoY', Hour='Hour')

      #+++ Initalize R5 reference class sEddyProc for processing of eddy data
      #+++ with all variables needed for processing later
      EddyProc.C <- REddyProc::sEddyProc$new('EQTP', EddyDataWithPosix.F, c(FLUX,'Rg','Tair','VPD', 'Ustar')) #change 'NEE' to other variable names if gapfilling other fluxes (have to match the variable name in the input data file)
      EddyProc.C$sSetLocationInfo(LatDeg=latitude, LongDeg=longitude, TimeZoneHour=timezone)  #Location of Halsingfors

      #+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
      EddyProc.C$sMDSGapFill('Tair', FillAll=FALSE)  	# Gap-filled Tair needed for partitioning and gapfilling
      EddyProc.C$sMDSGapFill('VPD', FillAll=FALSE)  	# Gap-filled VPD needed for gapfilling
      EddyProc.C$sMDSGapFill('Rg', FillAll=FALSE)     #Fill only the gaps for the meteo condition, e.g. 'Rg'
      EddyProc.C$sMDSGapFill(FLUX, FillAll=gapfill_flux)     #Fill all values to estimate flux uncertainties, and creates NEE_f
      #USTAR THRESHOLD

      if(isTRUE(filter_ustar)){
        EddyProc.C$sEstimateUstarScenarios(
          nSample = 100L, probs = c(0.05, 0.5, 0.95))
        # message("ARRIVED HERE")
        EddyProc.C$sGetEstimatedUstarThresholdDistribution()

        EddyProc.C$sMDSGapFillUStarScens(FLUX) # Creates NEE_uStar_f
        if(FLUX == "NEE") EddyProc.C$sMRFluxPartitionUStarScens()
      } else {
        if(FLUX == "NEE") EddyProc.C$sMRFluxPartition()
      }

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

    }, error=function(e){
      print("An error occurred:")
      print(e)
      CombinedData.F <- EddyData.F
    })
    if (year_index == 1){
      results_df <- CombinedData.F
    } else {
      results_df <- merge(results_df, CombinedData.F, all=TRUE)
    }
  }

  if(is.null(file_name)) file_name <- paste0("For_ReddyProc_", FLUX, "_GF.txt")

  # message(str(CombinedData.F))
  if(isTRUE(filter_ustar)){
    names(results_df)[which(names(results_df) == "GPP_uStar_f")] <- "GPP_f"
    names(results_df)[which(names(results_df) == "Reco_uStar")] <- "Reco"
  }
  # View(CombinedData.F)
  REddyProc::fWriteDataframeToFile(results_df, file_name, saving_folder) #update the output file path

  # #+++ Example plots of filled data to screen or to directory /plots
  #EddyProc.C$sPlotFingerprintY('NEE_f', Year=2021) #gapfilled NEE fingerprint, update the year accordingly

}
