#' Preparation of flux data for REddyProc gapfilling
#'
#' @param df Dataframe containing the flux column, timestamp and all the biomet variables.
#' If following the standard workflow, use the output from the quality check
#' @param datetime timestamp column name already as a POSIXct object
#' @param flux_col flux column, already quality checked/controlled
#' @param FLUX flux column in the final output: NEE for CO2 flux OR H2O for water flux
#' @param rg_col global radiation column name or other variable used in its place (e.g. SWin)
#' @param tair_col air temperature column name
#' @param tsoil_col soil temperature column name
#' @param rh_col relative humidity column name
#' @param vpd_col vapor pressure deficit column name
#' @param ustar_col u* column name
#' @param saving_path saving folder path where the formatted file will be saved
#' @param filename WITHOUT extension. name to be appended to saving_path to create full path name. If NULL, a default name will be written
#' @param use_tsoil_col Boolean variable, default is TRUE, set to FALSE TO IGNORE TSOIL
#' @param convert_vpd_kpa_to_hpa Boolean variable, default is TRUE, converts VPD from kpa to hpa if TRUE
#'
#' @return No value returned, but a file saved to disk
#' @export
formatting_fluxes_REddyProc <- function(df,
                                        datetime="datetime",
                                        flux_col="co2_flux_final",
                                        FLUX = c("NEE","H2O"),#either of them
                                        rg_col = "SWin_f",
                                        tair_col = "Ta_f",
                                        tsoil_col = "Ts_f",
                                        rh_col = "RH_f",
                                        vpd_col = "VPD_f",#VPD must be in kpa
                                        ustar_col = "u*",
                                        saving_path,
                                        filename = NULL,
                                        use_tsoil_col = TRUE,
                                        convert_vpd_kpa_to_hpa = TRUE){
  df$Year <- lubridate::year(df[,datetime])
  df$DoY <- lubridate::yday(df[,datetime])
  df$Hour <- lubridate::hour(df[,datetime]) + lubridate::minute(df[,datetime])/60

  # if(grepl("co2",tolower(flux_col), fixed=TRUE)) df$NEE <- df[,flux_col]
  # if(grepl("h2o",tolower(flux_col), fixed=TRUE)) df$H2O <- df[,flux_col]
  if (FLUX=="NEE") df$NEE <- df[, flux_col]
  if (FLUX=="H2O") df$H2O <- df[, flux_col]

  df$Rg <- df[,rg_col]
  df$Tair <- df[,tair_col]
  if(isTRUE(use_tsoil_col)) df$Tsoil <- df[,tsoil_col]
  df$rH <- df[,rh_col]
  if(isTRUE(convert_vpd_kpa_to_hpa)){
    df$VPD <- df[,vpd_col]*10 #to convert it to hPa from kPa
  }
  df$Ustar <- df[,ustar_col]

  ####
  if(isTRUE(use_tsoil_col)) {
    all_vars <- c("Year","DoY","Hour",FLUX,"LE","H","Rg","Tair","Tsoil","rH","VPD","Ustar")
  } else {
    all_vars <- c("Year","DoY","Hour",FLUX,"LE","H","Rg","Tair","rH","VPD","Ustar")
  }

  df_ReddyProc <- df[, all_vars]
  df_ReddyProc[is.na(df_ReddyProc)] <- -9999

  headers <- colnames(df_ReddyProc)
  # if(grepl("co2",tolower(flux_col), fixed=TRUE)) unit_flux <- "umolm-2s-1"
  # if(grepl("h2o",tolower(flux_col), fixed=TRUE)) unit_flux <- "mmolm-2s-1"
  if(FLUX == "NEE") unit_flux <- "umolm-2s-1"
  if(FLUX == "H2O") unit_flux <- "mmolm-2s-1"

  if(isTRUE(use_tsoil_col)) {
    all_units <- c("-", "-", "-",	unit_flux,	"Wm-2",	"Wm-2",	"Wm-2",	"degC",	"degC",	"%", "hPa",	"ms-1")
  } else {
    all_units <- c("-", "-", "-",	unit_flux,	"Wm-2",	"Wm-2",	"Wm-2",	"degC",	"%", "hPa",	"ms-1")
  }

  if(is.null(filename)) {
    saving_file_name <- file.path(saving_path,  paste0("For_ReddyProc_", FLUX, ".txt"))
  }else{
    saving_file_name <- file.path(saving_path,  paste0(filename, ".txt"))
  }



  #Creating the directory or removing it if it was already there.
  if(file.exists(saving_file_name)) {
    deletion <- utils::menu(c("Yes, remove it!","No"), graphics = TRUE, title="A file with the same name exists. Delete to continue...")


    file_check = FALSE
    if(deletion==1) {
      unlink(saving_file_name)
      print("The existing file has been removed")
      print("The new file will be created")
      file_check = TRUE
    }

    else {
      print("File writing will be aborted")
      file_check = FALSE
    }
  } else file_check = TRUE


  if(file_check == TRUE) {
    sink(saving_file_name)
    cat(cat(headers,sep="\t"),cat("\n"),cat(all_units, sep="\t"), sep = "\n")
    sink()
    data.table::fwrite(df_ReddyProc, file = saving_file_name, sep="\t", append = TRUE, col.names = FALSE)
  }




}
