#' Merge Eddypro and Biomet and Visualize fluxes
#'
#' @param check_path_EC logical, TRUE means you provide a path, FALSE means you provide a dataframe for Eddypro output instead. This is practical when merging beforehand several eddypro output and therefore no longer having several header lines
#' @param check_path_biomet logical, TRUE means you provide a path, FALSE means you provide a dataframe for biomet variables. This is practical for integrating this function within another workflow and not having to save to disk to read it again.
#' @param path_EC path to your untouched Eddypro full output
#' @param path_biomet path to your merged biomet file. If you used Eddypro to produce the merged biomet file, it will not work here because there are 2 header lines. In this case, you can set check_path_biomet to FALSE and use the variable biomet_df instead (i.e. read the df separately and use it as input here)
#' @param EC_df Eddypro output already read in as a dataframe.This is required when check_path_EC is set to FALSE
#' @param biomet_df biomet data read in as a dataframe.This is required when check_path_biomet is set to FALSE
#' @param date_format If the eddypro output has not been altered, the date format is "\%Y-\%m-\%d", the default. If it has been opened in excel for instance, the date format is likely "\%d/\%m/\%Y"
#' @param plot_co2 logical, TRUE (default) for plotting also CO2, FALSE otherwise
#' @param plot_ch4 logical, TRUE (default) for plotting also CH4, FALSE otherwise
#'
#' @return a named list, $data is a dataframe merged from flux and biomet files, $graphs is a graph of all gas fluxes
#' @export
visualization <- function(check_path_EC = TRUE,
                          check_path_biomet = TRUE,
                          path_EC = NULL,
                          path_biomet = NULL,
                          EC_df = NULL,
                          biomet_df = NULL,
                          date_format="%Y-%m-%d",
                          plot_co2 = TRUE,
                          plot_ch4 = TRUE
                          ){

  #checking that either paths or dfs have been provided
  if((is.null(path_EC) & is.null(EC_df)) | (is.null(path_biomet) & is.null(biomet_df))){
    stop("Both path_EC and EC_df, or both path_biomet and biomet_df cannot be missing at the same time")
  }
  if((isTRUE(check_path_EC) & is.null(path_EC)) | (isTRUE(check_path_biomet) & is.null(path_biomet))){
    stop("Please set check_path_EC or check_path_biomet to FALSE when not providing a path and instead using the EC_df or biomet_df variables")
  }
  #reading or using EC file
  if(isTRUE(check_path_EC)){
    headers <- utils::read.csv(path_EC, skip=1, header=FALSE, nrows=1, as.is=TRUE)
    fluxes_site <- utils::read.table(path_EC, sep=",", header = F,skip=3)
    colnames(fluxes_site)= headers
    fluxes_site$date <-as.Date(fluxes_site$date, format=date_format)
    fluxes_site$datetime <- as.POSIXct(paste(fluxes_site$date, fluxes_site$time, sep=" "), format="%Y-%m-%d %H:%M", tz="UTC")
    fluxes_site[fluxes_site == -9999] <- NA
    fluxes_site$datetime <- lubridate::round_date(fluxes_site$datetime, "30 minutes")
  } else{
    fluxes_site <- as.data.frame(EC_df)
    fluxes_site$date <-as.Date(fluxes_site$date, format=date_format)
    fluxes_site$datetime <- as.POSIXct(paste(fluxes_site$date, fluxes_site$time, sep=" "), format="%Y-%m-%d %H:%M", tz="UTC")
    fluxes_site[fluxes_site == -9999] <- NA
    fluxes_site$datetime <- lubridate::round_date(fluxes_site$datetime, "30 minutes")
  }

  #reading or using EC file
  if(isTRUE(check_path_biomet)){
    meteo_site <- utils::read.table(path_biomet, sep=",", header = T)
    meteo_site$datetime <- as.POSIXct(meteo_site$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  } else{
    meteo_site <- as.data.frame(biomet_df)
    meteo_site$datetime <- as.POSIXct(meteo_site$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  }

  fluxes_meteo <- merge(fluxes_site, meteo_site, by="datetime", all=TRUE)

  #correcting some columnn names that changed because they were the same in met and EC

  nams <-  colnames(fluxes_meteo)
  EC_cols <- nams[grepl(".x", nams,fixed = TRUE)]
  met_cols <- nams[grepl(".y", nams, fixed = TRUE)]
  new_EC_cols <- gsub(".x", "_EC", EC_cols, fixed=TRUE)
  new_met_cols <- gsub(".y", "", met_cols, fixed=TRUE)

  names(fluxes_meteo)[which(names(fluxes_meteo) %in% EC_cols)] <- new_EC_cols
  names(fluxes_meteo)[which(names(fluxes_meteo) %in% met_cols)] <- new_met_cols

  #ploting

  h2o <- ggplot2::ggplot(data=fluxes_meteo, ggplot2::aes_string(x="datetime", y="h2o_flux"))+
    ggplot2::geom_point(size=0.8)+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::ylim(-1.5,8)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank())

  # list_plots <- c(h2o)
  plot_heights = 1
  if(plot_co2 == TRUE){
    co2 <- ggplot2::ggplot(data=fluxes_meteo, ggplot2::aes_string(x="datetime", y="co2_flux"))+
      ggplot2::geom_point(size=0.8)+
      ggplot2::ylim(-10,15)+
      ggplot2::geom_hline(yintercept = 0)+
      ggplot2::theme_bw()
   if(plot_ch4 == TRUE){
     co2 <- co2 +
       ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank())

     plot_heights = c(1,1,1.15)
   }

    # list_plots <- c(co2, list_plots)
  }

  if(plot_ch4 == TRUE){
    ch4 <- ggplot2::ggplot(data=fluxes_meteo, ggplot2::aes_string(x="datetime", y="ch4_flux"))+
      ggplot2::geom_point(size=0.8)+
      ggplot2::xlab("date")+
      ggplot2::geom_hline(yintercept = 0)+
      ggplot2::ylim(-0.2,0.2)+
      ggplot2::theme_bw()

    # list_plots <- c(list_plots, ch4)

  }

  if((plot_ch4 + plot_co2) == 1) plot_heights = c(1,1.15)
  #print(list_plots)

  if((plot_ch4 + plot_co2) == 2){
    plots <- cowplot::plot_grid(h2o,co2,ch4, align = "v", ncol=1, rel_heights = plot_heights)
  } else if(plot_ch4 == TRUE) {
    plots <- cowplot::plot_grid(h2o,ch4, align = "v", ncol=1, rel_heights = plot_heights)
  } else if(plot_co2 == TRUE) {
    plots <- cowplot::plot_grid(h2o,co2, align = "v", ncol=1, rel_heights = plot_heights)
  } else plots <- cowplot::plot_grid(h2o, align = "v", ncol=1, rel_heights = plot_heights)

  #plots <- cowplot::plot_grid(h2o,co2, align = "v", ncol=1, rel_heights = plot_heights)

  return(list(data=fluxes_meteo, graphs=plots))
}
