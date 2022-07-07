#' Merge Eddypro and Biomet and Visualize fluxes
#'
#' @param path_EC The path to your untouched Eddypro full output
#' @param path_biomet The path to your merged biomet file
#' @param date_format If the eddypro output has not been altered, the date format is "\%Y-\%m-\%d", the default. If it has been opened in excel for instance, the date format is likely "\%d/\%m/\%Y"
#'
#' @return a named list, $data is a dataframe merged from flux and biomet files, $graphs is a graph of all gas fluxes
#' @export
visualization <- function(path_EC,
                          path_biomet,
                          date_format="%Y-%m-%d"
                          ){

  headers <- utils::read.csv(path_EC, skip=1, header=FALSE, nrows=1, as.is=TRUE)
  fluxes_site <- utils::read.table(path_EC, sep=",", header = F,skip=3)
  colnames(fluxes_site)= headers
  fluxes_site$date <-as.Date(fluxes_site$date, format=date_format)
  fluxes_site$datetime <- as.POSIXct(paste(fluxes_site$date, fluxes_site$time, sep=" "), format="%Y-%m-%d %H:%M", tz="UTC")
  fluxes_site[fluxes_site == -9999] <- NA
  fluxes_site$datetime <- lubridate::round_date(fluxes_site$datetime, "30 minutes")

  meteo_site <- utils::read.table(path_biomet, sep=",", header = T)
  meteo_site$datetime <- as.POSIXct(meteo_site$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  fluxes_meteo <- merge(fluxes_site, meteo_site, by="datetime", all=TRUE)

  #ploting

  co2 <- ggplot2::ggplot(data=fluxes_meteo, ggplot2::aes_string(x="datetime", y="co2_flux"))+
    ggplot2::geom_point(size=0.8)+
    ggplot2::ylim(-10,15)+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank())

  h2o <- ggplot2::ggplot(data=fluxes_meteo, ggplot2::aes_string(x="datetime", y="h2o_flux"))+
    ggplot2::geom_point(size=0.8)+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::ylim(-1.5,8)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank())


  ch4 <- ggplot2::ggplot(data=fluxes_meteo, ggplot2::aes_string(x="datetime", y="ch4_flux"))+
    ggplot2::geom_point(size=0.8)+
    ggplot2::xlab("date")+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::ylim(-0.2,0.2)+
    ggplot2::theme_bw()

  plots <- cowplot::plot_grid(co2,h2o,ch4, align = "v", ncol=1, rel_heights = c(1,1,1.15))

  return(list(data=fluxes_meteo, graphs=plots))
}
