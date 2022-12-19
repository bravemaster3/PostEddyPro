#' Plotting monthly anomalies average of every month over the years
#'
#' @param list_df a list of dataframes having the same structure and able to be bound by row. When this comes from the saved aggregated tables, they are just the _month element of the list.
#' @param period_start start date in the format YYYY-mm-dd
#' @param period_end End date in the format YYYY-mm-dd
#' @param x_label Label to write on the x axis
#' @param y_label Label to write on the y axis
#' @param x_col the x column name
#' @param y_col the y column name
#' @param full_periods_only logical. If TRUE, full years only will be used
#' @param show_x_label logical. Show or not show x label
#' @param show_y_label logical. Show or not show x label
#' @param add_geom_smooth logical. Show or not a linear fit
#' @param color_grey logical. color in grey or not
#' @param add_rsq whether or not to add r-squared
#' @param growing_season logical, whether or not to limit the dataset to the growing season (May to October)
#' @param levels_sites a vector showing the order of the site names.
#'
#' @return a ggplot object
#' @export
#'
plotter_monthly_anomalies_2 <- function(list_df,
                                      period_start="2020-04-01",
                                      period_end="2022-03-31",
                                      x_label="Monthly GPP anomaly (gC. m-2. month-1)",
                                      y_label="Monthly CH4 flux anomaly (g. m-2. month-1)",
                                      x_col = "GPP_sum",
                                      y_col = "FCH4_sum",
                                      full_periods_only=TRUE,
                                      show_x_label=TRUE,
                                      show_y_label=FALSE,
                                      add_geom_smooth=TRUE,
                                      color_grey=TRUE,
                                      add_rsq=TRUE,
                                      growing_season=FALSE,
                                      levels_sites = c("Stortjarn", "Halsingfors", "Halmyran", "Degero")){
  df <- Reduce(function(...) rbind(...), list_df)

  if(isTRUE(full_periods_only)){
    tab <- table(df$period)
    df <- df[df$period %in% names(tab)[tab==12*length(unique(df$Site))],]
  }


  #filtering the data to only keep data for the desired period

  #Sys.setlocale(locale = "English")

  period_start <- as.Date(period_start)
  period_end <- as.Date(period_end)
  yearmon_start <- zoo::as.yearmon(lubridate::year(period_start)+lubridate::month(period_start)/12)
  yearmon_end <- zoo::as.yearmon(lubridate::year(period_end)+lubridate::month(period_end)/12)

  df$yearmon <- zoo::as.yearmon(df$year + df$month/12)

  # df[df$yearmon > "May 2020",]
  df <- df %>% filter(yearmon >= yearmon_start & yearmon <= yearmon_end)

  #imputing missing values based on average values of other sites for that month
  df_month <- df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(x_mean = mean(get(x_col), na.rm=TRUE),
                     y_mean = mean(get(y_col), na.rm=TRUE))

  df$period_year <- NA
  for(i in 1:nrow(df)){
    if(is.na(df[i,x_col])) df[i,x_col] <- df_month[df_month$month==df[i,"month"],"x_mean"]
    if(is.na(df[i,y_col])) df[i,y_col] <- df_month[df_month$month==df[i,"month"],"y_mean"]

    df[i,"period_year"] <- paste0(min(df[df$period==df[i,"period"],"year"]),"/",max(df[df$period==df[i,"period"],"year"]))
  }

  #computing the anomalies for the x and y variables
  df_year_mean <- df %>%
    dplyr::group_by(Site,month) %>%
    dplyr::summarise(x_mean = mean(get(x_col), na.rm=TRUE),
                     y_mean = mean(get(y_col), na.rm=TRUE))

  df <- merge(df,df_year_mean, by=c("Site","month"), all=TRUE)

  df <- df %>%
    dplyr::mutate(x_anomaly = get(x_col)-x_mean,
                  y_anomaly = get(y_col)-y_mean)


  if(isTRUE(growing_season)) df <- df[df$month %in% c(4:10),]

  if(length(unique(df$period))==2) g <- ggplot2::ggplot(data=df, aes(x=x_anomaly,y=y_anomaly))

  if(length(unique(df$period))>2) g <- ggplot2::ggplot(data=df, aes(x=x_anomaly,y=y_anomaly, shape=period_year, color=period_year))


  df$Site <- factor(df$Site, levels = levels_sites)
  # print(levels(df$Site))
  g <- g+
    ggplot2::geom_vline(xintercept = 0)+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_point(size=0.7)+
    ggplot2:: xlab(x_label)+
    ggplot2:: ylab(y_label)+
    ggplot2::theme_classic()

  if(isTRUE(color_grey) & length(unique(df$period))>2)   g <- g+ ggplot2::scale_colour_grey(start = 0.1,end = 0.6)


  if(isTRUE(add_geom_smooth)) g <- g+ ggplot2::geom_smooth(method="lm", se=FALSE, size=0.8, linetype="longdash", color="red")

    if(isFALSE(show_x_label)){
      g <- g + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x=ggplot2::element_blank())
    }
  if(isFALSE(show_y_label)){
    g <- g + ggplot2::theme(axis.title.y=ggplot2::element_blank())
  }

  g <- g+ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA,size=1),legend.position = c(0.10, 0.8), legend.background = ggplot2::element_rect(fill = "white", colour = "black", size=0.5,linetype = 2),
                        legend.text=ggplot2::element_text(size=8),legend.title=ggplot2::element_blank(),legend.spacing.y = ggplot2::unit(0.01, 'cm'))+
    facet_wrap(~factor(Site, levels = levels_sites), ncol = length(unique(df$Site)), nrow=1)

  if(isTRUE(add_rsq)) g <- g+ ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), size=3.5, p.accuracy = 0.05)#ggpmisc::stat_poly_eq(mapping=ggpmisc::use_label(c("R2")), p.digits = 2) #ggpmisc::stat_poly_eq(aes(label = ..rr.label..), formula=y~x, size = 3, alpha = 0.2,parse = TRUE, na.rm=TRUE)
    #ggpmisc::stat_poly_eq(mapping=aes(ggpmisc::use_label(c("R2"))), p.digits = 2)
    #ggpubr::stat_cor(aes(label = ..rr.label..), color = "red", geom = "label")

  g
}
