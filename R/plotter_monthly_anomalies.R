#' Plotting monthly anomalies average from yearly or growing-season
#'
#' @param list_df a list of dataframes having the same structure and able to be bound by row. When this comes from the saved aggregated tables, they are just the _month element of the list.
#' @param x_label Label to write on the x axis
#' @param y_label Label to write on the y axis
#' @param x_col the x column name
#' @param y_col the y column name
#' @param full_periods_only logical. If TRUE, full years only will be used
#' @param show_x_label logical. Show or not show x label
#' @param show_y_label logical. Show or not show x label
#' @param add_geom_smooth logical. Show or not a linear fit
#'
#' @return a ggplot object
#' @export

plotter_monthly_anomalies <- function(list_df,
                                      x_label="Monthly GPP anomaly (gC. m-2. month-1)",
                                      y_label="Monthly CH4 flux anomaly (g. m-2. month-1)",
                                      x_col = "GPP_sum",
                                      y_col = "FCH4_sum",
                                      full_periods_only=TRUE,
                                      show_x_label=TRUE,
                                      show_y_label=FALSE,
                                      add_geom_smooth=TRUE){
  df <- Reduce(function(...) rbind(...), list_df)

  if(isTRUE(full_periods_only)){
    tab <- table(df$period)
    df <- df[df$period %in% names(tab)[tab==12*length(unique(df$Site))],]
  }
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
    dplyr::group_by(Site,period) %>%
    dplyr::summarise(x_mean = mean(get(x_col), na.rm=TRUE),
                     y_mean = mean(get(y_col), na.rm=TRUE))

  df <- merge(df,df_year_mean, by=c("Site","period"), all=TRUE)

  df <- df %>%
    dplyr::mutate(x_anomaly = get(x_col)-x_mean,
                  y_anomaly = get(y_col)-y_mean)

  g <- ggplot2::ggplot(data=df, aes(x=x_anomaly,y=y_anomaly, color=Site))+
    ggplot2::geom_vline(xintercept = 0)+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_point()+
    ggplot2:: xlab(x_label)+
    ggplot2:: ylab(y_label)+
    ggplot2::theme_classic()

  if(isTRUE(add_geom_smooth)) g <- g+ ggplot2::geom_smooth(method="lm", se=FALSE, size=0.8, linetype="longdash")

  if(isFALSE(show_x_label)){
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x=ggplot2::element_blank())
  }
  if(isFALSE(show_y_label)){
    g <- g + ggplot2::theme(axis.title.y=ggplot2::element_blank())
  }

    g <- g+ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA,size=1),legend.position = c(0.10, 0.8), legend.background = ggplot2::element_rect(fill = "white", colour = "black", size=0.5,linetype = 2),
                   legend.text=ggplot2::element_text(size=8),legend.title=ggplot2::element_blank(),legend.spacing.y = ggplot2::unit(0.01, 'cm'))+
    facet_wrap(~period_year)
  g
}
