#' Daily plotter for all sites for a given variable
#'
#' @param list_df a list of dataframes having the same structure and able to be bound by row. When this comes from the saved aggregated tables, they are just the _day element of the list.
#' @param y_label label of the y axis. it can also be a bquote if superscripts or underscripts are needed.
#' @param y_col string, name of the variable to be represented on the y axis
#' @param sd_col string, name of the variable to use for the ribbon
#' @param show_x_label logical, TRUE to show x-axis text and title, FALSE to hider x-axis text and title.#'
#' @return a ggplot object
#' @export
all_site_mean_sd_plotter <- function(list_df, y_label, y_col = "mean_FCH4", sd_col ="sd_FCH4", show_x_label=FALSE, ignore_NA=TRUE, GPP=FALSE, Reco=FALSE, NEE=FALSE
                                     ){


  df <- Reduce(function(...) rbind(...), list_df)


  df2 <- df
  if(GPP == TRUE){
    df2 <- df2 %>% dplyr::group_by(Site) %>% dplyr::mutate(GPP_sum = ifelse(is.na(GPP_sum) & date>as.Date("2020-12-31"), zoo::na.approx(GPP_sum), GPP_sum))
  }

  if(NEE == TRUE){
    df2 <- df2 %>% dplyr::group_by(Site) %>% dplyr::mutate(NEE_sum = ifelse(is.na(NEE_sum) & date>as.Date("2020-12-31"), zoo::na.approx(NEE_sum), NEE_sum))
  }

  if(Reco == TRUE){
    df2 <- df2 %>% dplyr::group_by(Site) %>% dplyr::mutate(Reco_sum = ifelse(is.na(Reco_sum) & date>as.Date("2020-12-31"), zoo::na.approx(Reco_sum), Reco_sum))
  }


  df2 <- df2 %>%
    group_by(date) %>%
    summarise(mean_FCH4 = mean(FCH4_sum),
              mean_GPP = mean(GPP_sum),
              mean_Reco = mean(Reco_sum),
              mean_NEE = mean(NEE_sum),
              mean_Ta = mean(Ta_f),
              mean_Ts = mean(Ts_f),
              mean_Ts1 = mean(Ts1),
              mean_Ts2 = mean(Ts2),
              mean_Ts4 = mean(Ts4),
              mean_Ts5 = mean(Ts5),
              mean_PAR = mean(PARin_f),
              mean_WTD = mean(WTD_f),
              sd_FCH4 = sd(FCH4_sum),
              sd_GPP = sd(GPP_sum),
              sd_Reco = sd(Reco_sum),
              sd_NEE = sd(NEE_sum),
              sd_Ta = sd(Ta_f),
              sd_Ts = sd(Ts_f),
              sd_Ts1 = sd(Ts1),
              sd_Ts2 = sd(Ts2),
              sd_Ts4 = sd(Ts4),
              sd_Ts5 = sd(Ts5),
              sd_PAR = sd(PARin_f),
              sd_WTD = sd(WTD_f))

  df2[,"y_min"] <- df2[,y_col] - df2[,sd_col]
  df2[,"y_max"] <- df2[,y_col] + df2[,sd_col]


  #str(df)
  g <- ggplot2::ggplot(data=df2,ggplot2::aes_string(x = "date",y = y_col))+
    ggplot2::geom_line(alpha=0.5, size=0.5)+
    geom_ribbon(aes_string(y = y_col, ymin = "y_min", ymax = "y_max"), alpha = .2, fill = "blue")

  #return(g)


  # if(ignore_NA == TRUE) {
  #   df2$na_y <- factor(ifelse(is.na(df2[,y_col]), 0, 1))
  #   df2$bloc <- factor(data.table::rleid(df2$na_y))
  #   #View(df2)
  #
  #   for(i in unique(df2$bloc)){
  #     #print(paste0("this is the current na_y"," : ", unique(df2[df2$bloc==i,"na_y"])))
  #     if(unique(df2[df2$bloc==i,"na_y"])==1){
  #       g <- g + ggplot2::geom_smooth(data = df2[df2$bloc==i,],method = "loess", span=0.05)
  #       #print(g)
  #     }
  #   }
  #
  # }else{
  #   g <- g + ggplot2::geom_smooth(data = df2, method = "loess", span=0.1)
  # }
  g <- g+ #ggplot2::geom_smooth(method = "loess", span=0.05)+
    ggplot2:: ylab(y_label)+
    ggplot2::theme_classic()+
    ggplot2::scale_x_date(breaks = "1 month", labels=scales::date_format("%b/%Y"))+
    ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA,size=1))+#,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    # facet_wrap(~ Site)+
    #+
    ggplot2::theme(legend.position = c(0.85, 0.8), legend.background = ggplot2::element_rect(fill = "white", colour = "black", size=0.5,linetype = 2),
                   legend.text=ggplot2::element_text(size=8),legend.title=ggplot2::element_blank(),legend.spacing.y = ggplot2::unit(0.01, 'cm'))

  if(isTRUE(show_x_label)){
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  }

  if(isFALSE(show_x_label)){
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x=ggplot2::element_blank())
  }

  g
  # plotly::ggplotly(g)
  # ggsave("D:/graphs/gf_daily_sum_FULL.png",width = 140, height = 100, unit="mm")
  # htmlwidgets::saveWidget(ggplotly(g), "D:/graphs/gf_daily_sum_FULL.html")

}


