#' Daily plotter for all sites for a given variable
#'
#' @param list_df a list of dataframes having the same structure and able to be bound by row. When this comes from the saved aggregated tables, they are just the _day element of the list.
#' @param y_label label of the y axis. it can also be a bquote if superscripts or underscripts are needed.
#' @param y_col string, name of the variable to be represented on the y axis.
#' @param show_x_label logical, TRUE to show x-axis text and title, FALSE to hider x-axis text and title.
#' @param ignore_NA logical. If TRUE, segments without NA will be plotted separately to avoid interpolation of geom_smooth where there are missing values
#' @param GPP logical. If TRUE, the graph us then tailored for GPP. default is FALSE
#' @param Reco logical. If TRUE, the graph us then tailored for Reco. default is FALSE
#' @param NEE logical. If TRUE, the graph us then tailored for NEE. default is FALSE
#' @param yintercept_line logical. If TRUE, a horizontal line will be added. default is FALSE
#' @param yintercept_value positive integer. specifies the y value at which the horizonal line will be drawn. default is 0
#'
#' @return a ggplot object
#' @export
plotter_daily <- function(lisUt_df, y_label, y_col = "FCH4_sum", show_x_label=FALSE, ignore_NA=TRUE, GPP=FALSE, Reco=FALSE, NEE=FALSE, yintercept_line = FALSE, yintercept_value= 0){

  df <- Reduce(function(...) rbind(...), list_df)
  #if(y_col=="GPP_sum") df[df[,y_col] < 0 & !is.na(df[,y_col]), y_col] <- 0
  #print(nrow(df[!is.na(df[,y_col]),]))
  #all sites together methane, daily
  g <- ggplot2::ggplot(data=df,ggplot2::aes_string(x = "date",y = y_col, color = "Site"))

  if(isTRUE(yintercept_line)){
    g <- g + ggplot2::geom_hline(yintercept = yintercept_value, color = "black", linewidth = 0.5, linetype="dashed")
  }

  g <- g+
    ggplot2::geom_point(alpha=0.1)

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

  if(ignore_NA == TRUE) {
    df2$na_y <- factor(ifelse(is.na(df2[,y_col]), 0, 1))
    df2$bloc <- factor(data.table::rleid(df2$na_y))
    #View(df2)

    for(i in unique(df2$bloc)){
      #print(paste0("this is the current na_y"," : ", unique(df2[df2$bloc==i,"na_y"])))
      if(unique(df2[df2$bloc==i,"na_y"])==1){
        g <- g + ggplot2::geom_smooth(data = df2[df2$bloc==i,],method = "loess", span=0.05)
        #print(g)
      }
    }

  }else{
    g <- g + ggplot2::geom_smooth(data = df2, method = "loess", span=0.1)
  }
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


