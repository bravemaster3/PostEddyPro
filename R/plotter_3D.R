#' A wrapper for plotly's 3D scatterplots.
#'
#' @param df dataframe of daily sum of fluxes and average of environmental variables
#' @param x_var variable on x axis
#' @param y_var variabke on y axis
#' @param z_var variable on z axis
#' @param col_var variable for coloring the points
#'
#' @return A 3D graph
#' @export
plotter_3D <- function(df,
                       x_var,
                       y_var,
                       z_var,
                       col_var){
  #https://plotly.com/r/figure-labels/
  #fig <- plotly::plot_ly(methane_daily_sum[methane_daily_sum$Site=="Hålmyran",], x = ~Ts, y = ~GPP_gc, z = ~CH4_flux_gf, color = ~doy,colors=palette(200))
  palette <- grDevices::colorRampPalette(c("blue", "white", "red"))

  fig <- plotly::plot_ly(df,
                         x = stats::as.formula(paste0("~", x_var)),
                         y = stats::as.formula(paste0("~", y_var)),
                         z = stats::as.formula(paste0("~", z_var)),
                         color = stats::as.formula(paste0("~", col_var)),
                         colors=palette(200))

  fig
  #htmlwidgets::saveWidget(fig, "D:/graphs/GPP_Ts_CH4_Halmyran.html")

}
