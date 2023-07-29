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
#' @param scale_x One of "month_only or "month_year", default is month_only
#' @param rotate_x_label Logical. default, FALSE
#' @param conf_int Logical. Display a ribbon confidence interval around the smoothed lines. default, FALSE.
#' @param window_size window size for centered rolling average, default is 14 days
#' @param sd_col column to use for error when conf_int is TRUE and we want to plot a ribbon. If not provided and column is NULL, a rolling sd will be computed with the same window_size as for the moving average line...
#' @param y_col_ori second Y column to use for points. useful for instance when y has been na.approximated to gapfill missing data and you still want to plot points with the original, and rollmeans with the interpolated
#' Remember to set different_y_line_point to TRUE for it to work.
#' @param different_y_line_point logical, default FALSE. set to TRUE along side y_col_ori to use y_col_ori to plot points
#'
#' @return a ggplot object
#' @export

plotter_daily_avg <- function (list_df, y_label, y_col = "FCH4_sum", show_x_label = FALSE,
                                ignore_NA = TRUE, GPP = FALSE, Reco = FALSE, NEE = FALSE,
                                yintercept_line = FALSE, yintercept_value = 0, scale_x = "month_only",
                                rotate_x_label = FALSE, conf_int = FALSE, window_size = 14, sd_col = NULL, y_col_ori = NULL, different_y_line_point = FALSE)
{

  for(i in 1:length(list_df)){
    list_df[[i]] <- list_df[[i]] %>%
      dplyr::mutate(Var_smooth = zoo::rollmean(.data[[y_col]], k = window_size, fill=NA, align = "center"))

    if(!is.null(sd_col)){
      list_df[[i]] <- list_df[[i]] %>%
        dplyr::mutate(Var_sd = zoo::rollmean(.data[[sd_col]], k = window_size, fill=NA, align = "center"))
    } else{
      list_df[[i]] <- list_df[[i]] %>%
        dplyr::mutate(Var_sd = zoo::rollapplyr(.data[[y_col]], width = window_size, FUN = sd, fill=NA, align = "center"))

    }

  }
  df <- Reduce(function(...) rbind(...), list_df)
  if(!is.null(y_col_ori) & isTRUE(different_y_line_point)){
    g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "date",
                                                        y = y_col_ori, color = "Site"))
  } else(
    g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "date",
                                                        y = y_col, color = "Site"))
  )
  if (isTRUE(yintercept_line)) {
    g <- g + ggplot2::geom_hline(yintercept = yintercept_value,
                                 color = "black", linewidth = 0.5, linetype = "dashed")
  }
  g <- g + ggplot2::geom_point(alpha = 0.2)
  df2 <- df
  if (GPP == TRUE) {
    df2 <- df2 %>% dplyr::group_by(Site) %>% dplyr::mutate(GPP_sum = ifelse(is.na(GPP_sum) &
                                                                              date > as.Date("2020-12-31"), zoo::na.approx(GPP_sum),
                                                                            GPP_sum))
  }
  if (NEE == TRUE) {
    df2 <- df2 %>% dplyr::group_by(Site) %>% dplyr::mutate(NEE_sum = ifelse(is.na(NEE_sum) &
                                                                              date > as.Date("2020-12-31"), zoo::na.approx(NEE_sum),
                                                                            NEE_sum))
  }
  if (Reco == TRUE) {
    df2 <- df2 %>% dplyr::group_by(Site) %>% dplyr::mutate(Reco_sum = ifelse(is.na(Reco_sum) &
                                                                               date > as.Date("2020-12-31"), zoo::na.approx(Reco_sum),
                                                                             Reco_sum))
  }
  if (ignore_NA == TRUE) {
    df2$na_y <- factor(ifelse(is.na(df2[, y_col]), 0, 1))
    df2$bloc <- factor(data.table::rleid(df2$na_y))
    for (i in unique(df2$bloc)) {
      if (unique(df2[df2$bloc == i, "na_y"]) == 1) {
        g <- g + ggplot2::geom_line(data = df2[df2$bloc ==
                                                 i, ], ggplot2::aes(y = Var_smooth), alpha = 0.8, linewidth = 1)
      }
    }
  } else {
    g <- g + ggplot2::geom_line(data = df2, ggplot2::aes(y = Var_smooth), alpha = 0.8, linewidth = 1)
  }
  g <- g + ggplot2::ylab(y_label) + ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA,
                                                        size = 1)) + ggplot2::theme(legend.position = c(0.85,
                                                                                                        0.8), legend.background = ggplot2::element_rect(fill = "white",
                                                                                                                                                        colour = "black", size = 0.5, linetype = 2), legend.text = ggplot2::element_text(size = 8),
                                                                                    legend.title = ggplot2::element_blank(), legend.spacing.y = ggplot2::unit(0.01, "cm"))

  if(isTRUE(conf_int == TRUE)){
    g <- g+geom_ribbon(mapping =  ggplot2::aes(ymin = Var_smooth - Var_sd, ymax = Var_smooth + Var_sd),
                       alpha = 0.1, linetype = 0)
  }

  if (scale_x == "month_year") {
    g <- g + ggplot2::scale_x_date(breaks = "1 month", labels = scales::date_format("%b/%Y"))
  }
  if (scale_x == "month_only") {
    g <- g + ggplot2::scale_x_date(date_breaks = "1 month",
                                   labels = dte_formatter, expand = c(0, 0))
  }
  if (isTRUE(show_x_label)) {
    if (isTRUE(rotate_x_label)) {
      g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
  }
  if (isFALSE(show_x_label)) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                            axis.title.x = ggplot2::element_blank())
  }
  g
}

