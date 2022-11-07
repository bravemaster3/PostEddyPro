#' Wrapper for Correlogram plots
#'
#' @param df dataframe containing variables to be used to compute the correlation coefficients
#' @param vec_var vector of variables for subsetting the dataframe df and used for computing the correlation coefficients
#'
#' @return a correlaogram
#' @export
plotter_correlogram <- function(df,
                                method_shape="circle",
                                na_handle="pairwise.complete.obs",
                                Site,
                                vec_var,
                                vec_var_newnames,
                                show_x=TRUE,
                                show_y=TRUE,
                                legend=FALSE){
  df <- df %>% dplyr::select(dplyr::all_of(vec_var))

  names(df) <- vec_var_newnames

  df_cor <- stats::cor(df, use=na_handle)
  #return(df_cor)

  cor_pmat <- ggcorrplot::cor_pmat(df)

  cor_pmat[is.na(df_cor)] <- NA
  #return(cor_pmat)

  label=grid::textGrob(label = Site, x = 0.5, y = 0.95, just = "top", gp=gpar(fontface = "italic", col = "black",size = 2.5))

  g <- ggcorrplot(df_cor, method=method_shape, type = "lower", hc.order = FALSE,
            outline.col = "white", show.diag = TRUE,lab = TRUE, lab_size = 3,
            p.mat = cor_pmat) +
    ggplot2::annotation_custom(label)
  if(isFALSE(show_x)) g <- g + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  if(isTRUE(show_x)) g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8))

  if(isFALSE(show_y)) g <- g + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  if(isTRUE(show_y)) g <- g + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))

  if(isFALSE(legend)) g <- g+ggplot2::theme(legend.position = "none")
  if(isTRUE(legend)) g <- g+ggplot2::theme(legend.position = c(0.15,0.75),legend.title = ggplot2::element_text(size = 8),
                                           legend.text  = ggplot2::element_text(size = 8),
                                           legend.key.size = unit(0.8, "lines"))
  return(g)
}
