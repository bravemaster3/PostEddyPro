#' Wrapper for Correlogram plots
#'
#' @param df dataframe containing variables to be used to compute the correlation coefficients
#' @param vec_var vector of variables for subsetting the dataframe df and used for computing the correlation coefficients
#'
#' @return a correlaogram
#' @export
plotter_correlogram <- function(df,
                                vec_var){
  df <- df %>% dplyr::select(vec_var)

  df_cor <- stats::cor(df)

  cor_pmat <- ggcorrplot::cor_pmat(df)

  ggcorrplot(df_corr, method="square", type = "lower", hc.order = FALSE,
            outline.col = "white", show.diag = TRUE,lab = TRUE, lab_size = 2.5,
            p.mat = cor_pmat)
}
