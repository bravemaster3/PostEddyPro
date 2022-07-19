gf_meas_unc <- function(df,
                        datetime = "datetime",
                        flux_col_gf = "ch4_flux_final_filled",
                        meas_unc_col = "rand_err_ch4_flux",
                        preds){

  df_sub <- df[,c(datetime, flux_col_gf, meas_unc_col, preds)]
}
