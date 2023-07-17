#' Random Forest Gapfilling of montecarlo simulated datasets
#'
#' @param mc_sim_path path to the montecarlo simulation files. This folder should only contain those files and nothing else.
#' @param preds same predictors as used in the real gapfilling
#' @param flux_col column holding the fluxes to be gapfilled
#' @param mc_sim_gf_path saving folder, where the gpafilled output should be saved
#' @param best_tune the best tuning grid obtained from the Caret training, used for the gapfilling
#'
#' @return this function will return nothing, but files should be saved in a specified folder
#' @export
montecarlo_sim_gf <- function(mc_sim_path,
                              best_tune, #this is the best tuning during the caret crossvalidation, should be fetched from the saved RF gapfilling model prior to using this function
                              preds, #same as used in the original gapfilling
                              flux_col,
                              mc_sim_gf_path){

  all_sim <- list.files(path=mc_sim_path, full.names = TRUE)

  parallel::mclapply(all_sim, function(x){
    print(x)
    df <- data.table::fread(x)

    df_no_na <- stats::na.omit(df)

    xgb_train = xgboost::xgb.DMatrix(data = as.matrix(df_no_na %>% dplyr::select(-dplyr::one_of(c(flux_col, datetime))), as.numeric),
                                         label = as.matrix(df_no_na %>% dplyr::select(dplyr::one_of(flux_col)), as.numeric))

    xgb_final <- xgboost::xgb.train(
      params = list(booster = "gbtree",
                    objective="reg:squarederror",
                    eta = best_tune$eta,
                    gamma = best_tune$gamma,
                    max_depth = best_tune$max_depth,
                    min_child_weight = best_tune$min_child_weight,
                    subsample = best_tune$subsample,
                    colsample_bytree = best_tune$colsample_bytree),
      data = xgb_train,
      nrounds = best_tune$nrounds,
      verbose = 0
    )

    df$predicted <- stats::predict(xgb_final, newdata = xgboost::xgb.DMatrix(as.matrix(sapply(df %>%
                                                                          dplyr::select(-dplyr::one_of(c(flux_col, datetime))), as.numeric))))

    df <- df %>% dplyr::mutate(co2_flux_final_filled = ifelse(!is.na(co2_flux_final),co2_flux_final,predicted),
                               quality = ifelse(!is.na(co2_flux_final),"original","gapfilled"))

    #preparing saving path

    input_basename <- gsub("\\.","_gf.", basename(x))
    saving_filename <- file.path(mc_sim_gf_path, input_basename)
    data.table::fwrite(df, saving_filename, dateTimeAs = "write.csv")
  }, mc.cores = parallel::detectCores() - 1)
}
