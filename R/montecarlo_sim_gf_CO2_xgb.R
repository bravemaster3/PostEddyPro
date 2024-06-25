#' Random Forest Gapfilling of montecarlo simulated datasets
#'
#' @param mc_sim_path path to the montecarlo simulation files. This folder should only contain those files and nothing else.
#' @param preds same predictors as used in the real gapfilling
#' @param flux_col column holding the fluxes to be gapfilled
#' @param mc_sim_gf_path saving folder, where the gpafilled output should be saved
#' @param best_tune the best tuning grid obtained from the Caret training, used for the gapfilling
#' @param datetime datetime column name as a POSIXct object
#'
#' @return this function will return nothing, but files should be saved in a specified folder
#' @export
montecarlo_sim_gf_CO2_xgb <- function(mc_sim_path,
                              best_tune, #this is the best tuning during the caret crossvalidation, should be fetched from the saved RF gapfilling model prior to using this function
                              preds, #same as used in the original gapfilling
                              flux_col,
                              mc_sim_gf_path,
                              datetime = "datetime"){

  all_sim <- list.files(path=mc_sim_path, full.names = TRUE)

  no_cores <-  parallel::detectCores() - 1

  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)

  `%dopar%` <- foreach::`%dopar%`
  useless_output = foreach::foreach(sim = all_sim, .packages = c("xgboost", "stats", "dplyr", "data.table")) %dopar% {

    df <- data.table::fread(sim)

    df_no_na <- stats::na.omit(df)

    message("Before xgb_train1")
    xgb_train = xgboost::xgb.DMatrix(data = as.matrix(df_no_na %>% dplyr::select(-dplyr::one_of(c(flux_col, datetime)))),
                                     label = as.matrix(df_no_na %>% dplyr::select(dplyr::one_of(flux_col))))
    message("After xgb_train1")
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
    message("After Training")

    df$predicted <- stats::predict(xgb_final, newdata = xgboost::xgb.DMatrix(as.matrix(sapply(df %>%
                                                                                                dplyr::select(-dplyr::one_of(c(flux_col, datetime))), as.numeric))))

    df <- df %>%
      dplyr::mutate(!!paste0(flux_col,"_filled") := ifelse(!is.na(!!sym(flux_col)), !!sym(flux_col), predicted),
                    quality = ifelse(!is.na(!!sym(flux_col)), "original", "gapfilled"))

    #preparing saving path

    input_basename <- gsub("\\.","_gf.", basename(sim))
    saving_filename <- file.path(mc_sim_gf_path, input_basename)
    data.table::fwrite(df, saving_filename, dateTimeAs = "write.csv")
  }
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
}

