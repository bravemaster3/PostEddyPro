#' Random Forest Gapfilling of montecarlo simulated datasets
#'
#' @param mc_sim_path path to the montecarlo simulation files. This folder should only contain those files and nothing else.
#' @param mtry the best mtry found during training in the real gapfilling. This should be fetched from the previously saved model.
#' @param preds same predictors as used in the real gapfilling
#' @param flux_col column holding the fluxes to be gapfilled
#' @param mc_sim_gf_path saving folder, where the gpafilled output should be saved
#'
#' @return this function will return nothing, but files should be saved in a specified folder
#' @export
montecarlo_sim_gf <- function(mc_sim_path,
                              mtry, #this is the best tuning during the caret crossvalidation, should be fetched from the saved RF gapfilling model prior to using this function
                              preds, #same as used in the original gapfilling
                              flux_col,
                              mc_sim_gf_path){

  formula <- ch4_flux_final <- predicted <- NULL
  formula <- paste(flux_col, "~" , paste(preds, collapse = " + ")) #this will create the string formula used in the random forest
  all_sim <- list.files(path=mc_sim_path, full.names = TRUE)

  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  `%dopar%` <- foreach::`%dopar%`

  useless_output = foreach::foreach(x = all_sim, .packages = c("randomForest",
       "stats", "dplyr", "data.table")) %dopar% {

         df <- data.table::fread(x)

         df_no_na <- stats::na.omit(df)

         rf_final <- randomForest::randomForest(
           stats::as.formula(formula), #this is the same formula as before
           data=df_no_na,             #Here we use the full set
           mtry=mtry, #this is the best tuning during the caret crossvalidation
           type="regression"
         )

         df$predicted <- stats::predict(rf_final, newdata = df)#this predicts for missing and non-missing values alike,

         df <- df %>% dplyr::mutate(ch4_flux_final_filled = ifelse(!is.na(ch4_flux_final),ch4_flux_final,predicted),
                                    quality = ifelse(!is.na(ch4_flux_final),"original","gapfilled"))

         #preparing saving path

         input_basename <- gsub("\\.","_gf.", basename(x))
         saving_filename <- file.path(mc_sim_gf_path, input_basename)
         data.table::fwrite(df, saving_filename, dateTimeAs = "write.csv")
       }
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
}
