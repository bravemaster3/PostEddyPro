#' A randomForest gapfiller, with an integrated tuning-training-testing from caret
#'
#' @param site_df dataframe containing all a POSIXct timestamp, the flux data and predictor columns (preds argument).
#' @param datetime name of the timestamp (datetime) column already in POSIXct format
#' @param flux_col flux column to be gapfilled, this function was developped for methane fluxes, but can be used for other fluxes as long as a good set of predictors are provided
#' @param preds vector of predictors that have been gapfilled in advance
#' @param max_mtry an integer, representing the maximum mtry parameter to be used in the random forest. it is calculated here as the maximum of sqrt of the number of predictors, and the 1/3rd of the number of predictors
#' @param sitename sitename just for labelling the plots
#' @importFrom stats as.formula
#'
#' @return large list of elements. tuningmodel: finalmodel (the model used for gapfilling), site_df (the final data.frame containing both the input dataframe and the gapfilled column), r_sq_cv (crossvalidated R-squared)
#' rmse_cv (crossvalidated rmse), pred_meas (graph showing predicted vs. measured values), gf_meas_time (a graph showing both measured and gapfilled values with time)
#' @export
rf_gapfiller <- function(site_df, #The dataframe containing all the flux data and predictor columns. This dataframe may contain several additional columns, reason why it is important to subset...
                         datetime="datetime", #the name of the date & time column! Though I use the same for all sites, setting it as an argument will allow to use the function easily with datasets with different names!
                         flux_col="ch4_flux_final", #The name of the flux column to gapfill
                         preds, #a vector of predictor variables, with default the previous vector created
                         max_mtry = NULL, #This is for trying values of mtry up to the max of either. It is calculated based on the length of preds
                         #sqrt of the number of predictors, or the 1/3rd of the number of predictors
                         sitename #This variable is simply for annotating the plots with the site name.
){
  message(paste0("!!!!Random Forest gapfilling started at: ", Sys.time()))
  message("This can take some time. Go get some coffee/tea!")
  # if(is.null(max_mtry) | max_mtry <= 0 | max_mtry > length(preds)) max_mtry <- ceiling(max(sqrt(length(preds)), length(preds)/3)) #calculates maxtry knowing the length of preds
  if(is.null(max_mtry)) max_mtry <- ceiling(max(sqrt(length(preds)), length(preds)/3)) #calculates maxtry knowing the length of preds

  folds <- predicted <- NULL
  site_df <- PostEddyPro::temporal_calculators(site_df,datetime = datetime)

  #Let's create a list to save all that is needed to be returned by the function
  output_list <- list()

  #Now let's extract only the necessary columns, and make sure there are no missing values. Normally there shouldn't be as I performed a gapfilling of predictors separately!
  df <- stats::na.omit(site_df[,c(datetime,flux_col,preds)])

  time_obj <- df[,datetime]
  df <- data.frame(df)
  df[,datetime] <- time_obj

  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  #Let's train the random forest, in the first step just for a proper tunning to get an optimal mtry value!
  #After that, we'll redo the crossvalidation & training & predictions manually since I would like to save the predictions of
  #each hold-out set during the training, for making plots of the predictive ability of the random forest.
  tc <- caret::trainControl(method = "cv", number = 10, savePredictions = TRUE)

  formula <- paste(flux_col, "~" , paste(preds, collapse = " + ")) #this will create the string formula used in the random forest

  message(paste0("........Starting Hyperparameter tuning with a grid and 10 fold cv: ", Sys.time()))
  m <- caret::train(stats::as.formula(formula),
             data = df,
             trControl = tc,
             method = "rf",
             tuneGrid = expand.grid(mtry=1:max_mtry),
             verbose = FALSE) #this is for finding the best mtry value to use

  message(paste0("........End of Hyperparameter tuning: ", Sys.time()))

  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  ########################################
  ##From the previous, we can retrieve the optimal mtry value!
  ##Now, let's write a personalized cross validation code to be able to plot the hold-out set predictions of the cross-validation process.

  #let's create the 10 folds, and collect their indices
  #Before that, I will reset the row indices of df, since I will not link it back the full site_df dataframe.
  #The resetting of row indices is because when I selected relevant columns and used na.omit earlier, the row indices are kept from the original dataframe
  rownames(df) <- NULL
  df$rowindex <- as.numeric(rownames(df))
  cv_folds <- caret::createFolds(df$rowindex, k = 10, list = FALSE, returnTrain = F)

  #Now let's put the folds indices into a column named folds in the dataframe df
  df$folds <- cv_folds

  #Now, we can loop through the previous indices and create a random forest on train set and predict the test set...
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)

  message(paste0("........Using now the best hyperparameters to gapfill each of the 10 folds: ", Sys.time()))

  `%dopar%` <- foreach::`%dopar%`

  pred_folds = foreach::foreach(fold = unique(df$folds), .packages = c("randomForest", "dplyr")) %dopar% {

    #Let's train the random forest on all BUT the current fold
    rf <- randomForest::randomForest(
      stats::as.formula(formula), #this is the same formula as before (when I used caret for the tuning of mtry hyperparameter)
      data=df %>% dplyr::filter(folds!=fold), #Since this is the training, I exclude the current fold
      mtry=m$bestTune$mtry, #this is the best tuning during the caret crossvalidation
      type="regression"
    )

    #Let's test the previous rf on the current fold, and write it to the column ch4_predicted in df

    stats::predict(rf, newdata = df[df$folds==fold,])

  }

  parallel::stopCluster(cl)
  foreach::registerDoSEQ()

  flux_col_pred = paste0(flux_col, '_filled')
  df[,flux_col_pred] <- NA

  i = 0
  for (fold in unique(df$folds)){
    i = i+1
    df[df$folds==fold, flux_col_pred] <- pred_folds[[i]]
  }

  message(paste0("........End of cross validation: ", Sys.time()))

  #Let's calculate the out of sample metrics for the plot
  r_sq <- stats::cor(df[,flux_col_pred],df[,flux_col])^2
  #r_sq
  rmse <- Metrics::rmse(df[,flux_col_pred],df[,flux_col])
  #rmse



  ####NOW, let's fill the gaps. We have 2 options, either use directly the model produced using caret,
  #or fit a personalized model with the full dataset, but the result should be the same/very similar. I will fit it myself

  message(paste0("........Starting training the final model on all the dataset: ", Sys.time()))

  rf_final <- randomForest::randomForest(
    as.formula(formula), #this is the same formula as before
    data=df,             #Here we use the full set
    mtry=m$bestTune$mtry, #this is the best tuning during the caret crossvalidation
    type="regression",
    importance = TRUE
  )

  message(paste0("........End of training the final model: ", Sys.time()))
  message(paste0("........Beginning prediction with the final model ", Sys.time()))

  site_df$predicted <- stats::predict(rf_final, newdata = site_df)#this predicts for missing and non-missing values alike,

  message(paste0("........End of prediction with the final model: ", Sys.time()))

  #and we can now take the values for filling the gaps

  #If the original flux data is available, we take that one for the new column. If instead it is missing, we use the predicted
  #Also, let's create a column for storing the quality (original, vs. gapfilled)
  #The logic here is that the new column will take measured values when they exist, and when they don't, use the predicted values to fill the gap!
  #We'll also add a quality column that makes it easy to know wether a value in the new column is original or gapfilled data.
  flux_col_var <- rlang::sym(flux_col)
  flux_col_pred_var <- rlang::sym(flux_col_pred)

  site_df <- site_df %>% dplyr::mutate(!!flux_col_pred_var := ifelse(!is.na(!!flux_col_var),!!flux_col_var,predicted),
                                quality = ifelse(!is.na(!!flux_col_var),"original","gapfilled"))

  ######
  #Let's save the relevant objects
  output_list$tuningmodel <- m

  output_list$finalmodel <- rf_final

  output_list$site_df <- site_df

  output_list$r_sq_cv <- round(r_sq,2)

  output_list$rmse_cv <- round(rmse,6)

  ######
  #Let's make some plots

  output_list$pred_meas <- ggplot2::ggplot(data=df, ggplot2::aes_string(x=flux_col,y=flux_col_pred))+
    ggplot2::geom_point(size=0.3)+
    ggplot2::theme_bw()+
    ggplot2::geom_abline(slope=1,intercept = 0, color="red")+
    ggplot2::xlab("Obs. Flux (umol.m-2.s-1)")+  #("Obs. Flux (mg.m-2.30min-1)")+
    ggplot2::ylab("Pred. Flux (umol.m-2.s-1)")+ #("Pred. Flux (mg.m-2.30min-1)")+
    ggplot2::annotate(geom="text", label=paste(sitename,", ", "r_sq=", round(r_sq,2), "\n", "rmse=",round(rmse,5), sep=""),
             x=-Inf,y=Inf, hjust = 0, vjust = 1, color="red") +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA,colour="black",size=1.5)) +
    tune::coord_obs_pred()



  output_list$gf_meas_time <- ggplot2::ggplot(data=output_list$site_df, ggplot2::aes_string(x=datetime,y=flux_col_pred,color="quality"))+
    ggplot2::geom_point(size=0.3)+
    ggplot2::theme_classic()+
    ggplot2::scale_color_manual(values=c("red", "black"))+
    ggplot2::geom_abline(slope=1,intercept = 0, color="red")+
    ggplot2::xlab("Datetime")+
    ggplot2::ylab("Flux (umol.m-2.s-1)")+  #("Flux (mg.m-2.30min-1)")+
    ggplot2::ylim(stats::quantile(site_df[,flux_col_pred], 0.001),stats::quantile(site_df[,flux_col_pred], 0.999))+
    ggplot2::scale_x_datetime(breaks="1 month", date_labels ="%b")+
    ggplot2::annotate(geom="text", label=sitename,
             x=min(site_df[,datetime]),y=Inf, hjust = 0, vjust = 1, color="red")+
    ggplot2::theme(legend.position = c(0.9,0.8), panel.border = ggplot2::element_rect(fill=NA,colour="black",size=1.5))

  message(paste0("!!!!Random Forest gapfilling completed at: ", Sys.time()))
  return(output_list) #Now we can return everything needed for use outside the function

}
