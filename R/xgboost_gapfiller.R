#' An XGBoost gapfiller, with an integrated tuning-training-testing from caret
#'
#' @param site_df dataframe containing all a POSIXct timestamp, the flux data and predictor columns (preds argument).
#' @param datetime name of the timestamp (datetime) column already in POSIXct format
#' @param flux_col flux column to be gapfilled, this function was developped for methane fluxes, but can be used for other fluxes as long as a good set of predictors are provided
#' @param preds vector of predictors that have been gapfilled in advance
#' @param sitename sitename just for labelling the plots
#' @importFrom stats as.formula
#'
#' @return large list of elements. tuningmodel: finalmodel (the model used for gapfilling), site_df (the final data.frame containing both the input dataframe and the gapfilled column), r_sq_cv (crossvalidated R-squared)
#' rmse_cv (crossvalidated rmse), pred_meas (graph showing predicted vs. measured values), gf_meas_time (a graph showing both measured and gapfilled values with time)
#' @export
xgboost_gapfiller <- function(site_df, #The dataframe containing all the flux data and predictor columns. This dataframe may contain several additional columns, reason why it is important to subset...
                         datetime="datetime", #the name of the date & time column! Though I use the same for all sites, setting it as an argument will allow to use the function easily with datasets with different names!
                         flux_col="co2_flux", #The name of the flux column to gapfill
                         preds, #a vector of predictor variables, with default the previous vector created
                         sitename #This variable is simply for annotating the plots with the site name.
){
  # if(is.null(max_mtry) | max_mtry <= 0 | max_mtry > length(preds)) max_mtry <- ceiling(max(sqrt(length(preds)), length(preds)/3)) #calculates maxtry knowing the length of preds
  # if(is.null(max_mtry)) max_mtry <- ceiling(max(sqrt(length(preds)), length(preds)/3)) #calculates maxtry knowing the length of preds

  folds <- predicted <- NULL
  site_df <- PostEddyPro::temporal_calculators(site_df,datetime = datetime)

  #Let's create a list to save all that is needed to be returned by the function
  output_list <- list()

  #Now let's extract only the necessary columns, and make sure there are no missing values. Normally there shouldn't be as I performed a gapfilling of predictors separately!
  df_with_na <- site_df[,c(datetime,flux_col,preds)]

  df <- stats::na.omit(df_with_na)

  time_obj <- df[,datetime]
  df <- data.frame(df)
  df[,datetime] <- time_obj


  # set.seed(112233)
  # library(parallel)
  # # Calculate the number of cores
  # no_cores <- detectCores() - 1
  #
  # library(doParallel)
  # # create the cluster for caret to use
  # cl <- makePSOCKcluster(no_cores)
  # registerDoParallel(cl)
  #
  # # do your regular caret train calculation enabling
  # # allowParallel = TRUE for the functions that do
  # # use it as part of their implementation. This is
  # # determined by the caret package.
  #
  # stopCluster(cl)
  # registerDoSEQ()


  no_cores <- parallel::detectCores() - 2
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)

  #Let's train the random forest, in the first step just for a proper tunning to get an optimal mtry value!
  #After that, we'll redo the crossvalidation & training & predictions manually since I would like to save the predictions of
  #each hold-out set during the training, for making plots of the predictive ability of the random forest.
  tc <- caret::trainControl(method = "cv", number = 10, savePredictions = TRUE, allowParallel = TRUE)

  formula <- paste(flux_col, "~" , paste(preds, collapse = " + ")) #this will create the string formula used in the random forest

  grid_tune <- expand.grid(
    nrounds = c(500,1000, 1500),#c(500,1000,1500), #number of trees
    max_depth = c(5,10),#c(2,4,6),
    eta = c(0.05,0.3),#c(0.025,0.05,0.1,0.3), #Learning rate
    gamma = c(0, 0.5),#c(0, 0.1, 0.5, 1.0), # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
    colsample_bytree = c(0.5, 1.0),#c(0.4, 0.6, 0.8, 1.0), #subsample ratio of columns for tree
    min_child_weight = c(1,2),#c(1,2,3), # the larger, the more conservative the model is; can be used as a stop
    subsample = c(0.5, 0.75)#c(0.5, 0.75, 1.0) # used to prevent overfitting by sampling X% training
  )

  message("........Starting Hyperparameter tuning with a grid and 10 fold cv........")
  m <- caret::train(stats::as.formula(formula),
                    data = df,
                    trControl = tc,
                    method = "xgbTree",
                    tuneGrid = grid_tune,
                    verbose = FALSE) #this is for finding the best mtry value to use

  message("........End of Hyperparameter tuning........")

  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  ########################################
  ##From the previous, we can retrieve the optimal hyperparameters!
  ##Now, let's write a personalized cross validation code to be able to plot the hold-out set predictions of the cross-validation process.

  #let's create the 10 folds, and collect their indices
  #Before that, I will reset the row indices of df, since I will not link it back the full site_df dataframe.
  #The resetting of row indices is because when I selected relevant columns and used na.omit earlier, the row indices are kept from the original dataframe
  rownames(df) <- NULL
  df$rowindex <- as.numeric(rownames(df))
  cv_folds <- caret::createFolds(df$rowindex, k = 10, list = FALSE, returnTrain = F)
  df$rowindex <- NULL
  #Now let's put the folds indices into a column named folds in the dataframe df
  df$folds <- cv_folds

  #Now, we can loop through the previous indices and create a random forest on train set and predict the test set...
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)

  message("........Using now the best hyperparameters to gapfill each of the 10 folds........")

  `%dopar%` <- foreach::`%dopar%`

  pred_folds = foreach::foreach(fold = unique(df$folds), .packages = c("xgboost", "dplyr")) %dopar% {
#https://stackoverflow.com/questions/66661306/how-to-parallelize-an-xgboost-fit
    message(paste0("........Starting FOLD:", fold,"........"))

    df_subset=df %>% dplyr::filter(folds!=fold) #Since this is the training, I exclude the current fold
    xgb_train = xgboost::xgb.DMatrix(data = as.matrix(df_subset %>% dplyr::select(-dplyr::one_of(c(flux_col, datetime, "folds")))),
                            label = as.matrix(df_subset %>% dplyr::select(dplyr::one_of(flux_col))))
    #Let's train the random forest on all BUT the current fold
    xgb <- xgboost::xgb.train(
      params = list(booster = "gbtree",
                    objective="reg:squarederror",
                    eta = m$bestTune$eta,
                    gamma = m$bestTune$gamma,
                    max_depth = m$bestTune$max_depth,
                    min_child_weight = m$bestTune$min_child_weight,
                    subsample = m$bestTune$subsample,
                    colsample_bytree = m$bestTune$colsample_bytree),
      data = xgb_train,
      nrounds = m$bestTune$nrounds,
      verbose = 0
    )

    #Let's test the previous xgb on the current fold, and write it to the column flux_col_predicted in df

    # df[df$folds==fold, flux_col_pred] <- stats::predict(xgb, newdata = df[df$folds==fold,])
    stats::predict(xgb, newdata = xgboost::xgb.DMatrix(as.matrix(sapply(df[df$folds==fold,] %>%
                                                                          dplyr::select(-dplyr::one_of(c(flux_col, datetime, "folds"))), as.numeric))))

  }

  parallel::stopCluster(cl)
  foreach::registerDoSEQ()

  flux_col_pred = paste0(flux_col, '_predicted')
  df[,flux_col_pred] <- NA

  i = 0
  for (fold in unique(df$folds)){
    i = i+1
    df[df$folds==fold, flux_col_pred] <- pred_folds[[i]]
  }

  message("........End of cross validation........")


  #Let's calculate the out of sample metrics for the plot
  r_sq <- stats::cor(df[,flux_col_pred],df[,flux_col])^2
  #r_sq
  rmse <- Metrics::rmse(df[,flux_col_pred],df[,flux_col])
  #rmse



  ####NOW, let's fill the gaps. We have 2 options, either use directly the model produced using caret,
  #or fit a personalized model with the full dataset, but the result should be the same/very similar. I will fit it myself

  xgb_train_all = xgboost::xgb.DMatrix(data = as.matrix(df %>% dplyr::select(-dplyr::one_of(c(flux_col, datetime, "folds", flux_col_pred)))),
                          label = as.matrix(df %>% dplyr::select(dplyr::one_of(flux_col))))

  message("........Starting training the final model on all the dataset........")

  xgb_final <- xgboost::xgb.train(
    params = list(booster = "gbtree",
                  objective="reg:squarederror",
                  eta = m$bestTune$eta,
                  gamma = m$bestTune$gamma,
                  max_depth = m$bestTune$max_depth,
                  min_child_weight = m$bestTune$min_child_weight,
                  subsample = m$bestTune$subsample,
                  colsample_bytree = m$bestTune$colsample_bytree,
                  nthread = no_cores),
    data = xgb_train_all,
    nrounds = m$bestTune$nrounds,
    verbose = 0
  )

  message("........End of training the final model........")
  message("........Beginning prediction with the final model........")
  #Let's  all gaps now

  site_df$predicted <- stats::predict(xgb_final, newdata = xgboost::xgb.DMatrix(as.matrix(sapply(df_with_na %>%
                                                                                                             dplyr::select(-dplyr::one_of(c(flux_col, datetime))), as.numeric))))#this predicts for missing and non-missing values alike,

  # stats::predict(xgb, newdata = xgboost::xgb.DMatrix(as.matrix(sapply(df[df$folds==fold,] %>%
  #                                                                       dplyr::select(-dplyr::one_of(c(flux_col, datetime, "folds"))), as.numeric))))
  message("........End of prediction with the final model........")
  #and we can now take the values for filling the gaps

  #If the original flux data is available, we take that one for the new column. If instead it is missing, we use the predicted
  #Also, let's create a column for storing the quality (original, vs. gapfilled)
  #The logic here is that the new column will take measured values when they exist, and when they don't, use the predicted values to fill the gap!
  #We'll also add a quality column that makes it easy to know wether a value in the new column is original or gapfilled data.
  flux_col_var <- rlang::sym(flux_col)
  site_df <- site_df %>% dplyr::mutate(ch4_flux_final_filled = ifelse(!is.na(!!flux_col_var),!!flux_col_var,predicted),
                                       quality = ifelse(!is.na(!!flux_col_var),"original","gapfilled"))

  ######
  #Let's save the relevant objects
  output_list$tuningmodel <- m

  output_list$finalmodel <- xgb_final

  output_list$site_df <- site_df

  output_list$r_sq_cv <- round(r_sq,2)

  output_list$rmse_cv <- round(rmse,6)

  ######
  #Let's make some plots

  output_list$pred_meas <- ggplot2::ggplot(data=df, ggplot2::aes_string(x=flux_col,y=flux_col_pred))+
    ggplot2::geom_point(size=0.3)+
    ggplot2::theme_bw()+
    ggplot2::geom_abline(slope=1,intercept = 0, color="red")+
    ggplot2::xlab("Obs. CH4 (umol.m-2.s-1)")+  #("Obs. CH4 (mg.m-2.30min-1)")+
    ggplot2::ylab("Pred. CH4 (umol.m-2.s-1)")+ #("Pred. CH4 (mg.m-2.30min-1)")+
    ggplot2::annotate(geom="text", label=paste(sitename,", ", "r_sq=", round(r_sq,2), "\n", "rmse=",round(rmse,5), sep=""),
                      x=-Inf,y=Inf, hjust = 0, vjust = 1, color="red") +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA,colour="black",size=1.5)) +
    tune::coord_obs_pred()



  output_list$gf_meas_time <- ggplot2::ggplot(data=output_list$site_df, ggplot2::aes_string(x=datetime,y="ch4_flux_final_filled",color="quality"))+
    ggplot2::geom_point(size=0.3)+
    ggplot2::theme_classic()+
    ggplot2::scale_color_manual(values=c("red", "black"))+
    ggplot2::geom_abline(slope=1,intercept = 0, color="red")+
    ggplot2::xlab("Datetime")+
    ggplot2::ylab("CH4 (umol.m-2.s-1)")+  #("CH4 (mg.m-2.30min-1)")+
    ggplot2::ylim(-0.005,stats::quantile(site_df[,"ch4_flux_final_filled"], 0.999))+
    ggplot2::scale_x_datetime(breaks="1 month", date_labels ="%b")+
    ggplot2::annotate(geom="text", label=sitename,
                      x=min(site_df[,datetime]),y=Inf, hjust = 0, vjust = 1, color="red")+
    ggplot2::theme(legend.position = c(0.9,0.8), panel.border = ggplot2::element_rect(fill=NA,colour="black",size=1.5))

  return(output_list) #Now we can return everything needed for use outside the function

}
