#' Generating n tables with each having a random laplacian noise added to the original fluxes, so called montecarlo simulation
#'
#' @param df_gf gapfilled flux dataframe. This would be retrieved for instance from the rf_gapfiller function
#' @param flux_col column name of the non-gapilled fluxes
#' @param flux_pred_col column name of the predicted fluxes (predictions of the non-gap values)
#' @param preds same predictors used for gapfilling
#' @param datetime datetime column
#' @param n number of montecarlo simulations to run
#' @param saving_folder folder where the simulations will be saved
#'
#' @return this function will return nothing, but files should be saved in a specified folder
#' @export
montecarlo_sim_old <- function(df_gf, #the gapfilled flux dataframe. This would be retrieved for instance from the rf_gapfiller function
                           flux_col="ch4_flux_final",
                           flux_pred_col="predicted",
                           datetime = "datetime",
                           preds, #same as used for gapfilling ("Ta_f","Ts_f","WTD_smooth","Pa_f","PARin_f","PARout_f","RH_f","VPD_f","P_f","yearly_sin","yearly_cos","delta")
                           n,
                           saving_folder
){
  #initializing global variables
  x <- NULL
  # residual_list <- list()
  #Calculation of residuals
  df_gf$residuals <- df_gf[,flux_col] - df_gf[,flux_pred_col]

  ##############################
  # plot(df_gf[,"residuals"] ~ df_gf[,flux_col])
  # summary_lm <- summary(lm(df_gf[,"residuals"] ~ df_gf[,flux_col]))
  # residual_list$slope <- summary_lm$coefficients[2,1]
  # residual_list$intercept <- summary_lm$coefficients[1,1]
  # abline(a=residual_list$intercept, b=residual_list$slope)
  ##############################

  median <- stats::median(df_gf$residuals,na.rm=TRUE)
  sigma <- mean(abs(df_gf$residuals-median), na.rm=TRUE)*sqrt(2) #https://stats.stackexchange.com/questions/281682/how-to-fit-a-data-against-a-laplace-double-exponential-distribution-and-check?fbclid=IwAR19u5jiNeW4KvokCBToA4_io2tJXbihm0BLP4gYgDvIdc0HnVFuNyw9Mhc
  #Visualizing the distribution of residuals to make sure it follows the laplacian distribution

  graphics::hist(df_gf$residuals, breaks = 100, freq = F)

  graphics::curve(PostEddyPro::laplace(x,m=0,t=sigma/sqrt(2)), col = "red", add = TRUE)

  #Now, let's subset the dataframe to only keep the datetime, flux column and predictors
  df_sub <- df_gf[,c(datetime,flux_col, preds)]

  for(i in 1:n){
    df_sub_i <- df_sub
    df_sub_i[,flux_col] <- unlist(lapply(df_sub[,flux_col], FUN=function(x) x + PostEddyPro::laprnd(0,sigma=sigma)))
    data.table::fwrite(df_sub_i, file.path(saving_folder,paste0("output_", i, ".csv")),dateTimeAs = "write.csv")
  }
}

