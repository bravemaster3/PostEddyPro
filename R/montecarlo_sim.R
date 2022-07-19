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
#' @return a dataframe containing weekly sigma of residuals and corresponding mean fluxes used for finding the relationship between residuals sigma and flux mean.
#' @export
montecarlo_sim <- function(df_gf, #the gapfilled flux dataframe. This would be retrieved for instance from the rf_gapfiller function
                           flux_col="ch4_flux_final",
                           flux_pred_col="predicted",
                           datetime = "datetime",
                           preds, #same as used for gapfilling ("Ta_f","Ts_f","WTD_smooth","Pa_f","PARin_f","PARout_f","RH_f","VPD_f","P_f","yearly_sin","yearly_cos","delta")
                           n,
                           saving_folder
){
  #initializing global variables
  x <- week <- residuals <- residuals_median<- NULL
  residual_list <- list()
  #Calculation of residuals
  df_gf$residuals <- df_gf[,flux_col] - df_gf[,flux_pred_col]

  ##############################
  # plot(df_gf[,"residuals"] ~ df_gf[,flux_col])
  # summary_lm <- summary(lm(df_gf[,"residuals"] ~ df_gf[,flux_col]))
  # residual_list$slope <- summary_lm$coefficients[2,1]
  # residual_list$intercept <- summary_lm$coefficients[1,1]
  # abline(a=residual_list$intercept, b=residual_list$slope)

  #Now let's get a relationship between weekly residuals sigma (laplacian distribution sigma) and mean fluxes for the same period. weeks all years together have been considered
  # df_gf$month <- lubridate::month(df_gf[,datetime])
  df_gf$week <- lubridate::week(df_gf[,datetime])
  # df_gf$year <- lubridate::year(df_gf[,datetime])
  df_gf_sum1 <- df_gf %>%
    dplyr::group_by(week) %>%
    dplyr::summarize(residuals_median = stats::median(residuals,na.rm=TRUE),
                     residuals_sigma = mean(abs(residuals - residuals_median), na.rm=TRUE)*sqrt(2)) #%>%

  df_gf_sum2 <- df_gf %>%
    dplyr::group_by(week) %>%
    dplyr::summarize_(flux_mean = lazyeval::interp(~mean(var, na.rm = T), var = as.name(flux_pred_col)))


  df_gf_sum <- merge(df_gf_sum1, df_gf_sum2, all = TRUE)
  residual_list$df_gf_sum_Week <- df_gf_sum

  plot(residuals_sigma~flux_mean, data=df_gf_sum)

  summary_lm <- summary(stats::lm(df_gf_sum[,"residuals_sigma"] ~ df_gf_sum[,"flux_mean"]))
  residual_list$slope <- summary_lm$coefficients[2,1]
  residual_list$intercept <- summary_lm$coefficients[1,1]
  graphics::abline(a=residual_list$intercept, b=residual_list$slope)
  ##############################

  #median <- stats::median(df_gf$residuals,na.rm=TRUE)
  #sigma <- mean(abs(df_gf$residuals-median), na.rm=TRUE)*sqrt(2) #https://stats.stackexchange.com/questions/281682/how-to-fit-a-data-against-a-laplace-double-exponential-distribution-and-check?fbclid=IwAR19u5jiNeW4KvokCBToA4_io2tJXbihm0BLP4gYgDvIdc0HnVFuNyw9Mhc
  #Visualizing the distribution of residuals to make sure it follows the laplacian distribution

  #graphics::hist(df_gf$residuals, breaks = 100, freq = F)

  #graphics::curve(PostEddyPro::laplace(x,m=0,t=sigma/sqrt(2)), col = "red", add = TRUE)

  #Now, let's subset the dataframe to only keep the datetime, flux column and predictors
  df_sub <- df_gf[,c(datetime,flux_col, preds)]

  for(i in 1:n){
    df_sub_i <- df_sub
    df_sub_i[,flux_col] <- unlist(lapply(df_sub[,flux_col], FUN=function(x) x + PostEddyPro::laprnd(0,sigma=residual_list$slope*x + residual_list$intercept)))
    data.table::fwrite(df_sub_i, file.path(saving_folder,paste0("output_", i, ".csv")),dateTimeAs = "write.csv")
  }

  return(residual_list)
}

