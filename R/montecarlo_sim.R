#' Generating n tables with each having a random laplacian noise added to the original fluxes, so called montecarlo simulation
#'
#' @param df_gf gapfilled flux dataframe. This would be retrieved for instance from the rf_gapfiller function
#' @param df_cv crossvalidation dataframe contaning predicted vs. observed.
#' @param flux_col column name of the non-gapilled fluxes
#' @param flux_pred_col column name of the predicted fluxes (predictions of the non-gap values)
#' @param preds same predictors used for gapfilling
#' @param datetime datetime column
#' @param n number of montecarlo simulations to run
#' @param saving_folder folder where the simulations will be saved
#' @param flux_sign
#'
#' @return a dataframe containing weekly sigma of residuals and corresponding mean fluxes used for finding the relationship between residuals sigma and flux mean.
#' @export
#'
montecarlo_sim <- function(df_gf, #the gapfilled flux dataframe. This would be retrieved for instance from the rf_gapfiller function
                          df_cv, #The cross validation output. Can be retrieved like this if not saved in the gapfilling list: ggplot2::ggplot_build(gapfilling_list_hals_xgb$pred_meas)$plot$data
                          flux_col="ch4_flux_final",
                           flux_pred_col="predicted",
                           datetime = "datetime",
                           preds, #same as used for gapfilling ("Ta_f","Ts_f","WTD_smooth","Pa_f","PARin_f","PARout_f","RH_f","VPD_f","P_f","yearly_sin","yearly_cos","delta")
                           n,
                           saving_folder,
                           flux_sign = "positive"#positive or both OR or empirical to use preexisting formula
){

  #initializing global variables
  x <- week <- residuals <- residuals_median<- NULL
  residual_list <- list()

  df_cv$residuals <- df_cv[,flux_col] - df_cv[,flux_pred_col]

  df_cv$week <- lubridate::week(df_cv[,datetime])
  # df_cv$year <- lubridate::year(df_cv[,datetime])
  df_cv_sum1 <- df_cv %>%
    dplyr::group_by(week) %>%
    dplyr::summarize(residuals_median = stats::median(residuals,na.rm=TRUE),
                     residuals_sigma = mean(abs(residuals - residuals_median), na.rm=TRUE)*sqrt(2)) #%>%

  df_cv_sum2 <- df_cv %>%
    dplyr::group_by(week) %>%
    # dplyr::summarize_(flux_mean = lazyeval::interp(~mean(var, na.rm = T), var = as.name(flux_pred_col)),
    #                   flux_median = lazyeval::interp(~stats::median(var, na.rm = T), var = as.name(flux_pred_col)))
    dplyr::summarize(flux_mean = mean(.data[[flux_pred_col]], na.rm = TRUE),
                     flux_median = stats::median(.data[[flux_pred_col]], na.rm = TRUE))


  df_cv_sum <- merge(df_cv_sum1, df_cv_sum2, all = TRUE)
  residual_list$df_cv_sum_Week <- df_cv_sum


  if(flux_sign == "positive")
  {

    plot(residuals_sigma~flux_mean, data=df_cv_sum)

    summary_lm <- summary(stats::lm(df_cv_sum[,"residuals_sigma"] ~ df_cv_sum[,"flux_mean"]))
    residual_list$slope <- summary_lm$coefficients[2,1]
    residual_list$intercept <- summary_lm$coefficients[1,1]
    graphics::abline(a=residual_list$intercept, b=residual_list$slope)
    ##############################

    #Now, let's subset the dataframe to only keep the datetime, flux column and predictors
    df_sub <- df_gf[,c(datetime,flux_col, preds)]

    for(i in 1:n){
      df_sub_i <- df_sub
      df_sub_i[,flux_col] <- unlist(lapply(df_sub[,flux_col], FUN=function(x) x + PostEddyPro::laprnd(0,sigma=residual_list$slope*x + residual_list$intercept)))
      data.table::fwrite(df_sub_i, file.path(saving_folder,paste0("output_", i, ".csv")),dateTimeAs = "write.csv")
    }

    return(residual_list)
  }

  if(flux_sign == "both")
  {
    #####
    plot(residuals_sigma~flux_median, data=df_cv_sum)
    abline(lm(residuals_sigma~flux_median, data=df_cv_sum[which(df_cv_sum$flux_median>=0),]))
    abline(lm(residuals_sigma~flux_median, data=df_cv_sum[which(df_cv_sum$flux_median<0),]))


    summary_lm_pos <- summary(stats::lm(df_cv_sum[which(df_cv_sum$flux_median>=0),"residuals_sigma"] ~ df_cv_sum[which(df_cv_sum$flux_median>=0),"flux_median"]))
    residual_list$slope_pos <- summary_lm_pos$coefficients[2,1]
    residual_list$intercept_pos <- summary_lm_pos$coefficients[1,1]
    graphics::abline(a=residual_list$intercept_pos, b=residual_list$slope_pos)

    summary_lm_neg <- summary(stats::lm(df_cv_sum[which(df_cv_sum$flux_median < 0),"residuals_sigma"] ~ df_cv_sum[which(df_cv_sum$flux_median < 0),"flux_median"]))
    residual_list$slope_neg <- summary_lm_neg$coefficients[2,1]
    residual_list$intercept_neg <- summary_lm_neg$coefficients[1,1]
    graphics::abline(a=residual_list$intercept_neg, b=residual_list$slope_neg)

    # summary_lm <- summary(stats::lm(df_cv_sum[,"residuals_sigma"] ~ df_cv_sum[,"flux_median"]))
    # residual_list$slope <- summary_lm$coefficients[2,1]
    # residual_list$intercept <- summary_lm$coefficients[1,1]
    # graphics::abline(a=residual_list$intercept, b=residual_list$slope)
    #
    # summary_lm_pos_ori <- summary(stats::lm(df_cv_sum[which(df_cv_sum$flux_median>=0),"residuals_sigma"] ~ df_cv_sum[which(df_cv_sum$flux_median>=0),"flux_median"] + 0))
    # residual_list$slope_pos_ori <- summary_lm_pos_ori$coefficients[1,1]
    # graphics::abline(a=0, b=residual_list$slope_pos_ori)
    ##############################

    #Now, let's subset the non-gapfilled dataframe to only keep the datetime, flux column and predictors
    df_sub <- df_gf[,c(datetime,flux_col, preds)]

    for(i in 1:n){
      df_sub_i <- df_sub
      df_sub_i[,flux_col] <- unlist(lapply(df_sub[,flux_col], FUN=function(x) {

        mod_flux <- NULL
        if(x >= 0 & !is.na(x)){
          mod_flux <-  x + PostEddyPro::laprnd(0,sigma=residual_list$slope_pos*x + residual_list$intercept_pos)
        } else if(x < 0 & !is.na(x)){
          mod_flux <- x + PostEddyPro::laprnd(0,sigma=residual_list$slope_neg*x + residual_list$intercept_neg)
        } else{
          mod_flux <- NA
        }
        return(mod_flux)
      }))

      data.table::fwrite(df_sub_i, file.path(saving_folder,paste0("output_", i, ".csv")),dateTimeAs = "write.csv")
    }

    return(residual_list)
  }

  if(flux_sign == "empirical")#This is the empirical formulas gotten from paper, but should not be applied blindly to all sites
  {
    #Now, let's subset the non-gapfilled dataframe to only keep the datetime, flux column and predictors
    df_sub <- df_gf[,c(datetime,flux_col, preds)]

    for(i in 1:n){
      df_sub_i <- df_sub
      df_sub_i[,flux_col] <- unlist(lapply(df_sub[,flux_col], FUN=function(x) {

        mod_flux <- NULL
        if(x >= 0 & !is.na(x)){
          mod_flux <-  x + PostEddyPro::laprnd(0,sigma=0.62+0.63*x)
        } else if(x < 0 & !is.na(x)){
          mod_flux <- x + PostEddyPro::laprnd(0,sigma=1.42-0.19*x)
        } else{
          mod_flux <- NA
        }
        return(mod_flux)
      }))

      data.table::fwrite(df_sub_i, file.path(saving_folder,paste0("output_", i, ".csv")),dateTimeAs = "write.csv")
    }
  }
}

