#' Generating n tables with each having a random laplacian noise added to the original fluxes, so called montecarlo simulation (For ReddyProc)
#'
#' @param path_to_file path to the gapfilled flux dataframe (Reddyproc gapfilled)
#' @param flux_col original flux column: 'NEE' for CO2 flux OR 'H2O' for water flux
#' @param flux_pred_col gapfilled flux column: 'NEE_fall' for CO2 flux OR 'H2O_fall' for water flux
#' @param n Number of montecarlo iterations
#' @param saving_folder path to the folder where the simulations will be saved
#'
#' @return No value returned, but a file saved to disk
#' @export
montecarlo_sim_noCH4 <- function(path_to_file,
                                 flux_col=c('NEE','H2O'),
                                 flux_pred_col=c("NEE_fall", "H2O_fall"),
                                 n=100,
                                 saving_folder
                                 ){

  #initializing global variables
  df_gf <- NULL
  x <- week <- residuals <- residuals_median<- NULL
  residual_list <- list()

  #Reading the REddyProc gapfilled dataframe
  headers <- colnames(data.table::fread(path_to_file))
  units <- scan(path_to_file, what = character(), skip=1, nlines = 1, sep="\t")
  df_gf <- data.table::fread(path_to_file, skip = 2, header = F)
  names(df_gf) <- headers

  df_gf <- as.data.frame(df_gf)
  df_gf[df_gf== -9999] <- NA

  df_gf$date <- as.Date(df_gf$DoY, origin = paste0(df_gf$Year-1,"-12-31"))
  df_gf$time <- hms::hms(hours=df_gf$Hour)

  df_gf$datetime <- as.POSIXct(paste(df_gf$date,df_gf$time), format="%Y-%m-%d %H:%M",tz="UTC")


  #Calculation of residuals
  df_gf$residuals <- df_gf[,flux_col] - df_gf[,flux_pred_col]



  df_gf$week <- lubridate::week(df_gf[,"datetime"])
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
  df_sub <- df_gf[,c(headers)]

  for(i in 1:n){

    df_sub_i <- df_sub
    df_sub_i[,flux_col] <- unlist(lapply(df_sub[,flux_col], FUN=function(x) x + PostEddyPro::laprnd(0,sigma=residual_list$slope*x + residual_list$intercept)))

    saving_file_name <- file.path(saving_folder,  paste0("output_", flux_col,"_",i,".csv"))

    sink(saving_file_name)
    cat(cat(headers,sep="\t"),cat("\n"),cat(units, sep="\t"), sep = "\n")
    sink()
    data.table::fwrite(df_sub_i, file = saving_file_name, sep="\t", append = TRUE, col.names = FALSE)
  }

  return(residual_list)

}
