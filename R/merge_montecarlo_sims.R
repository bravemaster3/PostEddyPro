#' merge all gapfilled montecarlo simulations into a single file
#'
#' @param dir folder containing only the individual files to be merged
#' @param saving_dir folder where the merged file will be saved
#' @param datetime1 name of the timestamp column
#' @param filled_flux_col1 name of the filled flux column
#'
#' @return merged dataframe, also saved on disk
#' @export
merge_montecarlo_sims <- function(dir,
                                  saving_dir,
                                  datetime1="datetime",
                                  filled_flux_col1="ch4_flux_final_filled"){
  datalist <- lapply(list.files(dir,full.names = TRUE), FUN = function(file_path,
                                                     datetime=datetime1,
                                                     filled_flux_col=filled_flux_col1
  ){
    df_i <- utils::read.table(file_path, header=TRUE,sep=",")
    df_i[,datetime] <- as.POSIXct(df_i[,datetime], format="%Y-%m-%d %H:%M:%S",tz="UTC")
    df_i <- df_i[,c(datetime,filled_flux_col)]

    df_i$iteration <- readr::parse_number(basename(file_path))
    return(df_i)

  })

  df_monte_carlo <- NULL
  df_monte_carlo <- Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
  df_mc_ordered  <- df_monte_carlo[order(df_monte_carlo[,datetime], df_monte_carlo$iteration), ]

  data.table::fwrite(df_mc_ordered, file.path(saving_dir,"monte_carlo_all.csv"), dateTimeAs = "write.csv")
  return(df_mc_ordered)
}
