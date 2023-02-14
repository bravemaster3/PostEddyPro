#' merge all gapfilled montecarlo simulations into a single file
#'
#' @param gf_type the type of gapfilling performed on the montecarlo simulations. Possible values are only "rf" or "REddyProc", which determine how to read the files
#' @param dir folder containing only the individual files to be merged
#' @param saving_dir folder where the merged file will be saved
#' @param datetime1 name of the timestamp column
#' @param filled_flux_col1 name of the filled flux column
#'
#' @return merged dataframe, also saved on disk
#' @export
merge_montecarlo_sims <- function(gf_type = "rf", #alternatively "REddyProc"
                                  dir,
                                  saving_dir,
                                  datetime1="datetime",
                                  filled_flux_col1="ch4_flux_final_filled"){
  datalist <- lapply(list.files(dir,full.names = TRUE), FUN = function(file_path,
                                                     datetime=datetime1,
                                                     filled_flux_col=filled_flux_col1,
                                                     gf_type2=gf_type
  ){
    if(gf_type2 == "rf"){
      df_i <- data.table::fread(file_path, header=TRUE,sep=",")
      df_i[,datetime] <- as.POSIXct(df_i[,datetime], format="%Y-%m-%d %H:%M:%S",tz="UTC")
    }

    if(gf_type2 == "REddyProc"){

      #Reading the REddyProc gapfilled dataframe
      headers <- colnames(data.table::fread(file_path))
      #units <- scan(path_to_file, what = character(), skip=1, nlines = 1, sep="\t")
      df_i <- data.table::fread(file_path, skip = 2, header = F)
      names(df_i) <- headers

      df_i <- as.data.frame(df_i)
      df_i[df_i== -9999] <- NA

      df_i$date <- as.Date(df_i$DoY, origin = paste0(df_i$Year-1,"-12-31"))
      df_i$time <- hms::hms(hours=df_i$Hour)

      df_i[,datetime] <- as.POSIXct(paste(df_i$date,df_i$time), format="%Y-%m-%d %H:%M:%S",tz="UTC")
    }

    df_i <- df_i[,c(datetime,filled_flux_col)]

    all_strings <- NA
    all_strings <- stringr::str_extract_all(basename(file_path),"\\(?[0-9]+\\)?")[[1]]#this is adapted when there is more than 1 number in the string
    df_i$iteration <- all_strings[length(all_strings)]#readr::parse_number(basename(file_path))
    return(df_i)

  })

  df_monte_carlo <- NULL
  df_monte_carlo <- Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
  df_mc_ordered  <- df_monte_carlo[order(df_monte_carlo[,datetime1], df_monte_carlo$iteration), ]

  data.table::fwrite(df_mc_ordered, file.path(saving_dir,"monte_carlo_all.csv"), dateTimeAs = "write.csv")
  return(df_mc_ordered)
}
