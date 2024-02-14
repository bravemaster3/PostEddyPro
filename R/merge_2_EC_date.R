#' Merge 2 Eddypro outputs, and choose to save the output to disk, but always return the merged dataframe. In this "date" version, you state the datetime where to merge, to remove overlaps
#'
#' @param path_EC1 path to the first eddypro full output file
#' @param path_EC2 path to the second eddypro full output file
#' @param datetime_merge timestamp as string, format "%YYYY-%mm-%dd %HH:%MM:%SS" at which to remove rows after -for df1- and rows before -for df2- . Note that the specified timestamp itself will be included in df2 and excluded from df1
#' @param date_format_EC1 date format of the date column in the first eddypro full output. If the eddypro output has not been altered, the date format is "\%Y-\%m-\%d", the default. If it has been opened in excel for instance, the date format is likely "\%d/\%m/\%Y"
#' @param date_format_EC2 date format of the date column in the second eddypro full output. If the eddypro output has not been altered, the date format is "\%Y-\%m-\%d", the default. If it has been opened in excel for instance, the date format is likely "\%d/\%m/\%Y"
#' @param check_write_df logical, TRUE if you want to save the output to disk, in which case you must also provide writing_path
#' @param writing_path string, folder to which the merged file will be saved. the file name "merged.csv" will be appended automatically to the folder path. The folder path should not end with a slash.
#'
#' @return the merged dataframe
#' @export
merge_2_EC_date <- function(path_EC1,
                       path_EC2,
                       datetime_merge,
                       date_format_EC1 = "%Y-%m-%d",
                       date_format_EC2 = "%Y-%m-%d",
                       check_write_df=FALSE,
                       writing_path = NULL){

  #Reading first EC file
  headers1 <- utils::read.csv(path_EC1, skip = 1, header = F, nrows = 1, as.is = T)
  fluxes_1 <- utils::read.table(path_EC1, sep=",", header = F,skip=3)
  colnames(fluxes_1)= headers1
  fluxes_1$date <-as.Date(fluxes_1$date, format=date_format_EC1)
  fluxes_1$datetime <- as.POSIXct(paste(fluxes_1$date, fluxes_1$time, sep=" "), format="%Y-%m-%d %H:%M", tz="UTC")
  fluxes_1[fluxes_1 == -9999] <- NA
  fluxes_1$datetime <- lubridate::round_date(fluxes_1$datetime, "30 minutes") #needed when somehow the minutes are not so precise on 30 min
  ####
  #Reading second EC file
  headers2 <- utils::read.csv(path_EC2, skip = 1, header = F, nrows = 1, as.is = T)
  fluxes_2 <- utils::read.table(path_EC2, sep=",", header = F,skip=3)
  colnames(fluxes_2)= headers2
  fluxes_2$date <-as.Date(fluxes_2$date, format=date_format_EC2)
  fluxes_2$datetime <- as.POSIXct(paste(fluxes_2$date, fluxes_2$time, sep=" "), format="%Y-%m-%d %H:%M", tz="UTC")
  fluxes_2[fluxes_2 == -9999] <- NA
  fluxes_2$datetime <- lubridate::round_date(fluxes_2$datetime, "30 minutes") #needed when somehow the minutes are not so precise on 30 min

  #removing rows after and before the datetime_merge
  datetime_merge <- str_posix(datetime_merge)

  fluxes_1 <- fluxes_1 %>%
    filter(datetime < datetime_merge)

  fluxes_2 <- fluxes_2 %>%
    filter(datetime >= datetime_merge)


  #Merging both EC files

  fluxes_df <- merge(fluxes_1, fluxes_2, all = TRUE)
  fluxes_df <- fluxes_df[order(fluxes_df$datetime), ]

  fluxes_df_copy <- fluxes_df
  if(isTRUE(check_write_df)){
    if(is.null(writing_path)) stop("please provide the path for saving the merged file")
    #saving the other headers for writing back a raw output-like...
    headers1_new <- c(as.character(headers1))
    headers2_new <- c(as.character(headers2))


    header_groups1 <- as.character(utils::read.csv(path_EC1, skip = 0, header = F, nrows = 1, as.is = T))
    header_groups1 <- gsub("NA","",header_groups1,fixed=TRUE)
    names(header_groups1) <- headers1_new[1:length(header_groups1)]

    header_groups2 <- as.character(utils::read.csv(path_EC2, skip = 0, header = F, nrows = 1, as.is = T))
    header_groups2 <- gsub("NA","",header_groups2,fixed=TRUE)
    names(header_groups2) <- headers2_new[1:length(header_groups2)]

    units1 <- as.character(utils::read.csv(path_EC1, skip = 2, header = F, nrows = 1, as.is = T))
    units1 <- gsub("NA","",units1,fixed=TRUE)
    names(units1) <- headers1_new[1:length(units1)]

    units2 <- as.character(utils::read.csv(path_EC2, skip = 2, header = F, nrows = 1, as.is = T))
    units2 <- gsub("NA","",units2,fixed=TRUE)
    names(units2) <- headers2_new[1:length(units2)]


    fluxes_df$datetime <- NULL

    all_headers <- colnames(fluxes_df)

    units <- lapply(all_headers, function(x){
      un1 <- as.character(units1[x])
      un2 <- as.character(units2[x])
      un <- unique(un1,un2)
      un[!is.na(un)]
    })
    units <- unlist(units)

    header_groups <- lapply(all_headers, function(x){
      hg1 <- as.character(header_groups1[x])
      hg2 <- as.character(header_groups2[x])
      hg <- unique(hg1,hg2)
      hg[!is.na(hg)]
    })
    header_groups <- unlist(header_groups)

    # test <- unique(c(as.character(units2["diag_77_mean"]),as.character(units1["diag_77_mean"])))
    #
    # test[!is.na(test)]
    #
    #
    #
    #
    # header <- unique(c(as.character(headers1),as.character(headers2)))
    # header_groups <- unique(c(header_groups2, header_groups2))
    # units <- unique(c(units1, units2))

    diff_n_units <- length(all_headers) - length(units)
    diff_n_hg <- length(all_headers) - length(header_groups)

    if(diff_n_units > 0) {
      units <- c(units,rep('--',diff_n_units))
    }
    if(diff_n_hg > 0) {
      header_groups <- c(header_groups,rep('',diff_n_hg))
    }
    colnames(fluxes_df) <- NULL

    fluxes_df[] <- lapply(fluxes_df, as.character)
    final_df <- rbind(header_groups, all_headers,units,fluxes_df)
    data.table::fwrite(final_df, file.path(writing_path,"merged.csv"),col.names = FALSE)
  }

  return(fluxes_df_copy)

}
