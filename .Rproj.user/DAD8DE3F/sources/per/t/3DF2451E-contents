#' Check correlations and Gapfill environmental variables between sites
#'
#' @param datetime name of datetime, POSIXct object column in each table. This name should be the same between sites
#' @param df_to_fill dataframe containing the column to gapfill
#' @param ind_df1 first dataframe containing the column or columns to use in the regression
#' @param ind_df2 second dataframe containing the  column or columns to use in the regression
#' @param ind_df3 third dataframe containing the  column or columns  to use in the regression
#' @param col_to_fill name of the column in df_to_fill, with missing data to fill
#' @param ind_col1 default, same name as col_to_fill; is the first independent column name from either of ind_df1, ind_df2, ind_df3, to be used to fill gaps in col_to_fill. Regression will be used and one dataframe
#' @param ind_col2 second independent column, to be used in case first column is absent, or if multiple regression will be tried
#'
#' @return the same dataframe with the column gapfilled
#' @export
gapfill_between_sites <- function(datetime="datetime",
                                  df_to_fill,
                                  ind_df1,
                                  ind_df2=NULL,
                                  ind_df3=NULL,
                                  col_to_fill,
                                  ind_col1=col_to_fill,
                                  ind_col2=NULL)
{
  # datetime="datetime"
  # df_to_fill=stortjarn
  # ind_df1=halsingfors
  # ind_df2=halmyran
  # ind_df3=degero
  # col_to_fill="Ta"
  # ind_col1=col_to_fill
  # ind_col2="Ts"

  # datetime="datetime"
  # df_to_fill=halmyran_Ta_PARin_SWin
  # ind_df1=halsingfors
  # ind_df2=stortjarn
  # ind_df3=NULL
  # col_to_fill="WTD"
  # ind_col1=col_to_fill
  # ind_col2="Ts"


  #let"s make sure first that each datetime column is properly set

  df_to_fill[,datetime] <- as.POSIXct(df_to_fill[,datetime],tz="UTC")
  ind_df1[,datetime] <- as.POSIXct(ind_df1[,datetime],tz="UTC")
  ind_df2[,datetime] <- as.POSIXct(ind_df2[,datetime],tz="UTC")
  ind_df3[,datetime] <- as.POSIXct(ind_df3[,datetime],tz="UTC")


  df_to_fill_sub <- df_to_fill[,c(datetime,col_to_fill)] #let's copy the necessary columns

  ind_cols <- NULL
  ind_cols <- c(datetime,ind_col1) #getting all columns to select from the independent dataframes
  if(!is.null(ind_col2)){
    ind_cols <- c(datetime,ind_col1,ind_col2) #if there is another variable to use when a column is absent, add it to the columns to retrieve from the other dataframes
  }

  #Selecing  only the needed columns, and renaming them to be able to merge them all
  ind_df1_sub <- ind_df1[,intersect(ind_cols,colnames(ind_df1))]
  colnames(ind_df1_sub) <- c(datetime, paste(intersect(ind_cols,colnames(ind_df1)),1,sep="_")[-1])

  if(!is.null(ind_df2)){
    ind_df2_sub <- ind_df2[,intersect(ind_cols,colnames(ind_df2))]
    colnames(ind_df2_sub) <- c(datetime, paste(intersect(ind_cols,colnames(ind_df2)),2,sep="_")[-1])
  }
  if(!is.null(ind_df3)){
    ind_df3_sub <- ind_df3[,intersect(ind_cols,colnames(ind_df3))]
    colnames(ind_df3_sub) <- c(datetime, paste(intersect(ind_cols,colnames(ind_df3)),3,sep="_")[-1])
  }
  #merging all the dataframes
  list_to_merge <- list(df_to_fill_sub)
  if(exists("ind_df1_sub")){
    list_to_merge <- c(list_to_merge,list(ind_df1_sub))
  }
  if(exists("ind_df2_sub")){
    list_to_merge <- c(list_to_merge,list(ind_df2_sub))
  }
  if(exists("ind_df3_sub")){
    list_to_merge <- c(list_to_merge,list(ind_df3_sub))
  }
  # complete_df <- Reduce(function(x,y) merge(x = x, y = y,by=datetime, all.x=TRUE),
  #                       list_to_merge)

  complete_df <- Reduce(function(x,y) merge(x = x, y = y, all=T),
                        list_to_merge)

  complete_df <- unique(complete_df)
  # complete_df <- reduce(list_to_merge, left_join, by = "datetime")

  #checking which parameter (which param and which site) correlates best with the one with missing values
  all_cols_for_filling <- colnames(complete_df)[!colnames(complete_df) %in% c(datetime,col_to_fill)] #gets only the new column names from other sites

  corr <- sapply(all_cols_for_filling, function(x)
    stats::cor(complete_df[,col_to_fill], complete_df[,x], use="complete.obs"))

  # sorted_corr <- sort(corr,decreasing=T)
  o <- order(abs(corr), decreasing = TRUE)
  sorted_corr <- corr[o]

  print(sorted_corr)

  complete_df$filled <- complete_df[[col_to_fill]]
  for(current_best_var in names(sorted_corr)){
    # current_best_var="Ta_3"
    # current_best_var <- names(sorted_corr)[sorted_corr == r]
    print(current_best_var)
    na_count <- sum(is.na(complete_df$filled))
    print(na_count)
    if (na_count > 0) {
      lm.cf <- stats::lm(stats::reformulate(current_best_var, col_to_fill), complete_df)$coef
      calculated <- lm.cf[1] + complete_df[[current_best_var]]*lm.cf[2]
      complete_df$filled[which(is.na(complete_df$filled))] <- calculated[which(is.na(complete_df$filled))]
      #complete_df$filled <- ifelse(is.na(complete_df[[col_to_fill]]) & is.na(complete_df$filled), lm.cf[1] + complete_df[[current_best_var]]*lm.cf[2], complete_df[[col_to_fill]])
    }
  }
  names(complete_df)[names(complete_df)=="filled"] <- paste(col_to_fill,"_f",sep="")
  complete_df_filled_only <- complete_df[,setdiff(colnames(complete_df), c(col_to_fill,all_cols_for_filling))]

  # filled_col_only <- merge(df_to_fill[,c("datetime",col_to_fill)],complete_df_filled_only, all.x=T)

  df_to_return <- merge(df_to_fill,complete_df_filled_only, all=T)
  df_to_return <- unique(df_to_return)
  return(df_to_return)

}

