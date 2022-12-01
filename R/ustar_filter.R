#' A function for change point detection of the u* threshold for low turbulence
#'
#' @param fluxes_meteo dataframe containing both fluxes and soil/air temperature data
#' @param gas gas name as denoted in Eddypro (e.g. "ch4" or "co2",includes also non gas like LE)
#' @param Temp_col temperature column
#' @param number_Temp_classes number of bin of soil/air temperature to be used, default is 6
#' @param number_ustar_classes number of bin of ustar to be used, default is 20
#' @param ustar_maximum_limit an upper limit to exclude for ustar. This argument is particularly useful when abnormally high values are contained in ustar...
#' @param ustarSetThresh A given threshold for ustar filtering if that is known (e.g. 0.1). The unit is m/s
#'
#' @return a list containing the ustar threshold based on the change point detection method, and a vector of flags (1 means low turbulence or ustar below thresold and 0 means enough turbulence) the same length as the number of rows of the input data.frame
#' @export
ustar_filter <- function(fluxes_meteo,
                         gas,
                         Temp_col = "Ts",
                         number_Temp_classes=6,
                         number_ustar_classes = 20,
                         ustar_maximum_limit=0.8,
                         ustarSetThresh=NULL
){

  #print(ustarSetThresh)

    fluxes_meteo$Temp <- fluxes_meteo[,Temp_col]
    fluxes_meteo$month <- as.numeric(format(fluxes_meteo$datetime, format="%m"))
    fluxes_meteo$period <- NA

    fluxes_meteo$period[which(fluxes_meteo$month %in% c(1:4))] <- 1
    fluxes_meteo$period[which(fluxes_meteo$month %in% c(5:8))] <- 2
    fluxes_meteo$period[which(fluxes_meteo$month %in% c(9:12))] <- 3
    # fluxes_meteo$period[which(fluxes_meteo$month %in% c(4:12))] <- 1

    ########################################################################################################
    #U star filtering
    if(!is.null(fluxes_meteo$u.)) fluxes_meteo$u_star <- fluxes_meteo$u.
    if(!is.null(fluxes_meteo$`u*`)) fluxes_meteo$u_star <- fluxes_meteo$`u*`
  if(is.null(ustarSetThresh)){

    # fluxes_meteo$Temp <- fluxes_meteo$Temp-273.15      #converts temperature from K to C ALREADY DONE


    plot(fluxes_meteo[which(fluxes_meteo$daytime==0),paste0(gas,"_flux_final")]~fluxes_meteo[which(fluxes_meteo$daytime==0),"u_star"],
    xlim = c(0,0.8), ylim=c(-0.2, 0.2))
    plot(Temp~datetime, data=fluxes_meteo)

    #copying fluxes_meteo to keep it safe
    fluxes_meteo_copy <- fluxes_meteo[which(!is.na(fluxes_meteo[,paste0(gas,"_flux_final")]) & !is.na(fluxes_meteo$Temp)),]
    #doing a loop for each period and in each period, we'll include the whole thing
    ustar_threshold_sub <- c()
    ustar_threshold <- NULL
    for (period in unique(fluxes_meteo_copy$period)){
      #period=2
      fluxes_period <- fluxes_meteo_copy[which(fluxes_meteo_copy$period == period),]
      #selecting a subset of the dataframe for the u_star code
      fluxes_meteo_copy_subset <- subset(fluxes_period, select = c("datetime", paste0(gas,"_flux_final"), "u_star", "Temp"))
      fluxes_meteo_copy_subset$u_star[which(fluxes_meteo_copy_subset$u_star > ustar_maximum_limit)] <- NA
      #creating the 6 quantile classes of Temp
      Temp_breaks <- unname(stats::quantile(fluxes_meteo_copy_subset$Temp, probs = seq(0, 1, 1/number_Temp_classes), na.rm=T))


      #creating the dataframe to fill inside the loop:
      u_df <- data.frame(matrix(nrow=number_Temp_classes*number_ustar_classes,ncol=8))
      names(u_df) <- c("Temp_class_ind", "ustar_class_ind", "Temp_class_max", "Temp_class_center", "ustar_class_max", "ustar_class_center", paste0(gas,"_avg"), "prop_avg")


      #increment for filling the table
      inc <- 0
      vec_ustarmin <- c()

      counter1 <- 0
      counter2 <- 0 #counters for first u* in a given Temp class
      for(i in 1:number_Temp_classes){
        try({
          Temp1 <- Temp_breaks[i]
          Temp2 <- Temp_breaks[i+1]
          Temp_mid <- (Temp1+Temp2)/2

          if(i < number_Temp_classes) Temp_subset <- fluxes_meteo_copy_subset[which(fluxes_meteo_copy_subset$Temp >= Temp1 & fluxes_meteo_copy_subset$Temp < Temp2 ),]

          if(i==number_Temp_classes) Temp_subset <- fluxes_meteo_copy_subset[which(fluxes_meteo_copy_subset$Temp >= Temp1 & fluxes_meteo_copy_subset$Temp <= Temp2 ),] #Without these conditions, we would will miss the last value

          r <- abs(stats::cor(Temp_subset$u_star, Temp_subset$Temp, use = "complete.obs")) #let's compute the correlation coefficient that is used later to check that the correlation between ustar and Temp is low or inexistent
          print(paste(round(r,2),"|###|","r for ", round(Temp1,2),"-",round(Temp2,2)))

          plot(Temp_subset[!is.na(Temp_subset[,paste0(gas,"_flux_final")]),paste0(gas,"_flux_final")]~Temp_subset[!is.na(Temp_subset[,paste0(gas,"_flux_final")]),"u_star"],
               pch=16, cex=0.4, xlab="u*(m.s-1)",ylab = paste0(toupper(gas), " flux (umol m-2 s-1)"))

          pos_x <- graphics::par("usr")[2] - 0.2*(graphics::par("usr")[2]-graphics::par("usr")[1])
          pos_y <- graphics::par("usr")[4] - 0.1*(graphics::par("usr")[4]-graphics::par("usr")[3])
          graphics::text(pos_x, pos_y, paste("Temp: ", round(Temp1,2), "C - ", round(Temp2,2),"C", sep=""), cex=0.7, col = "red")


          ##creating the u_star breaks for the classes
          range_ustar <- range(Temp_subset$u_star[which(!is.na(Temp_subset[,paste0(gas,"_flux_final")]) & !is.na(Temp_subset$u_star))])
          ustar_breaks <- seq(range_ustar[1],range_ustar[2],by=(range_ustar[2]-range_ustar[1])/number_ustar_classes)


          counter1 <- counter1+1
          #starting the loop for ustars
          for (j in 1:number_ustar_classes){
            ustar1 <- ustar_breaks[j]
            ustar2 <- ustar_breaks[j+1]
            ustar_mid <- (ustar1+ustar2)/2

            if(j < number_ustar_classes) gas_avg <- mean(Temp_subset[which(Temp_subset$u_star>=ustar1 & Temp_subset$u_star<ustar2),paste0(gas,"_flux_final")], na.rm = T)

            if(j == number_ustar_classes) gas_avg <- mean(Temp_subset[which(Temp_subset$u_star>=ustar1 & Temp_subset$u_star<=ustar2),paste0(gas,"_flux_final")], na.rm = T)  #Without these conditions, we would will miss the last value

            #filling the table u_df
            inc=inc+1   #this increment will be used to fill the table correctly row-wise
            u_df[inc,] <- c(i, j, Temp2, Temp_mid, ustar2, ustar_mid, gas_avg, NA)

            #plotting the points of average methane flux with the center value of the u_star class

            graphics::points(ustar2, gas_avg, pch=16, cex=1, col="red")
            graphics::text(ustar2, gas_avg, labels=round(ustar2,2), cex=0.5, font=2,col="green", srt=270, adj =-0.8)

            #testing if the past average is greater than 0.95* the current average
            if(j>1){
              # print(paste("ch4_avg:", ch4_avg, sep = ""))
              #print(u_df$ch4_avg[which(u_df$Temp_class_ind==i & u_df$ustar_class_ind==j-1)])
              prop_avg <- u_df[which(u_df$Temp_class_ind==i & u_df$ustar_class_ind==j-1),paste0(gas,"_avg")]/gas_avg

              u_df$prop_avg[which(u_df$Temp_class_ind==i & u_df$ustar_class_ind==j-1)] <- prop_avg


              #print(prop_avg)
              if(!is.na(prop_avg) & prop_avg >= 0.95) {#then we can plot those points in blue
                if(counter2 < counter1) {#this is to collect only the first u_star value respecting the 95%...
                  counter2 = counter2 + 1

                  #Let's check that the correlation between ustar and Temp is less than 0.3

                  if(r<0.3) vec_ustarmin <- c(vec_ustarmin, u_df$ustar_class_max[which(u_df$Temp_class_ind==i & u_df$ustar_class_ind==j-1)])

                }

                graphics::points(u_df$ustar_class_max[which(u_df$Temp_class_ind==i & u_df$ustar_class_ind==j-1)],
                       u_df[which(u_df$Temp_class_ind==i & u_df$ustar_class_ind==j-1),paste0(gas,"_avg")],
                       pch=16, cex=1, col="blue")
              }
            }
            # summary <- summary(lm(Temp~u_star, data=Temp_subset))
            # print(cor(Temp_subset$u_star, Temp_subset$Temp, use = "complete.obs"))
          }
        })
      }

      print(stats::median(vec_ustarmin))
      #U_star threshold value
      ustar_threshold_sub <- c(ustar_threshold_sub, stats::median(vec_ustarmin))

    }

    ustar_threshold <- round(max(ustar_threshold_sub),1)
  } else {
    ustar_threshold <- ustarSetThresh
  }

  fluxes_meteo$flag_ustar <- NA

  fluxes_meteo$flag_ustar <- ifelse(fluxes_meteo$u_star <= ustar_threshold, 1, 0)

  return(list(ustar_thresh=ustar_threshold, flag_ustar=fluxes_meteo$flag_ustar))

}
