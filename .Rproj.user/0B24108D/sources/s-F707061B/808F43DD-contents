#' REddyProc gapfilling of all
#'
#' @param mc_sim_path path to the montecarlo simulated files
#' @param mc_sim_gf_path path where the gapfilled simulations will be saved
#' @param flux_col flux column, either of "NEE" or "H2O"
#' @param longi longitude of the site
#' @param lati latitude of the site
#' @param timez timezone as a signed integer (e.g. 1 for UTC+1)
#'
#' @return no value returned, but files are gapfilled and saved to disk
#' @export
montecarlo_sim_noCH4_gf <- function(mc_sim_path,
                                    mc_sim_gf_path,
                                    flux_col=c("NEE","H2O"),
                                    longi=19.556646,
                                    lati=64.181980,
                                    timez=1
                                    ){

  all_sim <- list.files(path=mc_sim_path, full.names = TRUE)


  lapply(all_sim, function(x){
    print(x)
    base_name_x <- gsub("\\.","_gf.", basename(x))

    reddyproc_gapfiller(formatted_file_path=x,
                        saving_folder=mc_sim_gf_path,
                        file_name=base_name_x,
                        FLUX=flux_col,
                        longitude = longi,
                        latitude=lati,
                        timezone = timez)
  })

}
