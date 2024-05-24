#' REddyProc gapfilling of all using the old reddyproc_gapfiller_old, for H2O
#'
#' @param mc_sim_path path to the montecarlo simulated files
#' @param mc_sim_gf_path path where the gapfilled simulations will be saved
#' @param flux_col flux column, either of "NEE" or "H2O"
#' @param longi longitude of the site
#' @param lati latitude of the site
#' @param timez timezone as a signed integer (e.g. 1 for UTC+1)
#' @param gapfill_flux boolean. Default is TRUE. Set to false if gapfilling of NEE has been performed before and do only partitioning.
#'
#' @return no value returned, but files are gapfilled and saved to disk
#' @export
montecarlo_sim_noCH4_gf_old <- function(mc_sim_path,
                                    mc_sim_gf_path,
                                    flux_col=c("NEE","H2O"),
                                    longi=19.556646,
                                    lati=64.181980,
                                    timez=1,
                                    gapfill_flux = TRUE
                                    ){

  all_sim <- list.files(path=mc_sim_path, full.names = TRUE)

  #!!
  no_cores = parallel::detectCores() - 1
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  #!!

  `%dopar%` = foreach::`%dopar%`
  useless_output = foreach::foreach(file = all_sim, .packages = c("REddyProc", "data.table", "utils")) %dopar% {
    base_name_x <- gsub("\\.","_gf.", basename(file))

    reddyproc_gapfiller_old(formatted_file_path=file,
                        saving_folder=mc_sim_gf_path,
                        file_name=base_name_x,
                        FLUX=flux_col,
                        longitude = longi,
                        latitude=lati,
                        timezone = timez,
                        gapfill_flux = gapfill_flux)
  }

  #!!
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  #!!
}
