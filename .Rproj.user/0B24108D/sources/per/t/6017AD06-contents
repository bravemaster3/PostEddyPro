#' umolCO2.m-2.s-2 to gC.m-2.30min-1 Converter (useful to have CO2 fluxes that can be summed up directly to aggregate over day, month, year after gapfilling)
#'
#' @param co2_flux CO2 flux in umolCO2.m-2.s-2, a single value or a vector
#' @param umol_mol constant factor for converting umol to mol
#' @param M_C molar mass of carbon
#' @param min30_s conversion factor of 30 min to seconds
#'
#' @return CO2 flux in gC.m-2.30min-1, the same size as co2_flux
#' @export
#'
#' @examples umolCO2m2s_to_gCm2_30min(1)
umolCO2m2s_to_gCm2_30min <- function(co2_flux,
                                    umol_mol = 1e-06,
                                    M_C = 12.011,
                                    min30_s=30*60
){
  co2_flux * umol_mol * M_C * min30_s
}

