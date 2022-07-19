#' umolCH4.m-2.s-2 to mgCH4.m-2.30min-1 Converter (useful to have methane fluxes that can be summed up directly to aggregate over day, month, year after gapfilling)
#'
#' @param ch4_flux methane flux in umolCH4.m-2.s-2, a single value or a vector
#' @param umol_mol constant factor for converting umol to mol
#' @param M_CH4 molar mass of methane
#' @param g_mg constant factor for converting g to mg
#' @param min30_s conversion factor of 30 min to seconds
#'
#' @return methane flux in mgCH4.m-2.30min-1, the same size as ch4_flux
#' @export
#'
#' @examples umolCH4m2s_to_mgCH4m2_30min(0.2)
umolCH4m2s_to_mgCH4m2_30min <- function(ch4_flux,
                                        umol_mol = 10^-6,
                                        M_CH4 = 16.04,
                                        g_mg = 10^3,
                                        min30_s=30*60
                                        ){
  ch4_flux* umol_mol * M_CH4 * g_mg * min30_s
}
