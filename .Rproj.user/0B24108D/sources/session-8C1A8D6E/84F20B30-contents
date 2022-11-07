#' mmolH2O.m-2.s-1 to mm.30min-1 to mmH2O.30min-1 Converter (useful to have water fluxes that can be summed up directly to aggregate over day, month, year after gapfilling)
#'
#' @param h2o_flux single value or vector representing water fluxes in mmolH2O.m-2.s-1
#' @param M_h2O molar mass of water
#' @param mmol_mol constant for converting from mmol to mol
#' @param min30_s conversion factor of 30 min to seconds
#' @param mL_m3 constant for converting mililiters to cubic meter
#' @param m_mm constant for converting from meters to millimeters
#'
#' @return water fluxes in mm.30min-1 to mmH2O.30min-1, the same length as h2o_flux
#' @export
#'
#' @examples mmolH2Om2s_to_mm_30min(1)
mmolH2Om2s_to_mm_30min <- function(h2o_flux,
                                   M_h2O=18, #1 mol water = 18 mL
                                   mmol_mol=10^-3,
                                   min30_s=30*60,
                                   mL_m3=10^-6,
                                   m_mm=10^3
){
  #https://www.researchgate.net/post/Can_we_convert_transpiration_rate_as_mmole_m2_s_to_mm_of_water_transpired
  #note the m-2 simplifies with the m3 in the calculations
  h2o_flux*M_h2O*mmol_mol*min30_s*mL_m3*m_mm
}
