#' Calculate Vapor Pressure Deficit (VPD)
#'
#' This function calculates the Vapor Pressure Deficit (VPD) given air temperature
#' and relative humidity. VPD is the difference between the amount of moisture in
#' the air and how much moisture the air can hold when it is saturated, expressed
#' in kilopascals (kPa).
#'
#' @param Ta Numeric. Air temperature in degrees Celsius.
#' @param RH Numeric. Relative humidity as a percentage (0-100).
#'
#' @return Numeric. Vapor Pressure Deficit in kilopascals (kPa).
#'
#' @details
#' The function uses the Magnus formula to calculate saturation vapor pressure:
#' \deqn{e_s = 0.61365 \times \exp\left(\frac{17.502 \times T_a}{240.97 + T_a}\right)}
#'
#' Then calculates VPD as:
#' \deqn{VPD = e_s \times \left(1 - \frac{RH}{100}\right)}
#'
#' Where:
#' \itemize{
#'   \item \eqn{e_s} is saturation vapor pressure (kPa)
#'   \item \eqn{T_a} is air temperature (°C)
#'   \item \eqn{RH} is relative humidity (%)
#' }
#'
#' @examples
#' # Calculate VPD for 25°C and 60% relative humidity
#' VPD_calculator(25, 60)
#'
#' # Calculate VPD for multiple temperature and humidity combinations
#' temperatures <- c(20, 25, 30)
#' humidities <- c(50, 60, 70)
#' mapply(VPD_calculator, temperatures, humidities)
#'
#' # Calculate VPD for a range of conditions
#' VPD_calculator(Ta = c(15, 20, 25, 30), RH = c(40, 50, 60, 70))
#'
#' @references
#' Magnus, G. (1844). Versuche über die Spannkräfte des Wasserdampfs.
#' Annalen der Physik, 137(12), 225-247.
#'
#' @seealso
#' For more information on vapor pressure deficit and its applications in
#' plant physiology and meteorology, see relevant meteorological and
#' plant science literature.
#'
#' @export
VPD_calculator <- function(Ta,RH){
  es <- 0.61365*exp((Ta*17.502)/(240.97+Ta))
  VPD = es*(1-RH/100)
  return(VPD)
}
