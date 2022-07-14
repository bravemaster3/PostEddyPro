#' Laplace distribution
#'
#' @param x x value, a single value or vector for which the laplace y value will be calculated
#' @param m position parameter
#' @param t scale parameter
#'
#' @return y, same dimension as x, the laplace "transformed" value or vector
#' @export
#'
#' @examples laplace(2,0,1)
laplace <- function(x,#x value for which the laplace y value will be calculated
                    m=0, #position parameter
                    t=1 #scale parameter
){
  y <- exp(-abs(x-m)/t )/2/t
  return(y)
}
