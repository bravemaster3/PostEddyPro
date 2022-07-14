
#' generate i.i.d.(Independent and identically distributed) laplacian random number drawn from laplacian distribution
#'
#' @param mu mean of the double exponential (laplacian) distribution
#' @param sigma standard deviation of the double exponential (laplacian) distribution
#'
#' @return laplace random noise
#' @export
#'
#' @examples laprnd(0,1)
laprnd <- function(mu, sigma){
#Read more here http://en.wikipedia.org./wiki/Laplace_distribution
  u = stats::runif(1)-0.5
  b = sigma / sqrt(2)
  y = mu - b * sign(u)* log(1- 2* abs(u))
  return(y)
}
