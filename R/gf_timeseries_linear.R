#' Function for gap-filling time series using linear interpolation based on dates.
#'
#' @param dates vector of dates without missing values
#' @param values vector of values with missing values to gapfill
#'
#' @return a vector of the same length as dates and values, where gaps are filled. outside gaps are not filled
#' @export
#'
#' @examples gf_timeseries_linear(as.Date(c("2023-07-01", "2023-07-02", "2023-07-03", "2023-07-07", "2023-07-08", "2023-07-10", "2023-07-11", "2023-07-15", "2023-07-16")),
#' c(NA,34, NA, NA, 5, NA, NA, 15, 9))

gf_timeseries_linear <- function(dates, values) {
  # Sort the data by dates
  sorted_data <- data.frame(dates = dates, values = values)
  sorted_data <- sorted_data[order(dates), ]

  # Perform linear interpolation for missing values
  interpolated_values <- stats::approx(x = sorted_data$dates, y = sorted_data$values, xout = dates)$y

  return(interpolated_values)
}
