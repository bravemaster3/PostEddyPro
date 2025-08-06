#' Segmented Regression Analysis using Nonlinear Least Squares
#'
#' Performs segmented regression analysis to identify a single breakpoint in the
#' relationship between two variables using nonlinear least squares (NLS).
#' The function fits a piecewise linear regression model with different slopes
#' before and after the breakpoint, and returns the breakpoint coordinates along
#' with a ggplot visualization.
#'
#' @param x A numeric vector of x-values (predictor variable)
#' @param y A numeric vector of y-values (response variable)
#' @param t Initial guess for the x-coordinate of the breakpoint (default: 0.4)
#' @param a Initial guess for the slope before the breakpoint (default: -0.1)
#' @param b Initial guess for the y-value at the breakpoint (default: 0.04)
#' @param c Initial guess for the slope after the breakpoint (default: 0.2)
#'
#' @details
#' The function fits a piecewise linear model of the form:
#' \itemize{
#'   \item{For x < t: y = a*(x-t) + b}
#'   \item{For x >= t: y = c*(x-t) + b}
#' }
#' where t is the breakpoint location, b is the y-value at the breakpoint,
#' a is the slope before the breakpoint, and c is the slope after the breakpoint.
#'
#' The model is fitted using nonlinear least squares (nls), which requires
#' initial guesses for all parameters. Good initial values, especially for the
#' breakpoint location t, are crucial for convergence.
#'
#' @return A list containing:
#'   \describe{
#'     \item{breakpoint_x}{The x-coordinate of the estimated breakpoint}
#'     \item{breakpoint_y}{The y-coordinate at the estimated breakpoint}
#'     \item{plot}{A ggplot object showing the data points, fitted segmented line,
#'                  and breakpoint location with dashed reference lines}
#'     \item{model}{The fitted nls model object}
#'     \item{summary}{Summary statistics of the fitted model}
#'   }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_vline geom_hline theme_test theme element_text
#' @importFrom stats nls predict coef
#' @export
#'
#' @examples
#' # Example with sample data showing a clear breakpoint
#' x_data <- c(0.465, 0.574, 0.636, 0.686, 0.734, 0.782,
#'             0.837, 0.894, 0.960, 1.040, 1.163, 1.389)
#' y_data <- c(0.114, 0.141, 0.156, 0.162, 0.175, 0.181,
#'             0.191, 0.198, 0.207, 0.216, 0.237, 0.274)
#'
#' # Run segmented analysis with default initial values
#' results <- segmented_analysis(x_data, y_data)
#'
#' # View breakpoint coordinates
#' cat("Breakpoint X:", round(results$breakpoint_x, 3), "\n")
#' cat("Breakpoint Y:", round(results$breakpoint_y, 3), "\n")
#'
#' # Display the plot
#' print(results$plot)
#'
#' # Access the underlying model coefficients
#' coef(results$model)
#'
#' # Example with custom initial values for better convergence
#' results_custom <- segmented_analysis(x_data, y_data,
#'                                      t = 0.9,   # Breakpoint guess
#'                                      a = 0.2,   # Left slope guess
#'                                      b = 0.2,   # Y-value at breakpoint
#'                                      c = 0.3)   # Right slope guess
#'
#' @note
#' The convergence of the NLS algorithm depends heavily on the initial parameter values.
#' If the function fails to converge, try adjusting the initial guesses, particularly
#' the breakpoint location t, based on visual inspection of the data.
#'
segmented_analysis <- function(x, y, t = 0.4, a = -0.1, b = 0.04, c = 0.2) {

  # Check for required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed")
  }

  # Create data frame and clean
  data <- data.frame(x = x, y = y)
  data <- data[complete.cases(data), ]
  x <- data$x
  y <- data$y

  # Check we have enough data
  if (length(x) < 4) {
    stop("At least 4 data points are required for segmented regression")
  }

  # Define piecewise linear function
  f.lrp <- function(x, a, b, c, t) {
    ifelse(x < t, a * (x - t) + b, c * (x - t) + b)
  }

  # Fit the model
  xy <- data.frame(x = x, y = y)
  fitEC <- nls(y ~ f.lrp(x, a, b, c, t),
               data = xy,
               start = list(a = a, b = b, c = c, t = t))

  # Extract fitted parameters
  params <- coef(fitEC)
  breakpoint_x <- params["t"]
  breakpoint_y <- params["b"]

  # Generate predictions for smooth line
  xfit <- seq(min(x), max(x), len = 100)
  yfit <- predict(fitEC, data.frame(x = xfit))
  pred_data <- data.frame(x = xfit, y = yfit)

  # Create ggplot
  p <- ggplot2::ggplot(xy, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 3, color = "black") +
    ggplot2::geom_line(data = pred_data,
                       ggplot2::aes(x = x, y = y),
                       color = "red",
                       linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = breakpoint_x,
                        color = "blue",
                        linetype = "dashed",
                        linewidth = 1) +
    ggplot2::geom_hline(yintercept = breakpoint_y,
                        color = "blue",
                        linetype = "dashed",
                        linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = breakpoint_x, y = breakpoint_y),
                        color = "blue",
                        size = 4,
                        shape = 17) +
    ggplot2::theme_test() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  # Return results
  return(list(
    breakpoint_x = as.numeric(breakpoint_x),
    breakpoint_y = as.numeric(breakpoint_y),
    plot = p,
    model = fitEC,
    summary = summary(fitEC)
  ))
}
