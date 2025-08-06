#' Segmented Regression Analysis with Visualization
#'
#' Performs segmented regression analysis to identify a single breakpoint in the
#' relationship between two variables and creates a visualization of the results.
#' The function fits a piecewise linear regression model and returns the breakpoint
#' coordinates along with a ggplot visualization.
#'
#' @param x A numeric vector of x-values (predictor variable)
#' @param y A numeric vector of y-values (response variable)
#' @return A list containing:
#'   \describe{
#'     \item{breakpoint_x}{The x-coordinate of the estimated breakpoint}
#'     \item{breakpoint_y}{The y-coordinate of the estimated breakpoint}
#'     \item{plot}{A ggplot object showing the data points, fitted segmented line, and breakpoint}
#'     \item{model}{The fitted segmented regression model object}
#'   }
#' @importFrom segmented segmented
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_vline geom_hline labs theme_test theme element_text
#' @importFrom stats lm predict
#' @export
#' @examples
#' # Example with sample data showing a clear breakpoint
#' x_data <- c(0.465, 0.574, 0.636, 0.686, 0.734, 0.782,
#'             0.837, 0.894, 0.960, 1.040, 1.163, 1.389)
#' y_data <- c(0.114, 0.141, 0.156, 0.162, 0.175, 0.181,
#'             0.191, 0.198, 0.207, 0.216, 0.237, 0.274)
#'
#' # Run segmented analysis
#' results <- segmented_analysis(x_data, y_data)
#'
#' # View breakpoint coordinates
#' cat("Breakpoint X:", round(results$breakpoint_x, 3), "\n")
#' cat("Breakpoint Y:", round(results$breakpoint_y, 3), "\n")
#'
#' # Display the plot
#' print(results$plot)
#'
#' # Access the underlying segmented model
#' summary(results$model)
segmented_analysis <- function(x, y) {
  # Input validation
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors")
  }

  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }

  if (length(x) < 4) {
    stop("At least 4 data points are required for segmented regression")
  }

  # Load required packages
  if (!requireNamespace("segmented", quietly = TRUE)) {
    stop("Package 'segmented' is required but not installed")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed")
  }

  # Create data frame
  data <- data.frame(x = x, y = y)

  data <- data[complete.cases(data),]

  x <- data$x
  y <- data$y

  # Fit linear model
  lm_model <- stats::lm(y ~ x, data = data)

  # Find breakpoint using segmented regression
  seg_model <- segmented::segmented(lm_model, seg.Z = ~x, npsi = 1)

  # Get breakpoint coordinates
  breakpoint_x <- seg_model$psi[1, "Est."]
  breakpoint_y <- stats::predict(seg_model, data.frame(x = breakpoint_x))

  # Create sequence for smooth fitted line
  x_seq <- seq(min(x), max(x), length.out = 100)
  y_pred <- stats::predict(seg_model, data.frame(x = x_seq))
  pred_data <- data.frame(x = x_seq, y = y_pred)

  # Create ggplot visualization
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 3, color = "black") +
    ggplot2::geom_line(data = pred_data, ggplot2::aes(x = x, y = y),
                       color = "red", linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = breakpoint_x, color = "blue",
                        linetype = "dashed", linewidth = 1) +
    ggplot2::geom_hline(yintercept = breakpoint_y, color = "blue",
                        linetype = "dashed", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = breakpoint_x, y = breakpoint_y),
                        color = "blue", size = 4, shape = 17) +
    # ggplot2::labs(title = "Segmented Regression Analysis",
    #               subtitle = paste0("Breakpoint: (", round(breakpoint_x, 3),
    #                                 ", ", round(breakpoint_y, 3), ")"),
    #               x = "X values",
    #               y = "Y values") +
    ggplot2::theme_test() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  # Return results as a list
  return(list(
    breakpoint_x = breakpoint_x,
    breakpoint_y = breakpoint_y,
    plot = p,
    model = seg_model
  ))
}
