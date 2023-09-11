#' Fit a model to standard curve data
#'
#' @param data table of standard measurements
#' @param method type of fit
#'
#' @return a model fit
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
fitStandardCurve <- function(data, method=c("linear", "quadratic"))
{
  ## evaluate choices
  method <- match.arg(method)
  if (method == "linear")
  {
    fit <- lm(value ~ concentration, data=data$standards)
  } else if (method == "quadratic")
  {
    fit <- lm(value ~ concentration + I(concentration^2), data=data$standards)
  }
  return(fit)
}
