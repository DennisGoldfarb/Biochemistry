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
    fit <- lm(concentration ~ value, data=data)
    names(fit$coefficients) <- c("(Intercept)", "x")
  } else if (method == "quadratic")
  {
    fit <- lm(concentration ~ value + I(value^2), data=data)
    names(fit$coefficients) <- c("(Intercept)", "x", "x^2")
  }
  return(fit)
}
