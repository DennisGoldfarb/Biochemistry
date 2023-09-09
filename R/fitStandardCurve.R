#' Read florescence data from PerkinElmer Victor and format it
#'
#' @param data table of standard measurements
#' @param method type of fit
#'
#' @return
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
  } else if (method == "quadratic")
  {
    fit <- lm(concentration ~ value + I(value^2), data=data)
  }
  return(fit)
}
