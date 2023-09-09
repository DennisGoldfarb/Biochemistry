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
  fit <- lm(concentration ~ value, data=data)

  return(fit)
}
