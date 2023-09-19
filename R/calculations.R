getConcentration <- function(x, dilution)
{
  return(x * dilution)
}


getVolumeFor <- function(concentration, desired_mg, desired_units = c("uL", 'mL'))
{
  ## evaluate choices
  desired_units <- match.arg(desired_units)

  mL <- desired_mg/concentration

  if (desired_units == "uL")
  {
    return(mL * 1000)
  }

  else {
    return(mL)
  }
}

getCI <- function(data, fit, level=0.95, dilution=1, desired_mg=1, desired_units="uL")
{
  # determine if model is multi regression or nonlinear
  multi <- length(names(fit$coef)[-1]) > 1

  # take average
  sampleCI <- data$samples %>%
    dplyr::group_by(.data$condition)

  if (!multi) {
    # inverse predict linear
    sampleCI <- sampleCI %>%
      dplyr::summarize(descr = as.data.frame(investr::calibrate(fit, y0 = value, interval = "inversion", level = level)[1:3])) %>%
      tidyr::unpack(cols = descr)
  } else {
    # inverse predict nonlinear
    sampleCI <- sampleCI %>%
      dplyr::summarize(descr = as.data.frame(investr::invest(fit, y0 = value, interval = "inversion", level = level)[1:3])) %>%
      tidyr::unpack(cols = descr)
  }

  sampleCI$`Corrected estimate` <- getConcentration(sampleCI$estimate, dilution)
  sampleCI$`Corrected lower` <- getConcentration(sampleCI$lower, dilution)
  sampleCI$`Corrected upper` <- getConcentration(sampleCI$upper, dilution)

  sampleCI$Volume <- getVolumeFor(sampleCI$`Corrected estimate`, desired_mg, desired_units)

  return (sampleCI)
}
