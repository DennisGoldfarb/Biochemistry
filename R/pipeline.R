pipeline.BCA <- function(path, delim=",", tz="US/Central",
                     method=c("linear", "quadratic"),
                     dilution=1, desired_mg = 1, desired_units = c("uL", 'mL'))

{
  ## evaluate choices
  desired_units <- match.arg(desired_units)
  ## evaluate choices
  method <- match.arg(method)

  data <- formatVictor(path, delim=delim, tz=tz)
  fit <- fitStandardCurve(data, method=method)
  plotBCA(data, fit, dilution=dilution, desired_mg = desired_mg, desired_units = desired_units)
}
