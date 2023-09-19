pipeline.BCA <- function(path, delim=",", tz="US/Central",
                     method=c("linear", "quadratic"),
                     dilution=1, desired_mg = 1, desired_units = c("uL", 'mL'), save_path="./")

{
  ## evaluate choices
  desired_units <- match.arg(desired_units)
  ## evaluate choices
  method <- match.arg(method)

  data <- formatVictor(path, delim=delim, tz=tz)
  fit <- fitStandardCurve(data, method=method)
  fig <- plotBCA(data, fit, dilution=dilution, desired_mg = desired_mg, desired_units = desired_units)

  print(fig)
  saveFig(fig, save_path, stringr::str_c("BCA_", data$measurement_started_date), 11, 8.5)
  print(paste(save_path, stringr::str_c("BCA_", data$measurement_started_date), ".pdf", sep=""))
}

saveFig <- function(p, path, filename, height, width)
{
  createDir(path)
  ggplot2::ggsave(paste(path,filename, ".pdf", sep=""), width=width, height=height, compress=F, p)
}

# create directory if it doesn't exist
createDir <- function(path)
{
  if (!file.exists(path))
  {
    dir.create(path)
  }
}
