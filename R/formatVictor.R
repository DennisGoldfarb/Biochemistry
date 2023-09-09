#' Read florescence data from PerkinElmer Victor and format it
#'
#' @param path path to data file
#' @param delim file delimiter
#' @param tz time zone you want your datetimes to be in
#'
#' @return list of metadata and measurement table
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
formatVictor <- function(path, delim=",", tz="US/Central")
{
  # Read every line
  str_data <- readLines(path, warn=F)

  # Extract metadata
  protocol_name <- stringr::str_split_i(str_data[3], delim, 2)
  protocol_created_date <- lubridate::as_datetime(stringr::str_split_i(str_data[34], delim, 5), tz=tz)
  protocol_created_by <- stringr::str_split_i(str_data[35], delim, 2)
  measurement_started_date <- lubridate::as_datetime(stringr::str_split_i(str_data[5], delim, 5), tz=tz)
  plate_type <- stringr::str_split_i(str_data[42], delim, 2)
  plate_format <- stringr::str_split_i(str_data[43], delim, 2)
  plate_rows <- as.numeric(stringr::str_split_i(stringr::str_match(plate_format, "\\((.*)\\)")[2], "X", 1))
  plate_cols <- as.numeric(stringr::str_split_i(stringr::str_match(plate_format, "\\((.*)\\)")[2], "X", 2))

  # Extract measurements
  tmp_matrix <- matrix(unlist(stringr::str_split(str_data[16:23], ",")), nrow=plate_rows, byrow=T)
  rownames(tmp_matrix) <- tmp_matrix[,1]
  tmp_matrix <- tmp_matrix[,2:(plate_cols+1)]
  colnames(tmp_matrix) <- 1:plate_cols
  class(tmp_matrix) <- "numeric"
  measurements <- tmp_matrix

  # Extract plate map
  tmp_matrix <- matrix(unlist(stringr::str_split(str_data[63:70], ",")), nrow=plate_rows, byrow=T)
  rownames(tmp_matrix) <- tmp_matrix[,1]
  tmp_matrix <- tmp_matrix[,2:(plate_cols+1)]
  colnames(tmp_matrix) <- 1:plate_cols
  plate_map <- tmp_matrix

  # Format measurements into table
  data <- tibble::tibble(condition=array(plate_map), value=array(measurements)) %>%
    dplyr::filter(.data$condition != "X")

  data_standards <- data %>% dplyr::filter(stringr::str_detect(.data$condition, "^Standard"))
  data_samples <- data %>% dplyr::filter(!stringr::str_detect(.data$condition, "^Standard"))

  # extract concentration from standard
  data_standards <- data_standards %>%
    tidyr::separate_wider_delim(.data$condition, " ", names=c("condition", "concentration")) %>%
    dplyr::mutate_at("concentration", as.numeric)

  # create list of results
  full_data <- list("standards" = data_standards,
                    "samples" = data_samples,
                    "measurements" = measurements,
                    "plate_map" = plate_map,
                    "protocol_name" = protocol_name,
                    "protocol_created_date" = protocol_created_date,
                    "protocol_created_by" = protocol_created_by,
                    "measurement_started_date" = measurement_started_date,
                    "plate_type" = plate_type,
                    "plate_format" = plate_format)

  return(full_data)
}
