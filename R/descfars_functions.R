#' fars_read
#'
#' This function reads file from .csv input file.
#'
#' @source \url{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @importFrom   readr read_csv
#' @importFrom   dplyr tbl_df
#'
#' @param filename A character string with the name of the file to read
#' @param package_file
#'
#' @return  This function returns a tbl_df object
#' @return An error message if the data does not exist or a data frame with data readed from the csv input file.
#'
#' @section warning When the input file does not exist.
#'
#' @examples
#' \dontrun{fars_read("accident_2015.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' make_filename
#'
#' This function gets year and creates file name based on that input year.
#'
#' @param year  a numeric character representing the year.
#'
#' @return A character vector corresponding to the filenames for the  given year.
#'
#' @section warning When the input character cannot be converted to numeric character
#'
#' @examples
#' \dontrun{make_filename(2015)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata",
              sprintf("accident_%d.csv.bz2", year),
              package = "descfars",
              mustWork = TRUE)
}

#' fars_read_years
#'
#' This function reads data per input year and returns a tibble data matrix of 2 columns (Month,year).
#'
#' @importFrom dplyr mutate select
#' @importFrom  magrittr %>%
#'
#' @param years A numeric vector representing the years
#'
#' @return a tibble data-matrix of month and year and fatalities in that month
#'
#' @section warning when the input character cannot be numeric year or the year in the in the available data range: 2013-2015.
#'
#' @examples
#' \dontrun{fars_read_years(2015)}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = "year") %>%
        dplyr::select("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' fars_summarize_years
#'
#' This function gets the year in the datset and returns a tibble datamatrix of two columns showing the counts of fatalities by month
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @param years A numeric vector representing the years
#'
#' @return a tibble data-matrix of 12 by 2 (counts per month)
#'
#' @section warning when the input character cannot be numeric year or the year in the in the available data range: 2013-2015.
#'
#' @examples
#' \dontrun{fars_summarize_years(2015)}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = "n()") %>%
    tidyr::spread_("year", "n")
}

#' fars_map_state
#'
#' This function plots the map of pointwise car fatalities in given year at a given American state.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num A numeric vector representing the number of the American state: 1-51 @param years A numeric vector representing the years: 2013-2015
#' @param year the years of interest
#'
#' @return a map of American state and points of fatalities
#'
#' @section warning when the input characters  cannot be numeric year or numeric statenumber @section warning when the state.number in the in the available data range: 1-51. @section warning when the year in the in the available data range: 2013-2015.
#'
#'
#' @examples
#' \dontrun{fars_map_state(10,2015)}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
