#' @importFrom stringi stri_pad_left
NULL
#' @include episem.R lastepiweek.R
NULL

#' Returns previous Brazilian epiweek from a given day.
#'
#' Function \code{previous.epiyearweek.from.date} calculates the previous Brazilian
#' epiweek from a given date passed in the format YYYY-MM-DD
#'
#' @name previous.epiyearweek.from.date
#'
#' @param day String with date in the format YYYY-DD-MM or an object of class Date
#'
#' @return string with epiweek
#'
#' @examples
#' day <- '2010-03-27'
#' previous.epiyearweek.from.date(day)
#'
#' @export
previous.epiyearweek.from.date <- function(day){
  today <- episem(day)
  today.epiyear <- as.integer(strsplit(today, 'W')[[1]][1])
  today.epiweek <- as.integer(strsplit(today, 'W')[[1]][2])
  previous.epiweek.week <- ifelse(today.epiweek > 1, today.epiweek-1, as.integer(lastepiweek(today.epiyear-1)))
  previous.epiweek.year <- ifelse(previous.epiweek.week < today.epiweek, today.epiyear, today.epiyear-1)
  previous.epiyearweek <- paste0(previous.epiweek.year,'W',stri_pad_left(previous.epiweek.week, 2, pad=0))

  return(previous.epiyearweek)
}
