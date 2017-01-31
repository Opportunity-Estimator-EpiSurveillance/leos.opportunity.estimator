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
  lyear <- as.integer(strsplit(today, 'W')[[1]][1])
  today.week <- as.integer(strsplit(today, 'W')[[1]][2])
  previous.week <- ifelse(today.week > 1, today.week-1, as.integer(lastepiweek(lyear-1)))
  previous.epiyearweek <- paste0(lyear,'W',today.week)
  return(previous.epiyearweek)
}
