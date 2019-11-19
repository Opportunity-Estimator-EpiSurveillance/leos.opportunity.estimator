#' Function to obtain first day of corresponding Brazilian epidemiological
#' week provided
#'
#' Function \code{episem2date} uses the Brazilian definition of epidemiological
#' week and returns the date of the corresponding o the provided epi. week, using
#' week day requested. Uses Sunday as default, the first week day by Brazilian epi.
#' week definition.
#'
#' @name episem2date
#' @param epiyearweek Epidemiological week in the format "%Y\[*\]%W" where Y and W defined
#'  by the Brazilian epidemiological week system. The separator between Y and W is irrelevant.
#'  Ex.: 2014W02
#' @param weekday Week day to be used as representative of the epi. week. Uses Date week day
#' classification. 0: Sunday, 6:Saturday. Default: 0
#'
#' @return Date corresponding to the Sunday of epiyearweek
#' @export
#'
#' @examples
#' epiyearweek <- '2014W02'
#' episem2date(epiyearweek)
episem2date <- function(epiyearweek, weekday=0){
  # Separate year and week:
  epiyear <- stringi::stri_sub(epiyearweek, 1, 4)
  epiweek <- as.integer(stringi::stri_sub(epiyearweek, -2, -1))

  # Obtain sunday of first epiweek of epiyear
  day.one <- as.Date(paste(epiyear, '01', '01', sep='-'))
  day.one.week <- as.numeric(format(day.one,"%w")) # 0: sunday

  # Check wether week day of Jan 1st was before or after a Wednesday
  # and set the start of the epiyear accordingly
  first.epiweek.day <- ifelse (day.one.week <=3, day.one - day.one.week ,
                               day.one + (7 - day.one.week) )
  first.epiweek.day <- as.Date(first.epiweek.day,
                               origin = '1970-01-01')
  return(first.epiweek.day + 7*(epiweek-1) + weekday)
}
