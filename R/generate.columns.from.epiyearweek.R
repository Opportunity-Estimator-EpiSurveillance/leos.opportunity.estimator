#' Generate new columns epiweek and epiyear from epiyearweek
#'
#' Function \code{generate.columns.from.epiyearweek} creates new column from original one
#' written in the format YYYY*WW
#'
#' @name generate.columns.from.epiyearweek
#'
#' @param x String with date in the format YYYY-DD-MM or an object of class Date
#'
#' @return
#' \code{generate.columns.from.date} returns a named list with the values
#' epiweek, epiyear
#'
#' @examples
#' df <- data.frame(list(epiyearweek=c('2009W30', '2009W35', '2009W39', '2009W52')))
#' t(sapply(df$epiyearweek, generate.columns.from.epiyearweek))
#'
#' @export
generate.columns.from.epiyearweek <- function(x){

  epiweek.val <- as.integer(stri_sub(x, -2, -1))
  epiyear.val <- as.integer(stri_sub(x, 1, 4))

  return(list(epiweek=epiweek.val, epiyear=epiyear.val))
}
