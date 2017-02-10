#' @include episem.R
NULL

#' Generate new columns epiyearweek, epiweek and epiyear from date column
#'
#' Function \code{generate.columns.from.date} creates new column from original one
#' written in the format YYYY-MM-DD, using Brazilian epidemiological week system
#'
#' @name generate.columns.from.date
#'
#' @param df.in Data frame object
#' @param target.col Name or position of column with values of type Date or character in the format \%Y-\%m-\%d
#'
#' @return
#' \code{generate.columns.from.date} returns a new data frame with same columns of df.in plus columns epiyear, epiweek,
#'   epiyearweek. epiyearweek has the format \%Y'W'\%w (e.g., 2009W42)
#'
#' @examples
#' df <- data.frame(list(date=c('2009-08-01', '2009-09-01', '2009-10-01', '2010-01-01')))
#' df.new <- generate.columns.from.date(df, 'date')
#'
#' @export
generate.columns.from.date <- function(df.in, target.col){
  df.in$epiyearweek <- mapply(function(x) episem(x), df.in[, target.col])
  df.in$epiweek <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][2]),
                                          df.in[, 'epiyearweek'])
  df.in$epiyear <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][1]),
                                          df.in[, 'epiyearweek'])
  return(df.in)
}
