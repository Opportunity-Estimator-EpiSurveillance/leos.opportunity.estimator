#' Generate new columns epiweek and epiyear from epiyearweek
#'
#' Function \code{generate.columns.from.epiyearweek} creates new column from original one
#' written in the format YYYY*WW
#'
#' @name generate.columns.from.epiyearweek
#'
#' @param df.in Data frame object
#' @param target.col Name or position of column with values of type character in the format \%Y'W'\%w
#'
#' @return
#' \code{generate.columns.from.date} returns a data frame with the same columns as df.in plus columns
#'   epiweek and epiyear
#'
#' @examples
#' df <- data.frame(list(epiyearweek=c('2009W30', '2009W35', '2009W39', '2009W52')))
#' df.new <- generate.columns.from.epiyearweek(df, 'epiyearweek')
#'
#' @export
generate.columns.from.epiyearweek <- function(df.in, target.col){
  df.in[, 'epiweek'] <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][2]),
                                          df.in[, target.col])
  df.in[, 'epiyear'] <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][1]),
                                          df.in[, target.col])
  return(df.in)
}

