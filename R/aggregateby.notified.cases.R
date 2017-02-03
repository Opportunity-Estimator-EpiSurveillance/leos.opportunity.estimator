#' @include lastepiweek.R
NULL
#' Aggregate total notified cases per epiweekyear
#'
#'
#' @name aggregateby.notified.cases
#'
#' @param df.in Data frame object with first column providing location id and second column providing epiweekyear
#' @param current.epiweek Integer value specifing epiweek last entry to be considered. E.g, if latest epiyearweek is 2014W32,
#'   current.epiweek should be 32.
#' @param current.epiyear Integer value specifing epiyear last entry to be considered. E.g, if latest epiyearweek is 2014W32,
#'   current.epiweek should be 2014.
#'
#' @return Data frame with aggregate counts by epiyearweek
#'
#' @export
aggregateby.notified.cases <- function(df.in, current.epiweek, current.epiyear){
  df.in.weekly <- data.frame(table(df.in[[1]], df.in[[2]]))
  names(df.in.weekly) <- c('ID_MUNICIP', 'DT_NOTIFIC_epiyearweek', 'CASOS_NOTIFIC')
  df.in.weekly$epiweek <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][2]),
                             df.in.weekly$DT_NOTIFIC_epiyearweek)
  df.in.weekly$epiyear <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][1]),
                             df.in.weekly$DT_NOTIFIC_epiyearweek)
  # # Fill all epiweeks:
  fyear <- min(df.in.weekly$epiyear)
  min.week <- min(df.in.weekly$epiweek[df.in.weekly$epiyear == fyear])
  years.list <- c(fyear:current.epiyear)
  df.epiweeks <- data.frame(DT_NOTIFIC_epiyearweek=character(), UF=factor())
  # List of locations:
  mun_list <- unique(df.in.weekly$ID_MUNICIP)
  for (y in years.list){
    epiweeks <- c()
    fweek <- ifelse(y > fyear, 1, min.week)
    lweek <- ifelse(y < current.epiyear, as.integer(lastepiweek(y)), current.epiweek)
    for (w in c(fweek:lweek)){
      epiweeks <- c(epiweeks, paste0(y,'W',sprintf('%02d', w)))
    }
    for (mun in mun_list){
      df.epiweeks <- rbind(df.epiweeks, data.frame(list(DT_NOTIFIC_epiyearweek=epiweeks, ID_MUNICIP=mun)))
    }
  }

  df.in.weekly <- merge(df.epiweeks, df.in.weekly, by=c('DT_NOTIFIC_epiyearweek', 'ID_MUNICIP'), all.x=T)
  df.in.weekly[is.na(df.in.weekly$epiweek), 'epiweek'] <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][2]),
                                                         df.in.weekly[is.na(df.in.weekly$epiweek), 'DT_NOTIFIC_epiyearweek'])
  df.in.weekly[is.na(df.in.weekly$epiyear), 'epiyear'] <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][1]),
                                                         df.in.weekly[is.na(df.in.weekly$epiyear), 'DT_NOTIFIC_epiyearweek'])

  df.in.weekly[is.na(df.in.weekly)] <- 0

  return(df.in.weekly)
}
