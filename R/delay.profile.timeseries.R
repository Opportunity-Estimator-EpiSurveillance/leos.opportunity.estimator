#' @importFrom stringr str_split_fixed
NULL
#' Title delay.profile.timeseries
#'
#' Method to evaluate how delay profile changes through time, with varying time windows.
#'
#' @name delay.profile.timeseries
#'
#' @param dt data frame with at least the following ordered columns: location id [,1], epiweek [,2],
#'   and delay [,3] columns
#' @param trainning How many weeks from start of time series to use as training for
#' baseline quantiles for dealy distribution. Default: 52.
#'
#' @return Function \code{delay.profile.timeseries} returns a data.frame with quantiles
#' for historical delay by windows of 4, 8, 26, and 52 weeks.
#' Colum baseweek identify week of distribution calculation.
#' Columns start and end indicates notification date interval.
#' So, for baseweek 2014W01, start 2013W43, and end 2013W47 would report the
#' delay distribution for cases notified between 2013W43 and 2013W47 to be used as reference
#' for week 2014W01 delay pattern with a window of 4 weeks.
#'
#' @export
#'
delay.profile.timeseries <- function(dt, trainning=NULL){

  cnames <- colnames(dt)
  colnames(dt)[1:3] <- c('id', 'epiweek', 'delay')
  epiweek.fulllist <- unique(dt$epiweek[order(dt$epiweek)])
  if (is.null(trainning)){
    trainning = epiweek.fulllist[52]
  }

  epiweek.list <- epiweek.fulllist[epiweek.fulllist >= trainning]
  week <- epiweek.list[1]

  topquantile.trainning <- delay.profile(dt[dt$epiweek <= week, c('id', 'delay')])
  topquantile.trainning <- as.data.frame(matrix(unlist(topquantile.trainning), nrow=dim(topquantile.trainning)[1]), stringsAsFactors = F)
  colnames(topquantile.trainning) <- c('q5','q25', 'q50', 'q75', 'q95', 'id')

  topquantile.ts <- topquantile.trainning
  topquantile.ts$baseweek <- week
  topquantile.ts$start <- epiweek.fulllist[1]
  topquantile.ts$end <- epiweek.list[1]
  topquantile.ts$q95old <- topquantile.ts$q95
  topquantile.ts$window <- 'trainning'

  correct.week.shift <- function(y,wshift){
    while (wshift < 1){
      if (wshift == 0){
        year <- year - 1
        wshift <- as.integer(lastepiweek(year))
      } else if (wshift < 0){
        year <- year - 1
        wshift <- wshift + as.integer(lastepiweek(year))
      }
    }
    wshift <- paste0(year,'W',sprintf('%02d', wshift))
    return(wshift)
  }

  apply.delay.profile <- function(dt, ti, tf){
    id <- unique(dt$id)
    dt.slice <- dt[dt$epiweek >= ti & dt$epiweek <= tf, c('id', 'delay')]
    if (nrow(dt.slice) == 0){
      return(list(`5%`=NA, `25%`=NA, `50%`=NA, `75%`=NA, `95%`=NA, id=id))
    }
    res <- delay.profile(dt.slice)
    return(res)
  }

  id.list <- unique(topquantile.ts$id)
  for (id in id.list){
    q95 <- as.integer(topquantile.trainning$q95[topquantile.trainning$id == id])

    for (yearweek in epiweek.list){
      q95.old <- q95
      ywsplit <- stringr::str_split_fixed(yearweek, pattern = 'W', 2)
      year <- as.integer(ywsplit[1])
      week <- as.integer(ywsplit[2])
      wcut <- week - 2*q95
      tcut <- correct.week.shift(year, wcut)

      ywsplit <- stringr::str_split_fixed(tcut, pattern = 'W', 2)
      year <- as.integer(ywsplit[1])
      week <- as.integer(ywsplit[2])
      w <- week - c(4, 8, 26, 52)
      w <- sapply(X=w, correct.week.shift, y=year)

      topquantile <- t(sapply(X=w, FUN = function(y) apply.delay.profile(dt[dt$id == id, c('id', 'epiweek', 'delay')], ti=y, tf=tcut)))
      topquantile <- as.data.frame(matrix(unlist(topquantile), nrow=dim(topquantile)[1]), stringsAsFactors = F)
      colnames(topquantile) <- c('q5', 'q25', 'q50', 'q75', 'q95', 'id')
      topquantile$window <- c(4,8,26,52)
      topquantile$start <- w
      topquantile$end <- tcut
      topquantile$baseweek <- yearweek
      topquantile$q95old <- q95

      q95 <- as.integer(topquantile$q95[topquantile$window == 26])
      if (is.na(q95)){q95 <- q95.old}

      topquantile.ts <- rbind.data.frame(topquantile.ts, topquantile, make.row.names=F)
    }
  }
  topquantile.ts[, c('q5', 'q25', 'q50', 'q75', 'q95', 'q95old')] <- as.data.frame(lapply(topquantile.ts[, c('q5', 'q25', 'q50', 'q75', 'q95', 'q95old')], as.integer))
  topquantile.ts[,c('basedate', 'startdate', 'enddate')] <- as.data.frame(lapply(topquantile.ts[,c('baseweek', 'start', 'end')], episem2date))
  return(topquantile.ts)

}
