#' @importFrom stringi stri_sub
#' @importFrom grDevices colorRampPalette dev.off svg
#' @importFrom graphics abline axis barplot hist lines plot polygon text
#' @importFrom stats na.exclude quantile rnbinom
#' @importFrom utils write.csv write.table
NULL
#' @import RColorBrewer scales
NULL
#' @include episem.R lastepiweek.R generate.estimates.R post.thresholds.R post.sum.R aggregateby.notified.cases.R
NULL

#' Method to generate estimates based on notification opportunity profile for a sequence of epidemiological weeks
#'
#' Function \code{recursively.apply.leos.method} applies the following method for the estimates:
#' Notification delay modelling
#' by Leo Bastos
#'
#' N_t - number of notified cases at time t
#'
#' Y_{t,d} - number of notified cases from time t with notification delay d
#'
#' D - maximum acceptable time delay
#'
#' N_t = Y_{t,0} + sum_{d=1}^{D} Y_{t,d}
#'
#' Y_{0,t} is known forall t
#'
#' If T is today, Y_{t,d} is unknown for all (t,d) such that t+d > T
#'
#' Contributtors:
#' Claudia T Codeço and Marcelo F C Gomes
#'
#' @name recursively.apply.leos.method
#'
#' @param df.in Data frame with the FIRST THREE columns refering to [,1] location id, [,2] notification date,
#'   [,3] digitization date, unless all(c('ID_MUNICIP', 'DT_NOTIFIC', 'DT_DIGITA') \%in\% names(df.in)) == T
#' @param epiyearweek.list List like object with epidemiological weeks to be estimated
#' @param quantile.target Quantile to be used to determine Dmax from delay profile. Default: 0.95
#' @param low.activity List of location id's not to be estimated due to low activity. Default: NULL
#' @param generate.plots Boolean object to determine wether function should generate and save plots or not. Default: F
#'
#' @return Function \code{apply.leos.method} returns a list containing the following components:
#' \item{estimated.data.frame}{Data frame containing the weekly aggregate of df.in, plus columns with estimate mean,
#' quantiles 2.5\%, 50\% and 97.5\% and other relevant info}
#' \item{delay.cutoff}{Data frame with Dmax obtained for each locality, epiyearweek used as cutoff and execution date}
#'
#' @examples
#' data(opportunity.example.data)
#' epiyearweek.list <- c('2014W30', '2014W31', '2014W32', '2014W33')
#' res <- recursively.apply.leos.method(opportunity.example.data,
#'  epiyearweek.list=epiyearweek.list, quantile.target=0.95)
#'
#' @author Marcelo F C Gomes \email{marcelo.gomes@@fiocruz.br}
#'
#' @export

recursively.apply.leos.method <- function(df.in, epiyearweek.list, quantile.target=.95, low.activity=NULL,
                              generate.plots=F){

  d <- df.in
  target.cols <- c('ID_MUNICIP', 'DT_NOTIFIC', 'DT_DIGITA')
  if (!all(target.cols %in% names(d))){
    names(d)[1:3] <- c('ID_MUNICIP', 'DT_NOTIFIC', 'DT_DIGITA')
  }

  # Create columns with epiweek, epiyear, and epiyearweek for notification and digitalization ones:
  target.cols <- c('DT_NOTIFIC_epiyearweek', 'DT_NOTIFIC_epiweek', 'DT_NOTIFIC_epiyear')
  if (!all(target.cols %in% names(d))){
    d <- generate.columns.from.date(d, 'DT_NOTIFIC')
    names(d) <- sub("^epi", "DT_NOTIFIC_epi", names(d))
  }
  target.cols <- c('DT_DIGITA_epiyearweek', 'DT_DIGITA_epiweek', 'DT_DIGITA_epiyear')
  if (!all(target.cols %in% names(d))){
    d <- generate.columns.from.date(d, 'DT_DIGITA')
    names(d) <- sub("^epi", "DT_DIGITA_epi", names(d))
  }

  # Aggregate weekly data up to last notification:
  last.index <- length(epiyearweek.list)
  last.epiyearweek <- max(d$DT_NOTIFIC_epiyearweek)
  last.epiweek <- as.integer(stri_sub(last.epiyearweek, -2, -1))
  last.epiyear <- as.integer(stri_sub(last.epiyearweek, 1, 4))

  d.weekly <- aggregateby.notified.cases(d[,c('ID_MUNICIP', 'DT_NOTIFIC_epiyearweek')],
                                         current.epiweek = last.epiweek, current.epiyear = last.epiyear)
  d.weekly$Situation <- 'stable'
  d.weekly[,c("mean","50%","2.5%","97.5%")] <- d.weekly$CASOS_NOTIFIC
  d.weekly[,'Run date'] <- Sys.Date()

  mun.list <- unique(d$ID_MUNICIP)
  tspawn <- epiyearweek.list
  mun.col <- rep(mun.list, length(tspawn))
  epiyearweek.col <- sort(rep(tspawn, length(mun.list)))
  delay.cutoff <- data.frame(list(ID_MUNICIP=mun.col, epiyearweek=epiyearweek.col, Dmax=NA, Execution=Sys.Date()),
                             stringsAsFactors = F)

  estimate.columns <- c("Situation","mean","50%","2.5%","97.5%", "Run date")
  current.epiyearweek <- epiyearweek.list[1]
  res <- apply.leos.method(d, current.epiyearweek=current.epiyearweek, quantile.target=quantile.target, low.activity=low.activity,
                           generate.plots=generate.plots)
  d.weekly[d.weekly$DT_NOTIFIC_epiyearweek <= current.epiyearweek, estimate.columns] <- res$estimated.data.frame[
    res$estimated.data.frame$DT_NOTIFIC_epiyearweek <= current.epiyearweek, estimate.columns]
  delay.cutoff[delay.cutoff$epiyearweek == current.epiyearweek, ] <- res$delay.cutoff

  previous.epiyearweek <- current.epiyearweek
  for (current.epiyearweek in epiyearweek.list[2:last.index]){
    res <- apply.leos.method(d, current.epiyearweek=current.epiyearweek, quantile.target=quantile.target, low.activity=low.activity,
                             generate.plots=generate.plots)

    cols.d.weekly <- (d.weekly$DT_NOTIFIC_epiyearweek > previous.epiyearweek &
                        d.weekly$DT_NOTIFIC_epiyearweek <= current.epiyearweek)
    cols.res <- (res$estimated.data.frame$DT_NOTIFIC_epiyearweek > previous.epiyearweek &
                   res$estimated.data.frame$DT_NOTIFIC_epiyearweek <= current.epiyearweek)
    d.weekly[cols.d.weekly, estimate.columns] <- res$estimated.data.frame[cols.res, estimate.columns]

    cols.delay.cutoff <- (delay.cutoff$epiyearweek > previous.epiyearweek &
                            delay.cutoff$epiyearweek <= current.epiyearweek)
    cols.res <- (res$delay.cutoff$epiyearweek > previous.epiyearweek &
                   res$delay.cutoff$epiyearweek <= current.epiyearweek)
    delay.cutoff[cols.delay.cutoff, ] <- res$delay.cutoff[cols.res, ]
  }
  return(list(estimated.data.frame=d.weekly, delay.cutoff=delay.cutoff))
}
