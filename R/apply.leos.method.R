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

#' Method to generate estimates based on notification opportunity profile.
#'
#' Function \code{leos_method} applies the following method for the estimates:
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
#' @name apply.leos.method
#'
#' @param df.in Data frame with the FIRST THREE columns refering to [,1] location id, [,2] notification date,
#'   [,3] digitization date.
#' @param current.epiyearweek Most recent epidemiological week to be considered and estimated.
#'   Expected format YYYY*WW, e.g., 2010W03
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
#' apply.leos.method(opportunity.example.data, current.epiyearweek='2014W52', quantile.target=0.95)
#'
#' @author Marcelo F C Gomes \email{marcelo.gomes@@fiocruz.br}
#'
#' @export

apply.leos.method <- function(df.in, current.epiyearweek, quantile.target=.95, low.activity=NULL,
                              generate.plots=F){

  d <- df.in[,1:3]
  names(d) <- c('ID_MUNICIP', 'DT_NOTIFIC', 'DT_DIGITA')
  current.epiweek <- as.integer(stri_sub(current.epiyearweek, -2, -1))
  current.epiyear <- as.integer(stri_sub(current.epiyearweek, 1, 4))

  # Create columns with epiweek, epiyear, and epiyearweek for notification and digitalization ones:
  d <- cbind(d, t(sapply(d$DT_NOTIFIC,generate.columns.from.date)))
  names(d) <- sub("^epi", "DT_NOTIFIC_epi", names(d))
  d <- cbind(d, t(sapply(d$DT_DIGITA,generate.columns.from.date)))
  names(d) <- sub("^epi", "DT_DIGITA_epi", names(d))

  # Unlist new columns:
  for (c in grep('epi', names(d), value=TRUE)){
    d[[c]] <- unlist(d[[c]])
  }

  # Discar incomplete data from the current week for estimation:
  d <- d[d$DT_DIGITA_epiyear < current.epiyear | (d$DT_DIGITA_epiyear==current.epiyear & d$DT_DIGITA_epiweek<=current.epiweek), ]

  # Aggregate weekly data:
  d.weekly <- aggregateby.notified.cases(d[,c('ID_MUNICIP', 'DT_NOTIFIC_epiyearweek')],
                                       current.epiweek = current.epiweek, current.epiyear = current.epiyear)
  d.weekly$Situation <- 'stable'
  d.weekly[,c("mean","50%","2.5%","97.5%")] <- d.weekly$CASOS_NOTIFIC

  # Calculate opportunity between notification and upload:
  d$DelayWeeks <- d$DT_DIGITA_epiweek - d$DT_NOTIFIC_epiweek +
    (d$DT_DIGITA_epiyear - d$DT_NOTIFIC_epiyear)*as.integer(sapply(d$DT_NOTIFIC_epiyear,lastepiweek))

  # Discard notifications with delay greater than 6 months (> 26 weeks)
  d <- na.exclude(d[d$DelayWeeks < 27, ])

  # Grab target quantile from delay distribution for each UF
  delay.topquantile <- c(ceiling(with(d, tapply(DelayWeeks, ID_MUNICIP, FUN = function(x,...) max(8,quantile(x,...)),
                                                probs=quantile.target))))
  if (generate.plots){
    # Check if plot folder exists
    if (!dir.exists('./plots')) {
      dir.create(file.path('./plots'), showWarnings = FALSE)
    }
    # Load palette
    cores <- colorRampPalette((RColorBrewer::brewer.pal(9, 'Oranges')))(27)
  }

  # Prepare filled epiweeks data frame:
  # # Fill all epiweeks:
  fyear <- min(d$DT_NOTIFIC_epiyear)
  years.list <- c(fyear:current.epiyear)
  df.epiweeks <- data.frame(DT_NOTIFIC_epiyearweek=character())
  for (y in years.list){
    epiweeks <- c()
    lweek <- ifelse(y < current.epiyear, as.integer(lastepiweek(y)), current.epiweek)
    for (w in c(1:lweek)){
      epiweeks <- c(epiweeks, paste0(y,'W',sprintf('%02d', w)))
    }
    df.epiweeks <- rbind(df.epiweeks, data.frame(list(DT_NOTIFIC_epiyearweek=epiweeks)))
  }
  rownames(df.epiweeks) <- df.epiweeks$DT_NOTIFIC_epiyearweek

  # List of locations:
  mun_list <- unique(d$ID_MUNICIP)

  for (mun in mun_list){

    d.tmp <- droplevels(d[d$ID_MUNICIP==mun,])
    qthreshold <- delay.topquantile[as.character(mun)]

    # Prepare delay table
    aux <- tapply(d.tmp$DelayWeeks >= 0, INDEX = list(d.tmp$DT_NOTIFIC_epiyearweek), FUN = sum, na.rm = T)
    delay.tbl.tmp <- data.frame(Notifications = aux[order(rownames(aux))])

    for(k in 0:26){
      aux <- tapply(d.tmp$DelayWeeks == k, INDEX = d.tmp$DT_NOTIFIC_epiyearweek, FUN = sum, na.rm = T)
      delay.tbl.tmp[paste("d",k, sep="")] <- aux[order(rownames(aux))]
    }

    delay.tbl.tmp <- merge(df.epiweeks, delay.tbl.tmp, by=0, all.x=T)
    delay.tbl.tmp[is.na(delay.tbl.tmp)] <- 0
    rownames(delay.tbl.tmp) <- delay.tbl.tmp$Row.names

    if (generate.plots){
      if (!dir.exists(file.path('./plots',mun))) {
        dir.create(file.path('./plots',mun), showWarnings = FALSE)
      }
      # Plot UF's delay distribution
      svg(paste0('./plots/',mun,'/delay_pattern.svg'))
      histo <- hist(d.tmp$DelayWeeks, breaks=c(-1:27), plot=F)
      barplot.fig <- barplot(histo$density, xlab = "Delay (weeks)", ylab = "Notifications frequency",
                             xaxs='i', yaxs='i')
      abline(v=barplot.fig[qthreshold+1], col='gray')
      axis(1, at = barplot.fig, labels = c(0:(length(barplot.fig)-1)))
      text(x=barplot.fig[qthreshold+1], y=.55*max(histo$density), 'Dmax', srt=90, pos=2)
      dev.off()

      # Plot UF's time series
      svg(paste0('./plots/',mun,'/timeseries.svg'))
      # # Time series
      fyear = min(d.tmp$DT_NOTIFIC_epiyear)
      plot(delay.tbl.tmp$Notifications , type = "l", axes=F, xlab="Time", ylab="Notifications")
      axis(2)
      axis(1, at = seq(0,52*(current.epiyear-fyear+1),52) ,labels = fyear:(current.epiyear+1))
      dev.off()

      # Plot time series with delay profile
      svg(paste0('./plots/',mun,'/delay_timeseries.svg'))
      delay.week <- paste("d",0:26, sep="")
      barplot.fig <- barplot(t(as.matrix(delay.tbl.tmp[,delay.week])), beside = F, col=cores, axisnames = F,
                             xlab  =  "Time", ylab = "Notifications", border = NA)
      lines(x=barplot.fig,y=delay.tbl.tmp$d0, type = "l")
      axis(1, at = barplot.fig[seq(1,53*(current.epiyear-fyear+1),52)] , labels = c(fyear:(current.epiyear+1)) )
      #legend(x='topright', legend = c(seq(0,25,5)), fill=cores[seq(1,26,5)], pch = '.')
      dev.off()
    }

    ##################################################################
    # Preparing the data to be modelled
    ##################################################################

    # Time index of the unknown counts (Dmax+1,...,Tactual)
    mun.indexes <- rownames(d.weekly[d.weekly$ID_MUNICIP==as.character(mun),])
    Tactual <- length(mun.indexes)
    index.time <- mun.indexes[(Tactual-qthreshold+1):Tactual]

    if (!(mun %in% low.activity)) {

      # Calculate estimates
      df.tbl.tmp.estimates <- generate.estimates(delay.tbl.tmp, Dmax=qthreshold, do.plots=generate.plots, uf=mun)

      # Generate quantiles estimates
      aux2 <- round(t(apply(df.tbl.tmp.estimates$samples,1,FUN = post.sum)))

      # # Calculate corresponding incidence
      # years <- d.weekly[index.time, 'epiyear']
      # pop <- sapply(years, FUN=function(x) d_pop$Total[d_pop$`Código`==mun & d_pop$Ano==x])
      # aux2 <- aux2*100000/pop

      # For estimated region, update with obtained predictions
      d.weekly[index.time, 'Situation'] <- 'estimated'
      d.weekly[index.time,colnames(aux2)] <- aux2

      # # Calculate probability of falling in each activity region
      # # Obtain location's thresholds
      # mun.threshold <- as.numeric(df.thresholds[df.thresholds$ID_MUNICIP == as.character(mun), c("limiar pré-epidêmico",
      #                                                                                  "intensidade alta",
      #                                                                                  "intensidade muito alta")])
      # mun.threshold.absolute <- mun.threshold*d_pop[d_pop[,'Código']==mun & d_pop$Ano==current.epiyear, 'Total']/100000
      # d.weekly[index.time,thres.prob.cols] <- t(apply(df.tbl.tmp.estimates$samples,1,FUN = post.thresholds, lims = mun.threshold.absolute ))

    } else {
      d.weekly[index.time, 'Situation'] <- 'unknown'
    }

  }

  d.weekly[,'Run date'] <- Sys.Date()
  df.Dmax <- data.frame(list(ID_MUNICIP=names(delay.topquantile), epiyearweek=current.epiyearweek, Dmax=delay.topquantile, Execution=Sys.Date()))

  return(list(estimated.data.frame=d.weekly, delay.cutoff=df.Dmax))
}
