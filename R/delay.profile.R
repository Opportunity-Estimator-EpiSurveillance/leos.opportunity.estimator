#' Delay Profile
#'
#' @param dt Notification table with at least two columns identifying locality[[1]] and delay[[2]]
#' @param q List of quantiles to calculate. Default c(.5, .25, .5, .75, .95)
#'
#' @return named list with quantiles by locality
#' @export
#'
#'
delay.profile <- function(dt, q=c(.05, .25, .5, .75, .95)){
  # Grab target quantile from delay distribution for each UF
  cnames <- colnames(dt)
  delay.topquantile <- c(with(dt, tapply(get(cnames[2]), get(cnames[1]), FUN = function(x,...) ceiling(quantile(x,...)),
                                                probs=q)))
  delay.topquantile <- data.frame(t(data.frame(delay.topquantile, check.names = F)), check.names = F)
  delay.topquantile[, cnames[1]] <- rownames(delay.topquantile)

  return(delay.topquantile)
}
