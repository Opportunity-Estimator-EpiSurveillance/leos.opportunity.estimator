#' @import INLA
NULL
#' Function to generate INLA estimates
#'
#' Function \code{generate.estimates} applies Leo's Method to a table with counts and delays
#'
#' @name generate.estimates
#'
#' @param delay.tbl.tmp Table with at least Dmax+1 columns. First column is the total count,
#'   followed by counts with delay 0, 1, 2, ...
#' @param Dmax Maximum delay to be taken into account. Data is assumed to be stable after that.
#' @param do.plots Wether to generate plots or not. Default: F
#' @param plot.folder name for plot subfolder. Default: 'tmp'
#'
#' @keywords internal
#' @author Leo Bastos, Marcelo F C Gomes

generate.estimates <- function(delay.tbl.tmp, Dmax, do.plots=F, plot.folder='tmp'){

  # Generate etimates for the previous Dmax weeks based on notification oportunity profile
  # found in delay.tbl.tmp
  delay.week <- paste0("d", 0:Dmax)
  delay.tbl.tmp <- delay.tbl.tmp[delay.week]

  # Total number of weeks
  Tmax <- nrow(delay.tbl.tmp)

  # Current time
  Tactual <- dim(delay.tbl.tmp)[1]

  # Prepare table structure dropping columns corresponding to delays greater than Dmax
  delay.tbl.tmp.obs <- delay.tbl.tmp[1:Tactual, (0:Dmax)+1]

  # Time index of the unknown counts (Tactual - (Dmax - 1), Tactual - (Dmax - 2), ..., Tactual)
  index.time <- (Tactual-Dmax+1):Tactual

  # Creating the run-off triangle data frame
  delay.tbl.tmp.obs.trian <- delay.tbl.tmp.obs
  delay.tbl.tmp.obs.trian[outer(1:Tactual, 0:Dmax, FUN = "+") > Tactual] <- NA

  # This function creates a data frame from the run-off triangle matrix to be used in INLA
  make.df.trian <- function(M){
    Time <- nrow(M)
    Delay <- ncol(M)
    aux.df <- data.frame(Y = as.vector(as.matrix(M)),
                         Time = rep(x = 1:Time, times = Delay),
                         Delay = rep(x = 0:(Delay-1), each=Time)
    )
    aux.df
  }

  # A <- data.frame(matrix(1:12, nrow=3))
  # as.vector(as.matrix(A))
  # make.df.trian(A)

  # Creating a data frame for INLA
  delay.inla.trian <- make.df.trian(delay.tbl.tmp.obs.trian)

  # Find the missing values
  index.missing <- which(is.na(delay.inla.trian$Y))

  # Model equation: intercept + time effect + delay effect
  model <- Y ~ 1 +
    f(Time, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001)))) +
    f(Delay, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001))))

  # Negative binomial likelihood fit using INLA
  output <- inla(model, family = "nbinomial", data = delay.inla.trian,
                 control.predictor = list(link = 1, compute = T),
                 control.compute = list( config = T, waic=TRUE, dic=TRUE),
                 control.family = list(
                   hyper = list("theta" = list(prior = "loggamma", param = c(0.1, 0.1)))
                 )
  )

  # If you want to compare models or extract the hyperparameters, return the following objects:
  waic <- output$waic$waic
  dic <- output$dic$dic
  hyperpar <-  output$summary.hyperpar

  ################## Plot: ################
  if (do.plots == T){
    plot.inla.re = function(outputRE, x = outputRE$ID){
      plot( x, y = outputRE$mean, type = "n", ylim = range(outputRE[,c(4,6)]), ylab="", xlab="" )
      polygon(x = c(x, rev(x)),
              y = c(outputRE$'0.025quant', rev(outputRE$'0.975quant')),
              border = "black", col = "gray")
      lines(x, outputRE$mean, lty=1, lwd=2)
      lines(x = range(x), y = rep(0,2), lty=2)
    }

    # Check if plot folder exists
    if (!dir.exists('./plots')) {
      dir.create(file.path('./plots'), showWarnings = FALSE)
    }
    if (!dir.exists(file.path('./plots', plot.folder))) {
      dir.create(file.path('./plots', plot.folder), showWarnings = FALSE)
    }

    svg(paste0('./plots/', plot.folder, '/time_effect.svg'))
    plot.inla.re(output$summary.random$Time)
    dev.off()

    svg(paste0('./plots/', plot.folder, '/delay_effect.svg'))
    plot.inla.re(output$summary.random$Delay)
    dev.off()

  }

  #################### Generate posterior samples ####################
  # Generate samples from parameters' posteriors from INLA model
  delay.samples.list <- inla.posterior.sample(n = 250, output)

  # Sampling the missing triangle from inla output in vector format from the model likelihood
  trian.sample <- lapply(X = delay.samples.list,
                         FUN = function(x, idx = index.missing)
                           rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1]))

  # Creating a vectorized version of the triangle matrix
  delay.vec.trian <- inla.matrix2vector(as.matrix(delay.tbl.tmp.obs.trian[index.time, ]))

  # Transforming back from the vector form to the matrix form
  delay.matrix.trian <- lapply(trian.sample, FUN = function(x, data = delay.vec.trian){
    data[which(is.na(data))] <- x
    inla.vector2matrix(data, ncol = Dmax+1) } )


  # Samples of {N_t : t=Tactual-Dmax+1,...Tactual}
  sampled.counts <- sapply(delay.matrix.trian, FUN = function(x) rowSums(x) )

  return(list(samples=sampled.counts, waic=waic, dic=dic, hyperpar=hyperpar))
}
