% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episem2date.R
\name{episem2date}
\alias{episem2date}
\title{Function to obtain first day of corresponding Brazilian epidemiological
week provided}
\usage{
episem2date(epiyearweek, weekday = 0)
}
\arguments{
\item{epiyearweek}{Epidemiological week in the format "%Y\[*\]%W" where Y and W defined
by the Brazilian epidemiological week system. The separator between Y and W is irrelevant.
Ex.: 2014W02}

\item{weekday}{Week day to be used as representative of the epi. week. Uses Date week day
classification. 0: Sunday, 6:Saturday. Default: 0}
}
\value{
Date corresponding to the Sunday of epiyearweek
}
\description{
Function \code{episem2date} uses the Brazilian definition of epidemiological
week and returns the date of the corresponding o the provided epi. week, using
week day requested. Uses Sunday as default, the first week day by Brazilian epi.
week definition.
}
\examples{
epiyearweek <- '2014W02'
episem2date(epiyearweek)
}
