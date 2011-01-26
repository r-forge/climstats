\name{spi}
\alias{spi.matrix}
\alias{spi}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Calculates the Standardized Precipitation Index (SPI) }


\description{
Calculates the standardized precipitation index (SPI).
}
\usage{
spi(ppt, na.rm = TRUE)
spi.matrix(ppt, na.rm=TRUE) 
}

\arguments{
  \item{ppt}{
A time series of precipitation amounts in a vector or a matrix with precipitation time series in
each column. 
}
  \item{na.rm}{
a logical indicating whether missing values should be removed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{spi}{returns the SPI value for a precipitation vector.}
  \item{spi.matrix}{returns a vector of
standardized precipitation indices for a precipitation matrix or the SPI value for
a precipitation vector.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jonathan A. Greenberg, Alison R. Mynsberge
}

