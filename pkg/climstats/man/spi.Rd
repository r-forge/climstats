\name{spi}
\alias{spi}
\alias{spi.matrix}
\title{Calculates the Standardized Precipitation Index (SPI)}
\usage{
  spi(ppt, na.rm = TRUE)
}
\arguments{
  \item{ppt}{A time series of precipitation amounts in a
  vector or a matrix with precipitation time series in each
  column.}

  \item{na.rm}{a logical indicating whether missing values
  should be removed.}
}
\value{
  \item{spi}{returns the SPI value for a precipitation
  vector.} \item{spi.matrix}{returns a vector of
  standardized precipitation indices for a precipitation
  matrix or the SPI value for a precipitation vector.}
}
\description{
  Calculates the standardized precipitation index (SPI).
}
\author{
  Jonathan A. Greenberg, Alison R. Mynsberge
}
\keyword{climate}

