\name{climstats-package}
\alias{climstats-package}
\alias{climstats}
\docType{package}

\title{ An overview of functions in climstats }

\description{
These functions are designed to prepare and summarize climate data time series (point based or raster) for use in, for instance, species distribution models.
}
\details{
\tabular{ll}{
Package: \tab climstats\cr
Type: \tab Package\cr
Version: \tab 0.6\cr
Date: \tab 2010-10-13\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}

\section{I. Download climate data}{
  \tabular{ll}{
    \code{\link{get_climate_data}}\tab Download or import climate datasets \cr
	}
}

\section{II. Standardize climate data}{
  \tabular{ll}{
  	\code{\link{temporal_sync_raster}}\tab Synchronize temporal grain and extent of data \cr 
  	\code{\link{spatial_sync_raster}}\tab Synchronize spatial grain and extent of data \cr
  }
}
\section{III. Calculate climate metrics}{
  \tabular{ll}{
  	\code{\link{eto}}\tab Calculate evapotranspiration \cr
  	\code{\link{eto_save}}\tab Calculate and save evapotranspiration \cr
  	\code{\link{water_balance_save}}\tab Calculate and save water balance \cr
  	\code{\link{climate_summaries}}\tab Summarize climate variables across time periods \cr
  	}
}
\author{
Jonathan Asher Greenberg

Maintainer: Jonathan Asher Greenberg <climstats@estarcion.net>
}
\references{
%~~ Literature or other references for background information ~~
}


\keyword{package}

