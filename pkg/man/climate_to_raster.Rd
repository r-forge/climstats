\name{climate_to_raster}
\alias{climate_to_raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
climate_to_raster(raster_files, raster_source, zname, zvalue, proj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raster_files}{
%%     ~~Describe \code{raster_files} here~~
}
  \item{raster_source}{
%%     ~~Describe \code{raster_source} here~~
}
  \item{zname}{
%%     ~~Describe \code{zname} here~~
}
  \item{zvalue}{
%%     ~~Describe \code{zvalue} here~~
}
  \item{proj}{
%%     ~~Describe \code{proj} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (raster_files, raster_source, zname, zvalue, proj) 
{
    require(raster)
    input_files = dir(dirname(raster_files), pattern = basename(raster_files), 
        full.names = TRUE)
    input_files_N = length(input_files)
    days_in_months = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
        30, 31)
    middays_in_months = ceiling(days_in_months/2)
    if (input_files_N == 0) {
        print("No files found, exiting")
        return()
    }
    if (raster_source == "prism_dem") {
        raster_object_from_files = stack(sort(input_files))
        projection(raster_object_from_files) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        raster_object_from_files@zname = "elev"
    }
    if (raster_source == "prism_800m") {
        raster_object_from_files = stack(sort(input_files))
        projection(raster_object_from_files) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        dates = vector("character", length = input_files_N)
        for (j in (1:input_files_N)) {
            temp_date_vector = as.numeric(strsplit(strsplit(basename(input_files[j]), 
                "_")[[1]][3], "[.]")[[1]])
            if (temp_date_vector[2] >= 1 & temp_date_vector[2] <= 
                12) {
                dates[j] = paste(temp_date_vector[1], temp_date_vector[2], 
                  middays_in_months[temp_date_vector[2]], sep = "-")
            }
            else {
                dates[j] = "NA"
            }
        }
        raster_object_from_files@zname = "Date/time"
        raster_object_from_files@zvalue = dates
    }
    if (raster_source == "narr") {
        if (input_files_N > 1) {
            print("For NARR, there should only be one input file (a .nc file), please fix...")
            return()
        }
        raster_object_from_files = brick(sort(input_files))
        projection(raster_object_from_files) = "+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546"
    }
    if (raster_source == "generic") {
        raster_object_from_files = stack(sort(input_files))
        if (!missing(proj)) {
            projection(raster_object_from_files) = proj
        }
        if (missing(zname)) {
            raster_object_from_files@zname = "NA"
        }
        else {
            raster_object_from_files@zname = zname
            if (zname == "months") {
                if (nlayers(raster_object_from_files) != 12) {
                  print("zname=months requires exactly 12 layers in the files and zvalue to be of length 12...")
                  return()
                }
                if (missing(zvalue)) {
                  raster_object_from_files@zvalue = as.character(1:12)
                }
                else {
                  raster_object_from_files@zvalue = zvalue
                }
            }
            if (zname == "Date/time") {
                if (missing(zvalue)) {
                  print("zname=Date/time requires a zvalue vector the length of the number of input files")
                  return()
                }
                else {
                  raster_object_from_files@zvalue = zvalue
                }
            }
        }
    }
    return(raster_object_from_files)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
