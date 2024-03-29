% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_posix.R
\name{str_posix}
\alias{str_posix}
\title{Wrapper for as.POSIXct to make conversion simpler}
\usage{
str_posix(str, format = "\%Y-\%m-\%d \%H:\%M:\%S", tz = "UTC")
}
\arguments{
\item{str}{string regresenting a timestamp}

\item{format}{format of the timestamp, default is "%Y-%m-%d %H:%M:%S"}

\item{tz}{time zone, default is "UTC"}
}
\value{
a POSIXct timestamp object
}
\description{
Wrapper for as.POSIXct to make conversion simpler
}
\examples{
str_posix("2024-01-01 12:30:00")
}
