% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Capstone.R
\docType{data}
\name{GeomTimeline}
\alias{GeomTimeline}
\title{function to build ggproto for geom}
\format{An object of class \code{GeomTimeline} (inherits from \code{Geom}, \code{ggproto}) of length 7.}
\usage{
GeomTimeline
}
\description{
Here will be the usage of two functions, required to create custom geom: ggplot2:ggproto and geom_* (in this case geom_hurricane) in order to
create new geom_timeline, which shows magnitude and year of hurricane occured
}
\examples{
\dontrun{  ggplot2::layer(
   geom = GeomTimeline, mapping = mapping,
   data = data, stat = stat, position = position,
   show.legend = show.legend, inherit.aes = inherit.aes,
   params = list(na.rm = na.rm, ...))}
}
\keyword{datasets}
