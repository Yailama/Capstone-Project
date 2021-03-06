% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Capstone.R
\docType{data}
\name{theme_timeline}
\alias{theme_timeline}
\title{modification of existing geom classic theme}
\format{An object of class \code{theme} (inherits from \code{gg}) of length 57.}
\usage{
theme_timeline
}
\description{
Here is slight modification of existing classic theme: everything is the same except default position of the legend: it is at the bottom, instead of right side
}
\examples{
\dontrun{cleaned_base\%>\%filter(YEAR>=2000 & YEAR<=2010 & !is.na(EQ_MAG_ML))\%>\%ggplot()+
                   geom_timeline(aes(fill=DEATHS, colour=DEATHS, x=YEAR, size=EQ_MAG_ML))+theme_timeline}
}
\keyword{datasets}
