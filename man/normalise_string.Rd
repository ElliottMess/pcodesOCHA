% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{normalise_string}
\alias{normalise_string}
\title{Normalise string}
\usage{
normalise_string(string)
}
\arguments{
\item{string}{character string to be normalised. Accepts vectors too.}
}
\value{
normalise string
}
\description{
Normalise provided string by :
1. removing extra spaces ([stringr::str_squish()]) and [trimws()]) 2. passing
the string to lower case ([tolower()]) 3. removing all accents
([stringi::stri_trans_general()]), with id = "Latin-ASCII") 4. removing other
special characters ('[-',.()/ ]') and replacing with '_'
}
\examples{
normalise_string("Ã aF   kgfk")
}
