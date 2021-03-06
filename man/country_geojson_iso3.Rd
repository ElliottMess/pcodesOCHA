% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcodes_scrapping.R
\name{country_geojson_iso3}
\alias{country_geojson_iso3}
\title{Scrape geoJSON layers on a specific country by iso3 code}
\usage{
country_geojson_iso3(country_iso3)
}
\arguments{
\item{country_iso3}{character - A valid country {https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{iso3 code}}
}
\value{
list - a list for the specified URL containing two elements:
    - name : character - string identifying the name of the layer
    - data : data.frame - data frame containing the layer data.
}
\description{
Scrape geoJSON layers on a specific country by iso3 code
}
\examples{
\dontrun{
country_geojson_iso3("AFG")
}
}
