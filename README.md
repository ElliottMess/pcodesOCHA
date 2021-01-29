
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pcodesOCHA

<!-- badges: start -->

<!-- badges: end -->

The goal of pcodesOCHA is to scrap pcodes and geoJSON from [UNOCHA
Common Operational Datasets web
server](https://gistmaps.itos.uga.edu/arcgis/rest/services)

While the package can be used as a standalone, the scraped files are
available in output folder.

A Cron job runs once a month at 12:00 am to update the outputs thanks to
github actions.

# Functions available

  - all\_pcodes : list all pcodes availables
  - all\_pcodes\_feature\_servers : legacy all\_pcodes
  - available\_countries : list all available countries and their
    corresponding [ISO 3 country
    code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
  - country\_geojson\_URL : scrape geoJSON for a country from URL
  - country\_geojson\_iso3 : scrape geoJSON for a country from ISO 3
    code
  - country\_pcodes\_URL : scrape pcodes for a country from URL
  - country\_pcodes\_iso3 : scrape pcodes for a country from ISO 3 code
  - one\_URL\_geoJSON : scrape geoJSON from a specific URL for the API
  - one\_URL\_properties : scrape properties (pcodes) from a specific
    URL for the API

# Datasets available

the easiest way to use the data is probably by importing directly the
data from the raw data in the folder output: - CSV :
<https://raw.githubusercontent.com/ElliottMess/pcodesOCHA/master/output/all_pcodes_20210112.csv>
- JSON :
<https://raw.githubusercontent.com/ElliottMess/pcodesOCHA/master/output/all_pcodes_20210112.json>

For instance to read it with readr: pcodes \<-
readr::read\_csv(“<https://raw.githubusercontent.com/ElliottMess/pcodesOCHA/master/output/all_pcodes_20210112.csv>”)
