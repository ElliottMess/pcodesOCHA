
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pcodesOCHA

<!-- badges: start -->
![R-CMD-check](https://github.com/ElliottMess/pcodesOCHA/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

The goal of pcodesOCHA is to scrap pcodes from [UNOCHA Common
Operational Datasets web
server](https://gistmaps.itos.uga.edu/arcgis/rest/services)

There is just one function exported: all\_pcodes\_feature\_servers Other
helper functions are available.

One function implemented partially to scrap geoJSON files:
scrap\_country\_geojson

the easiest way to use the data is probably by importing directly the data from the raw data in the folder output:
https://raw.githubusercontent.com/ElliottMess/pcodesOCHA/master/output/all_pcodes_20210112.csv

For instance to read it with readr:
pcodes <- readr::read_csv("https://raw.githubusercontent.com/ElliottMess/pcodesOCHA/master/output/all_pcodes_20210112.csv")
