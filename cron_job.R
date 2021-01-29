if(!require("pcodesOCHA", character.only = TRUE)){
  remotes::install_github("ElliottMess/pcodesOCHA")
  library(pcodesOCHA)
}

library(pcodesOCHA)
library(readr)
library(purrr)

all_pcodes_df <- all_pcodes()
all_pcodes_df <- all_pcodes_df[,colSums(is.na(all_pcodes_df))<nrow(all_pcodes_df)] %>%
  distinct()

write_csv(all_pcodes_df, paste0("output/all_pcodes", ".csv"))

countries_list <- available_countries()

write_csv(countries_list, "output/available_countries.csv")

if(!dir.exists("output/countries")){
  dir.create("output/countries")
}

country_iso3 <- purrr::map(countries_list$country_iso3code,
                           ~ write_csv(country_pcodes_iso3(.x),
                                       paste0("output/countries/", .x,"_pcodes", ".csv")))
