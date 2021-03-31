#' List countries with pcodes available
#'
#' @return data.frame - a dataframe containing with countries where pcodes are
#'     available.
#' @export
#'
#' @examples
#' \dontrun{
#' available_countries()
#' }

available_countries <- function(){
  COD_URL <- "https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External"

  iso3_df <- countrycode::codelist %>%
    select(iso3c, country.name.en) %>%
    distinct()

  COD_list <-read_html(COD_URL) %>%
    html_nodes("a")%>%
    xml_attr("href") %>%
    tibble(iso3c = .) %>%
    filter(str_detect(iso3c, "FeatureServer$")) %>%
    mutate(iso3c = str_remove_all(iso3c, "^/arcgis/rest/services/COD_External/"),
           iso3c = str_remove_all(iso3c, "_pcode/FeatureServer$")
    ) %>%
    left_join(iso3_df, by = c("iso3c")) %>%
    rename(country_name = country.name.en,
           country_iso3code = iso3c)

  return(COD_list)

}

#' Rename pcode dataframe with uniform column names
#'
#' @param pcodes data.frame containing pcodes extracted from OCHA server
#'
#' @return dataframe with uniform column names
#' @export
#'
#' @examples
#'
rename_pcodes <- function(pcodes){

  cols_names_admins <- c("admin2Name_en", "admin2Name_da", "admin2Pcode",
                      "admin2RefName", "admin2AltName1_en", "admin2AltName2_en",
                      "admin2AltName1_da",   "admin2AltName2_da" ,  "admin1Name_en",
                      "admin1Name_da", "admin1Pcode", "admin0Name_en", "admin0Name_da",
                      "admin0Pcode", "regionName_en",
                      "regionName_da", "regionCode", "admin3Name_en", "admin3Pcode",
                      "admin3RefName",  "admin3AltName1_en", "admin3AltName2_en",
                      "admin2Name_fr", "admin2AltName1_fr", "admin2AltName2_fr",
                      "admin1Name_fr", "admin0Name_fr", "admin3Name_fr", "admin3AltName1_fr",
                      "admin3AltName2_fr", "admin4Name_en", "admin4Pcode", "admin4RefName",
                      "admin4AltName1_en", "admin4AltName2_en", "admin3Name_es", "admin3AltName1_es",
                      "admin3AltName2_es", "admin2Name_es", "admin1Name_es", "admin0Name_es",
                      "admin4Name_fr", "admin4AltName1_fr", "admin4AltName2_fr","admin2AltName1_es",
                      "admin2AltName2_es","admin2Name_ar", "admin2AltName1_ar","admin2AltName2_ar",
                      "admin1Name_ar", "admin0Name_ar", "admin2Name_ka", "admin2AltName1_ka",
                      "admin2AltName2_ka","admin1Name_ka","admin0Name_ka", "admin1RefName",
                      "admin1AltName1_es","admin1AltName2_es","admin3Name_ht", "admin3AltName1_ht",
                      "admin3AltName2_ht","admin2Name_ht", "admin1Name_ht", "admin0Name_ht",
                      "admin2Name_fa", "admin2AltName1_fa","admin2AltName2_fa","admin1Name_fa",
                      "admin0Name_fa", "admin3Name_ar", "admin3AltName1_ar","admin3AltName2_ar",
                      "admin3Name_ru", "admin3Name_ky", "admin3AltName1_ru","admin3AltName2_ru",
                      "admin3AltName1_ky","admin3AltName2_ky","admin2Name_ru", "admin2Name_ky",
                      "admin1Name_ru", "admin1Name_ky", "admin0Name_ru", "admin0Name_ky","admin3Type_en",
                      "admin3Type_ru", "admin3Type_ky", "admin2Name_lo", "admin2AltName1_lo","admin2AltName2_lo",
                      "admin1Name_lo", "admin0Name_lo", "admin1AltName1_en","admin1AltName2_en","admin1AltName1_ar",
                      "admin1AltName2_ar", "admin4Name_si", "admin4Name_ta", "admin4AltName1_si","admin4AltName2_si",
                      "admin4AltName1_ta","admin4AltName2_ta", "admin3Name_si", "admin3Name_ta", "admin2Name_si",
                      "admin2Name_ta", "admin1Name_si", "admin1Name_ta", "admin0Name_si", "admin0Name_ta", "admin1Name_mn",
                      "admin1AltName1_mn","admin1AltName2_mn","admin0Name_mn",  "region_en", "admin3Name_pt",
                      "admin3AltName1_pt","admin3AltName2_pt","admin2Name_pt", "admin1Name_pt", "admin0Name_pt", "admin3Name_th",
                      "admin3AltName1_th","admin3AltName2_th", "admin2Name_th", "admin1Name_th",
                      "admin0Name_th", "admin4Name_ua", "admin4Name_ru", "admin4AltName1_ua", "admin4AltName2_ua",
                      "admin4AltName1_ru","admin4AltName2_ru","admin3Name_ua", "admin2Name_ua", "admin1Name_ua",
                      "admin0Name_ua", "admin2Name_vi", "admin2AltName1_vi","admin2AltName2_vi", "admin1Name_vi",
                      "admin0Name_vi") %>%
    as_tibble() %>%
    mutate(original_cols = value) %>%
    separate(value, into=c("admin_level", "language"), sep = "_", fill = "right" )

  col_names <- data.frame(names = names(pcodes)) %>%
    left_join(cols_names_admins, by = c("names" = "original_cols")) %>%
    mutate(admin_level = case_when(is.na(admin_level)~names,
                                   TRUE ~ admin_level)) %>%
    group_by(admin_level) %>%
    mutate(admin_level = case_when(
             n() > 1 & (language == "en"| is.na(language)) ~ admin_level,
             n() > 1 & language == "fr" ~ admin_level,
             n() > 1 & language == "es" ~ admin_level,
             n() > 1 & language == "pt" ~ admin_level,
             n() == 1 ~ admin_level,
             TRUE ~ names
           ))

  pcodes_reduced <- pcodes %>%
    select(any_of(col_names$names))

  names(pcodes_reduced) <- r3c(names(pcodes_reduced), col_names$names, col_names$admin_level)

  pcodes_renamed <- pcodes_reduced %>%
    mutate(
    across(matches("^admin[0-9]Name$"), normalise_adm, .names = "{.col}_ref"),
    across(matches("^admin[0-9]Pcode$"), toupper)
    )%>%
    relocate(any_of(sort(col_names$admin_level)))

  return(pcodes_renamed)
}

#' Scrap pcodes datasets from UNOCHA REST API
#'
#' @return a dataframe containing all pcodes at the lowest administrative level available
#' @export
#'
#' @examples
#' \dontrun{
#' all_pcodes()
#' }
all_pcodes <- function(){

  COD_URL <- "https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External"

  COD_list <-read_html(COD_URL) %>%
    html_nodes("a")%>%
    xml_attr("href")

  COD_list <- COD_list[grepl("FeatureServer$", COD_list)]

  all_dfs <- list()

  all_dfs <- lapply(COD_list, function(x){country_pcodes_URL(paste0("https://gistmaps.itos.uga.edu", x))}) %>%
    bind_rows()

  all_dfs_rearranged <- rename_pcodes(all_dfs)

  return(all_dfs_rearranged)

}

#' Scrap data properties from one URL on OCHA COD API
#'
#' @param layer_URL character - A valid FeatureServer layer directory URL.
#'     Example: "https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer/2"
#'
#' @return list - a list containing two elements:
#'     - name : character - string identifying the name of the layer
#'     - data : data.frame - data frame containing the layer data.
#' @export
#'
#' @examples
#' \dontrun{
#' one_URL_properties("https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer/2")
#' }

one_URL_properties <- function(layer_URL){

  url_name <- tryCatch({layer_URL%>%
      read_html() %>%
      html_nodes("h2") %>%
      as.character() %>%
      stringr::str_extract("(?<=: ).*?(?= \\()")},
      error=function(e){cat("URL not available for", layer_URL,":", conditionMessage(e), "\n")
      })

  query_URL <- paste0(layer_URL, "/query?where=OBJECTID%20%3E%200&outFields=%2A&returnGeometry=false&f=json")

  json <- tryCatch({fromJSON(query_URL)}, error=function(e){cat("URL not available for", layer_URL,":", conditionMessage(e), "\n")})

  properties <- json$features$attributes
  list_properties <- list(name = url_name, data = properties)
  return(list_properties)

}

#' Scrape all pcodes information on a specific country by iso3 code
#'
#' @param country_iso3 character - A valid country {https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{iso3 code}
#'
#' @return list - a list for the specified URL containing two elements:
#'     - name : character - string identifying the name of the layer
#'     - data : data.frame - data frame containing the layer data.
#' @export
#'
#' @examples
#' \dontrun{
#' country_pcodes_iso3("AFG")
#' }

country_pcodes_iso3 <- function(country_iso3){
  country_URL <- paste0("https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/",country_iso3,"_pcode/FeatureServer")
  country_data <- rename_pcodes(country_pcodes_URL(country_URL))
  return(country_data)
}

#' Scrape all pcodes information on a specific country
#'
#' @param country_URL character - A valid country FeatureServer directory URL.
#'     Example: "https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer"
#'
#' @return list - a list for the specified URL containing two elements:
#'     - name : character - string identifying the name of the layer
#'     - data : data.frame - data frame containing the layer data.
#' @export
#'
#' @examples
#' \dontrun{
#' country_pcodes_URL("https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer")
#' }

country_pcodes_URL <- function(country_URL){

  all_layers <- tryCatch({paste0(country_URL, "/layers")%>%
      read_html() %>%
      html_nodes("a")%>%
      xml2::xml_attr("href")},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")}
  )

  all_layers <- all_layers[grepl("[0-9]$", all_layers)]

  all_layers_available <- tryCatch({paste0(country_URL, "/layers")%>%
      read_html() %>%
      html_nodes("h3") %>%
      as.character()},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")}
  )

  if(is.null(all_layers) | is.null(all_layers_available) ){
    message("URL not available")
    return(NULL)
  }else{

    all_layers_names <-stringr::str_extract(all_layers_available,'(?<=">).*?(?=<)')
    all_layers_names <- all_layers_names[grepl("[0-9]$", all_layers_names)]
    all_layers_id <- stringr::str_extract(all_layers_available,"(?<=<\\/a> \\().*?(?=\\)<\\/h3>)")
    all_layers_id <- all_layers_id[1:length(all_layers_names)]

    admin_levels_nb <- as.numeric(unlist(str_extract_all(all_layers_names[grepl("^Admin[0-9]", all_layers_names)],  "[0-9]")))
    lowest_admin  <- all_layers_names[grepl(paste0("Admin",max(admin_levels_nb)), all_layers_names)]
    lowest_admin_id <- all_layers_id[grepl(lowest_admin, all_layers_names)]


    lowest_admin_layer <- all_layers[grepl(paste0(lowest_admin_id, "$"), all_layers)]
    lowest_admin_layer_URL <- paste0(country_URL, "/", lowest_admin_id)

    all_properties <- one_URL_properties(lowest_admin_layer_URL)

    return(all_properties$data)
  }
}

#' Scrap geoJSON from one URL on \href{https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External}{OCHA COD REST API}
#'
#' @param layer_URL character - A valid FeatureServer layer directory URL.
#'     Example: "https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer/2"
#'
#' @return geoJSON -  a geoJSON object containing the layer's features
#' @export
#'
#' @examples
#' \dontrun{
#' one_URL_geoJSON("https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer/2")
#' }
one_URL_geoJSON <- function(layer_URL){

  url_name <- tryCatch({layer_URL%>%
      read_html() %>%
      html_nodes("h2") %>%
      as.character() %>%
      stringr::str_extract("(?<=: ).*?(?= \\()")},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")
      })

  query_URL <- paste0(layer_URL, "/query?where=OBJECTID+>%3D+0&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=&resultOffset=&resultRecordCount=&returnTrueCurves=false&sqlFormat=none&f=geojson")
  readlines_geojson <- fromJSON(query_URL)

  return(readlines_geojson)

}


#' Scrape geoJSON features on a specific country
#'
#' @param country_URL character - A valid country FeatureServer directory URL.
#'     Example: "https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer"
#'
#' @return geoJSON -  a geoJSON object containing the layer's features
#' @export
#'
#' @examples
#' \dontrun{
#' country_geojson_URL("https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/ZMB_pcode/FeatureServer")
#' }
country_geojson_URL <- function(country_URL){

  all_layers <- tryCatch({paste0(country_URL, "/layers")%>%
      read_html() %>%
      html_nodes("a")%>%
      xml_attr("href")},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")}
  )

  all_layers <- all_layers[grepl("[0-9]$", all_layers)]

  all_layers_available <- tryCatch({paste0(country_URL, "/layers")%>%
      read_html() %>%
      html_nodes("h3") %>%
      as.character()},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")}
  )

  if(is.null(all_layers) | is.null(all_layers_available) ){
    message("URL not available")
    return(NULL)
  }else{

    all_layers_names <-stringr::str_extract(all_layers_available,'(?<=">).*?(?=<)')
    all_layers_names <- all_layers_names[grepl("[0-9]$", all_layers_names)]
    all_layers_id <- stringr::str_extract(all_layers_available,"(?<=<\\/a> \\().*?(?=\\)<\\/h3>)")
    all_layers_id <- all_layers_id[1:length(all_layers_names)]

    admin_levels_nb <- as.numeric(unlist(str_extract_all(all_layers_names[grepl("^Admin[0-9]", all_layers_names)],  "[0-9]")))
    lowest_admin  <- all_layers_names[grepl(paste0("Admin",max(admin_levels_nb)), all_layers_names)]
    lowest_admin_id <- all_layers_id[grepl(lowest_admin, all_layers_names)]

    lowest_admin_layer <- all_layers[grepl(paste0(lowest_admin_id, "$"), all_layers)]
    lowest_admin_layer_URL <- paste0(country_URL, "/", lowest_admin_id)

    geo_json <- one_URL_geoJSON(lowest_admin_layer_URL)

    return(geo_json)
  }
}

#' Scrape geoJSON layers on a specific country by iso3 code
#'
#' @param country_iso3 character - A valid country {https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{iso3 code}
#'
#' @return list - a list for the specified URL containing two elements:
#'     - name : character - string identifying the name of the layer
#'     - data : data.frame - data frame containing the layer data.
#' @export
#'
#' @examples
#' \dontrun{
#' country_geojson_iso3("AFG")
#' }

country_geojson_iso3 <- function(country_iso3){
  country_URL <- paste0("https://gistmaps.itos.uga.edu/arcgis/rest/services/COD_External/",country_iso3,"_pcode/FeatureServer")
  country_data <- country_geojson_URL(country_URL)
  return(country_data)
}
