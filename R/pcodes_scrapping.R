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

  all_dfs <- lapply(paste0("https://gistmaps.itos.uga.edu", COD_list), country_pcodes_URL) %>%
    bind_rows()

  all_dfs_rearranged <- all_dfs %>%
    select(-OBJECTID) %>%
    mutate(admin0Name = case_when(
      is.na(admin0Name_en) & !is.na(admin0Name_fr) ~ admin0Name_fr,
      is.na(admin0Name_en) & !is.na(admin0Name_es) ~ admin0Name_es,
      !is.na(admin0Name_en) ~ admin0Name_en,
      TRUE ~ NA_character_
    ),
    admin1Name = case_when(
      is.na(admin1Name_en) & !is.na(admin1Name_fr) ~ admin1Name_fr,
      is.na(admin1Name_en) & !is.na(admin1Name_es) ~ admin1Name_es,
      !is.na(admin1Name_en) ~ admin1Name_en,
      TRUE ~ NA_character_
    ),
    admin2Name = case_when(
      is.na(admin2Name_en) & !is.na(admin2Name_fr) ~ admin2Name_fr,
      is.na(admin2Name_en) & !is.na(admin2Name_es) ~ admin2Name_es,
      !is.na(admin2Name_en) ~ admin2Name_en,
      TRUE ~ NA_character_
    ),
    admin3Name = case_when(
      is.na(admin3Name_en) & !is.na(admin3Name_fr) ~ admin3Name_fr,
      is.na(admin3Name_en) & !is.na(admin3Name_es) ~ admin3Name_es,
      !is.na(admin3Name_en) ~ admin3Name_en,
      TRUE ~ NA_character_
    ),
    admin4Name = case_when(
      is.na(admin4Name_en) & !is.na(admin4Name_fr) ~ admin4Name_fr,
      !is.na(admin4Name_en) ~ admin4Name_en,
      TRUE ~ NA_character_
    ),
    across(matches("^admin[0-9]Name$"), normalise_adm, .names = "{.col}_ref"),
    across(matches("^admin[0-9]Pcode$"), toupper)
    )%>%
    relocate(admin0Name,admin0Pcode, admin1Name,admin1Pcode, admin2Name, admin2Pcode, admin3Name, admin3Pcode, admin4Name,admin4Pcode)

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
  country_data <- country_pcodes_URL(country_URL)
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
