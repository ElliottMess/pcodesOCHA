
#' Scrap pcodes datasets from UNOCHA REST API
#'
#' @return a dataframe containing all pcodes at the lowest administrative level available
#' @export
#'
#' @examples
#' \dontrun{
#' all_pcodes_feature_servers()
#' }
all_pcodes_feature_servers <- function(){
  base_URL <- "https://gistmaps.itos.uga.edu/"

  COD_URL <- paste0(base_URL, "arcgis/rest/services/COD_External")

  COD_list <-read_html(COD_URL) %>%
    html_nodes("a")%>%
    xml_attr("href")

  COD_list <- COD_list[grepl("FeatureServer$", COD_list)]

  all_dfs <- list()

  # for(i in 1:length(COD_list)){
  #   all_dfs[[1]] <- scrap_country(COD_list[2])
  # }

  all_dfs <- lapply(COD_list, scrap_country) %>%
    bind_rows()

  all_dfs_rearranged <- all_dfs %>%
    select(-OBJECTID) %>%
    mutate(admin0Name = case_when(
      is.na(admin0Name_en) & !is.na(admin0Name_fr) ~ admin0Name_fr,
      is.na(admin0Name_en) & !is.na(admin0Name_es) ~ admin0Name_es,
      is.na(admin0Name_en) & !is.na(admin0Name_pt) ~ admin0Name_pt,
      !is.na(admin0Name_en) ~ admin0Name_en,
      TRUE ~ NA_character_
    ),
    admin1Name = case_when(
      is.na(admin1Name_en) & !is.na(admin1Name_fr) ~ admin1Name_fr,
      is.na(admin1Name_en) & !is.na(admin1Name_es) ~ admin1Name_es,
      is.na(admin1Name_en) & !is.na(admin1Name_pt) ~ admin1Name_pt,
      !is.na(admin1Name_en) ~ admin1Name_en,
      TRUE ~ NA_character_
    ),
    admin2Name = case_when(
      is.na(admin2Name_en) & !is.na(admin2Name_fr) ~ admin2Name_fr,
      is.na(admin2Name_en) & !is.na(admin2Name_es) ~ admin2Name_es,
      is.na(admin2Name_en) & !is.na(admin2Name_pt) ~ admin2Name_pt,
      !is.na(admin2Name_en) ~ admin2Name_en,
      TRUE ~ NA_character_
      ),
    admin3Name = case_when(
      is.na(admin3Name_en) & !is.na(admin3Name_fr) ~ admin3Name_fr,
      is.na(admin3Name_en) & !is.na(admin3Name_es) ~ admin3Name_es,
      is.na(admin3Name_en) & !is.na(admin3Name_pt) ~ admin3Name_pt,
      !is.na(admin3Name_en) ~ admin3Name_en,
      TRUE ~ NA_character_
    ),
    admin4Name = case_when(
      is.na(admin4Name_en) & !is.na(admin4Name_fr) ~ admin4Name_fr,
      is.na(admin4Name_en) & !is.na(admin4Name_es) ~ admin4Name_es,
      is.na(admin4Name_en) & !is.na(admin4Name_pt) ~ admin4Name_pt,
      !is.na(admin4Name_en) ~ admin4Name_en,
      TRUE ~ NA_character_
    )
    )%>%
    relocate(admin0Name,admin0Pcode, admin1Name,admin1Pcode, admin2Name, admin2Pcode, admin3Name, admin3Pcode, admin4Name,admin4Pcode)

  return(all_dfs_rearranged)


}

scrap_one_URL_properties <- function(URL){
  COD_URL <- "https://gistmaps.itos.uga.edu"

  url_name <- tryCatch({paste0(COD_URL, URL)%>%
    read_html() %>%
    html_nodes("h2") %>%
    as.character() %>%
    stringr::str_extract("(?<=: ).*?(?= \\()")},
    error=function(e){cat("URL not available:", conditionMessage(e), "\n")
      })

  query_URL <- paste0(COD_URL, URL, "/query?where=OBJECTID%20%3E%200&outFields=%2A&returnGeometry=false&f=json")

  json <- tryCatch({fromJSON(query_URL)}, error=function(e){cat("URL not available:", conditionMessage(e), "\n")})

  properties <- json$features$attributes
  list_properties <- list(name = url_name, data = properties)
  return(list_properties)

}


scrap_country <- function(URL){
  COD_URL <- "https://gistmaps.itos.uga.edu"

  all_layers <- tryCatch({paste0(COD_URL, URL, "/layers")%>%
    read_html() %>%
    html_nodes("a")%>%
    xml_attr("href")},
    error=function(e){cat("URL not available:", conditionMessage(e), "\n")}
  )

  all_layers <- all_layers[grepl("[0-9]$", all_layers)]

  all_layers_available <- tryCatch({paste0(COD_URL, URL, "/layers")%>%
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
    all_layers_id <- stringr::str_extract(all_layers_available,"(?<=<\\/a> \\().*?(?=\\)<\\/h3>)")

    admin_levels_nb <- as.numeric(unlist(str_extract_all(all_layers_names[grepl("^Admin[0-9]", all_layers_names)],  "[0-9]")))
    lowest_admin  <- all_layers_names[grepl(paste0("Admin",max(admin_levels_nb)), all_layers_names)]
    lowest_admin_id <- all_layers_id[grepl(lowest_admin, all_layers_names)]


    lowest_admin_layer <- all_layers[grepl(paste0(lowest_admin_id, "$"), all_layers)]

    all_properties <- scrap_one_URL_properties(lowest_admin_layer)

    return(all_properties$data)
  }
}

scrap_one_URL_geoJSON <- function(URL){

  COD_URL <- "https://gistmaps.itos.uga.edu"

  url_name <- tryCatch({paste0(COD_URL, URL)%>%
      read_html() %>%
      html_nodes("h2") %>%
      as.character() %>%
      stringr::str_extract("(?<=: ).*?(?= \\()")},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")
      })

  query_URL <- paste0(COD_URL, URL, "/query?where=OBJECTID+>%3D+0&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=&resultOffset=&resultRecordCount=&returnTrueCurves=false&sqlFormat=none&f=geojson")
  readlines_geojson <- readLines(query_URL, warn = FALSE)

  return(readlines_geojson)

}


scrap_country_geojson <- function(URL){
  COD_URL <- "https://gistmaps.itos.uga.edu"

  all_layers <- tryCatch({paste0(COD_URL, URL, "/layers")%>%
      read_html() %>%
      html_nodes("a")%>%
      xml_attr("href")},
      error=function(e){cat("URL not available:", conditionMessage(e), "\n")}
  )

  all_layers <- all_layers[grepl("[0-9]$", all_layers)]

  all_layers_available <- tryCatch({paste0(COD_URL, URL, "/layers")%>%
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
    all_layers_id <- stringr::str_extract(all_layers_available,"(?<=<\\/a> \\().*?(?=\\)<\\/h3>)")

    admin_levels_nb <- as.numeric(unlist(str_extract_all(all_layers_names[grepl("^Admin[0-9]", all_layers_names)],  "[0-9]")))
    lowest_admin  <- all_layers_names[grepl(paste0("Admin",max(admin_levels_nb)), all_layers_names)]
    lowest_admin_id <- all_layers_id[grepl(lowest_admin, all_layers_names)]


    lowest_admin_layer <- all_layers[grepl(paste0(lowest_admin_id, "$"), all_layers)]

    geo_json <- scrap_one_URL_geoJSON(lowest_admin_layer)

    return(geo_json)
  }
}

