geocode_one_admin <- function(admin,
                          country_iso3 = NULL,
                          google_api_key,
                          language = NULL) {
  if (is.null(google_api_key)) {
    stop("A valid private key for Google API 'Places' must be provided.")
  }
  if(!is.null(country_iso3)){
    cntry_name <-
      countrycode::countrycode(country_iso3, origin = 'iso3c', destination = "country.name")
  }

  admin_loc <- paste(admin, cntry_name)
  admin_levels <- paste0("admin", 0:3)
  admin_levels_df <- bind_rows(setNames(rep("", length(admin_levels)), admin_levels))[0, ]

  geocode <-
    google_geocode(admin_loc, key = google_api_key, language = language)$results

  if(length(geocode) == 0){
    warning(paste0(admin, " was not found by google. Check the inputs. Providing a country code will improve the results."))
    return(admin_levels_df)
  }

  geocode_admin_lvl <- geocode %>% select(address_components) %>%
    unnest(cols = c(address_components))
  geocode_admin_lvl_unnested <-
    suppressMessages(unnest_wider(geocode_admin_lvl, types)) %>%
    select(-...2) %>%
    rename(admin_lvl = ...1)

  geocode_admin_lvl_unnested_renamed <- geocode_admin_lvl_unnested %>%
    mutate(
      admin_lvl = case_when(
        admin_lvl == "locality" ~ "locality",
        admin_lvl == "administrative_area_level_3" ~ "admin3",
        admin_lvl == "administrative_area_level_2" ~ "admin2",
        admin_lvl == "administrative_area_level_1" ~ "admin1",
        admin_lvl == "country" ~ "admin0",
        TRUE ~ NA_character_)) %>%
    filter(!is.na(admin_lvl))

  adm_lvls_not_in <- admin_levels[!admin_levels %in% geocode_admin_lvl_unnested_renamed$admin_lvl]

  if(length(adm_lvls_not_in) != 0){
    geocode_admin_lvl_unnested_renamed<- add_row(geocode_admin_lvl_unnested_renamed, admin_lvl = adm_lvls_not_in) %>%
      bind_rows() %>%
      distinct()
  }

  if(length(geocode_admin_lvl_unnested_renamed$admin_lvl[geocode_admin_lvl_unnested_renamed$admin_lvl %in% admin_levels])> length(admin_levels)){
    warning(paste0(admin, " could not find an unique match. Check the inputs, including the country code."))
    return(admin_levels_df)

  }else{

    final_geocoded_admins<- geocode_admin_lvl_unnested_renamed %>%
      as.data.frame() %>%
      pivot_wider(-long_name, names_from = admin_lvl, values_from = c(short_name)) %>%
      relocate(!!admin_levels) %>%
      mutate(name_origin = admin,
             across(any_of(admin_levels),
                    ~ trimws(str_remove_all(.x,"Region|Department|Commune|Municipality|Province|Governorate|Urban Community"))))
  }

  return(final_geocoded_admins)
}

geocode_admin <- function(admin,
                          country_iso3 = NULL,
                          google_api_key,
                          language = NULL){
  if(length(admin) == 1){
    result <- geocode_one_admin(admin, country_iso3, google_api_key = google_api_key, language = language)
  }else{
    result <- map(admin, geocode_one_admin, country_iso3 = country_iso3,
                  google_api_key = google_api_key, language = language) %>%
      bind_rows()
  }
  return(result)
}

find_pcodes_one_admin <- function(admin,
                                  country_iso3 = NULL,
                                  google_api_key = NULL,
                                  admin_level = NULL,
                                  pcodes_df = NULL,
                                  language = NULL){

  if(!is.null(country_iso3) & is.null(pcodes_df)){
    pcodes <- country_pcodes_iso3(country_iso3 = country_iso3)
  }else if(!is.null(country_iso3)){
    pcodes <- pcodes_df
  }

  pcodes_admin_nbs <- sort(as.numeric(stringr::str_extract(names(pcodes)[grepl("Pcode|pcode", names(pcodes))], "[0-9]")))

  regex_admin_names <- lapply(pcodes_admin_nbs, function(x){
    paste0("admin",x,"(N|n)ame(_[a-z]{2}$|$)")
  }) %>% unlist()

  actual_names <- lapply(regex_admin_names, function(x){
    names(pcodes)[grep(x, names(pcodes))]
  }) %>% unlist()

  regex_admin_pcodes <- lapply(pcodes_admin_nbs, function(x){
    paste0("admin",x,"(Pcode|pcode)")
  }) %>% unlist()

  actual_names_pcodes <- lapply(regex_admin_pcodes, function(x){
    names(pcodes)[grep(x, names(pcodes))]
  }) %>% unlist()

  normalised_pcodes <- pcodes %>%
    mutate(across(matches(actual_names), normalise_string)) %>%
    rename_with(~paste0("admin", pcodes_admin_nbs), .cols = all_of(actual_names)) %>%
    select(all_of(paste0("admin", pcodes_admin_nbs)),all_of(actual_names_pcodes))

  normalised_admin <- normalise_string(admin)
  normalised_admin <- normalised_admin[!is.na(normalised_admin)]
  normalised_admin <- normalised_admin[!duplicated(normalised_admin)]

  admin_names <- paste0("admin", pcodes_admin_nbs)

  if(is.null(admin_level)){
    admin_in_df <-
      normalised_pcodes %>%
      filter(rowSums(across(!!admin_names, ~ grepl(paste0("^",!!normalised_admin, "$"), .x))) > 0) %>%
      distinct()

    if(nrow(admin_in_df) == 0){
      admin_in_df[1,] <- NA
      return(admin_in_df)
    }else{
      max_adm_present_col_n <- max(which(normalised_admin == admin_in_df, arr.ind = TRUE)[,2])

      max_adm_present_adm_name <- names(admin_in_df)[max_adm_present_col_n]
      max_adm_present_adm_num <- as.numeric(str_extract(max_adm_present_adm_name, "[0-9]"))

      if(max_adm_present_adm_num == max(pcodes_admin_nbs)){
        names_no_match <- paste0("admin", max(pcodes_admin_nbs)+1)
      }else{
        names_no_match <- paste0("admin", pcodes_admin_nbs[pcodes_admin_nbs >max_adm_present_adm_num])
      }

      filtered_pcodes <- admin_in_df %>%
        mutate(across(matches(names_no_match), ~paste0(NA_character_))) %>%
        distinct()
    }
  }else{

    max_adm_present_adm_num <- as.numeric(str_extract(admin_level, "[0-9]"))
    max_adm_present_adm_name <- paste0("admin",max_adm_present_adm_num)
    names_no_match <- paste0("admin", pcodes_admin_nbs[pcodes_admin_nbs >max_adm_present_adm_num])

    filtered_pcodes <-normalised_pcodes %>%
      filter(!!sym(max_adm_present_adm_name) == normalise_string(admin)) %>%
      mutate(across(matches(names_no_match), ~paste0(NA_character_))) %>%
      distinct()
  }

  if(nrow(filtered_pcodes) == 0 & !is.null(google_api_key)){
    country_codes <- select(countrycode::codelist, iso2c, iso3c)
    geocodes_no_match <- map_dfr(normalised_admin,
                                 geocode_admin,
                                 country_iso3 = country_iso3,
                                 google_api_key = google_api_key,
                                 language = language) %>%
      left_join(country_codes, by = c("admin0" = "iso2c")) %>%
      mutate(admin0 = iso3c) %>% select(-iso3c) %>%
      filter(admin0 == country_iso3) %>%
      mutate(across(where(is.character), normalise_string))

    max_adm_present_col <- geocodes_no_match %>%
      select(matches("admin[0-9]")) %>%
      summarise(across(everything(), ~sum(is.na(.x))))
    max_adm_present_adm_name <- tail(names(max_adm_present_col)[max_adm_present_col == 0],1)
    max_adm_present_adm_num <- as.numeric(str_extract(max_adm_present_adm_name, "[0-9]"))

    matched_admin <- as.character(geocodes_no_match[,paste0("admin", max_adm_present_adm_num)])

    if(!is.null(admin_level)){
      admin_level_n <- as.numeric(str_extract(admin_level, "[0-9]"))

      names_no_match <- paste0("admin", admin_level_n+1: max_adm_present_adm_num)
      filtered_pcodes <- normalised_pcodes %>%
        rowwise() %>%
        mutate(distance = diag(adist(normalise_string(admin), !!sym(admin_level)))/nchar(admin)) %>%
        filter(!!sym(admin_level) == matched_admin || distance <=0.2) %>%
        select(-distance) %>%
        mutate(across(matches(names_no_match), ~paste0(NA_character_))) %>%
        distinct()
    }else{
      names_no_match <- paste0("admin", pcodes_admin_nbs[pcodes_admin_nbs >max_adm_present_adm_num])
      if(names_no_match == "admin"){
        names_no_match <- paste0("admin", max_adm_present_adm_num+1)
      }

      filtered_pcodes <- normalised_pcodes %>%
        rowwise() %>%
        mutate(distance = diag(adist(normalise_string(admin), !!sym(max_adm_present_adm_name)))/nchar(admin)) %>%
        filter(!!sym(max_adm_present_adm_name) == matched_admin || distance <=0.1) %>%
        select(-distance) %>%
        mutate(across(matches(names_no_match), ~paste0(NA_character_))) %>%
        distinct()
    }

  }

  if(nrow(filtered_pcodes) == 0){
    filtered_pcodes[1,]<-NA
  }

  min_pcode_col <- paste0(max_adm_present_adm_name, c("pcode","Pcode"), collapse = "|")
  pcode_col <- names(filtered_pcodes)[grepl(min_pcode_col, names(filtered_pcodes))]

  pcode_cols <- names(pcodes)[grepl("Pcode|pcode", names(pcodes))]
  pcode_cols_new <- paste0("admin", pcodes_admin_nbs, "pcode")

  final_pcodes <- data.frame(ref_name = admin, filtered_pcodes,
                             lowest_pcode = filtered_pcodes[,pcode_col],
                             lowest_admin_name = max_adm_present_adm_name) %>%
    rename_with(.fn = ~paste0(pcode_cols_new), .cols = all_of(pcode_cols))

  # no_match_pcodes <- names(pcodes)[grepl(paste0("admin[",max_adm_present_adm_num +1,"-9]"), names(pcodes))]
  #
  # final_pcodes <- pcodes %>%
  #   filter(!!sym(pcode_col) == !!filtered_pcodes_final) %>%
  #   mutate(across(matches(!!no_match_pcodes), ~paste0(NA_character_))) %>%
  #   distinct()

  if(nrow(final_pcodes) == 0){
    warning(paste0(admin, " could not be found in OCHA's pcodes servers."))
    if(is.null(google_api_key)){
      warning("You did not provide a google API key. We could not match the names with google geocoding servers. Please try providing one for better results. See ?google_geocode for more details."
      )
    }
    return(final_pcodes)
  }else{
    return(final_pcodes)
  }
}

#' Find pcode for a given administrative unit name
#'
#' @param admin character or vector identifying administrative unit.
#' @param country_iso3 character string with the ISO3 country code for the
#'     country analysed. See [here](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) for more details.
#' @param admin_level character string identifying a specific administrative
#'     level where the pcode should be looked for. Three values are possible:
#'     "admin1", "admin2", and "admin3". For instance, if you are looking to
#'     pcode the region of Bamako in Mali, you should specify "admin1", as there
#'     is also an administrative unit 2 with the same name. By default the
#'     lowest administrative unit found will be returned
#' @param google_api_key a string with a google API key. See vignette("googleway-vignette") for more details.
#' @param pcodes_df dataframe containing matching key between pcodes and admin
#'     names. For more details see [here](https://github.com/elliottmess/pcodesOCHA).
#' @param language character string identifying which language is being used.
#'
#'
#' @return dataframe containing
#' @export
#'
#' @examples
#' find_pcodes_admin("Bamako", country_iso3 = "MLI", admin_level = "admin1")
#'
find_pcodes_admin <- function(admin,
                              country_iso3 = NULL,
                              admin_level = c("admin1", "admin2", "admin3"),
                              google_api_key = NULL,
                              pcodes_df = NULL,
                              language = NULL){
  if(length(admin) == 1){
    result <- find_pcodes_one_admin(admin, country_iso3,
                                    admin_level = admin_level,
                                    google_api_key = google_api_key,
                                    pcodes_df = pcodes_df, language = language)
  }else{
    if(is.null(pcodes_df)){
      pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
    }

    result <- map_dfr(admin, find_pcodes_one_admin, admin_level = admin_level,
                      country_iso3 = country_iso3,
                      google_api_key = google_api_key, pcodes_df = pcodes_df,
                      language = language)
  }
  return(result)

}
