normalise_adm <- function(string){
  no_ws <- stringr::str_squish(string)
  lower <- trimws(tolower(no_ws))
  no_accent <- iconv(lower, to='ASCII//TRANSLIT')
  no_accent <- stringr::str_replace_all(no_accent,"AC", "e")
  no_accent <- stringr::str_replace_all(no_accent,"A\\?", "e")
  remove_other <- stringr::str_replace_all(no_accent, "[-',.()/ ]", "_")

  return(remove_other)
}
