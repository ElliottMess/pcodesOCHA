normalise_adm <- function(string){
  no_ws <- stringr::str_squish(string)
  lower <- trimws(tolower(no_ws))
  no_accent <- iconv(lower, to='ASCII//TRANSLIT')
  no_accent <- stringr::str_replace_all(no_accent,"AC", "e")
  no_accent <- stringr::str_replace_all(no_accent,"A\\?", "e")
  remove_other <- stringr::str_replace_all(no_accent, "[-',.()/ ]", "_")

  return(remove_other)
}

r3c<-function(vec,name,label){
  name <-  name %>% as.character()
  vec <-  vec %>% as.character()
  label <-  label %>% as.character()

  if(length(name)==length(label)){
    for (i in 1:length(name)){
      cond<-which(vec%in%name[i])
      if(length(cond)>0){
        vec[cond]<-label[i]
      }
      if(length(grep(paste0(name[i], "\\."), vec)) > 0){
        vec[grep(paste0(name[i], "\\."), vec)] <- gsub(paste0(name[i], "\\."), paste0(label[i],"\\."), vec[grep(paste0(name[i], "\\."), vec)])
      }
    }
    return(vec)
  } else {
    print("y and z must have the length")
  }
}
#' Normalise string
#'
#' Normalise provided string by :
#' 1. removing extra spaces ([stringr::str_squish()]) and [trimws()]) 2. passing
#' the string to lower case ([tolower()]) 3. removing all accents
#' ([stringi::stri_trans_general()]), with id = "Latin-ASCII") 4. removing other
#' special characters ('[-',.()/ ]') and replacing with '_'
#'
#' @param string character string to be normalised. Accepts vectors too.
#'
#' @return normalise string
#'
#' @export
#'
#' @examples
#' normalise_string("àaF   kgfk")
normalise_string <- function(string){
  no_ws <- stringr::str_squish(string)
  lower <- trimws(tolower(no_ws))
  no_accent <- stringi::stri_trans_general(lower,"Latin-ASCII")
  remove_other <- stringr::str_replace_all(no_accent, "[-',.()/ ]", "_")

  return(remove_other)
}
