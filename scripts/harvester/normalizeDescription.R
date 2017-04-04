normalizeDescription <- function(localOffersDescription){
  
  library(dplyr)
  library(stringr)
  stringToBlank <- "ochronę przetwarzanych danych.*|przetwarzania danych osobowych.*|zgoda na przetwarzanie danych.*|przetwarzanie i przechowywanie moich danych osobowych.*|przetwarzanie danych osobowych.*|przetwarzanie moich danych osobowych.*"
  
  
  localOffersDescription <- localOffersDescription %>%
                            as.data.frame() %>%
                            mutate(description = str_replace_all(pattern = "([[:alpha:]])([[:lower:]])([[:space:]]?)([[:punct:]]?)([[:space:]])?([[:upper:]])", 
                                                                   replacement = "\\1\\2 \\6", 
                                                                   description)) %>%
                            mutate(description = gsub(pattern = paste0(stringToBlank), replacement = "", description))
  
  return(localOffersDescription)
  
}



