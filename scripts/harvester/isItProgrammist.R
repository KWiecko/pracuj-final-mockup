isItProgrammist <- function(keywordsVector, offersPosition, offersHREF){
  
  universalPositionVector <- c("engin(e){1,2}r", "inżynier", "de[vw]eloper")
  
  gradeRegex <- c()
  hrefRegex <- c()
  hrefKeywordsVector <- keywordsVector %>%
               gsub(pattern = "[[:space:]]", replacement = "\\-", .) 
  
  for(positionKeyword in universalPositionVector){
    gradeRegex <- c(gradeRegex, paste0(positionKeyword, " ", keywordsVector, collapse = "|"), paste0(keywordsVector, " ", positionKeyword, collapse = "|"))
    hrefRegex <- c(hrefRegex, paste0(positionKeyword, "\\-", hrefKeywordsVector, collapse = "|"), paste0(hrefKeywordsVector, "\\-", positionKeyword, collapse = "|"))
  }
  gradeRegex <- paste0(gradeRegex, collapse = "|") %>%
                paste0("de[vw]eloper|programista|programmist|software engin(e){1,2}r|application engineer|inżynier aplikacji|", .)
  
  hrefRegex <- paste0(hrefRegex, collapse = "|") %>%
               paste0("de[vw]eloper|programista|programmist|software-engine(e){1,2}r|application-engineer|inzynier-aplikacji", .)
  
  
  
  if(
    grepl(pattern = hrefRegex, offersHREF) || grepl(pattern = gradeRegex, offersPosition)
    ){
    
   
      return(1)
  }else{
    
    return(0)
  }
  
}
