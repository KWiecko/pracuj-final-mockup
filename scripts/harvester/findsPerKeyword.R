findsPerKeyword <- function (keywordRow , contextPhrasesTable, offersDescriptionVector){

  library(dplyr) 

 if(length(keywordRow) > 5){
   assign("keyword", paste0(keywordRow$keyword1regex, "([[:space:]])?([[:punct:]])?([[:space:]])?", keywordRow$keyword2regex))
   offersDescriptionVector <- gsub(pattern = paste0(keywordRow$exceptionregex), replacement = "", offersDescriptionVector)
   finalKeywordCount <- (length(
                                  str_split(paste0(offersDescriptionVector), pattern = paste0(keyword)) %>%
                                  unlist() %>% as.vector()
     
                                ) - 1)
   
 }else{
   offersDescriptionVector <- str_split(offersDescriptionVector, pattern = "([[:space:]]){1,}") %>%
     unlist() %>%
     as.vector()
   assign ("keyword", keywordRow$keywordregex) 
 }
  
  
  if(grep(pattern = paste0(keyword), offersDescriptionVector, ignore.case = keywordRow$iskeywordcasesens) %>% length() == 0){
    
    return(0)
    
  }else{
    # looking the keyword up in the description vector
    
    findsIndices <- grep(pattern = paste0(keyword), offersDescriptionVector, ignore.case = keywordRow$iskeywordcasesens)
    
    # selecting finds from description
    finds <- offersDescriptionVector[findsIndices]
    
    # keeping track of finds' ids - they are needed for context elimination:
    # - saving finds idx in 'findsIndices' vector
    # - saving exceptions in 'findsExceptionsIndices'
    findsExceptionsIndices <- c()
    
    if(grep( pattern = paste0(keywordRow$exceptionregex), finds) %>% length() > 0){
      findsExceptionsIndices <- grep( pattern = paste0(keywordRow$exceptionregex), finds)
      
      findsIndices <- findsIndices[-grep(pattern = paste0("^", findsExceptionsIndices, "$", collapse = "|"),
                                         findsIndices)]
      
      finds <- finds[-findsExceptionsIndices]
      
    }
    
    finalKeywordCount <- length(finds)
    
    if(
      contextPhrasesTable %>%
      select(keywordname) %>%
      filter(keywordname == paste0(keyword)) %>%
      unlist() %>% as.vector() %>% length() > 0
    ){
      
      finalKeywordCount <- sapply(findsIndices, function(x){
                                                             
                                                             return(
                                                               returnRow <- paste0(offersDescriptionVector[x-1]," ", offersDescriptionVector[x], " ",offersDescriptionVector[x+1])
                                                             )
        
                                                            })
      
      contextExceptions <- contextPhrasesTable %>%
                           filter(keywordname == paste0(keyword)) %>%
                           select(contextstring) %>%
                           unlist() %>% as.vector() %>% paste0(., collapse = "|")
      
      finalKeywordCount <- finalKeywordCount[-grep(pattern = contextExceptions, finalKeywordCount, fixed = TRUE)] %>%
                           length()
      
    }
    
    #finalKeywordCount
    return(finalKeywordCount)
    
  }
  
  
}


