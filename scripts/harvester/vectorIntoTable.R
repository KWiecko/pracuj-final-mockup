vectorIntoTable <- function(vectorOfFinds, vectorOfDestinationColumns){
  
  
    
    localTable <- as.data.frame(
      setNames(replicate(length(vectorOfDestinationColumns %>% unique()), 0, simplify = FALSE),
               c(vectorOfDestinationColumns %>% unique())
      )
    ) 
    
  
  
  localTransferDF <- data.frame(vectorOfDestinationColumns, vectorOfFinds)
  names(localTransferDF) <- c("destinationkeywordcolumn", "keywordResults")
  localTransferDF <- localTransferDF %>%
                     group_by(destinationkeywordcolumn) %>%
                     summarise(keywordResults = sum(keywordResults))
 
  names(localTable)  <- localTransferDF$destinationkeywordcolumn
  localTable[1,] <- localTransferDF$keywordResults
  
  #vectorToReturn <- localTransferDF$keywordResults
  #names(vectorToReturn) <- localTransferDF$destinationkeywordcolumn
  
  return(localTable)
  
  
}