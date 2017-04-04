createOffersScoreTable <- function (localListOfTables) {
  return(
    
    as.data.frame(
      setNames(replicate(length(localListOfTables$skillcategorytable$keywordname)+5, 0, simplify = FALSE),
               c("id", 
                 localListOfTables$skillcategorytable$keywordname, 
                 "DS_skill", 
                 "Prog_skill",
                 "DB_skill",
                 "Universal_skill")
      )
    ) 
    
  )

  
}