extractSkillsTable <- function(offersId, offersDescription, offersPosition, offersHREF, listOfTables, bestModel){

  library(dplyr)
  library(stringr)

  pathToFuns <- "scripts/harvester/"


  # create table for predictive model (needs all columns specified in training set)
  source(paste0(pathToFuns, "createOffersScoreTable.R"))
  offersScoreTable <- createOffersScoreTable(listOfTables)

  source(paste0(pathToFuns, "normalizeDescription.R"))
  offersDescription <- normalizeDescription(offersDescription)



  # splitting the offer
  # splitOffersDescription <- str_split(offersDescription, pattern = "([[:space:]]){1,}") %>%
  #                         unlist() %>%
  #                         as.vector()

  # preliminary search - all crap gets in
  singleKeywordsResults <- c()
  doubleKeywordsResults <- c()

  source(paste0(pathToFuns, "findsPerKeyword.R"))
  for (keywordIdx in 1:length(listOfTables$singlekeyworddictionary$keywordname)){
    singleKeywordsResults <- c(singleKeywordsResults,
                               findsPerKeyword(listOfTables$singlekeyworddictionary[keywordIdx, ], listOfTables$contextphrasestable, offersDescription)
                               )
  }

  for (keywordIdx2 in 1:length(listOfTables$doublekeyworddictionary$keywordname)){
    doubleKeywordsResults <- c(doubleKeywordsResults,
                               findsPerKeyword(listOfTables$doublekeyworddictionary[keywordIdx2,], listOfTables$contextphrasestable, offersDescription)
    )
  }

  initialDF <- rbind(data.frame(keywordResults = singleKeywordsResults,
                                destinationkeywordcolumn = listOfTables$singlekeyworddictionary$destinationkeywordcolumn),
                     data.frame(keywordResults = doubleKeywordsResults,
                                destinationkeywordcolumn = listOfTables$doublekeyworddictionary$destinationkeywordcolumn))


  # creating combined skill table
  source(paste0(pathToFuns, "vectorIntoTable.R"))
  atomicRow <- vectorIntoTable(initialDF$keywordResults, initialDF$destinationkeywordcolumn)
  atomicRow <- atomicRow[, order(names(atomicRow))]

  atomicRow <- atomicRow %>%
               mutate(id = offersId, Is_DataScientist = 0, Is_Programmist = 0, DS_skill = 0, Prog_skill = 0, DB_skill = 0, Universal_skill = 0) %>%
               select(id, Is_DataScientist, Is_Programmist, everything(), DS_skill, Prog_skill, DB_skill, Universal_skill)


  source(paste0(pathToFuns ,"isItProgrammist.R"))
  atomicRow$Is_Programmist <- isItProgrammist(
                                                c(listOfTables$singlekeyworddictionary$destinationkeywordcolumn,
                                                  listOfTables$doublekeyworddictionary$destinationkeywordcolumn) %>%  unique() %>%
                                                gsub(pattern = "\\_", replacement = " ", .),
                                                offersPosition,
                                                offersHREF
                                             )
  transferDF <- listOfTables$skillcategorytable %>%
                filter(row_number() <= 2)

  listOfTables$skillcategorytable <- listOfTables$skillcategorytable %>%
                                     filter(row_number() > 2) %>%
                                     arrange(keywordname) %>%
                                     rbind(transferDF, .)

  for(counter in 2:5){
    atomicRowIdx <- length(names(atomicRow)) - 4 + counter - 1
    atomicRow[, atomicRowIdx] <- ((atomicRow[,3:(length(atomicRow)-4)] %>% unlist() %>% as.vector()) * listOfTables$skillcategorytable[-1, counter]) %>% sum()
  }

  # fixed DS treshold vote by optimal cutpoints
  print(atomicRow %>%
          select(-id, -Is_DataScientist) %>%
          mutate(RS = rowSums(.)) %>%
          select(RS) %>%
          as.vector() %>%
          sum())

  dsTreshold <- 0.119

  library(randomForest)

  doesItHaveAnySkills <- atomicRow %>%
                         select(-id, -Is_DataScientist) %>%
                         mutate(RS = rowSums(.)) %>%
                         select(RS) %>%
                         as.vector() %>%
                         sum()

  prediction <- ifelse(doesItHaveAnySkills == 0,
                       "0KeyWords",
                       ifelse(myBestTRee$modelType == "Regression",
                              ifelse(predict(myBestTRee, atomicRow[,c(-1, -2)])[,1] >= dsTreshold,
                                     1,
                                     0),
                              ifelse(predict(myBestTRee, atomicRow[,c(-1, -2)],  type = "prob")[,1] >= dsTreshold,
                                     1,
                                     0)
                              )

                    )



     if(prediction == "0KeyWords"){
      return(c("0KeyWords", "0KeyWords"))
     }else{
       return(c(prediction,
                predict(myBestTRee, atomicRow[,c(-1, -2)],  type = "prob")[,1] ))


     }



}


