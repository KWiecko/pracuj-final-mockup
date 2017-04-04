# trees crossvalidation
setwd("Desktop/eR/pracuj/preliminary scores/liveFilter")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

mapVectToRange <- function(numericVect, rangeMin, rangeMax){
  
  if (rangeMin < rangeMax){#setting minimum to zero
  numericVect <- numericVect - min(numericVect)
  #making vect 0 - 1
  numericVect <- numericVect/max(numericVect)
  #maping to desired range
  numericVect <- numericVect * (rangeMax - rangeMin)
  #seting mimnal valuer to range Min
  numericVect <- numericVect + rangeMin
  } else
  {
    print("Wrong range")
  }
  
  return(numericVect)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

set.seed(42)

library(caret)
library(dplyr)
library(readr)

# w legitnej wersji usunąć FOSS !!!!!!!!! ############################## !!!!!!!!!!!!!!!!!!!!!! ##################
# prog_tag_skills_table_KW_COUNT_retagged_XtraCols.csv
# prog_tag_skills_table.csv

myDataTable <- read_csv("finalScoresTable.csv") %>% select(-id)
set.seed(42)
myTestTableIdx <- createDataPartition(myDataTable$Is_DataScientist, p = 0.75, list = FALSE, times = 1)

myTestDataTable <- myDataTable[-myTestTableIdx, ]
myDataTable <- myDataTable[myTestTableIdx, ]

# creating 5700 rows ofnon DS offers
DSTable <- myDataTable %>%
           filter(Is_DataScientist == 1)

nonDSTable <- myDataTable %>%
              filter(Is_DataScientist == 0)

nonDSIdxs <- createDataPartition(myDataTable$Is_DataScientist, p = 0.08, list = FALSE, times = 1)
nonDSTable <- nonDSTable[nonDSIdxs,]

myDataTable <- rbind(DSTable, nonDSTable)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# train params.

myTrainControl <- trainControl(#method = "boot",
                               number = 3, #ile zestawów danych
                               #repeats = 3, #ile razy powtarza trening
                               verboseIter = TRUE,
                               classProbs = TRUE)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

mapVectToRange <- function(numericVect, rangeMin, rangeMax){
  
  #setting minimum to zero
  numericVect <- numericVect - min(numericVect)
  #making vect 0 - 1
  numericVect <- numericVect/max(numericVect)
  #maping to desired range
  numericVect <- numericVect * (rangeMax - rangeMin)
  #seting mimnal valuer to range Min
  numericVect <- numericVect + rangeMin
  
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# RF crossval (long)
setwd("diffDFsAndMods/xtraCols01")
myDataTable <- read_csv("prog_tag_skills_table_xtraCols.csv") %>% select(-id)

library(doMC)
registerDoMC(cores = 3)
set.seed(42)

myDataTable$Is_DataScientist[myDataTable$Is_DataScientist == 0] <- "notDS"
myDataTable$Is_DataScientist[myDataTable$Is_DataScientist == 1] <- "DS"
myDataTable$Is_DataScientist <- factor(myDataTable$Is_DataScientist)


myBestTRee <- train(Is_DataScientist ~.,
                    data = myDataTable,
                    method = 'parRF',
                    trControl = myTrainControl,
                    metric = "Kappa")

myBestPredictions <- predict(myBestTreeReg, myDataTable[,-1])
myBestPredictions <- myBestPredictions %>% 
                     as.character() 
myBestPredictions[myBestPredictions == "DS"] <- 1
myBestPredictions[myBestPredictions == "notDS"] <- 0

save(myBestTRee, file = "bestRFKappa.Rdata")

bestRFPreds <- predict(myBestTRee, myDataTable[,-1], type = "prob")[,1]
rocCVDF <- data.frame(myDataTable[,1], bestRFPreds)
names(rocCVDF) <- c("rfTag", "rfScore")

library(ROCR)


rfPerf <- performance(prediction(rocCVDF$rfScore, rocCVDF$rfTag),"tpr","fpr")
plot(rfPerf, colorize=T, ylab = "tpr", xlab = "fpr", xlim = c(0, .1), main = paste0("CV RF's ROC"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# GLM crossval (quick)

myDataTable <- read_csv("prog_tag_skills_table_xtraCols.csv") %>% select(-id)

myTrainControl <- trainControl(method = "repeatedcv",
                               number = 10, #ile zestawów danych
                               repeats = 3, #ile razy powtarza trening
                               verboseIter = TRUE)

set.seed(42)
myBestGLM <- train(Is_DataScientist ~.,
                    data = myDataTable,
                    method = 'glm',
                    trControl = myTrainControl)



myBestPredictions <- predict(myBestGLM, myDataTable[,-1])
myBestPredictions <- mapVectToRange(myBestPredictions, 0, 1)

myBestGLMData <- list(myBestGLM, myDataTable$Is_DataScientist, myBestPredictions)
names(myBestGLMData) <- c("model", "tags", "scores")
#setwd("diffDFsAndMods/xtraCols01")

save(myBestGLMData, file = "myBestGLM_01_xtraCols.Rdata")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# XGB crossval (quick)

library(xgboost)
setwd("diffDFsAndMods/raw01/xgbData")
xgbTrainMatrix <- xgb.DMatrix("xgbTrainMtx df_no_1.data")
xgbTestMatrix <- xgb.DMatrix("xgbTestMtx df_no_1.data")
set.seed(42)
xgbCV <- xgb.cv(data = xgbTrainMatrix, nround = 250, nfold = 25, nthread = 4, max.depth = 25, objective = "binary:logistic")


library(ROCR)
myDataTable$Is_DataScientist <- myDataTable$Is_DataScientist %>% 
                                as.character()
myDataTable$Is_DataScientist[myDataTable$Is_DataScientist == "DS"] <- 1
myDataTable$Is_DataScientist[myDataTable$Is_DataScientist == "notDS"] <- 0

ROCDF <- data.frame(myDataTable$Is_DataScientist %>% as.numeric(), myBestPredictions %>% as.numeric())
colnames(ROCDF) <- c("RFTag", "RFScore")


pred <- prediction(ROCDF$RFScore, ROCDF$RFTag)
perf <- performance(pred, "tpr","fpr")
plot(perf, colorize=T, ylab = "tpr", xlab = "fpr", xlim = c(0, .1), main = "Best ROC", add=TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# saving model to proper dir.
setwd("diffDFsAndMods/raw01")
save(myBestTRee, file = "bestTree_01_raw.Rdata")
setwd("../../")
