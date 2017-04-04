library(readr)
library(dplyr)
library(Matrix)
library(SparseM)
library(ROCR)
library(randomForest)
library(caret)


set.seed(42)
#setwd("Desktop/eR/pracuj/preliminary scores/liveFilter")


##########################################################

getMyDFReadyForSkillsGenres <- function(localDF){
  
  localDF <- select(localDF, -id, -Is_DataScientist, -DS_skill, -Prog_skill, -DB_skill, -Universal_skill)
  
  return(localDF)
  
}

##########################################################

getMyDFReadyForSkillsGenres_SDF <- function(localSmallDF){
  
  localSmallDF <- filter(localSmallDF ,df_full_n != "id") %>% filter(df_full_n != "Is_DataScientist") %>% filter(df_full_n != "skills_per_offer")
  
  return(localSmallDF)
}

##########################################################

getDifferentSkillsAmount <- function(localDF, skillGenre){
  

vectorToReturn <- c()
  
  for(i in 1:length(as.numeric(unlist(localDF[,1])))){
    
    vectorToReturn[i] <- sum(as.numeric(unlist(localDF[i,])) * as.numeric(unlist(skillGenre)))
    
  }
  
return(vectorToReturn)
}

##########################################################

multiplyMyCols <- function(localDF, localMultipicatorVect){
  
  for (multIndex in 1:length(as.numeric(unlist(df_full_weights[, 2])))){
    localVect <- as.vector(unlist(localDF[, multIndex]))
    
    localVect <- (localVect * as.numeric(localMultipicatorVect[multIndex,2]))/length(localVect)
    localDF[, multIndex] <- localVect
  }
  
  return(localDF)
}


##########################################################

dataSplitter <- function(localDf, pParam, nOfSubsets) {
  
  library(caret)
  
  set.seed(42)
  trainIndexes <- createDataPartition(localDf$Is_DataScientist, p = pParam, list = FALSE, times = nOfSubsets)
  return (trainIndexes)
}

#########################################################

nameChanger <- function(localDf, Mtx) {
  
  samplesNames <- c()
  i <- 1
  
  for(colNam in colnames(Mtx)){
    
    samplesNames <- c(samplesNames, paste0("dataset_no_", i))
    i <- i+1  
  }
    
  colnames(Mtx) <- samplesNames
  return(Mtx)
}

#########################################################

dfsNames <- function(numberOfDfs){
  
  dfsNamesVect <- c()
  for(counter in 1:numberOfDfs){
    
    dfsNamesVect <- c(dfsNamesVect, paste0("df_no_", counter))
    
  }
  
  return(dfsNamesVect)
}

#########################################################

createMLDfs <- function(localDf, indexMtx, datasetsNames)
{
  globalDatasetList <- list()
  trainTestList <- list()
  i <- 1
  for(counter in colnames(indexMtx)){
    
    trainTestList [[1]] <- localDf[indexMtx[, i],] 
    trainTestList[[2]] <- localDf[-indexMtx[, i],] 
    names(trainTestList) <- c("train", "test")
    
    globalDatasetList[[i]] <- trainTestList
    i <- i+1 
  }
  
  names(globalDatasetList) <- datasetsNames
  
  return(globalDatasetList)
}

#########################################################

globalRf <- function(localGlobalDataset, datasetsNames){
  
  library(randomForest)
  
  localRFList <- list()
  rfModelList <- list()
  

   for(count in 1:length(datasetsNames)){
  

     #localGlobalDataset[[count]]$train$Is_DataScientist <- factor(localGlobalDataset[[count]]$train$Is_DataScientist)
     set.seed(42)

     rfModelList[[1]] <- randomForest(Is_DataScientist ~ ., data = localGlobalDataset[[count]]$train[, -1], mtry = 75 )
     rfModelList[[2]] <- predict(rfModelList[[1]], localGlobalDataset[[count]]$test[, -2])#, type = "prob")
  
     names(rfModelList) <- c("model", "scores")
     localRFList[[count]] <- rfModelList
     print(paste0(count, ". forest trained"))
   }
  
  names(localRFList) <- datasetsNames
  return(localRFList)
  
}

#########################################################

globalGLM <- function (localGlobalDataset, datasetsNames) {
  
localGlmList <- list()
glmModelList <- list()

#model <- glm(Is_DataScientist ~ ., data=df[,wspolneKolumny[-1]], family = "binomial")
#summary(model)

#scoryGLM <- predict(model, df2[,-2], type = "response")

for(count in 1:length(datasetsNames)){
 
  localGlobalDataset[[count]]$train$Is_DataScientist <- factor(localGlobalDataset[[count]]$train$Is_DataScientist)
  set.seed(42)
  glmModelList[[1]] <- glm(Is_DataScientist ~ ., data = localGlobalDataset[[count]]$train[, -1], family = "binomial")
  glmModelList[[2]] <- predict(glmModelList[[1]], localGlobalDataset[[count]]$test[, -2], type = "response")
  
  names(glmModelList) <- c("model", "scores")
  localGlmList[[count]] <- glmModelList 
  print(paste0(count, ". glm trained"))
}

names(localGlmList) <- datasetsNames
  
  return(localGlmList)
}

#########################################################

trainTestDFIterator <- function(dfToConvert){
  
  for(colNum in 1:length(colnames(dfToConvert))){
    dfToConvert[,colNum] <- as.numeric(dfToConvert[,colNum])
    
  }
  
  return(dfToConvert)
  
  
}

#########################################################

getMatrixFromDF <- function(dfToConvert){
  
  matrixToReturn <- Matrix(0, nrow = nrow(dfToConvert), ncol = ncol(dfToConvert))
  
  for(colNum in 1:length(colnames(dfToConvert))){
    
    #localTranslator <- unlist(c(dfToConvert[, colNum]))
    
    matrixToReturn[, colNum] <- as.numeric(unlist(c(dfToConvert[, colNum])))
  }
  
  colnames(matrixToReturn) <- colnames(dfToConvert)
  return(Matrix(matrixToReturn))
}

#########################################################
getMyDFReadyForXGB <- function(localGlobalDataset, datasetsNames){
  
  library(xgboost)
  globalMatrixList <- list()
  trainTestMatrixList <- list()
  
  for(dfNum in 1:length(names(localGlobalDataset))){
    trainTestMatrixList[[1]] <- getMatrixFromDF(localGlobalDataset[[dfNum]]$train)
    trainTestMatrixList[[2]] <- getMatrixFromDF(localGlobalDataset[[dfNum]]$test)
    
      names(trainTestMatrixList) <- c("train", "test")
      globalMatrixList[[dfNum]] <- trainTestMatrixList
  }
  
  names(globalMatrixList) <- datasetsNames
  return(globalMatrixList)
}

#########################################################

convertMeToNumeric <- function (localGlobalDataset ){
  
  for(dsetNo in 1:length(names(localGlobalDataset))){
    localGlobalDataset[[dsetNo]]$train <- trainTestDFIterator(localGlobalDataset[[dsetNo]]$train)
    localGlobalDataset[[dsetNo]]$test <- trainTestDFIterator(localGlobalDataset[[dsetNo]]$test)
    
  }  
    
  return(localGlobalDataset)
}

#########################################################

globalXgboosting <- function(localGlobalMatrixDataset, datasetsNames, nRounds, maxDepth){
  
  library(xgboost)
  xgbModelList <- list()
  localXGBoostList <- list()
  
  for(dfNum in 1:length(datasetsNames)){
    set.seed(42)
    #setwd("diffDFsAndMods/raw01/xgbData")
    trainingData <- xgb.DMatrix(localGlobalMatrixDataset[[dfNum]]$train[, -(1:2)], label = localGlobalMatrixDataset[[dfNum]]$train[, 2])
    #xgb.DMatrix.save(trainingData, paste0("xgbTrainMtx ", datasetsNames[dfNum], ".data"))
    testingData <- xgb.DMatrix(localGlobalMatrixDataset[[dfNum]]$test[, -(1:2)], label = localGlobalMatrixDataset[[dfNum]]$test[, 2])
    #xgb.DMatrix.save(testingData, paste0("xgbTestMtx ", datasetsNames[dfNum], ".data"))
    set.seed(42)
    xgbModelList[[1]] <- xgboost(data = trainingData, nrounds = nRounds, max.depth = maxDepth,  objective = "binary:logistic" ) #objective = "binary:logistic"
    xgbModelList[[2]] <- predict(xgbModelList[[1]], testingData)
    
    names(xgbModelList) <- c("model", "scores")
    localXGBoostList[[dfNum]] <- xgbModelList
    #setwd("../../../")
  }
  
  names(localXGBoostList) <- datasetsNames
  
  return(localXGBoostList)
}

#########################################################

makeMyROCdf <- function(rf, glm, xgboo, rfDataSets, xgbooDataSets, datasetsNames){
  
  #localROCdf <- data.frame()
  glmDataSets <- rfDataSets
  
  for(dfNum in 1:length(datasetsNames)){
    
    print(dfNum)
    
    if(dfNum == 1){
      localROCdf <- data.frame(rfDataSets[[dfNum]]$test[, 2],    rf[[dfNum]]$scores[,2], 
                               glmDataSets[[dfNum]]$test[, 2],   glm[[dfNum]]$scores, 
                               xgbooDataSets[[dfNum]]$test[, 2], xgboo[[dfNum]]$scores)
      
    }else{
      
      transferDF <- data.frame(rfDataSets[[dfNum]]$test[, 2],    rf[[dfNum]]$scores[,2], 
                               glmDataSets[[dfNum]]$test[, 2],   glm[[dfNum]]$scores, 
                               xgbooDataSets[[dfNum]]$test[, 2], xgboo[[dfNum]]$scores)
      localROCdf <- rbind(localROCdf, transferDF)
      
    }
   
   
     
  }
  
  names(localROCdf) <- c("rfTag", "rfScore", "glmTag", "glmScore", "xgbTag", "xgbScore")
  
  return(localROCdf)
}

#########################################################

measureMyPerf <- function(localGlobalScore, localGlobalTag, modelName){
  
  library(ROCR)
  
  
  #pred <- prediction( scory[,2], df2$Is_Programmist)
  #perf <- performance(pred,"tpr","fpr")
  
  localGlobalPred <- prediction(localGlobalScore, localGlobalTag)
  perf <- performance(localGlobalPred,"tpr","fpr")
  plot(perf, colorize=T, ylab = "tpr", xlab = "fpr", xlim = c(0, 1), main = paste0(modelName, " ROC"))
  
  return(perf)
}

#########################################################

skillSignatures <- function(localDF) {
  
  skillsIds <- c()
  
    for(i in 1:length(colnames(localDF))){
      
      if(colnames(localDF)[i] == "id" || colnames(localDF)[i] == "Is_DataScientist" ){ #|| colnames(localDF)[i] == "Is_Programmist"
        skillsIds[i] <- paste0("")
        
      }else {
      skillsIds[i] <- paste0("x", i)
      }
      
    }
  
  return(skillsIds)
  
}



#########################################################

getIdentifierForID <- function(localDF, skillsIds){
  
  localDF <- mutate(localDF, skillsId = "")
  
  localSkillsIdenfs <- c(1:length(unlist(localDF[,1])))
  
  for(i in 1:length(unlist(localDF[,1]))){
    localSkillsIdenfs[i] <- paste0(skillsIds[localDF[i,] >= 1], collapse = "")
   
  }
  
 
  
  localDF$skillsId <- localSkillsIdenfs
  
  return(localDF)
  
  
}

#########################################################

mapMyVectToNumeric <- function(vectorToMap){
  
  vectorToReturn <- vectorToMap
  vectorToMapUniq <- unique(vectorToMap)
  myMap <- c(1:length(vectorToMapUniq))
  
  myMapDF <- data.frame(myMap, vectorToMapUniq)
  
  for(i in 1:length(vectorToMap)){
    
    vectorToReturn[i] <- grep(pattern = paste0("^",vectorToMap[i], "$"), vectorToMapUniq)
    
  }
  
  vectorToReturn <- as.integer(unlist(vectorToReturn))
  
  maxSkillId <- max(vectorToReturn)
  vectorToReturn <- vectorToReturn/maxSkillId
  
  return(vectorToReturn)
  
}

#########################################################
createProbabilityColumn <- function(vectorToConvertToProbability){
  
  
  vectorToReturn <- vectorToConvertToProbability
  localVect <- data.frame(vectorToReturn)
  skillProb <- arrange(localVect, vectorToReturn) %>% group_by(vectorToReturn) %>% summarise(n()) %>% mutate(prob = `n()`/sum(`n()`))
  
  for (q in 1:length(vectorToReturn)){
    vectorToReturn[q] <- unlist(skillProb[grep(pattern = paste0("^",vectorToReturn[q], "$"), skillProb$vectorToReturn), 3])
    
  }
  return(vectorToReturn)
  
}


#########################################################

normalizeByColMean <- function(localCol){
  
  
  return(localCol - mean(localCol))
  
}

#########################################################

getLeadingSkill <- function(localDF) {

   #DS_skill   - 1
   #Prog_skill - 2
   #DB_skill   - 3
  skillsToChooseFrom <- select(localDF, id, DS_skill, Prog_skill, DB_skill, Universal_skill)  
  comparator <- c()
  leadingSkillID <- c(1:length(unlist(localDF[,1])))
  
  for(i in 1:length(unlist(skillsToChooseFrom[,1]))){
    
    comparator <- skillsToChooseFrom[i, 2:length(skillsToChooseFrom[1,])]
    leadingSkillID[i] <- paste0(as.numeric(grepl(pattern = paste0(max(comparator)), comparator)), collapse = "")
    
    
  }

  
  return(mapMyVectToNumeric(leadingSkillID))
}


#########################################################

getSingleModelsScoresFromROCDF <- function(localTrees,localDatasets){
  
  localIsDSCrossValList <- list()
  
  
  for(i in 1:length(localTrees)){
    ValidatingDF <- data.frame(localDatasets[[i]]$test$id,localDatasets[[i]]$test$Is_DataScientist, localTrees[[i]]$scores[,2])
    colnames(ValidatingDF) <- c("id", "Is_DataScientist", "RF_score")
    localIsDSCrossValList[[i]] <- ValidatingDF
    
  }
  
  return(localIsDSCrossValList)
}

#########################################################

getAverageROCPackage <- function(pathString){
setwd(paste0("averageComparison/", pathString))
myLocalDataList <- get(load("dataListRfGLM.Rdata"))    # could be faster -> return(get(load("dataListRfGLM.Rdata")))
                                                       # but decided to go for longer version to easier change dir
setwd("../../")
return(myLocalDataList)
}

#########################################################

prepareDFForSingleAverageROC <- function(localGlobalDataModelList){
  
  for(dfNum in 1:length(names(localGlobalDataModelList$data))){
    print(dfNum)
    
    if(dfNum == 1){
      localROCrfDf <-data.frame(localGlobalDataModelList$data[[dfNum]]$test[, 2],    localGlobalDataModelList$rf[[dfNum]]$scores[,2])
      localROCglmDf <- data.frame(localGlobalDataModelList$data[[dfNum]]$test[, 2],   localGlobalDataModelList$glm[[dfNum]]$scores)
      
    }else{
      localROCrfDf <- rbind(localROCrfDf, data.frame(localGlobalDataModelList$data[[dfNum]]$test[, 2], localGlobalDataModelList$rf[[dfNum]]$scores[,2]))
      localROCglmDf <- rbind(localROCglmDf, data.frame(localGlobalDataModelList$data[[dfNum]]$test[, 2],   localGlobalDataModelList$glm[[dfNum]]$scores))
      
    }
  
  }
  localROCDataList <- list(localROCrfDf, localROCglmDf)
  names(localROCDataList[[1]]) <- c("rfTag", "rfScore")
  names(localROCDataList[[2]]) <- c("glmTag", "glmScore")
  names(localROCDataList) <- c("rF", "GLM")
  
  return(localROCDataList)
  
}

#########################################################


plotAllAverages <- function(localAverageModelsROC, localLabelsMatrix, xMax, mainTitle){
  
  localAverageModelROCPred <- prediction(localAverageModelsROC[, -c(1,4)], labels = localLabelsMatrix)
  averageModelsRocPerformance <- performance(localAverageModelROCPred, "tpr", "fpr")
  plot(averageModelsRocPerformance, colorize = TRUE, xlim = c(0, xMax), ylim = c(0, 1), main = mainTitle)
}

#########################################################

plotAllBest <- function(localBestScores, localBestLabels, xMax, mainCaption){
  localAverageModelROCPred <- prediction(localBestScores, labels = localBestLabels)
  averageModelsRocPerformance <- performance(localAverageModelROCPred, "tpr", "fpr")
  plot(averageModelsRocPerformance, colorize = TRUE, xlim = c(0, xMax), ylim = c(0, 1), main = mainCaption)
  
  
}

#########################################################


labelMyPlot <- function(labelsStringVector){
  library(IDPmisc)
  i<-1
  while (i <= length(labelsStringVector)){
    labelCoords <- locator(1)
    text(labelCoords, labelsStringVector[i])
    myArrowCoords <- locator(2)
    Arrows(x1 = myArrowCoords$x[1], y1 = myArrowCoords$y[1], x2 = myArrowCoords$x[2], y2 = myArrowCoords$y[2], size=1, open = FALSE)
    i <- i + 1
  }

  
}

#########################################################

createVarImpDF <- function(localTrees, localColnames, diffVarsIndic) {
  
  if(diffVarsIndic == 1){
    goodVarNames <- selectCommonVars(localTrees, localColnames)
  }else{
    goodVarNames <- rownames(varImp(localTrees[[1]]$model))
  }
  
  for(q in 1:length(localTrees)){
    if(q == 1){
     
      if(diffVarsIndic == 1){
        locVarImpDF <- data.frame(varImp(localTrees[[q]]$model)[grep(pattern = paste0("^", goodVarNames, "$", collapse = "|"), rownames(varImp(localTrees[[q]]$model))),]) 
        
      }else{
        locVarImpDF <- data.frame(varImp(localTrees[[q]]$model))
      }
      
    }else{
      if(diffVarsIndic == 1){
        locVarImpDF <- cbind(locVarImpDF, data.frame(varImp(localTrees[[q]]$model)[grep(pattern = paste0("^", goodVarNames, "$", collapse = "|"), rownames(varImp(localTrees[[q]]$model))),]))
        
      }else{
        locVarImpDF <- cbind(locVarImpDF, data.frame(varImp(localTrees[[q]]$model)))
        
      }
      
    }
    
  }
  

  colnames(locVarImpDF) <- localColnames
  
  
  locVarImpDF <- locVarImpDF %>% 
                 mutate(myMean = 0, myMedian = 0) %>% 
                 mutate( varName = goodVarNames[row_number()])

  for(p in 1:length(locVarImpDF[,1])){
    locVarImpDF$myMean[p] <- round(mean(unlist(locVarImpDF[p, c(1:length(localTrees))])), 3)
    locVarImpDF$myMedian[p] <- round(median(unlist(locVarImpDF[p, c(1:length(localTrees))])), 3)
  }

  locVarImpDF <- locVarImpDF %>% arrange(desc(myMean), desc(myMedian)) %>%
                                 mutate(meanMedianSpread = (myMean - myMedian)/myMean*100, myRank = row_number()) %>%
                                 select(varName, myMean, myMedian, meanMedianSpread, everything()) 
  return(locVarImpDF)
}

#########################################################

selectCommonVars <- function(localGLMs, localColnames) {
  

  for(q in 1:length(localGLMs)){
      if(q==1){
        localRownamesDF <- data.frame(ifelse(is.na(localGLMs[[q]]$model$coefficients), 0, 1))
      } else{
        localRownamesDF<- cbind(localRownamesDF, ifelse(is.na(localGLMs[[q]]$model$coefficients), 0, 1))
      }
  }
 
  colnames(localRownamesDF) <- localColnames
  
  localRownamesDF <- localRownamesDF[-1,] %>%
                     mutate(varName = rownames(localRownamesDF[-1,])[row_number()], alwaysPresent = rowSums(.)) %>%
                     filter(alwaysPresent >= length(localGLMs)) %>%
                     select(varName)
 
  
  return(unlist(localRownamesDF$varName))
  
}

#########################################################


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Files list:
#prog_tag_skills_table.csv,                     0-1, newest tags, new KWs, no additional cols
#prog_tag_skills_table_KW_COUNT_retagged.csv    0-N, newest tags, new KWs, no additioanl cols

# w  legitnej wersji usunąć FOSS 

df_DS_full <- read_csv("finalScoresTable.csv")
df_full_weights <- read_csv("prog_tag_skills_table_names.csv") %>% select(-skill_multip) 
df_full_weights_head <- df_full_weights[1:3, ]
df_full_weights_tail <- df_full_weights[4:nrow(df_full_weights), ] %>% 
                        arrange(df_full_n)
df_full_weights <- rbind(df_full_weights_head, df_full_weights_tail)

                                                                    #dla wszystkich NIEprogramistów 
df_full <- read_csv("finalScoresTable.csv")
df_full <- df_full[, order(names(df_full))] %>%
           select(id, Is_DataScientist, Is_Programmist, everything()) %>%
                                                     mutate(DS_skill = 0, 
                                                            Prog_skill = 0,
                                                            DB_skill = 0,
                                                            Universal_skill = 0)                   #Is_DataScientist = NIEPrgoramista --> JEŚLI BĘDIZE ŹLE TO ODRÓCIĆ
df_full_forGenres <- getMyDFReadyForSkillsGenres(df_full)
df_full_weights_foGenres <-getMyDFReadyForSkillsGenres_SDF(df_full_weights)

#df_full <- select(df_full, -Analitics_analysis)

df_full$DS_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$DS_skill)
df_full$Prog_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$Prog_skill)
df_full$DB_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$DB_skill)
df_full$Universal_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$Universal_skill)

df_full$DS_skill <- normalizeByColMean(df_full$DS_skill) %>% createProbabilityColumn(.)
df_full$Prog_skill <- normalizeByColMean(df_full$Prog_skill)
df_full$DB_skill <- normalizeByColMean(df_full$DB_skill)
df_full$Universal_skill <- normalizeByColMean(df_full$Universal_skill)
# removing people below average
#df_full <- filter(df_full, DS_skill <= 0.2)                                                          # PRZY KLASYCZNYM PODEJŚCIU TO POMAGA!!! (PODNOSI tpf DO 0,6 W SUMIE 115 DATASCIENTISTÓW W NWOYM SECIE)

#identificating equally qualified people

skillsIdentifiers <- skillSignatures(df_full)   
df_full <- getIdentifierForID(df_full, skillsIdentifiers)
df_full$skillsId <- mapMyVectToNumeric(df_full$skillsId) %>% createProbabilityColumn()




# To omijać - daje dużo jedynek!!
#       #      #      #
df_full <- mutate(df_full, skillsSimProb = 0)
df_full$skillsSimProb <- createProbabilityColumn(df_full$skillsId)

df_full <- mutate(df_full, leadingSkill = 0)

df_full$leadingSkill <- getLeadingSkill(df_full) %>% createProbabilityColumn(.)     
  #       #      #      #
  


#df_full <- filter(df_full, skillsId >= 0.055)  

#df_full <- mutate(df_full, Prog_skill = ifelse(Is_Programmist == 1, Prog_skill, 0))

#df_full <- multiplyMyCols(df_full, df_full_weights)

#df_full <- select(df_full,id, Is_DataScientist, everything())
#df_full$skills_per_offer <- rowSums(df_full[, -(1:2)])
#maxSkills <- max(df_full$skills_per_offer)
#df_full <- mutate(df_full, skills_per_offer = (skills_per_offer)*(skills_per_offer))
#df_full <- filter(df_full, skills_per_offer >=1)%>% filter(skills_per_offer <=7) ## %>% filter(id != 4478809)#
#df_full <- mutate(df_full, skills_per_offer = (skills_per_offer)/(maxSkills*maxSkills))

#df_DS_full_n <- colnames(df_DS_full)
df_full_n <- colnames(df_full)
df_full <- select(df_full,  -DS_skill) # -Is_Programmist,
df_full <- select(df_full, id, Is_DataScientist ,skillsId,  leadingSkill, DS_skill, Prog_skill, DB_skill,  Universal_skill, Is_Programmist )#,skillsSimProb, big_data, business_intelligence, excel, ms_office, Microsoft_office, machine_learning, data_mining, data_warehouse, java, C_sharp, C_pp, Hadoop, SQL, java_script, mathematics, SAP, SAS, Spark, uczenie_maszynowe, VBA, R , -VBA, -UniData.UniVerse, -excel, -ms_access, -ms_office, -Microsoft_office, -Is_Programmist, -DB_skill)
#df_full <- select(df_full,id,Is_DataScientist, Is_Programmist, big_data, business_intelligence, excel, ms_office, Microsoft_office, machine_learning, data_mining, data_warehouse, java, C_sharp, C_pp, Hadoop, SQL, java_script, mathematics, SAP, SAS, Spark, uczenie_maszynowe, VBA, R,  DS_skill, Prog_skill, DB_skill)

#commonCols <- intersect(df_DS_full_n, df_full_n)
q <- filter(df_full, Is_DataScientist == 1)
q1 <- group_by(q, DS_skill) %>% summarise(sum(Prog_skill))
q2 <- group_by(q, Prog_skill)%>% filter(DS_skill != 0) %>%summarise(sum(DS_skill))
q3 <- group_by(q, DB_skill)%>% summarise(n())

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



trainIndexes <- dataSplitter(df_full, .75, 1)
trainIndexes <- nameChanger(df_full, trainIndexes)
myNames <- dfsNames(1)

myGlobalDataset <- createMLDfs(df_full, trainIndexes, myNames)

myTrees <- globalRf(myGlobalDataset, myNames)
myGLMs <- globalGLM(myGlobalDataset, myNames)
#myGlobalDataset <- convertMeToNumeric(myGlobalDataset)
myGlobalMatrixDataset <- getMyDFReadyForXGB(myGlobalDataset, myNames)
myXGBoost <- globalXgboosting(myGlobalMatrixDataset, myNames, 48, 8)
rocDF<-makeMyROCdf(myTrees,  myGLMs, myXGBoost , myGlobalDataset, myGlobalMatrixDataset, myNames)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(OptimalCutpoints)
OCrf <- optimal.cutpoints(X = "rfScore", status = "rfTag", methods = "MCT", tag.healthy = 0, data = rocDF)
OCglm <- optimal.cutpoints(X = "glmScore", status = "glmTag", methods = "MCT", tag.healthy = 0, data = rocDF)
OCxgboost <- optimal.cutpoints(X = "xgbScore", status = "xgbTag", methods = "MCT", tag.healthy = 0, data = rocDF)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rfPerf <- measureMyPerf(rocDF$rfScore, rocDF$rfTag, "randomForest")
glmPerf <- measureMyPerf(rocDF$glmScore, rocDF$glmTag, "GLM")
xgbPerf <- measureMyPerf(rocDF$xgbScore, rocDF$xgbTag, "xgb")

# plotting all ROCs

plot(rfPerf, colorize=T, ylab = "tpr", xlab = "fpr", xlim = c(0, 0.2), ylim = c(0, 1), main = "Combined ROC")
plot(glmPerf, colorize=T, ylab = "tpr", xlab = "fpr", xlim = c(0, 0.2), ylim = c(0, 1),   add = TRUE)
plot(xgbPerf, colorize=T, ylab = "tpr", xlab = "fpr", xlim = c(0, 0.2), ylim = c(0, 1),  add = TRUE)


# examining false positives

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rfScoreList <- getSingleModelsScoresFromROCDF(myTrees, myGlobalDataset)
names(rfScoreList) <- myNames

listToSave <- list(myGlobalDataset, myTrees, myGLMs)
names(listToSave) <- c("data", "rf", "glm")
save(listToSave, file = "averageComparison/xtraCols_KWCount/dataListRfGLM.Rdata")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# average ROCs comparison
# reading all average ROCS (withouts xgb) to lists

myRawCols_01 <- getAverageROCPackage("rawCols_01")
myXtraCols_01 <- getAverageROCPackage("xtraCols_01")
myRawCols_KWCount <- getAverageROCPackage("rawCols_KWCount")
myXtraCols_KWCols <- getAverageROCPackage("xtraCols_KWCount")

# creating data frame for average ROCs (RF and GLM) 
# 4 averages per model (4 ROCS, 10 models for each loaded dataset)

averageRFGLMRawCols_01 <- prepareDFForSingleAverageROC(myRawCols_01)
averageRFGLMXtraCols_01 <- prepareDFForSingleAverageROC(myXtraCols_01)
averageRFGLMRawCols_KWCount <- prepareDFForSingleAverageROC(myRawCols_KWCount)
averageRFGLMXtraCols_KWCols <- prepareDFForSingleAverageROC(myXtraCols_KWCols)

#

averageRFGLMRawCols_01ROCMtx <- cbind(averageRFGLMRawCols_01$rF, averageRFGLMRawCols_01$GLM)
averageRFGLMXtraCols_01ROCMtx <- cbind(averageRFGLMXtraCols_01$rF, averageRFGLMXtraCols_01$GLM)
averageRFGLMRawCols_KWCountROCMtx <- cbind(averageRFGLMRawCols_KWCount$rF, averageRFGLMRawCols_KWCount$GLM)
averageRFGLMXtraCols_KWColsROCMtx <- cbind(averageRFGLMXtraCols_KWCols$rF, averageRFGLMXtraCols_KWCols$GLM)

# zła anzwa funkcji - plotAllBest dorbze drukuje ale źle nazwana

plotAllBest(averageRFGLMRawCols_01ROCMtx[, c(2,4)],averageRFGLMRawCols_01ROCMtx[, c(1,3)],
            .2, "Average RF and GLM comparison: 0-1 dataset, no additional columns")
labelMyPlot(c("RF", "GLM"))
plotAllBest(averageRFGLMXtraCols_01ROCMtx[, c(2,4)], averageRFGLMXtraCols_01ROCMtx[, c(1,3)],
            .2, "Average RF and GLM comparison: 0-1 dataset, additional columns")
labelMyPlot(c("RF", "GLM"))
plotAllBest( averageRFGLMRawCols_KWCountROCMtx[, c(2,4)], averageRFGLMRawCols_KWCountROCMtx[, c(1,3)],
             .2, "Average RF and GLM comparison: counted keywords dataset, no additional columns")
labelMyPlot(c("RF", "GLM"))
plotAllBest(averageRFGLMXtraCols_KWColsROCMtx[, c(2,4)], averageRFGLMXtraCols_KWColsROCMtx[, c(1,3)],
            .2, "Average RF and GLM comparison: counted keywords dataset, additional columns")
labelMyPlot(c("RF", "GLM"))
# combined ROCs DF for all RFs models
# procedures/functions for automatic ROCs plotting not yet created:
# - data should be somehow normalized so the procedure won't change along with increase in col nums f.e.

averageRF <- cbind(averageRFGLMRawCols_01$rF, averageRFGLMXtraCols_01$rF$rfScore, averageRFGLMRawCols_KWCount$rF,  averageRFGLMXtraCols_KWCols$rF$rfScore)
names(averageRF) <- c("rfTag_01", "rfRawColsScore_01", "rfXtraColsScore_01", "rfTag_KWCount", "rfRawColsScore_KWCount", "rfXtraColsScore_KWCount")
averageGLM <-  cbind(averageRFGLMRawCols_01$GLM, averageRFGLMXtraCols_01$GLM$glmScore, averageRFGLMRawCols_KWCount$GLM, averageRFGLMXtraCols_KWCols$GLM$glmScore)
names(averageGLM) <- c("glmTag_01", "glmRawColsScore_01", "glmXtraColsScore_01", "glmTag_KWCount", "glmRawColsScore_KWCount", "glmXtraColsScore_KWCount")

labelsRfMatrix <- matrix(0, ncol=4, nrow = length(averageRF$rfTag_01))
labelsGLMMatrix <- matrix(0, ncol=4, nrow = length(averageGLM$glmTag_01))

labelsRfMatrix[, 1:2] <- averageRF$rfTag_01 ; labelsRfMatrix[, 3] <- averageRF$rfTag_KWCount; labelsRfMatrix[, 4] <- averageRFGLMXtraCols_KWCols$rF$rfTag
labelsGLMMatrix[, 1:2] <- averageGLM$glmTag_01 ; labelsGLMMatrix[, 3] <- averageGLM$glmTag_KWCount; labelsGLMMatrix[, 4] <- averageRFGLMXtraCols_KWCols$GLM$glmTag


library(ROCR)

plotAllAverages(averageRF, labelsRfMatrix, .2, "Average RFs on different datasets")
labelMyPlot(c("case 1.", "case 2.", "case 3.", "case 4."))
plotAllAverages(averageGLM, labelsGLMMatrix, .2, "Average GLMs on different datasets")
labelMyPlot(c("case 1.", "case 2.", "case 3.", "case 4."))

averageRFRoc <- prediction(averageRF[, 6], labels = labelsRfMatrix[,4])
averageRFRocPerformance <- performance(averageRFRoc, "tpr", "fpr")
plot(averageRFRocPerformance, colorize = TRUE, lty = 1, xlim = c(0, 0.3))
legend("topright", lty = 2, title = "Type of DF", c("01"), horiz = TRUE)

# reading cved RFs

bestRFRawCols_01 <- get(load("diffDFsAndMods/raw01/bestTree_01_raw.Rdata"))
bestRFXtraCols_01 <- get(load("diffDFsAndMods/xtraCols01/bestTree_01_xtraCols.Rdata"))
bestRFRawCols_KWCount <- get(load("diffDFsAndMods/rawKWCount/bestTree_KWCount_raw.Rdata"))
bestRFXtraCols_KWCount <- get(load("diffDFsAndMods/xtraColsKWCount/bestTree_KWCount_xtraCols.Rdata")) # załaduj ze środowiska !!

df_full_raw_01 <- read_csv("prog_tag_skills_table.csv")
df_full_xtraCols_01 <- read_csv("prog_tag_skills_table_xtraCols.csv")
df_full_raw_KWCount <- read_csv("prog_tag_skills_table_KW_COUNT_retagged.csv")
df_full_xtraCols_KWCount <- read_csv("prog_tag_skills_table_KW_COUNT_retagged_XtraCols.csv")

bestRFTags <- cbind(df_full_raw_01$Is_DataScientist, 
                    df_full_xtraCols_01$Is_DataScientist,
                    df_full_raw_KWCount$Is_DataScientist ,
                    df_full_xtraCols_KWCount$Is_DataScientist
                    )
names(bestRFTags) <- c("raw_01", "xtraCols_01", "raw_KWCount", "xtraCols_KWCount")
bestRFScores <- cbind(predict(bestRFRawCols_01, df_full_raw_01[,-(1:2)]), 
                      predict(bestRFXtraCols_01, df_full_xtraCols_01[,-(1:2)]),
                      predict(bestRFRawCols_KWCount, df_full_raw_KWCount[,-c(1,2)]),
                      predict(bestRFXtraCols_KWCount, df_full_xtraCols_KWCount[,-c(1,2,51)])
                      )
bestRFScores[bestRFScores < 0] <- 0
names(bestRFScores) <- c("raw_01", "xtraCols_01", "raw_KWCount", "xtraCols_KWCount")

plotAllBest(bestRFScores, bestRFTags, .1, "Best RFs trained on different datasets")
labelMyPlot(c("case 1.", "case 2.", "case 3.", "case 4."))

library(OptimalCutpoints)

rocDF <- cbind(bestRFScores[,4], bestRFTags[,4]) %>%
         as.data.frame()
names(rocDF) <- c("rfScore", "rfTag")
OCrfYoud <- optimal.cutpoints(X = "rfScore", status = "rfTag", methods = "Youden", tag.healthy = 0, data = rocDF)
OCrfSpEqualSe <- optimal.cutpoints(X = "rfScore", status = "rfTag", methods = "SpEqualSe", tag.healthy = 0, data = rocDF)


# reading cved GLMs

bestGLMRawCols_01 <- get(load("diffDFsAndMods/raw01/myBestGLM_01_Raw.Rdata"))
bestGLMXtraCols_01 <- get(load("diffDFsAndMods/xtraCols01/myBestGLM_01_xtraCols.Rdata"))
bestGLMRawCols_KWCount <- get(load("diffDFsAndMods/rawKWCount/myBestGLM_KWCount_Raw.Rdata"))
bestGLMXtraCols_KWCount <- get(load("diffDFsAndMods/xtraColsKWCount/myBestGLM_KWCount_xtraCols.Rdata")) # załaduj ze środowiska !!

bestGLMsTags <- cbind(bestGLMRawCols_01$tags, bestGLMXtraCols_01$tags, 
                      bestGLMRawCols_KWCount$tags, bestGLMXtraCols_KWCount$tags)
names(bestGLMsTags) <- c("raw_01", "xtraCols_01", 
                         "raw_KWCount", "xtraCols_KWCount")

bestGLMsScores <- cbind(bestGLMRawCols_01$scores, bestGLMXtraCols_01$scores, 
                        bestGLMRawCols_KWCount$scores, bestGLMXtraCols_KWCount$scores)
names(bestGLMsScores) <- c("raw_01", "xtraCols_01", 
                           "raw_KWCount", "xtraCols_KWCount")

plotAllBest(bestGLMsScores, bestGLMsTags, .1, "Best GLMs trained on different datasets")
labelMyPlot(c("case 1. and 2.", "case 3. and 4."))

# cved curves comparison
# case 1

# XGB training
#
# df_full_raw_01
# df_full_xtraCols_01
# df_full_raw_KWCount
# df_full_xtraCols_KWCount

trainIndexes <- dataSplitter(df_full_raw_KWCount, .9, 1)
trainIndexes <- nameChanger(df_full_raw_KWCount, trainIndexes)
myNames <- dfsNames(1)
myGlobalDataset <- createMLDfs(df_full_raw_KWCount, trainIndexes, myNames)
myGlobalMatrixDataset <- getMyDFReadyForXGB(myGlobalDataset, myNames)

trainingData <- xgb.DMatrix(myGlobalMatrixDataset[[1]]$train[, -(1:2)], label = myGlobalMatrixDataset[[1]]$train[, 2])
set.seed(42)
xgbCV <- xgb.cv(data = trainingData, nround = 50, nfold = 10, nthread = 4, max.depth = 7, objective = "binary:logistic")
# nround = 29 max.depth = 7
# xgbModelList[[1]] <- xgboost(data = trainingData, nrounds = 48, max.depth = 8,  objective = "binary:logistic" ) #objective = "binary:logistic"

myXGBoost <- globalXgboosting(myGlobalMatrixDataset, myNames, 29, 7)

# full data frame
xgboostMatrix <- matrix(data = 0, ncol = length(colnames(myGlobalMatrixDataset[[1]]$train)), nrow = length(unlist(df_full_raw_KWCount[,2])))
transferDF <- df_full_raw_KWCount
for(colNum in 1:length(colnames(myGlobalMatrixDataset[[1]]$train))){
  xgboostMatrix[, colNum] <- unlist(transferDF[, colNum])
  }
xgboostMatrix <- Matrix(xgboostMatrix)
xgbBestMtx <- xgb.DMatrix(data = xgboostMatrix[, -c(1:2)], label = xgboostMatrix[, 2])
xgbBestPreds <- predict(myXGBoost$df_no_1$model, xgbBestMtx)

bestXGBXtraCols_KWCount <- list(myXGBoost$df_no_1$model, xgbBestPreds,xgboostMatrix[, 2])
names(bestXGBXtraCols_KWCount) <- list("model", "scores" ,"tags")
# nround = 20 max.depth = 20

bestXGBRawCols_KWCount <- list(myXGBoost$df_no_1$model, xgbBestPreds, xgboostMatrix[, 2])
names(bestXGBRawCols_KWCount) <- list("model", "scores" ,"tags")
# nround = 47, nfold = 10, nthread = 4, max.depth = 7

bestXGBXtraCols_01 <- list(myXGBoost$df_no_1$model, xgbBestPreds, xgboostMatrix[, 2])
names(bestXGBXtraCols_01) <- list("model", "scores" ,"tags")
# nround = 29 max.depth = 7

bestXGBRawCols_01 <- list(myXGBoost$df_no_1$model, xgbBestPreds, xgboostMatrix[, 2])
names(bestXGBRawCols_01) <- list("model", "scores" ,"tags")
# nround = 32 max.depth = 9

bestXGBTags <- cbind(bestXGBRawCols_01$tags, 
                     bestXGBXtraCols_01$tags, 
                     bestXGBRawCols_KWCount$tags, 
                     bestXGBXtraCols_KWCount$tags)
names(bestXGBTags) <- c("raw_01", "xtraCols_01", "raw_KWCount", "xtraCols_KWCount")
bestXGBScores <- cbind(bestXGBRawCols_01$scores, 
                       bestXGBXtraCols_01$scores, 
                       bestXGBRawCols_KWCount$scores, 
                       bestXGBXtraCols_KWCount$scores)
names(bestXGBTags) <- c("raw_01", "xtraCols_01", "raw_KWCount", "xtraCols_KWCount")

plotAllBest(bestXGBScores, bestXGBTags, .1, "Best XGBs trained on different datasets")
labelMyPlot(c("case 1.", "case 2.", "case 3.", "case 4."))

# cv ROC comparison

bestROCsRawCols_01Tags <- cbind(df_full_raw_01$Is_DataScientist, 
                                bestGLMRawCols_01$tags, 
                                bestXGBRawCols_01$tags
                                )
names(bestROCsRawCols_01Tags) <- c("RF", "GLM", "XGB")
bestROCsRawCols_01Scores <- cbind(predict(bestRFRawCols_01, df_full_raw_01[,-(1:2)]), 
                                  bestGLMRawCols_01$scores, 
                                  bestXGBRawCols_01$scores
                                  )
names(bestROCsRawCols_01Scores) <- c("RF", "GLM", "XGB")


plotAllBest(bestROCsRawCols_01Scores, bestROCsRawCols_01Tags, .1,  "Crossvalidated RF, GLM and XGB, 0-1 dataset, no additional columns")
labelMyPlot(c("RF", "GLM", "XGB"))

bestROCsXtraCols_01Tags <- cbind(df_full_xtraCols_01$Is_DataScientist, bestGLMXtraCols_01$tags, bestXGBXtraCols_01$tags)
names(bestROCsXtraCols_01Tags) <- c("RF", "GLM", "XGB")
bestROCsXtraCols_01Scores <- cbind(predict(bestRFXtraCols_01, df_full_xtraCols_01[,-(1:2)]), bestGLMXtraCols_01$scores, bestXGBXtraCols_01$scores)
names(bestROCsXtraCols_01Scores) <- c("RF", "GLM", "XGB")


plotAllBest(bestROCsXtraCols_01Scores, bestROCsXtraCols_01Tags, .1 , "Crossvalidated RF, GLM and XGB, 0-1 dataset, additional columns")
labelMyPlot(c("RF", "GLM", "XGB"))

bestROCsXtraCols_KWCountTags <- cbind(df_full_xtraCols_KWCount$Is_DataScientist, bestGLMXtraCols_KWCount$tags, bestXGBXtraCols_KWCount$tags)
names(bestROCsXtraCols_KWCountTags) <- c("RF", "GLM", "XGB")
bestROCsXtraCols_KWCountScores <- cbind(predict(bestRFXtraCols_KWCount, df_full_xtraCols_KWCount[,-(1:2)]), bestGLMXtraCols_KWCount$scores, bestXGBXtraCols_KWCount$scores)
names(bestROCsXtraCols_KWCountScores) <- c("RF", "GLM", "XGB")


plotAllBest(bestROCsXtraCols_KWCountScores, bestROCsXtraCols_KWCountTags , .1, "Crossvalidated RF, GLM and XGB, counted keywords dataset, additional columns")
labelMyPlot(c("RF", "GLM", "XGB"))

averageRFRoc <- prediction(xgbBestPreds, labels = xgboostMatrix[,2])
averageRFRocPerformance <- performance(averageRFRoc, "tpr", "fpr")
plot(averageRFRocPerformance, colorize = TRUE, lty = 1)

# # # # # # # # # # # # # # # # # # # # 

save(myTrees, file = "average_myTreesBest.RData")
save(myGLMs, file = "average_myGLMsBest.RData")

myTrees <- get(load("average_myTreesBest.RData"))
myGLMs <- get(load("average_myGLMsBest.RData"))

library(tidyr)
library(dplyr)
myNames <- dfsNames(10)
myTreesVarImpDF <- createVarImpDF(myTrees, myNames, 0)
myTreesVarImpDF <- myTreesVarImpDF[1:20,]
myTreesVarImpDFGathered <- select(myTreesVarImpDF, -myMean, -myMedian, - meanMedianSpread, -myRank) %>% gather( varName)
myTreesVarImpDFGathered <- myTreesVarImpDFGathered[, c(1,3)] %>%
                           group_by(varName) %>%
                           mutate(myRank = grep(pattern = paste0("^",varName,"$"), myTreesVarImpDF$varName)) %>%
                           ungroup() %>%
                           arrange(desc(myRank))
                           
# 1szy boxplopt
myTreesVarImpDFGathered$varName <- factor(myTreesVarImpDFGathered$varName, levels=unique(as.character(myTreesVarImpDFGathered$varName)) )
rfBoxplots <- ggplot(myTreesVarImpDFGathered, aes(factor(varName), value)) + 
     geom_boxplot() +  
     stat_summary(fun.y=mean, colour="red", geom="point", shape=5) +  
     xlab("Variable name") + 
     ylab("Variable's importance") +
     ggtitle("RF Variable Importance Stability (counted keywords dataset, additional columns)") +
     coord_flip()
# 2gi boxplot
qplot(varName, value, data = myTreesVarImpDFGathered, geom = "boxplot") +  coord_flip()

## GLM ##

myNames <- dfsNames(10)

myGLMsVarImpDF <- createVarImpDF(myGLMs, myNames, 1)
myGLMsVarImpDF <- myGLMsVarImpDF[1:20,]
myGLMsVarImpDFGathered <- select(myGLMsVarImpDF, -myMean, -myMedian, - meanMedianSpread, -myRank) %>% gather( varName)
myGLMsVarImpDFGathered <- myGLMsVarImpDFGathered[, c(1,3)] %>%
  group_by(varName) %>%
  mutate(myRank = grep(pattern = paste0("^",varName,"$"), myGLMsVarImpDF$varName)) %>%
  ungroup() %>%
  arrange(desc(myRank))

# 1szy boxplopt
myGLMsVarImpDFGathered$varName <- factor(myGLMsVarImpDFGathered$varName, levels=unique(as.character(myGLMsVarImpDFGathered$varName)) )
glmBoxplots <- ggplot(myGLMsVarImpDFGathered, aes(factor(varName), value)) + 
  geom_boxplot() +  
  stat_summary(fun.y=mean, colour="red", geom="point", shape=5) +  
  xlab("Variable name") + 
  ylab("Variable's importance") +
  ggtitle("GLM Variable Importance Stability (counted keywords dataset, no additional columns)") +
  coord_flip()
# 2gi boxplot
qplot(varName, value, data = myGLMsVarImpDFGathered, geom = "boxplot") +  coord_flip()


bestRFXtraCols_KWCount <- get(load("diffDFsAndMods/xtraColsKWCount/bestTree_KWCount_xtraCols.Rdata")) # załaduj ze środowiska !!
bestRFScores <- as.data.frame(bestRFXtraCols_KWCount$finalModel$importance) %>% 
       mutate(varNames = rownames(.)[row_number()], varImportance = IncNodePurity/max(IncNodePurity)) %>%
       select(varNames, varImportance) %>% 
       arrange(desc(varImportance))

bestRFScores <- bestRFScores[1:20, ]
bestRFScores$varNames <- factor(bestRFScores$varNames, levels = unique(as.character(bestRFScores$varNames)))
bestVarImpPlot <- ggplot(bestRFScores, aes(factor(varNames), varImportance) ) + 
  geom_point() +
  scale_x_discrete(
                    limits = reorder(bestRFScores$varNames, bestRFScores$varImportance, mean) %>% levels()
                    ) +
  xlab("Variable name") + 
  ylab("Variable's importance/max(variables importance)") +
  ggtitle("Best RF variables importance plot") +
  coord_flip()

##################################### poprawić - Is_Programmist dodać i otagować Is_DataScientist

df_full_weights <- read_csv("../../../pracuj_clean/pracuj/ml/data/prog_tag_skills_table_names.csv") %>% select(-skill_multip) 
df_full_weights_head <- df_full_weights[1:3, ]
df_full_weights_tail <- df_full_weights[4:nrow(df_full_weights), ] %>% 
  arrange(df_full_n)
df_full_weights <- rbind(df_full_weights_head, df_full_weights_tail)

#dla wszystkich NIEprogramistów 
df_full <- read_csv("big_DFs/29_09/bigKWCountTab_dates.csv") %>%
          select( -date, -year, -month) %>%
           mutate(Is_DataScientist = 0, Is_Programmist = 0, Foss = 0) %>%
           select(id, Is_DataScientist, Is_Programmist, everything())
df_full <- df_full[, order(names(df_full))] %>%
  select(id, Is_DataScientist, Is_Programmist, everything()) %>%
  mutate(DS_skill = 0, 
         Prog_skill = 0,
         DB_skill = 0,
         Universal_skill = 0)                   #Is_DataScientist = NIEPrgoramista --> JEŚLI BĘDIZE ŹLE TO ODRÓCIĆ
df_full_forGenres <- getMyDFReadyForSkillsGenres(df_full)
df_full_weights_foGenres <-getMyDFReadyForSkillsGenres_SDF(df_full_weights)

#df_full <- select(df_full, -Analitics_analysis)

df_full$DS_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$DS_skill)
df_full$Prog_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$Prog_skill)
df_full$DB_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$DB_skill)
df_full$Universal_skill <- getDifferentSkillsAmount(df_full_forGenres, df_full_weights_foGenres$Universal_skill)

##################################### poprawić - Is_Programmist dodać i otagować Is_DataScientist

myColSums <- read_csv("big_DFs/29_09/bigKWCountTabDS_dates.csv") %>%
             select(-id, -date, -year, -month) %>%
             summarise_all(sum) %>% select(-DS_skill, -Prog_skill, -DB_skill, -Universal_skill)
myColSums <- data.frame(colnames(myColSums), unlist(myColSums[1,]))
names(myColSums) <- c("varNames", "varCount")
myColSums <- myColSums %>%
             arrange(desc(varCount))
myColSums$varName <- factor(myColSums$varName, levels = unique(as.character(myColSums$varName)))

modelAndDFSummary <- merge(bestRFScores, myColSums, by.x = "varNames") %>%
                     select(varNames, varImportance, varCount) %>%
                     mutate(varCount = varCount/max(varCount)) %>% 
                     arrange(varImportance)

modelAndDFSummary_top <- modelAndDFSummary %>% 
                     select(varNames, varImportance)
names(modelAndDFSummary_top) <- c("varNames", "varVal")
modelAndDFSummary_bot <- modelAndDFSummary %>% 
                         select(varNames, varCount)
names(modelAndDFSummary_bot) <- c("varNames", "varVal")

modelAndDFSummary <- rbind (modelAndDFSummary_top, modelAndDFSummary_bot) %>%
                     mutate(`Type of data` = 0)
modelAndDFSummary$`Type of data`[1:16] <- "fraction of max importance"
modelAndDFSummary$`Type of data`[17:32] <- "fraction of max. apperance"
modelAndDFSummary$`Type of data` <- factor(modelAndDFSummary$`Type of data`)



modelAndDFSummaryPlot <- ggplot(data = modelAndDFSummary,  aes(x = varNames, y = varVal, fill = `Type of data`)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
  scale_x_discrete(
    limits = modelAndDFSummary$varNames[1:16]
  ) +
  xlab("Variable name") + 
  ylab("Variable's fraction of importance or variable's fraction of appearances") +
  ggtitle("Importance vs number of appearances of variable/keyword") +
  theme(legend.position="bottom") +
  coord_flip() 

bigDF2Score <- read_csv("big_DFs/29_09/bigDF_mlCols.csv")
bigDFFull <- read_csv("big_DFs/29_09/bigKWCountTab_dates.csv")
df_full$Is_DataScientist <-  predict(bestRFXtraCols_KWCount, df_full[,-c(1,2)])
df_full <- df_full %>% mutate(Is_DataScientist = ifelse(Is_DataScientist <0, 0, Is_DataScientist))
df_full$Is_DataScientist <- round(df_full$Is_DataScientist, 3)

bigDFDS <- df_full %>% filter(Is_DataScientist >= 0.119)
write_csv(df_full, "big_DFs/29_09/bigDFDS.csv")
