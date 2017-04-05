library(readr)
library(dplyr)

setwd("/home/kebs/Desktop/eR/pracuj/preliminary scores")

freshDataset <- read_csv("pracuj_dataset_27_09.csv")
freshDataset <- freshDataset[!(is.na(freshDataset$description)),]
freshDataset <- freshDataset[freshDataset$description != "NULL",]
freshDataset <- freshDataset %>% select(id, description, position, href)

setwd("/home/kebs/Desktop/eR/pracuj/preliminary scores/liveFilter")
# must read tables at least once!
 source("readNeededTables.R")
 listOfTables <- readNeededTables()


source("extractSkillsTable.R")
scoresTable <- extractSkillsTable(freshDataset$id[1],
                                  freshDataset %>% select(description) %>% filter(row_number()==1),
                                  freshDataset %>% select(position) %>% filter(row_number()==1), 
                                  freshDataset %>% select(href) %>% filter(row_number()==1),
                                  listOfTables
                                  )



library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)

finalIdx <- length(freshDataset$id)

finalScoresTable <- foreach(counter=2:finalIdx, .combine = rbind) %dopar% {
  
      
                         extractSkillsTable(freshDataset$id[counter],
                                            freshDataset %>% select(description) %>% filter(row_number()==counter),
                                            freshDataset %>% select(position) %>% filter(row_number()==counter), 
                                            freshDataset %>% select(href) %>% filter(row_number()==counter),
                                            listOfTables
                        
    )
  

}




stopCluster(cl)

finalScoresTable <- rbind(scoresTable, finalScoresTable)

write_csv(finalScoresTable, "finalScoresTableNT.csv")

oldDF <- read_csv("/home/kebs/Desktop/eR/pracuj/preliminary scores/prog_tag_skills_table_KW_COUNT_retagged_XtraCols.csv") %>%
         select(id, Is_DataScientist) %>%
         filter(Is_DataScientist == 1)

z$Is_DataScientist[grep(pattern = paste0(oldDF$id, collapse = "|"), z$id)] <- 1

write_csv(z, "finalScoresTable.csv")

# for(counter in 2:length(freshDataset$id)){
#   if(counter%%100 == 0){
#     print(counter/length(freshDataset$id) * 100)
#     print(paste0("ETA = ", ((as.numeric(Sys.time(), digits = 15) - startTime)/counter * (length(freshDataset$id) - counter))/60))
#   }
#   
#   scoresTable <- rbind(scoresTable,
#                        extractSkillsTable(freshDataset$id[counter],
#                                           freshDataset %>% select(description) %>% filter(row_number()==counter),
#                                           freshDataset %>% select(position) %>% filter(row_number()==counter), 
#                                           freshDataset %>% select(href) %>% filter(row_number()==counter),
#                                           listOfTables
#                        )
#   )
# }

