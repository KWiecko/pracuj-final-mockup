readNeededTables <- function(myConnection){

  library(DBI)
  library(RPostgreSQL)

  #############################
  # connecting to database

   # myConnection <- dbConnect(
   #   PostgreSQL(),
   #   dbname = "pracuj",
   #   user = "pracuj",
   #   password = "",
   #   host = "services.mini.pw.edu.pl"
   # )

  #############################
  # required tables:
  #   -offers - table with offer descripiton (only for test purposes)
  #   -singlekeyworddictionary
  #   -doublekeyworddictionary
  #   -contextphrasetable
  #   -skillcategorytable

  singleKeywordDict <- dbReadTable(myConnection, "singlekeyworddictionary")
  doubleKeywordDict <- dbReadTable(myConnection, "doublekeyworddictionary")
  doubleKeywordDict$keyword1regex <- gsub(pattern = "qqqqqqqqqq", replacement = "\\,", doubleKeywordDict$keyword1regex )
  doubleKeywordDict$keyword2regex <- gsub(pattern = "qqqqqqqqqq", replacement = "\\,", doubleKeywordDict$keyword2regex )
  doubleKeywordDict$exceptionregex <- gsub(pattern = "qqqqqqqqqq", replacement = "\\,", doubleKeywordDict$exceptionregex )
  contextPhrases <- dbReadTable(myConnection, "contextphrasestable")
  skillCategory <- dbReadTable(myConnection, "skillcategorytable")

  listToReturn <- list(singleKeywordDict,
                       doubleKeywordDict,
                       contextPhrases,
                       skillCategory)
  names(listToReturn) <- c("singlekeyworddictionary", "doublekeyworddictionary", "contextphrasestable", "skillcategorytable")

  return(listToReturn)

}
