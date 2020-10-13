library(dplyr)
library(XML)
library(europepmc)

#This seems to work as a way to filter by year
#Need to find way to search for pubYear instead of firstPDate
data2 <- europepmc::epmc_search(query = '(opioid) AND PUB_YEAR:2019', limit = 100)
data <- data2 %>% filter(!is.na(pmcid))

#Create list
pmcid_list <- as.list(data$pmcid)

# Convert the input xml file to a data frame.
#Tried to do 100 instead of 10 and it took so long
TextList <- list()
for (i in pmcid_list[1:100]){
  tryCatch({
    temp <- europepmc::epmc_ftxt(i)}, error=function(e){})
  xml_1 <- xmlParse(temp)
  body <- paste(xpathSApply(xml_1, '//body', xmlValue), collapse = "|")
  abstract <- paste(xpathSApply(xml_1, '//abstract', xmlValue), collapse = "|")
  references <- paste(xpathSApply(xml_1, '//ref', xmlValue), collapse = "|")
  pmcid <- i
  temp_df <- cbind.data.frame(pmcid,abstract,body,references)
  TextList[[length(TextList)+1]] <- temp_df
}

#Make data frame
text_data <- do.call(rbind, TextList)

#combine data frames
AllData <- text_data %>% inner_join(data, by = "pmcid") %>%
  select(pmid,pmcid,title,journalTitle,pubYear,firstPublicationDate,pubType,citedByCount,
         abstract,body,references)
