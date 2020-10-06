library(dplyr)
library(XML)
library(europepmc)

data1 <- europepmc::epmc_search('opioid', limit = 100000)
data <- data1 %>% filter(!is.na(pmcid))

#Create list
pmcid_list <- as.list(data$pmcid)

# Convert the input xml file to a data frame.
TextList <- list()
for (i in pmcid_list[1:10]){
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
  select(pmid,pmcid,title,journalTitle,pubYear,pubType,citedByCount,
         abstract,body,references)

