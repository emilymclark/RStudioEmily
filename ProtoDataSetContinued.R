library(dplyr)
library(XML)
library(europepmc)

data1 <- europepmc::epmc_search('opioid', limit = 100000)
data <- data1 %>% filter(!is.na(pmcid))

#Create list
pmcid_list <- as.list(data$pmcid)

# Convert the input xml file to a data frame.
TextList <- list()
for (i in pmcid_list){
  temp <- europepmc::epmc_ftxt(i)
  xml_1 <- xmlParse(temp)
  body <- paste(xpathSApply(xml_1, '//body', xmlValue), collapse = "|")
  abstract <- paste(xpathSApply(xml_1, '//abstract', xmlValue), collapse = "|")
  references <- paste(xpathSApply(xml_1, '//ref', xmlValue), collapse = "|")
  temp_df <- cbind.data.frame(abstract,body,references)
  TextList[[length(TextList)+1]] <- temp_df
  }

#extract row from original dataframe
row <- subset(data1, pmcid =="PMC7372935")

#cbind with temp_df (text) and select key variables
whole <- cbind(row, temp_df) %>%
  select(pmid,pmcid,title,journalTitle,pubYear,pubType,citedByCount,
         abstract,body,references)


