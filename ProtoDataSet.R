library(dplyr)
library(XML)
library(europepmc)

data1 <- europepmc::epmc_search('opioid')
test1 <- europepmc::epmc_ftxt("PMC7372935")


xml_1 <- xmlParse(test1) #Using the xmlParse function we can read 
                         #the xml file into a variable.

#looking at the data
xmltop = xmlRoot(xml_1) #gives content of root
xmlName(xmltop) #give name of node, article

xmlSize(xmltop) #how many children in node, 10
xmlName(xmltop[[9]]) #name of root's children, body

# Create list
TextList <- list()

# Convert the input xml file to a data frame.
for (i in test1){
  body <- paste(xpathSApply(xml_1, '//body', xmlValue), collapse = "|")
  abstract <- paste(xpathSApply(xml_1, '//abstract', xmlValue), collapse = "|")
  references <- paste(xpathSApply(xml_1, '//ref', xmlValue), collapse = "|")
  temp_df <- cbind.data.frame(abstract,body,references)
  TextList[[length(TextList)+1]] <- temp_df}

#extract row from original dataframe
row <- subset(data1, pmcid =="PMC7372935")

#cbind with temp_df (text) and select key variables
whole <- cbind(row, temp_df) %>%
  select(pmid,pmcid,title,journalTitle,pubYear,pubType,citedByCount,
         abstract,body,references)


