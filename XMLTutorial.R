library(dplyr)
library(XML)
library(europepmc)

data1 <- europepmc::epmc_search('opioid')
test1 <- europepmc::epmc_ftxt("PMC7451889")


xml_1 <- xmlParse(test1) #Using the xmlParse function we can read 
                         #the xml file into a variable.
print(xml_1)
class(xml_1) # provides the class of the parsed file
xmlRoot(xml_1) #identify the root node of the xml to view the entire file

#looking at the data
xmltop = xmlRoot(xml_1) #gives content of root
xmlName(xmltop) #give name of node, article

xmlSize(xmltop) #how many children in node, 18
xmlName(xmltop[[17]]) #name of root's children, body
xmlName(xmltop[[16]]) #front
xmltop[[16]] #shows text w/in front
xmlName(xmltop[[17]][[2]]) #name of section 2 w/in body

# Create list
TextList <- list()

# Convert the input xml file to a data frame.
for (i in test1){
  body <- paste(xpathSApply(xml_1, '//body', xmlValue), collapse = "|")
  abstract <- paste(xpathSApply(xml_1, '//abstract', xmlValue), collapse = "|")
  references <- paste(xpathSApply(xml_1, '//ref', xmlValue), collapse = "|")
  temp_df <- cbind.data.frame(abstract,body,references)
  TextList[[length(TextList)+1]] <- temp_df
}
