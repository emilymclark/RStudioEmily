
# PMC Full Text Download --------------------------------------------------
# Created for public health ethics research project Graham & Clark 


# Pre-Flight --------------------------------------------------------------

#Load libraries 
library(dplyr)
library(XML)
library(europepmc)
library(purrr)
library(rentrez)

# Set Entrez API key (https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/)
key = "2cdae0e32dfdfbd4751843f66c5c791d8409"
set_entrez_key(key)

# Functions ---------------------------------------------------------------
# Function searches Europe PMC per query (set limit to? )
get_opioid_data <- function(x){
  europepmc::epmc_search(query = paste0('(opioid) AND PUB_YEAR:',x, ' AND IN_EPMC:y'),
                         limit = 10, sort = "cited")
}

#Function uses Enterz API key to extract and parse full text 
extract_text <- function(x){
  file <- entrez_fetch("pmc", id = x,  rettype="xml",parsed = FALSE)
  xml_1 <- xmlParse(file)
  body <- paste(xpathSApply(xml_1, '//body', xmlValue), collapse = "|")
  abstract <- paste(xpathSApply(xml_1, '//abstract', xmlValue), collapse = "|")
  references <- paste(xpathSApply(xml_1, '//ref', xmlValue), collapse = "|")
  pmcid <- x
  Sys.sleep(1)
  cbind.data.frame(pmcid,abstract,body,references)
}

# Data Collection ---------------------------------------------------------

# Set PMC Year range 
years <- c(2005:2019)


#!!!!
# Get top cited opioid articles per year 
data <- map_df(years,get_opioid_data)

# Get available text data for top cited opioid articles
text_data <- map_df(data$pmcid, extract_text)


# Count N of articles with full body text available
text %>% filter(nchar(body)>1)%>% nrow()

text_data <- text

#combine data frames
AllData <- text_data %>% inner_join(data, by = "pmcid") %>%
  select(pmid,pmcid,title,journalTitle,pubYear,firstPublicationDate,pubType,citedByCount,
         abstract,body,references)
