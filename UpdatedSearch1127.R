
# PMC Full Text Download --------------------------------------------------
# Created for public health ethics research project Graham & Clark 


# Pre-Flight --------------------------------------------------------------

#Load libraries 
library(dplyr)
library(XML)
library(europepmc)
library(purrr)
library(rentrez)
library(tidytext)

# Set Entrez API key (https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/)
key = ""
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

# Get top cited opioid articles per year 
data <- map_df(years,get_opioid_data)

# Get available text data for top cited opioid articles
text_data <- map_df(data$pmcid, extract_text)

# Count N of articles with full body text available
#Should this be "text_data" not "text"? - tried and worked
#idk if this is right since I probably wasn't supposed to change anything
text_data %>% filter(nchar(body)>1)%>% nrow()

#but then what's this line for?
text_data <- text

#combine data frames
AllData <- text_data %>% inner_join(data, by = "pmcid") %>%
  select(pmid,pmcid,title,journalTitle,pubYear,firstPublicationDate,pubType,citedByCount,
         abstract,body,references)

#Filter data to include only rows that have both abstract and body text data. 
AllData2 <- subset(AllData,AllData$abstract>0 & AllData$body>0)
y
#Write a function that:
SentTok <- function(x){
  #Tokenizes by sentence 
  #Need to figure out how to apply to any character column
sent <- unnest_tokens(AllData2, sentences, body, token = "sentences")
  #Counts the total number of sentences
  #Is this per article or total sentences?
  #Since total sentences is just the total # of rows, I'll go with per article
countperart <- sent %>%
  group_by(pmcid) %>%
  tally()
sentcount <- merge(sent, countperart, by = c("pmcid","pmcid"))

  #Evaluates each sentence for statements of obligation 
ob_sent <- subset(sentcount, str_detect(sent$sentences,"oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability")==TRUE)
  #Classifies obligation sentences: patient vs. society 
ob_sent$topat <- ifelse(str_detect(ob_sent$sentences,"patient|treat|pain|suffer|relief|relieve")==TRUE,1,0)
ob_sent$tosoc <- ifelse(str_detect(ob_sent$sentences,"societ|communit|public|law")==TRUE,1,0)

#Seems that some sentences may have multiple obligations if I try to run all together 
#ob_sent$obto <- ifelse(str_detect(ob_sent$sentences, "patient|treat|pain|suffer|relief|relieve")==TRUE,"patient", ifelse(str_detect(ob_sent$sentences,"societ|communit|public|law")==TRUE,"society",0))

#Counts number of patient obligation sentences
    #ob_sent$topat %>% length(which(topat == "patient"))
#Counts number of society obligation sentences
    tosoc_count <- ob_sent$tosoc %>% count()

  #Returns a df row: n_sent, n_pat, n_soc
#This works to count but doesn't create df 
table(ob_sent$topat)
tab <- count(ob_sent$tosoc == 1)

#counts but table is weird, better ways for sure
stats <- count(ob_sent, topat, tosoc)

statt <- mutate(ob_sent)

count_if(1, ob_sent$topat) # 2
ob_sent %>% count(topat,1)

wtf <- ob_sent %>%
  group_by(topat,tosoc) %>%
  tally()
count(ob_sent$tosoc,"1")
