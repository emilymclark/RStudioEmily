
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
library(quanteda)
library(tidyselect)
library(tidyr)
library(stringr)
library(ggplot2)

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
#text_data <- text

#combine data frames
AllData <- text_data %>% inner_join(data, by = "pmcid") %>%
  select(pmid,pmcid,title,journalTitle,pubYear,firstPublicationDate,pubType,citedByCount,
         abstract,body,references)

#Filter data to include only rows that have both abstract and body text data. 
AllData2 <- subset(AllData,AllData$abstract>0 & AllData$body>0)

#TOKENIZE ABSTRACT
sent <- unnest_tokens(AllData2, ab_sentences, "abstract", token = "sentences")
#APPLY TO ABSTRACT
ob_sent <- subset(sent, str_detect(sent$ab_sentences,"oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability")==TRUE)
ob_sent$topat <- ifelse(str_detect(ob_sent$ab_sentences,"patient|treat|pain|suffer|relief|relieve")==TRUE,1,0)
ob_sent$tosoc <- ifelse(str_detect(ob_sent$ab_sentences,"societ|communit|public|law")==TRUE,1,0)
#TALLY number of obligation sentences, to patient, to society, included BOTH column, but can be removed
an_sent <- sent %>% tally(,name = "n_sent")
an_topat <- ob_sent %>% tally(topat, name = "n_topat")
an_tosoc <- ob_sent %>% tally(tosoc, name = "n_tosoc")
an_both <- ob_sent %>% tally(topat & tosoc, name = "n_both")
Totals_ab <- cbind(an_sent,an_topat,an_tosoc,an_both)

#TOKENIZE BODY
sent2 <- unnest_tokens(AllData2, bo_sentences, "body", token = "sentences")
#APPLY TO BODY
ob_sent2 <- subset(sent2, str_detect(sent2$bo_sentences,"oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability")==TRUE)
ob_sent2$topat <- ifelse(str_detect(ob_sent2$bo_sentences,"patient|treat|pain|suffer|relief|relieve")==TRUE,1,0)
ob_sent2$tosoc <- ifelse(str_detect(ob_sent2$bo_sentences,"societ|communit|public|law")==TRUE,1,0)
#TALLY number of obligation sentences, to patient, to society, included BOTH column, but can be removed
bn_sent <- sent2 %>% tally(, name = "n_sent")
bn_topat <- ob_sent2 %>% tally(topat, name = "n_topat")
bn_tosoc <- ob_sent2 %>% tally(tosoc, name = "n_tosoc")
bn_both <- ob_sent2 %>% tally(topat & tosoc, name = "n_both")
Totals_bo <- cbind(bn_sent,bn_topat,bn_tosoc,bn_both)

#MERGE tokenized dfs
test <- rbind(Totals_ab,Totals_bo)
rownames(test) <- c("abstract", "body")

test$section <- c("ab","bo")

#8. Create pairwise correlation plots:
  #a. Abstract vs. body text percent obligation
#pairs(test)
#Error in pairs.default(test) : non-numeric argument to 'pairs'

corr_pat <- cor.test(x=test$n_sent, y=test$n_topat, method = 'spearman')

  #b. Abstract vs. body text percent patient
  #c. Abstract vs. body text percent society
corr_soc <- cor.test(x=test$n_sent, y=test$n_tosoc, method = 'spearman')
