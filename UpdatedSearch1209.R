
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
install.packages("psych")
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
ob_sent <- na.omit(ob_sent)
#TALLY number of obligation sentences, to patient, to society, included BOTH column, but can be removed
an_sent <- ob_sent %>% tally(,name = "an_sent")
an_topat <- ob_sent %>% tally(topat, name = "an_topat")
an_tosoc <- ob_sent %>% tally(tosoc, name = "an_tosoc")
an_both <- ob_sent %>% tally(topat & tosoc, name = "an_both")
Totals_ab <- cbind(an_sent,an_topat,an_tosoc,an_both)

#TOKENIZE BODY
sent2 <- unnest_tokens(AllData2, bo_sentences, "body", token = "sentences")
#APPLY TO BODY
ob_sent2 <- subset(sent2, str_detect(sent2$bo_sentences,"oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability")==TRUE)
ob_sent2$topat <- ifelse(str_detect(ob_sent2$bo_sentences,"patient|treat|pain|suffer|relief|relieve")==TRUE,1,0)
ob_sent2$tosoc <- ifelse(str_detect(ob_sent2$bo_sentences,"societ|communit|public|law")==TRUE,1,0)
ob_sent2 <- na.omit(ob_sent2)

#TALLY number of obligation sentences, to patient, to society, included BOTH column, but can be removed
bn_sent <- ob_sent2 %>% tally(, name = "bn_sent")
bn_topat <- ob_sent2 %>% tally(topat, name = "bn_topat")
bn_tosoc <- ob_sent2 %>% tally(tosoc, name = "bn_tosoc")
bn_both <- ob_sent2 %>% tally(topat & tosoc, name = "bn_both")
Totals_bo <- cbind(bn_sent,bn_topat,bn_tosoc,bn_both)

#MERGE tokenized dfs
#by row
test <- rbind(Totals_ab,Totals_bo)
#by column

test2 <- cbind(Totals_ab,Totals_bo)
rownames(test) <- c("abstract", "body")

test$section <- c("ab","bo")

#8. Create pairwise correlation plots:
  #a. Abstract vs. body text percent obligation
      #corr_sent <- cor.test(x=test2$bn_sent, y=test2$an_sent, method = 'spearman')
        #Error in cor.test.default(x = test2$bn_sent, y = test2$an_sent, method = "spearman"): not enough finite observations
      #corr_sent <- corr.test(x=test2$bn_sent, y=test2$an_sent, method = 'spearman')
        #Error in corr.test(x = test2$bn_sent, y = test2$an_sent, method = "spearman"): could not find function "corr.test"
      #corr_sent <- corr.test(test2)
        #Error in corr.test(test2) : could not find function "corr.test"
      #pairs(test)
        #Error in pairs.default(test) : non-numeric argument to 'pairs'
   #b. Abstract vs. body text percent patient
      #corr_pat <- cor.test(x=test2$bn_topat, y=test2$an_topat, method = 'spearman')
        #Error in cor.test.default(x = test2$bn_topat, y = test2$an_topat, method = "spearman"): not enough finite observations
      #corr_pat <- cor.test(test2$bn_topat, test2$an_topat, method = 'spearman')
        #Error in cor.test.default(test2$bn_topat, test2$an_topat, method = "spearman"): not enough finite observations
      #Tried installing psych package bcus I thought I might have needed it to run corr.test (didn't work)
      #corr_pat <- corr.test(test2$bn_topat, test2$an_topat, method = 'spearman')
        #Error in corr.test(test2$bn_topat, test2$an_topat, method = "spearman"): could not find function "corr.test"
      #corr_pat <- psych::corr.test(test2$bn_topat, test2$an_topat, method = 'spearman')
        #Warning messages:
          #1: In sqrt(n - 2) : NaNs produced
          #2: In psych::corr.test(test2$bn_topat, test2$an_topat, method = "spearman") :
            #Number of subjects must be greater than 3 to find confidence intervals.
          #3: In sqrt(n - 3) : NaNs produced
      #corr_pat <- corr.test(test2$bn_topat, test2$an_topat, use = "pairwise", method = 'spearman')
        #Error in corr.test(test2$bn_topat, test2$an_topat, use = "pairwise", method = "spearman"): could not find function "corr.test"
    #c. Abstract vs. body text percent society
      #corr_soc <- cor.test(x=test2$bn_tosoc, y=test2$an_tosoc, method = 'spearman')
        #Error in cor.test.default(x = test2$bn_tosoc, y = test2$an_tosoc, method = "spearman"): not enough finite observations
      #cor.test(formula = ~ bn_tosoc + an_tosoc, data = test2)
        #Error in cor.test.default(x = 62, y = 3) : not enough finite observations
        #Going tp try restarting r since I keep getting this error
#---------------------------------------------------------------

#LETS TRY CREATING DIF DF

#Here I kept all of the other df info and attached tallies
#There is likely a prettier way of doing this 
#To Abstract
an_sent2 <- ob_sent %>%
  group_by(pmcid) %>%
  tally(,name = "an_sent")
sentcount <- merge(ob_sent, an_sent2, by = c("pmcid","pmcid"))

an_topat2 <- ob_sent %>%
  group_by(pmcid) %>%
  tally(topat, name = "an_topat")
sentcount2 <- merge(sentcount, an_topat2, by = c("pmcid","pmcid"))

an_tosoc2 <- ob_sent %>%
  group_by(pmcid) %>%
  tally(tosoc, name = "an_tosoc")
sentcount3 <- merge(sentcount2, an_tosoc2, by = c("pmcid","pmcid"))

an_both2 <- ob_sent %>%
  group_by(pmcid) %>%
  tally(topat & tosoc, name = "an_both") 
sentcount4 <- merge(sentcount3, an_both2, by = c("pmcid","pmcid"))


#To Body
n_sent2 <- ob_sent2 %>%
  group_by(pmcid) %>%
  tally(,name = "n_sent")
b_sentcount <- merge(ob_sent2, n_sent2, by = c("pmcid","pmcid"))

n_topat2 <- ob_sent2 %>%
  group_by(pmcid) %>%
  tally(topat, name = "n_topat")
b_sentcount2 <- merge(b_sentcount, n_topat2, by = c("pmcid","pmcid"))

n_tosoc2 <- ob_sent2 %>%
  group_by(pmcid) %>%
  tally(tosoc, name = "n_tosoc")
b_sentcount3 <- merge(b_sentcount2, n_tosoc2, by = c("pmcid","pmcid"))

n_both2 <- ob_sent2 %>%
  group_by(pmcid) %>%
  tally(topat & tosoc, name = "n_both") 
b_sentcount4 <- merge(b_sentcount3, n_both2, by = c("pmcid","pmcid"))

#b. Abstract vs. body text percent patient
corr_pat <- cor.test(x=sentcount4$an_topat, y=b_sentcount4$n_topat, method = 'spearman')
#Error in cor.test.default(x = sentcount4$an_topat, y = b_sentcount4$n_topat,  : 'x' and 'y' must have the same length
corr_pat <- cor.test(x=sentcount4$an_topat, y=b_sentcount4$n_topat, method = 'spearman', by = c("pmcid","pmcid"))
#Error in cor.test.default(x = sentcount4$an_topat, y = b_sentcount4$n_topat,  : 'x' and 'y' must have the same length
cor.test(x=sentcount4$an_topat, y=b_sentcount4$n_topat, method = 'spearman')
#Error 'x' and 'y' must have the same length