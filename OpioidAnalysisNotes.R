###OPIOID ANALYSIS 3.0 (final|codebase locked)###

##LOAD LIBRARIES 
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(reshape2)
library(RSQLite)
library(DBI)
library(gridExtra)

#Load data
nejm <- read.csv(file = "nejmdata.csv",header = TRUE)

#Load data by connecting to database
  #SQLite =  a C-language library that implements a small, fast, 
    #self-contained, high-reliability, full-featured, SQL database engine. SQLite is the 
    #most used database engine in the world
  #RSQLite = RSQLite is the easiest way to use a database from R because the package 
    #itself contains SQLite; no external software is needed
  #Jama? =  The Journal of the American Medical Association; a peer-reviewed medical 
    #journal published 48 times a year by the American Medical Association
jamadb <- dbConnect(RSQLite::SQLite(),"jama-data.sqlite")

#The function dbListTables() displays the names tables in the remote database
dbListTables(jamadb)

#Imports database table from the jama as a data frame
#!!What is the role of "jama" here?
jama <- dbReadTable(jamadb,"jama")

#Create column specifying journal and then bind data frames
nejm$journal <- "nejm"
jama$journal <- "jama"
arts <- rbind(nejm,jama)

#Clean 
#Ensure variables are in character format where needed
arts$title <- as.character(arts$title)
arts$url <- as.character(arts$url)
arts$type <- as.character(arts$type)
#Same but condense body by reducing white space 
arts$body <- as.character(str_squish(arts$body))

#Creates new column called 'group' that labels observations based on time period specified
  #Example: ifelse(expression, returns this if true, returns this if false)
  #!!Why are they labeled this way?
arts$group <- ifelse(arts$date<2001,"ah",ifelse(arts$date>=2008,"oe","fvs"))


##DESCRIBE DATA 

#Describe raw data set (articles per year)
#Creates new df that have a row for each year an observation appears and tallies 
#how many observations are seen that year
arts_date <- arts %>% group_by(date)%>% tally()

#iterated for date ranges 
#Gives mean of the number of observations seen btwn 2010 and 2019
  #na.rm = tells the function whether or not to remove NA values from the calculation
mean(arts_date$n[arts_date$date>=2010 & arts_date$date<2020],na.rm=TRUE)

# (articles per journal)
arts_journal <- arts %>% group_by(journal) %>% tally()

# (articles per journal per year, 1980-2018)
#Creates df with date,journal,n column
arts_datejournal <- arts %>% group_by(date,journal)%>% tally()
#Removes any rows with years that do not meet the specified criteria
arts_datejournal <- subset(arts_datejournal,date>1979 & date<2019)



##TRIGRAMS ANALYSIS 

#Create trigrams, filter "opioid|pain"; remove double stop_word trigrams
    #unnest_tokens function is a way to convert a df with a text column to be 
    #one-token-per-row
    #Here the token is defined as ngrams
    #ngrams = sequences of words (specified as 3)
bigrams <- unnest_tokens(arts, ngram, body, token = "ngrams", n = 3)

#Specifically pull observations where opioid/pain are mentioned
bigrams <- subset(bigrams, str_detect(bigrams$ngram,"opioid|pain")==TRUE)

#Creates df that splits strings into 3 separate columns 
bgsp <- as.data.frame(str_split(bigrams$ngram," ",simplify = TRUE))

#Binds split columns to bigrams column
bigrams <- cbind(bigrams,bgsp)

#Creates value listing out stop words
stops<-stop_words$word

#sprintf() allows you to create strings as output using formatted data
#paste function in R will accept an unlimited number of scalars, and join 
#them together, separating each scalar with a space by default
#!! Can you better explain this line?
stops<-paste(sprintf("\\b%s\\b", stops), collapse = '|')

#Creates stops column in bigrams which counts the number of stop words in each row
bigrams$stops <- (str_count(bigrams$V1,stops))+(str_count(bigrams$V2,stops))+(str_count(bigrams$V3,stops))

#Gets rid of trigrams with more than 1 stop word
bigrams <- subset(bigrams,bigrams$stops<2)

#Separate each specified time period into a new df
ah <- subset(bigrams,bigrams$date<2001)
fvs <- subset(bigrams,bigrams$date >= 2001 & bigrams$date < 2008)
oe <- subset(bigrams,bigrams$date >= 2008)

#Visualize findings 
#Makes bar graph of top 20 trigrams
ah_plot <- ah %>%
  count(ngram, sort = TRUE) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  top_n(20,ngram)%>%
  ggplot(aes(ngram, n)) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL)+
  coord_flip()+
  theme_minimal()

fvs_plot <- fvs %>%
  count(ngram, sort = TRUE) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  top_n(20,ngram)%>%
  ggplot(aes(ngram, n)) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL)+
  coord_flip()+
  theme_minimal()

oe_plot <- oe %>%
  count(ngram, sort = TRUE) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  top_n(20,ngram)%>%
  ggplot(aes(ngram, n)) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL)+
  coord_flip()+
  theme_minimal()


##TEXT MINING ASSESSMENTS | following trigrams analysis 

#Automated summative content analysis for each coding category 

#Turns text lowercase
arts$body <- tolower(arts$body)

#Creates new columns that count the number of times each them is 
#mentioned within a text
arts$pain <- str_count(arts$body,"pain|chronic|relief|relieve|manage|undertreat")
arts$abuse <- str_count(arts$body,"abuse|diversion|addict|overdose|overprescrib|dependen")
arts$stigma <- str_count(arts$body,"abuser|addict |addicts|user")
arts$n_eth <- str_count(arts$body," moral| ethic|duty|oblig|rights") 


#Objligation Analysis 
#Makes df that has a row for every sentence?
arts_sent <- unnest_tokens(arts, sentences, body, token = "sentences")
#Removes any rows that do now contain mentions of key words
arts_sent <- subset(arts_sent, str_detect(arts_sent$sentences,"oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability")==TRUE)

#!!What are these two comment lines for?
#arts_sent$obligor <- str_extract(arts_sent$sentences,"^.+?(?=(oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability|onus))")
#arts_sent$obligee <- str_extract(arts_sent$sentences,"(oblig|responsib|need|must|duty|duties|account|bound| owe|require|liability|onus).+?(?=$)")

#Creates 3 new columns that label observations as 1(true) or 0(false) for
#whether or not the key words appear
arts_sent$fromdoc <- ifelse(str_detect(arts_sent$sentences,"doctor|provider|physician|clinician|provider")==TRUE,1,0)
arts_sent$topat <- ifelse(str_detect(arts_sent$sentences,"patient|treat|pain|suffer|relief|relieve")==TRUE,1,0)
arts_sent$tosoc <- ifelse(str_detect(arts_sent$sentences,"societ|communit|public|law")==TRUE,1,0)

#filter out sentences without Dr. obligors 
#Creates df that only shows rows where "fromdoc" = 1(true) AND one of the
#other occurrences is true (topat OR to soc)
arts_ob <- subset(arts_sent,arts_sent$fromdoc==1 & (arts_sent$topat==1 | arts_sent$tosoc ==1))


