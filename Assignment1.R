#load libraries
library(europepmc)
library(dplyr)

#search database by term w/ specified limit - chose large number so that
#I could make sure to have at least 1000 rows of observations w/ source PMC
opioid <- epmc_search('opioid', limit = 55000)

#Filter for observations with the source "PMC" and take 1000 out of that
opioidPMC <- opioid %>% 
  filter(source=="PMC") %>%
  sample_n(1000)


