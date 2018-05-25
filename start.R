########  Put all files in a folder #########
########  Create a R project and set project path to that folder ##########

################# Load Packages ##################

library(tidyverse)
library(readr)
library(readxl)

################# Read Files #################


rawreview <- read.csv("PizzaReviews.csv", header = T)

excel_sheets("ProjectSup1.xlsx")

rawpzid <- read_excel("ProjectSup1.xlsx", sheet = "PizzaIDs")

rawstoreid <- read_excel("ProjectSup1.xlsx", sheet = "StoreIDs")

raw_sub <- read_rds("subset1.rds")


################# Formatting Data   ####################

####  file: sub1

# sapply(raw_sub, class)

sub1 <- raw_sub %>% 
  select(2:18)        # drop variable

sub1$TransID <- as.character(sub1$TransID)
sub1$CardID <- as.character(sub1$CardID)
sub1$StoreID <- as.character(sub1$StoreID)
sub1$ItemID <- as.character(sub1$ItemID)
sub1$UOM <- as.character(sub1$UOM)
sub1$CTGR_DESC <- as.character(sub1$CTGR_DESC)
sub1$DEPT_DESC <- as.character(sub1$DEPT_DESC)
sub1$SCTGR_DESC <- as.character(sub1$SCTGR_DESC)
sub1$SDEPT_DESC <- as.character(sub1$SDEPT_DESC)
sub1$ITEM_DESC <- as.character(sub1$ITEM_DESC)      # change data types

# sapply(sub1, class)\






for (i in c(1:nrow(rawpzid))){
  
  if (grepl(".*CALIFORNIA.*|.*CPK.*", x = rawpzid$Description[i])){
    rawpzid$Brand <- "CALIFORNIA"
    rawpzid$Company <- "Nestle"
  } else if (grepl(".*DIGIORNO*|.*DIGORNO.*", x = rawpzid$Description[i])){
    rawpzid$Brand <- "Digiorno"
    rawpzid$Company <- "Nestle"
  } else if (grepl(".*TOMBSTONE*", x = rawpzid$Description[i])){
    rawpzid$Brand <- "TOMBSTONE"
    rawpzid$Company <- "Nestle"
  } else if (grepl("JACK.*", x = rawpzid$Description[i])){
    rawpzid$Brand <- "JACK"
    rawpzid$Company <- "Nestle"
  } else if (grepl("RED.*|RB.*", x = rawpzid$Description[i])){
    rawpzid$Brand <- "Red Baron"
    rawpzid$Company <- "Schwan"
  } else if (grepl("FRES.*", x = rawpzid$Description[i])){
    rawpzid$Brand <- "FRESCHETTA"
    rawpzid$Company <- "Schwan"
  } else 
    rawpzid$Brand <- "Tony"
  rawpzid$Company <- "Schwan"
}
