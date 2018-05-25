########  Put all files in a folder #########
########  Create a R project and set project path to that folder ##########

################# Load Packages ##################

library(plyr)
library(caret)
library(pscl)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(stringr)
library(arules)
library(arulesViz)
library(dummy)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud)
library(plotly)

################# Read Files #################

rawdata <- read.csv("LowesCombined2.txt", header = T, sep = "~", stringsAsFactors = F)

rawreview <- read.csv("PizzaReviews.csv", header = T, stringsAsFactors = F)

pzlines <- readLines("PizzaBrands.csv")
pzlines <- gsub('([^,])"([^,])', '\\1""\\2', pzlines)
rawpzid <- read.csv(textConnection(pzlines))

excel_sheets("ProjectSup1.xlsx")

rawstoreid <- read_excel("ProjectSup1.xlsx", sheet = "StoreIDs")

################# rename columns and transfer data types  #################

################# rawdata #########

rawdata <- rawdata[c(2,3,4,5,6,7,8,9,10,11,12,13,17,15,14,16,18)]   # reorder columns

rawdata$TransID <- as.character(rawdata$TransID)
rawdata$CardID <- as.character(rawdata$CardID)
rawdata$StoreID <- as.character(rawdata$StoreID)
rawdata$ItemID <- as.character(rawdata$ItemID)
rawdata$TransDate <- as.Date(rawdata$TransDate)

################ rawreivew ########

################ rawpzid #######

rawpzid$ProdID <- as.character(rawpzid$ProdID)
rawpzid$Description <- as.character(rawpzid$Description)
names(rawpzid)[names(rawpzid) == "ProdID"] <- "ItemID"

rawpzid$Brand <- rawpzid$Description
rawpzid$Company <- rawpzid$Description

rawpzid$Brand <- gsub(".*CALIFORNIA.*|.*CPK.*|.*C/P.*|CALIF.*", replacement = "CALIFORNIA", x = rawpzid$Brand)
rawpzid$Company <- gsub(".*CALIFORNIA.*|.*CPK.*|.*C/P.*|CALIF.*", replacement = "Nestle", x = rawpzid$Company)

rawpzid$Brand <- gsub(".*DIGIORNO.*|.*DIGORNO.*", replacement = "DIGIORNO", x = rawpzid$Brand)
rawpzid$Company <- gsub(".*DIGIORNO.*|.*DIGORNO.*", replacement = "Nestle", x = rawpzid$Company)

rawpzid$Brand <- gsub(".*TOMBSTONE.*", replacement = "TOMBSTONE", x = rawpzid$Brand)
rawpzid$Company <- gsub(".*TOMBSTONE.*", replacement = "Nestle", x = rawpzid$Company)

rawpzid$Brand <- gsub(".*JACK.*", replacement = "JACK", x = rawpzid$Brand)
rawpzid$Company <- gsub(".*JACK.*", replacement = "Nestle", x = rawpzid$Company)

rawpzid$Brand <- gsub("RED.*|RB.*", replacement = "RED BARON", x = rawpzid$Brand)
rawpzid$Company <- gsub("RED.*|RB.*", replacement = "Schwan", x = rawpzid$Company)

rawpzid$Brand <- gsub(".*FRES.*|.*FRESCHETTA.*", replacement = "FRESCHETTA", x = rawpzid$Brand)
rawpzid$Company <- gsub(".*FRES.*|.*FRESCHETTA.*", replacement = "Schwan", x = rawpzid$Company)

rawpzid$Brand <- gsub("TONY.*", replacement = "TONY", x = rawpzid$Brand)
rawpzid$Company <- gsub("TONY.*", replacement = "Schwan", x = rawpzid$Company)

rawpzid <- rawpzid %>% 
  filter(Brand %in% c("CALIFORNIA","DIGIORNO", "TOMBSTONE", "JACK", "RED BARON", "FRESCHETTA","TONY"), Company %in% c("Nestle","Schwan"))

############### rawstoerid ########

names(rawstoreid) <- c("StoreID", "Addr", "City", "State", "Zip", "Community", "Pop", "medi_income", "House_unit", "medi_age", "pover", "umemploy", "number_store")

rawstoreid$StoreID <- as.character(rawstoreid$StoreID)
rawstoreid$Zip <- as.character(rawstoreid$Zip)

########################## Business Question #################################

############### Join Tables

txnrecord <- rawdata %>% 
  left_join(rawpzid, by = "ItemID") %>% 
  left_join(rawstoreid, by = "StoreID")

############# Overall Sales Trend ################

############# Get Freschetta Product ID ###

FresID <- txnrecord %>% 
  select(5, 17, 19) %>% 
  filter(Brand == "FRESCHETTA")

FresID <- as.data.frame(unique(FresID)) 

names(FresID) <- c("ItemID","Description", "Brand")



txnrecord$ym <- format(txnrecord$TransDate,"%Y-%m")

####### Units

txnrecord %>% 
  filter(Brand == "FRESCHETTA") %>% 
  group_by(ym) %>% 
  summarise(unitsum = sum(Quantity)) %>% 
  ggplot()+
  geom_line(aes(x = ym, y = unitsum, group = 1), linetype = 1, color = "#006699")+
  geom_point(aes(x = ym, y = unitsum), size = 4, color = "#FF9933")+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))


######## Dollars

txnrecord %>% 
  filter(Brand == "FRESCHETTA") %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(ym) %>% 
  summarise(salesum = sum(sales)) %>% 
  ggplot()+
  geom_line(aes(x = ym, y = salesum, group = 1), linetype = 1, color = "#006699")+
  geom_point(aes(x = ym, y = salesum), size = 4, color = "#FF9933")+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))




############# Different Product Sales Trend ################

######## Units

txnrecord %>% 
  filter(Brand == "FRESCHETTA") %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(Description) %>% 
  summarise(unitsum = sum(Quantity)) %>% 
  ggplot()+
  geom_bar(aes(x = reorder(Description, unitsum), y = unitsum), stat = "identity")+
  theme(axis.text.x = element_text(angle = -90, hjust = -0.05))+
  coord_flip()


############### Dollars

txnrecord %>% 
  filter(Brand == "FRESCHETTA") %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(Description) %>% 
  summarise(salesum = sum(sales)) %>% 
  ggplot()+
  geom_bar(aes(x = reorder(Description, salesum), y = salesum), stat = "identity")+
  theme(axis.text.x = element_text(angle = -90, hjust = -0.05))+
  coord_flip()

############## Dollar Sales Trend

txnrecord %>% 
  filter(Brand == "FRESCHETTA") %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(Description, ym) %>% 
  summarise(salesum = sum(sales)) %>% 
  plot_ly(x =  ~ym, y = ~salesum, group = ~Description,
type = "scatter", color = ~Description, mode = "lines+markers")


#################### New product Introduction ##########################

##### find the first day when the product is introduced

Fres_first_Date <- txnrecord %>% 
  filter(ItemID %in% FresID$ItemID) %>% 
  group_by(ItemID) %>% 
  arrange(TransDate) %>% 
  slice(1L) %>% 
  select(5,17,33)

#######  calculate total market share

pzmkt <- txnrecord %>% 
  filter(CTGR_DESC == "FZ PIZZA") %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(ym) %>% 
  summarise(salesum = sum(sales))

######## calculate fres market share
  
fres_pzmkt_share <- txnrecord %>% 
  filter(Brand == "FRESCHETTA") %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(ItemID, ym) %>% 
  summarise(prodsum = sum(sales)) %>% 
  left_join(pzmkt, by = "ym") %>% 
  mutate(mktshr = prodsum / salesum) 
  
fres_remove_top <- fres_pzmkt_share %>% 
  filter(ym == "2013-05" & mktshr < 0.0015) 

fres_pzmkt_share %>% 
  filter(ItemID %in% fres_remove_top$ItemID | ItemID == "7218063649") %>% 
ggplot()+ 
  geom_line(aes(x = ym, y = mktshr, group = ItemID, color = ItemID))+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))

#######  calculate average market share  around 0.005

pzmkt_share <- txnrecord %>% 
  filter(!is.na(Brand)) %>% 
  mutate(sales = Quantity * NetRevenue) %>%
  group_by(ym, ItemID) %>% 
  summarise(prodsum = sum(sales)) %>% 
  left_join(pzmkt, by = "ym") %>% 
  mutate(mktshr = prodsum / salesum) %>% 
  group_by(ym) %>% 
  summarise(avg_shr = mean(mktshr)) %>% 
  arrange(desc(avg_shr))

summary(pzmkt_share$avg_shr)

################################ Sales within 6 brands ###############################

pzbrandsale <- txnrecord %>% 
  dplyr::select(1, 6, 10, 17, 19, 33) %>% 
  filter(!is.na(Brand)) %>% 
  mutate(pzsale = Quantity * NetRevenue) %>% 
  group_by(Brand, ym) %>% 
  summarize(salesum = sum(pzsale))

pzbrandsale <- pzbrandsale %>% 
  group_by(ym) %>% 
  mutate(saleper = salesum / sum(salesum))

pzbrandsale$hlt[pzbrandsale$Brand == "FRESCHETTA"] <- "#66CC33"
pzbrandsale$hlt[!pzbrandsale$Brand == "FRESCHETTA"] <- "#999999"

cbPalette <- c("#66CC33", "#999999")

ggplot(pzbrandsale)+
  geom_line(aes(x = ym, y = saleper, group = Brand, color = hlt, linetype = hlt), size =1)+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))+
  scale_color_manual(values = cbPalette)+
  guides(linetype=FALSE, color = FALSE)+
  theme(panel.background = element_blank())+
  xlab("Year")+ylab("Market Share")

frsbrandsale <- pzbrandsale %>% 
  dplyr::filter(Brand == "FRESCHETTA")

digibrandsale <- pzbrandsale %>% 
  dplyr::filter(Brand == "DIGIORNO")

####################### product ####################################

pzprodsale <- txnrecord %>% 
  dplyr::select(1, 6, 10, 17, 19, 33) %>% 
  dplyr::filter(Brand == "FRESCHETTA") %>% 
  mutate(pzsale = Quantity * NetRevenue) %>% 
  dplyr::group_by(ITEM_DESC) %>% 
  summarize(salesum = sum(pzsale), qty = sum(Quantity))

pzprodsale <- pzprodsale %>% 
  mutate(per = salesum / 99500.84)

pzprodsale$per_round <- round(pzprodsale$per, 2)

newprodid <- Fres_first_Date %>% 
  dplyr::filter(ym != "2013-04")

oldpzprodsale <- pzprodsale %>% 
  dplyr::filter(!ITEM_DESC %in% newprodid$ItemID)


italian <- txnrecord %>% 
  dplyr::select(1, 6, 10, 17, 19, 33) %>% 
  filter(Brand == "FRESCHETTA" & ITEM_DESC == "FRESCHETTA PIZZA BRICK OVEN ITALIAN PEPPERONI" & ym > "2013-10")

################################### avg levels ##################

frstxnlst <- txnrecord %>% 
  select(1,19)

frstxnlst$fi[frstxnlst$Brand == 'FRESCHETTA'] <- "1"
frstxnlst$fi[!frstxnlst$Brand == 'FRESCHETTA'] <- "0"
  
txnlst <- frstxnlst %>% 
  filter(fi == "1") %>% 
  select(1) %>% 
  unique()

avglst <- txnrecord %>% 
  select(1,2,6,10,33) %>% 
  filter(CardID != "0" & TransID %in% txnlst$TransID) %>% 
  mutate(sale = Quantity * NetRevenue) 

dollarqty <- avglst %>% 
  group_by(CardID, TransID) %>% 
  summarize(salesum = sum(sale), qtysum = sum(Quantity)) %>% 
  group_by(CardID) %>% 
  summarize(avgsale = mean(salesum), avgqty = mean(qtysum))

freqvis <- avglst %>% 
  select(1,2,5) %>% 
  unique() %>% 
  group_by(CardID,ym) %>% 
  summarize(fr = n()) %>% 
  group_by(CardID) %>% 
  summarise(fre = mean(fr))
  
################### Cannibalization Brand ###################


Fres_first_Date$newgroup[Fres_first_Date$ym == "2013-04"] <- "1304"
Fres_first_Date$newgroup[Fres_first_Date$ym == "2013-06"] <- "1306"
Fres_first_Date$newgroup[Fres_first_Date$ym == "2013-09"] <- "1309"
Fres_first_Date$newgroup[Fres_first_Date$ym == "2013-11"] <- "1311"
Fres_first_Date$newgroup[Fres_first_Date$ym == "2014-05"] <- "1405"

Fres_first_Date %>% 
  select(1, 4) %>% 
  right_join(txnrecord, by = "ItemID") %>% 
  filter(Brand == "FRESCHETTA" & newgroup %in% c("1306", "1309", "1311", "1405")) %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(newgroup, ym) %>% 
  summarise(salesum = sum(sales)) %>% 
  ggplot()+
  geom_line(aes(x = ym, y = salesum, group = newgroup, color = newgroup))+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))
  


####################### cannibalization Competition #############################

txnrecord %>% 
  filter(!is.na(Brand)) %>% 
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(Brand, ym) %>% 
  summarise(salesum = sum(sales)) %>% 
  ggplot()+
  geom_line(aes(x = ym, y = salesum, group = Brand, color = Brand))+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))

############################ cross sell or up sell ###########################


######################### cross sell


######### create basket analysis datasets



txn_item <- txnrecord %>% 
  select(1,17) %>% 
  filter(TransID != "0") %>% 
  arrange(TransID)



itemList <- ddply(txn_item,"TransID", 
                  function(txn_item)paste(txn_item$ITEM_DESC, 
                                     collapse = ","))

itemList <- itemList %>% 
  select(2)

names(itemList) <- "items"

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

########## read file as transaction format

mkt_bas <- read.transactions("market_basket.csv", format = "basket", sep = ",")

summary(mkt_bas)

itemFrequencyPlot(mkt_bas, topN = 20, type = "absolute")  # interesting   totinos pizza

############## create rules trail 

try_rules <- apriori(mkt_bas, parameter = list(supp = 0.001, conf = 0.5, minlen =2, maxtime =100))
try_rules <- sort(try_rules, by = "support", decreasing = T)
summary(try_rules)

arules::inspect(try_rules)   ### rule # 20  23  36  44  46   50 

############################### create rules in brand level

txnrecord_brand <- txnrecord

txnrecord_brand$ITEM_DESC <- gsub(txnrecord_brand$ITEM_DESC, pattern = ".*FRESCHETTA.*", replacement = "FRESCHETTA")

txn_brand <- txnrecord_brand %>% 
  select(1,17) %>% 
  filter(TransID != "0") %>% 
  arrange(TransID)

brandList <- ddply(txn_brand,"TransID", 
                  function(txn_brand)paste(txn_brand$ITEM_DESC, 
                                          collapse = ","))

brandList <- brandList %>% 
  select(2)

names(brandList) <- "items"

write.csv(brandList,"market_basket_brand.csv", quote = FALSE, row.names = TRUE)

########## read file as transaction format

mkt_bas_brand <- read.transactions("market_basket_brand.csv", format = "basket", sep = ",")

summary(mkt_bas_brand)

itemFrequencyPlot(mkt_bas_brand, topN = 20, type = "absolute")  # interesting   totinos pizza

############## create rules trail 

brand_left_rule <- apriori(mkt_bas_brand, parameter = list(supp = 0.0001, conf = 0.5, minlen =2, maxtime = 100),
                           appearance = list(default="lhs",rhs="FRESCHETTA"))
summary(brand_left_rule)
brand_left_rule <- sort(brand_left_rule, by = "confidence", decreasing = T)
arules::inspect(brand_left_rule)

brand_left_rule2 <- apriori(mkt_bas_brand, parameter = list(supp = 0.001, conf = 0.01, minlen =2, maxtime = 100),
                           appearance = list(default="lhs",rhs="FRESCHETTA"))
summary(brand_left_rule2)
brand_left_rule2 <- sort(brand_left_rule2, by = "confidence", decreasing = T)
arules::inspect(brand_left_rule2)

############## target our brand  very useful!!!!!!!!!!!!!!

txn_prod <- txnrecord 
txn_prod$prodindi[txn_prod$Brand == "FRESCHETTA"] <- 1
txn_prod$prodindi[!txn_prod$Brand == "FRESCHETTA" | is.na(txn_prod$Brand)] <- 0

txnlst <- txn_prod %>% 
  select(1,34) %>% 
  filter(TransID != "0") %>% 
  group_by(TransID) %>% 
  summarise(indi_sum= sum(prodindi)) %>% 
  filter(indi_sum > 0)

txn_prod <- txn_brand %>% 
  filter(TransID != "0" & TransID %in% txnlst$TransID) %>% 
  arrange(TransID)

prodList <- ddply(txn_prod,"TransID", 
                   function(txn_prod)paste(txn_prod$ITEM_DESC, 
                                            collapse = ","))

prodList <- prodList %>% 
  select(2)

names(prodList) <- "items"

write.csv(prodList,"market_basket_prod.csv", quote = FALSE, row.names = TRUE)

########## read file as transaction format

mkt_bas_prod <- read.transactions("market_basket_prod.csv", format = "basket", sep = ",")

summary(mkt_bas_prod)

itemFrequencyPlot(mkt_bas_prod, topN = 20, type = "absolute")  # interesting   totinos pizza

############## create rules trail

try_rules_prod <- apriori(mkt_bas_prod, parameter = list(supp = 0.01, conf = 0.5, minlen =2),
                          appearance = list(default="lhs",rhs="FRESCHETTA"))
try_rules_prod <- sort(try_rules_prod, by = "support", decreasing = T)
summary(try_rules_prod)

arules::inspect(try_rules_prod[1:50])


############################### create rules in sub category level

txn_subcate <- txnrecord %>% 
  select(1,16) %>% 
  filter(TransID != "0") %>% 
  arrange(TransID)

cateList <- ddply(txn_subcate,"TransID", 
                   function(txn_subcate)paste(txn_subcate$SCTGR_DESC, 
                                            collapse = ","))

cateList <- cateList %>% 
  select(2)

names(cateList) <- "items"

write.csv(cateList,"market_basket_subcate.csv", quote = FALSE, row.names = TRUE)

########## read file as transaction format

mkt_bas_subcate <- read.transactions("market_basket_subcate.csv", format = "basket", sep = ",")

summary(mkt_bas_subcate)

itemFrequencyPlot(mkt_bas_subcate, topN = 20, type = "absolute")  # interesting   totinos pizza

############## create rules trail 

try_rules_subcate <- apriori(mkt_bas_subcate, parameter = list(supp = 0.01, conf = 0.5, minlen =2, maxtime = 100))
try_rules_subcate <- sort(try_rules_subcate, by = "support", decreasing = T)
summary(try_rules_subcate)

arules::inspect(try_rules_subcate[1:50])

###################### up sell discount analysis #####################

#upsell <- txnrecord

############### add pizza and discount indicator in item level

#upsell$pzmark[upsell$Brand %in% c("CALIFORNIA","DIGIORNO", "TOMBSTONE", "JACK", "RED BARON", "FRESCHETTA", "TONY")] <- 1
#upsell$pzmark[!upsell$Brand %in% c("CALIFORNIA","DIGIORNO", "TOMBSTONE", "JACK", "RED BARON", "FRESCHETTA","TONY")] <- 0

#upsell$dismark[upsell$Brand %in% c("CALIFORNIA","DIGIORNO", "TOMBSTONE", "JACK", "RED BARON", "FRESCHETTA","TONY") & upsell$Discount != 0.00] <- 1
#upsell$dismark[!(upsell$Brand %in% c("CALIFORNIA","DIGIORNO", "TOMBSTONE", "JACK", "RED BARON", "FRESCHETTA","TONY") & upsell$Discount != 0.00)] <- 0


############### add pizza and discount indicator in transaction level

#pizzatxn <- upsell %>% 
  ##group_by(TransID) %>% 
  #summarise(pzsum = sum(pzmark)) %>% 
  #filter(pzsum > 0) %>% 
  #select(1)
  
#distxn <- upsell %>% 
  #group_by(TransID) %>% 
  #summarise(dissum = sum(dismark)) %>% 
  #filter(dissum > 0) %>% 
  #select(1)

#upsell$pz_indi[upsell$TransID %in% pizzatxn$TransID] <- 1
#upsell$pz_indi[!upsell$TransID %in% pizzatxn$TransID] <- 0
#upsell$dis_indi[upsell$TransID %in% distxn$TransID] <- 1
#upsell$dis_indi[!upsell$TransID %in% distxn$TransID] <- 0

#chidata <- upsell %>% 
  #select(1,36,37) 

#chidata <- chidata[!duplicated(chidata$TransID),]

#chidata <- chidata %>% 
  #select(2,3)
  
#upsell_chisq <- chisq.test(table(chidata))

#print(upsell_chisq)  it's not working

dis_txn <- txnrecord %>% 
 select(1,8,9,10,17,19) %>% 
  filter(TransID != "0" & Brand == "FRESCHETTA")

dis_txn$disindi[dis_txn$Discount != 0.00] <- "1"
dis_txn$disindi[dis_txn$Discount == 0.00] <- "0"
dis_txn$disindi <- as.factor(dis_txn$disindi)


dis_size <- dis_txn %>% 
  group_by(TransID, disindi) %>% 
  summarise(size = n()) 

anova_fit <- aov(size ~ disindi, data = dis_size)

  ggplot(dis_size,aes(x = disindi, y = size))+
  geom_point()+
    geom_jitter()

############################## Predictive Model #######################################


############## Create Variables ###############

##### store info variables

pred_store <- rawstoreid %>% 
  dplyr::select(-2,-3,-4,-5)

##### number of store tried

## calculate frequenct in each store

pred_storenum_raw <- txnrecord %>% 
  dplyr::select(2,4,5) %>% 
  group_by(CardID, StoreID) %>% 
  summarise(storect = n())

## number of stores tried

pred_storenum <- pred_storenum_raw %>% 
  filter(CardID != "0") %>%
  group_by(CardID) %>% 
  summarise(storetried=n())

## get the main store

pred_main_storen <- pred_storenum_raw %>% 
  filter(CardID != "0") %>% 
  group_by(CardID) %>% 
  top_n(1)

pred_main_storen$StoreID <- as.numeric(pred_main_storen$StoreID)

pred_main_storen <- pred_main_storen %>% 
  group_by(CardID) %>% 
  top_n(1,StoreID) %>% 
  dplyr::select(1,2)

pred_main_storen$StoreID <- as.character(pred_main_storen$StoreID)

##### number of product tried

pred_prodnum <- txnrecord %>% 
  dplyr::select(2,5,15) %>% 
  filter(CTGR_DESC == "FZ PIZZA" & CardID != "0") %>% 
  unique() %>% 
  group_by(CardID) %>% 
  summarise(prodct = n())

##### Frequency of Transaction

## get period

pred_txnperiod <- txnrecord %>% 
  dplyr::select(2,33) %>% 
  filter(CardID != "0") %>% 
  unique() %>% 
  group_by(CardID) %>% 
  summarise(txnperiod = n())

## get total number

pred_txnnum <- txnrecord %>% 
  dplyr::select(1, 2) %>% 
  filter(CardID != "0") %>% 
  unique() %>% 
  group_by(CardID) %>% 
  summarise(txnnum = n())

## get frequency

pred_txnfre <- pred_txnnum %>% 
  left_join(pred_txnperiod, by = "CardID") %>% 
  mutate(txnfre = txnnum / txnperiod) %>% 
  dplyr::select(1,4)

##### Discount

pred_dis <- txnrecord %>% 
  dplyr::select(1,2,9) %>% 
  filter(CardID != "0") %>% 
  group_by(CardID,TransID) %>% 
  summarise(dissum = sum(Discount)) %>% 
  group_by(CardID) %>% 
  summarise(avg_dis = mean(dissum))

##### Units

pred_unit <- txnrecord %>% 
  dplyr::select(1,2,6) %>% 
  filter(CardID != "0") %>% 
  group_by(CardID,TransID) %>% 
  summarise(unitsum = sum(Quantity)) %>% 
  group_by(CardID) %>% 
  summarise(avg_unit = mean(unitsum))

##### dollars

pred_sales <- txnrecord %>% 
  dplyr::select(1,2,6,10) %>% 
  filter(CardID != "0") %>%
  mutate(sales = Quantity * NetRevenue) %>% 
  group_by(CardID,TransID) %>% 
  summarise(salesum = sum(sales)) %>% 
  group_by(CardID) %>% 
  summarise(avg_sale = mean(salesum))

##### Y Variable (define time span and brands)

## new prod ID 

# newid <- Fres_first_Date %>% 
  # filter(ym != "2013-04") %>% 
  # select(1)

## add product indicator

pred_y_df <- txnrecord

pred_y_df$newindi[pred_y_df$ItemID %in% rawpzid$ItemID] <- 1
pred_y_df$newindi[!pred_y_df$ItemID %in% rawpzid$ItemID] <- 0

## get customers list 

new_cuslst <- pred_y_df %>% 
  dplyr::select(2,34) %>% 
  filter(CardID != "0") %>% 
  group_by(CardID) %>% 
  summarise(indisum = sum(newindi)) %>% 
  filter(indisum > 0) %>% 
  dplyr::select(1)

## add target variable

pred_y <- txnrecord %>% 
  dplyr::select(2) %>% 
  filter(CardID != "0") %>% 
  unique()

pred_y$Y[pred_y$CardID %in% new_cuslst$CardID] <- 1
pred_y$Y[!pred_y$CardID %in% new_cuslst$CardID] <- 0


### get raw model dataset

raw_model_data <- pred_sales %>% 
  left_join(pred_unit, by = "CardID") %>% 
  left_join(pred_dis, by = "CardID") %>% 
  left_join(pred_prodnum, by = "CardID") %>% 
  left_join(pred_storenum, by = "CardID") %>% 
  left_join(pred_main_storen, by = "CardID") %>% 
  left_join(pred_store, by = "StoreID") %>% 
  left_join(pred_y, by = "CardID")


### create model dataset

model_data <- raw_model_data %>% 
  dplyr::select(-1)

## rename columns

names(model_data) <- c("spent", "cart_size", "discount", "item_tried", "store_tried", "storeid","community","pop","medi_income","house_unit","medi_age","pover","unemploy","number_store","Y")

## round size and age

model_data$cart_size <- round(model_data$cart_size,0)

### create dummy variables

dummy_data <- dummy(model_data, p ="all")

### final dataset to build model

final_model_data <- cbind(model_data, dummy_data) %>% 
  dplyr::select(-6,-7)

final_model_data$Y <- as.factor(final_model_data$Y)


### Data Partition

intrain <- createDataPartition(y = final_model_data$Y, p =0.7, list=FALSE)
training <- final_model_data[intrain,]
testing <- final_model_data[-intrain,]

testing_noy <- testing %>% 
  dplyr::select(-13)

### Build Model

mod_fit_train <- glm(Y ~ spent + cart_size + discount + item_tried +
                         store_tried + pop + medi_income + house_unit + medi_age +
                         pover + unemploy + number_store + storeid_125 + storeid_149 +
                         storeid_155 + storeid_164 + storeid_175 + storeid_179 + storeid_182 + 
                         storeid_206 + storeid_226 + storeid_236 + storeid_241 + storeid_243 +
                         community_rural + community_urban, data = training, family="binomial")

## significant variables 

summary(mod_fit_train)

## Pseudo R^2 9%

pR2(mod_fit_train)

## coefficient

exp(coef(mod_fit_train))

## variable importance   

varImp(mod_fit_train)

## Misclasification rate    

pred <-  predict(mod_fit_train, newdata=subset(testing, select= c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27)), type="response")
pred <- ifelse(pred > 0.5,1,0)
misClasificError <- mean(pred!= testing$Y)
print(paste('Accuracy',1-misClasificError))


########################## Text Mining #################

########### Rating for 7 brands #########

## Red Baron  most are positive  CPK most are positive
## Digiorno most are positive  Jack's most are positive
## Tombstone worse compared to other brands
## Freschetta no score lower than 3
## Tony's only 4.0 5.0

rawreview %>% 
  filter(Brand == "Tony's") %>% 
  ggplot()+
  geom_bar(aes(x = Rating))

########## word frequency for all reviews

newreview <- data_frame(line = 1:nrow(rawreview), text = rawreview$Review)

tidyreview <- newreview %>% 
  unnest_tokens(word, text)

data("stop_words")

tidyreview <- tidyreview %>% 
  anti_join(stop_words)

tidyreview %>% 
  count(word, sort = T) %>% 
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n))+
  geom_col() +
  xlab(NULL) +
  coord_flip()

######### sentiment analysis for all reviews

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidyreview %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = T)

all_sentiment <- tidyreview %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, index = line %/% 1, sentiment) %>% 
  spread(sentiment, n ,fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(all_sentiment, aes(index, sentiment))+
  geom_col(show.legend = F)

############ word clouds for all reviews

tidyreview%>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 10))

########## break reviews by brands

frsreview <- rawreview %>% 
  filter(Brand == "Freschetta")

newfrsreview <- data_frame(line = 1:nrow(frsreview), text = frsreview$Review)

tidyfrsreview <- newfrsreview %>% 
  unnest_tokens(word,text)

tidyfrsreview <- tidyfrsreview %>% 
  anti_join(stop_words)

tidyfrsreview %>% 
  count(word, sort = T) %>% 
  filter(n > 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n))+
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidyfrsreview %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = T)


fres_sentiment <- tidyfrsreview %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, index = line %/% 1, sentiment) %>% 
  spread(sentiment, n ,fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(fres_sentiment, aes(index, sentiment))+
  geom_col(show.legend = F)

posword <- fres_sentiment %>% 
  dplyr::filter(sentiment > 0) %>% 
  select(1)

negword <- fres_sentiment %>% 
  dplyr::filter(sentiment < 0) %>% 
  select(1)

posword %>%
  count(word) %>%
  with(wordcloud(word, n))

ggplot(negword)+
  geom_bar(aes(x = reorder(word,word,
                           function(x)-length(x))))+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))
#########################################################################

rbreview <- rawreview %>% 
  filter(Brand == "Red Baron")

newrbreview <- data_frame(line = 1:nrow(rbreview), text = rbreview$Review)

tidyrbreview <- newrbreview %>% 
  unnest_tokens(word,text)

tidyrbreview <- tidyrbreview %>% 
  anti_join(stop_words)

tidyrbreview %>% 
  count(word, sort = T) %>% 
  filter(n > 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n))+
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidyrbreview %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = T)


rb_sentiment <- tidyrbreview %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, index = line %/% 1, sentiment) %>% 
  spread(sentiment, n ,fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(rb_sentiment, aes(index, sentiment))+
  geom_col(show.legend = F)


######################## 

digreview <- rawreview %>% 
  filter(Brand == "Digiorno")













#############################################################################
cpkreview <- rawreview %>% 
  filter(Brand == "California Pizza Kitchen")

digreview <- rawreview %>% 
  filter(Brand == "Digiorno")

jkreview <- rawreview %>% 
  filter(Brand == "Jack's")

tbsreview <- rawreview %>% 
  filter(Brand == "Tombstone")


tyreview <- rawreview %>% 
  filter(Brand == "Tony's")
