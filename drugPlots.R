## Last edit 2/23/18
## This file contains the code for the cleanup of the datasets and the plots generated for the paper written
## Overall objective - regression for predicting pharmaceutical stock return deviations 

library("foreign")
library("dplyr")
library("lubridate")
library("ggplot2")

######## Cleaning Bloomberg Data ########

mydata <- read.csv("https://www.dropbox.com/s/drl1ufrk0kbq90h/fda_calendar_bloomberg.csv?dl=1")
mydata <- as_tibble(mydata)

mydata$newcolumn<-NA

data_indexes_beg <- seq(1,1855,8)
data_indexes_end <- seq(8,1856,8)

listofdfs <- list()

for(i in 1:232 ) {
  smdf <- mydata[,data_indexes_beg[i]:data_indexes_end[i]]    # grabs a columns from each company and makes a new df for each
  listofdfs[[i]] <- smdf     # creates a list of dataframes
} 

for(i in 1:length(listofdfs)) {
  listofdfs[[i]] <- listofdfs[[i]][,-c(3,4,6,7,8)] # removes extra columns
  colnames(listofdfs[[i]])[2:3] <- ""              # removes extra header values  
  listofdfs[[i]] <- cbind(id = colnames(listofdfs[[i]])[1], listofdfs[[i]])   # creates new column to left with company name
  colnames(listofdfs[[i]])<-unlist(listofdfs[[i]][1,])   # removes header with just company name
  listofdfs[[i]] <- listofdfs[[i]][-1,]   # removes extra row which should have been the header 
  colnames(listofdfs[[i]])[1] <- "id"    # names column with company name "id"
}

regData <- bind_rows(listofdfs)       # stacks all the data of the individual companies
regData <- regData[!(regData$Date == ""), ]    # removes rows with missing date values 
regData$id <- sub('\\..*', '', regData$id)    # renames tickers from id.US.Equity to id

# setwd("/Users/Swati/Dropbox")
# write.csv(regData, file = "regData.csv")

######## Cleaning up drug data and combining with bloomberg data ########

drugData <- read.csv("https://www.dropbox.com/s/3cojxq6xfstspml/drugData.csv?dl=1")

drugData <- drugData[,-c(1,2)]
colnames(drugData)<-unlist(drugData[1,]) 
drugData <- drugData[-1,]

colnames(drugData)[1] <- "id"     # renames ticker name id

drugData$Date <- mdy(drugData$Date)
regData$Date <- mdy(regData$Date)

#combData <- inner_join(drugData,regData, by = c("id","Date"))

combData <- full_join(regData,drugData, by = c("id","Date"))     # merges two datasets by id and Date, keeping all rows

rm(mydata, drugData, regData, smdf, listofdfs, data_indexes_beg, data_indexes_end, i)   # freeing up memory 

combData <- transform(combData, PX_LAST = as.numeric(PX_LAST)) %>%     # changes PX_LAST to numeric type
  mutate(changeReturn = PX_LAST-lag(PX_LAST) )   # calculate percent change in PX_LAST

colnames(combData)[8] <- "drug_type"

colnames(combData)[3] <- "price"

combData <- combData %>% filter(between(Date, as.Date("2017-06-22"), as.Date("2017-12-29")))
combData$Date <- lubridate::mdy(combData$Date)

temp <- combData %>% filter(Stage == "Approved")
temp$id <- as.factor(temp$id)

names <- list()

names <- levels(temp$id)

names2 <- temp %>% count(id)

names2 <- names2 %>% filter(n>2)

target <- c("Approved", NA)

passing <- combData %>% filter(Stage %in% target, id %in% names)

passing <- semi_join(passing, names2, by = "id")

passing <- passing %>% filter(id %in% c("AZN"))

passing$Date <- base::as.Date(passing$Date)

dates <- c("2017-08-17","2017-08-28","2017-10-31","2017-11-14")

dates <- as.factor(c("2017-08-17","2017-08-28","2017-10-31","2017-11-14"))

dates <- as.Date(c("2017-08-17","2017-08-28","2017-10-31","2017-11-14"), origin="1970-01-01")

int <- as.Date(2017-06-22, origin="1970-01-01")

######## Plots ########
library(ggthemes)

ggplot(passing, aes(x = Date, y = changeReturn)) +
  geom_point() +
  geom_smooth(span = 0.05) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = int) +
  theme_tufte() +
  labs(title="Spikes in Stock Returns after Drug Approval",
       subtitle="Changes in Stock Return of Company with Approved Drugs",
       caption="Source: Bloomberg and FDA Drug Release Calendar",
       x = "Time in year 2017",
       y = "Changes in Return")

####### Failing drug plot  #######

combData <- combData %>% filter(between(Date, as.Date("2017-06-22"), as.Date("2017-12-29")))

temp <- combData %>% filter(Stage == "CRL")
temp$id <- as.factor(temp$id)

names <- list()

names <- levels(temp$id)

names2 <- temp %>% count(id)

target <- c("CRL", NA)

failing <- combData %>% filter(Stage %in% target, id %in% names)

failing <- semi_join(failing, names2, by = "id")

failing <- failing %>% filter(id %in% c("ATRS"))

ggplot(failing, aes(x = Date, y = changeReturn)) +
  geom_point() +
  geom_smooth(span = 0.05) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = int) +
  theme_tufte() +
  labs(title="Negative Shock in Stock Returns after Drug Failure", 
       subtitle="Changes in Stock Returns of Company with Failed Drugs",
       caption="Source: Bloomberg and FDA Drug Release Calendar",
       x = "Time in year 2017",
       y = "Changes in Return")

####### Failing drug plot #######

temp <- combData %>% filter(between(Date, as.Date("2017-06-22"), as.Date("2017-12-29")))

temp$drug_type = as.factor(temp$drug_type)

heart <- combData %>% filter(drug_type == 2)
cancer <- combData %>% filter(drug_type == 3)
lungs <- combData %>% filter(drug_type == 12)
eyes <- combData %>% filter(drug_type == 4)
stomach <- combData %>% filter(drug_type == 9)
neurological <- combData %>% filter(drug_type == 18)

temp <- temp %>% filter(id %in% c("ABIO","AZN","GBT","AERI","IRWD","NEOS"))

ggplot(temp, aes(x = Date, y = changeReturn)) +
  geom_point(alpha = 0.7, size = 0.5, colour = "steelblue") +
  geom_smooth(colour = "black", size = 0.5, se = FALSE, span = 0.05) +
  geom_hline(yintercept = 0) + 
  theme_tufte() +
  facet_wrap(~ id, nrow = 3)

ggplot(temp, aes(x = Date, y = changeReturn, color = id)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 0.05) +
  geom_hline(yintercept = 0) + 
  theme_tufte() 

#labs(title="Negative Shock in Stock Returns after Drug Failure", 
#       subtitle="Changes in Stock Returns of Company with Failed Drugs",
#       caption="Source: Bloomberg and FDA Drug Release Calendar",
#       x = "Time in year 2017",
#       y = "Changes in Return")


