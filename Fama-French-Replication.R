########################################################################################################
#-------------------------------------------------------------------------------------------------------
# Author    : A. John Woodill
# Date      : 04/07/2015
# Filename  : Fama-French-Replication.R
# Code      : Fama-French '92 Replication 
# Sections  : 
#            (1) CRSP and Compustat Data Merge
#            (2) Data wrangling
#-------------------------------------------------------------------------------------------------------
########################################################################################################

rm(list=ls(all=TRUE))

library(dplyr)      # Data wrangling

#-----------------------------------
# (1) CRSP and Compustat Data Merge
#-----------------------------------

setwd("/run/media/john/1TB/Projects/Fama-French Replicatoin/")

compustat <- read.csv("Compustat.csv", header = TRUE, stringsAsFactors = FALSE)
crsp <- read.csv("Crsp.csv", header = TRUE, stringsAsFactors = FALSE)

# Remove last cusip digit from compustat to merge with crsp

compustat$cusip <- substr(compustat$cusip, 1, nchar(compustat$cusip) - 1)

# Merge by cusip and date, NA -> 0, subset for years and save

data <- full_join(compustat, crsp, by=c("cusip" = "CUSIP", "datadate" = "date"))
data[is.na(data)] <- 0
data <- filter(data, datadate >= 19580731 & datadate <= 19890631)
write.csv(data, "compustat_crsp_merged_1958-1989.csv")

# Read in data from csv

data <- read.csv("compustat_crsp_merged_1958-1989.csv")

#-----------------------------------
# (2) Data wrangling
#-----------------------------------

# Fix missing fyears
data$fyear <- substr(data$datadate, 1, 4)

# Fix data elements

data$cusip <- as.numeric(data$cusip)
data$fyear <- as.numeric(data$fyear)

# Only keep those stocks with returns at the end of June
data %>%
  group_by(cusip, fyear) %>%
  mutate(month = substr(datadate, 5, 6),
         has_June = any(month == "06")) -> data

data <- filter(data, has_June == TRUE)

# Only keep those stocks with returns at the end of December
data %>%
  group_by(cusip, fyear) %>%
  mutate(month = substr(datadate, 5, 6),
         has_Dec = any(month == "12")) -> data

data <- filter(data, has_Dec == TRUE)

## Monthly returns for at least 24 of 60 months preceding July of year t

testdata <- select(data, cusip, datadate, fyear)

testdata$check <- 

mutate(testdata, month = as.numeric(substr(datadate, 5, 6))) %>%
mutate(datadate = as.POSIXct(gsub("^(\\d{4})(\\d{2}).*$", "\\1-\\2-01", datadate),
                  format("%Y-%m-%d"), tz = "GMT")) %>%  
arrange(cusip, datadate) %>%                        
filter(between(datadate, 
       datadate[tail(which(month == 6, arr.ind = TRUE), n = 1)] - (60*60*24*30*60),
       datadate[tail(which(month == 6, arr.ind = TRUE), n = 1)] -(60*60*24*30*24))) %>%
group_by(cusip) %>%
mutate(check = abs(lead(month)-month) == 11|abs(lead(month)-month) == 1|abs(lead(month)-month) == 0) %>%
filter(all(check == TRUE | check %in% NA)) -> output

write.csv(testdata, "testdata.csv")


for(i in min(testdata$cusip):max(testdata$cusip)){ 
    for (j in min(testdata$fyear):max(testdata$fyear)) {
      monthcheck <- filter(testdata, cusip == i & (fyear == j-1 | fyear == j-2 | fyear == j-3 | fyear == j-4))
      if(length(monthcheck$month) / 60 >= 0.4) if(any(testdata$fyear == j)) testdata$check <- 1 
}}