########################################################################################################
#-------------------------------------------------------------------------------------------------------
# Author    : A. John Woodill
# Date      : 04/07/2015
# Filename  : Fama-French-Replication.R
# Code      : Fama-French '92 Replication 
# Sections  : 
#            (1) CRSP Data setup
#            (2) Table 1 pre-beta, post-beta, post-beta (ln(ME))
#            (3) 
#           (**) CRSP and Compustat Data Merge)
#           (**) Data Wrangling
#-------------------------------------------------------------------------------------------------------
########################################################################################################

rm(list=ls(all=TRUE))

library(dplyr)      # Data wrangling

#-----------------------------------
# (1) CRSP Data setup
#-----------------------------------

setwd("/run/media/john/1TB/Projects/Fama-French Replicatoin/")
crsp <- read.csv("Crsp.csv")

# Fix missing fyears
crsp$fyear <- substr(crsp$date, 1, 4)

# Only keep those stocks with returns at the end of June
crsp <- crsp %>%
  group_by(PERMCO, fyear) %>%
  mutate(month = substr(date, 5, 6),
         has_June = any(month == "06"))

crsp <- filter(crsp, has_June == TRUE)

# Only keep those stocks with returns at the end of December
crsp <- crsp %>%
  group_by(PERMCO, fyear) %>%
  mutate(month = substr(date, 5, 6),
         has_Dec = any(month == "12"))

crsp <- filter(crsp, has_Dec == TRUE)

# Calculate Market Equity (ME) : ME = prcc_f*csho
crsp$me <- abs(crsp$PRC)*abs(crsp$SHROUT)
crsp <- filter(crsp, me > 0)   # Ensure has me

## Monthly returns for at least 24 of 60 months preceding July of year t

# Need to ungroup data to run this
crsp <- ungroup(crsp)

crsp <- mutate(crsp, month = as.numeric(substr(date, 5, 6))) %>%
  mutate(date = as.POSIXct(gsub("^(\\d{4})(\\d{2}).*$", "\\1-\\2-01", date),
                    format("%Y-%m-%d"), tz = "GMT")) %>%  
  arrange(PERMCO, date) %>%                        
  filter(between(date, 
         date[tail(which(month == 6, arr.ind = TRUE), n = 1)] - (60*60*24*30*60),
         date[tail(which(month == 6, arr.ind = TRUE), n = 1)] -(60*60*24*30*24))) %>%
  group_by(PERMCO) %>%
  mutate(check = abs(lead(month)-month) == 11|abs(lead(month)-month) == 1|abs(lead(month)-month) == 0) %>%
  filter(all(check == TRUE | check %in% NA)) 

# Write out sample dataset after all checks and ready for regressions (obs = 11,721)
write.csv(crsp, "crsp_92_data.csv")





#---------------------------------------------------------
# (2) Table 1 pre-beta, post-beta, post-beta (ln(ME))
#---------------------------------------------------------


# Get 10% percentile for each June 
percentile_check <- filter(crsp, date = substr(date, 6, 7) == "06" & fyear == "1970")
per <- quantile(percentile_check$me, c(.05, .15, .25, .35, .45, .55, .65, .75, .85, .95))

                 
       
#----------------------------------------
# (**) CRSP and Compustat Data Merge)
#----------------------------------------
                 
                                            
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

#----------------------------------------
# (**) Data Wrangling
#----------------------------------------
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

# Calculate Book Equity (BE) : BE = CEQ + TXDB
data$be <- data$ceq + data$txdb

# Calculate Market Equity (ME) : ME = prcc_f*csho
data$me <- data$prcc_f*data$csho
data <- filter(data, me > 0)   # Ensure has me

# Calculate Book-to-Market (BE/ME) : be / me
data$beme <- data$be/data$me

# Calculate EP : EP = IB + TXDFED + TXDFO + TXDS -  DVP/PRCC_F
data$ep <- data$ib + data$txdfed + data$txdfo + data$txds - (data$dvp/data$prcc_f)


## Monthly returns for at least 24 of 60 months preceding July of year t

# Need to ungroup data to run this
data <- ungroup(data)

data <- mutate(data, month = as.numeric(substr(datadate, 5, 6))) %>%
  mutate(datadate = as.POSIXct(gsub("^(\\d{4})(\\d{2}).*$", "\\1-\\2-01", datadate),
                    format("%Y-%m-%d"), tz = "GMT")) %>%  
  arrange(cusip, datadate) %>%                        
  filter(between(datadate, 
         datadate[tail(which(month == 6, arr.ind = TRUE), n = 1)] - (60*60*24*30*60),
         datadate[tail(which(month == 6, arr.ind = TRUE), n = 1)] -(60*60*24*30*24))) %>%
  group_by(cusip) %>%
  mutate(check = abs(lead(month)-month) == 11|abs(lead(month)-month) == 1|abs(lead(month)-month) == 0) %>%
  filter(all(check == TRUE | check %in% NA)) 

# Write out sample dataset after all checks and ready for regressions (obs = 11,721)
write.csv(data, "92_data.csv")


##########################
