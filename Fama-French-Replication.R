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
library(DataCombine)   # For lag variables

#-----------------------------------
# (1) CRSP Data setup
#-----------------------------------

setwd("/run/media/john/1TB/Projects/Fama-French Replicatoin/")
crsp2 <- read.csv("Crsp.csv", stringsAsFactors = FALSE)

# Convert colnames to lower case

colnames(crsp) <- tolower(colnames(crsp))

# Fix missing fyears
crsp$fyear <- substr(crsp$date, 1, 4)

# Only keep those stocks with returns at the end of June
crsp <- crsp %>%
  group_by(cusip, fyear) %>%
  mutate(month = substr(date, 5, 6),
         has_June = any(month == "06"))

crsp <- filter(crsp, has_June == TRUE)

# Only keep those stocks with returns at the end of December
crsp <- crsp %>%
  group_by(cusip, fyear) %>%
  mutate(month = substr(date, 5, 6),
         has_Dec = any(month == "12"))

crsp <- filter(crsp, has_Dec == TRUE)

# Calculate Market Equity (ME) : ME = prc*shrout
crsp$me <- (abs(crsp$prc)*crsp$shrout)/1000
crsp <- filter(crsp, me > 0)   # Ensure has me

## Monthly returns for at least 24 of 60 months preceding July of year t

check_crsp <- crsp %>%
  group_by(cusip, fyear) %>%
  summarise(number = n(), share = n() / 60)  %>% 
  mutate( cum_y = lag(cumsum(share)), 
          cum_y4 = lag(cum_y, 4),
          last4 = ifelse(is.na(cum_y4), cum_y, cum_y - cum_y4),
          check = as.numeric( last4 >= 0.4 )
          ) %>%
  select(fyear, last4, check)

# Join in checked crsp
tcrsp <- left_join(crsp, check_crsp, by = c("cusip", "fyear"))

# Remove all those not meeting check
crsp <- filter(tcrsp, check == 1)

# Remove all obs outside July 1962 - Dec 1990 ( Because of lag need to include 1962)
crsp <- filter(crsp, date >= 19620700 & date <= 19901231)

# Keep only share code 10, 11
crsp <- filter(crsp, shrcd == 10 | shrcd == 11)

# Write to full sample
write.csv(crsp, "crsp_92_data.csv")

#---------------------------------------------------------
# (2) Table 1 pre-beta, post-beta, post-beta (ln(ME))
#---------------------------------------------------------

crsp <- read.csv("crsp_92_data.csv", stringsAsFactors = FALSE)

# Get 10% percentile for each June and assign to portfolio

for(i in unique(crsp$fyear)){
  check <- filter(crsp, month == 06 & fyear == i)                         ###
  per <- quantile(check$me, c(.10, .20, .30, .40, .50, .60, .70, .80, .90))
  crsp$portf[crsp$me < per[[1]]] <- "M1"
  crsp$portf[crsp$me >= per[[1]] & crsp$me < per[[2]] & crsp$fyear == i] <- "M2"
  crsp$portf[crsp$me >= per[[2]] & crsp$me < per[[3]] & crsp$fyear == i] <- "M3"
  crsp$portf[crsp$me >= per[[3]] & crsp$me < per[[4]] & crsp$fyear == i] <- "M4"
  crsp$portf[crsp$me >= per[[4]] & crsp$me < per[[5]] & crsp$fyear == i] <- "M5"
  crsp$portf[crsp$me >= per[[5]] & crsp$me < per[[6]] & crsp$fyear == i] <- "M6"
  crsp$portf[crsp$me >= per[[6]] & crsp$me < per[[7]] & crsp$fyear == i] <- "M7"
  crsp$portf[crsp$me >= per[[7]] & crsp$me < per[[8]] & crsp$fyear == i] <- "M8"
  crsp$portf[crsp$me >= per[[8]] & crsp$me < per[[9]] & crsp$fyear == i] <- "M9"
  crsp$portf[crsp$me >= per[[9]]]  <- "M10"
  }

# Pre-Ranking betas
crsp <- filter(crsp, ret != "C")
crsp$ret <- as.numeric(crsp$ret)

# Select only June
pre <- filter(crsp, month == 06)

# Lag ewretd
pre <- pre %>% 
  group_by(cusip) %>% 
  arrange(desc(date)) %>% 
  mutate(lagewretd = lag(ewretd))

pre <- filter(pre, lagewretd != "NA")

# Pre-ranking beta regressions
res1 <- pre %>%  
  group_by(cusip) %>% 
  arrange(desc(date)) %>% 
  mutate(n=n()) %>%
  do(data.frame(., beta=ifelse(.$n > 2,
   sum(coef(lm(ret~ewretd+lagewretd, data=.))[-1]), NA)))

max(unique(res1$beta))




# Regress ret ~ vwretd + lag(vwretd)
crsp <- filter(crsp, ret != "C")
crsp$ret <- as.numeric(crsp$ret)


crsp <- arrange(crsp, desc(date))
crsp <- slide(crsp, Var = "ewretd", GroupVar = c("cusip", "fyear"), slideBy = -1)
crsp <- filter(crsp, ewretd-1 != "NA")

# Lag ewretd
crsp <- crsp %>% 
  group_by(cusip) %>% 
  arrange(desc(date)) %>% 
  mutate(lagewretd = lag(ewretd))
crsp <- filter(crsp, lagewretd != "NA")


res1 <- crsp %>%  
  group_by(fyear) %>% 
  arrange(desc(date)) %>% 
  mutate(n=n()) %>%
  do(data.frame(., beta=ifelse(.$n > 2,
   sum(coef(lm(ret~ewretd+lagewretd, data=.))[-1]), NA)))
max(unique(res1$beta))

library(plm)

test <- filter(pre, fyear == 1970 & portf == "M1")

tlm <- lm(ret ~ ewretd + lagewretd, data = pre)
summary(tlm)

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
