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
library(readr)         # For reading the data
library(dplyr)         # Data wrangling

#-----------------------------------
# (1) CRSP Data setup
#-----------------------------------

setwd("/run/media/john/1TB/Projects/Fama-French Replicatoin/")
crsp <- read_csv(("Crsp.csv"))
# crsp2 <- read.csv("Crsp.csv", stringsAsFactors = FALSE)

# Convert colnames to lower case

colnames(crsp) <- tolower(colnames(crsp))

# Fix missing fyears
crsp$fyear <- substr(crsp$date, 1, 4)

# Add Month

crsp$month <- substr(crsp$date, 5, 6)
# Only keep those stocks with returns at the end of June
# crsp <- crsp %>%
#   group_by(permco, fyear) %>%
#   mutate(month = substr(date, 5, 6),
#          has_June = any(month == "06"))
# 
# crsp <- filter(crsp, has_June == TRUE)

# Only keep those stocks with returns at the end of December
# crsp <- crsp %>%
#   group_by(permco, fyear) %>%
#   mutate(month = substr(date, 5, 6),
#          has_Dec = any(month == "12"))
# 
# crsp <- filter(crsp, has_Dec == TRUE)

# Calculate Market Equity (ME) : ME = prc*shrout
crsp$me <- (abs(crsp$prc)*crsp$shrout)/1000
crsp <- filter(crsp, me > 0)   # Ensure has me

# Remove all obs outside July 1962 - Dec 1990 ( Because of lag need to include 1962)
crsp <- filter(crsp, date >= 19620700 & date <= 19901231)

# Keep only share code 10, 11
crsp <- filter(crsp, shrcd == 10 | shrcd == 11)

# Remove ret with C as value
crsp <- filter(crsp, ret != "C")
crsp$ret <- as.numeric(crsp$ret)

# Write to full sample
write_csv(crsp, "crsp_92_data.csv")

#---------------------------------------------------------
# (2) Table 1 pre-beta, post-beta, post-beta (ln(ME))
#---------------------------------------------------------

library(dplyr)
library(readr)

setwd("/run/media/john/1TB/Projects/Fama-French Replicatoin/")
crsp <- read_csv("crsp_92_data.csv")

# Select only columns needed
crsp <- select(crsp, permco,date, ret, vwretd, ewretd, fyear, month, me)

# Get 10% decile ME for each June and assign to portfolio
crsp$fyear <- as.integer(crsp$fyear)
crsp$month <- as.integer(crsp$month)

# Build portfolios based on ME
crsp$portfo=cut(crsp$me, breaks=quantile(crsp$me,probs=seq(0,1,1/10),na.rm=T),labels=F)

# Lag ewretd
crsp <- crsp %>% 
  group_by(permco) %>% 
  arrange(desc(date)) %>% 
  mutate(lagewretd = lag(ewretd))

# Remove initial lagged variables
crsp <- filter(crsp, lagewretd != "NA")

## convert fyear to a proper number and then exploit for sorting
crsp <- crsp %>%
  mutate(fyear = fyear %>% as.integer) %>%
  arrange(fyear, month)

## figure out cumulative months available for each year (for each permco)
years <- crsp %>%  
  group_by(permco, fyear) %>% 
  summarize(n = n()) %>% 
  mutate(n_cum = cumsum(n)) 

# function to get coefficients 
# (further optimization should probably focus on improving this function)
get_coefs <- function(.permco, .fyear, .n_cum){
  if(.n_cum < 24) {
    data_frame(`(Intercept)` = NA_real_, ewretd = NA_real_, lagewretd = NA_real_)
  } else {
    my_dat <- crsp %>%
      filter(permco == .permco, fyear <= .fyear) %>%
      mutate(rn = row_number(desc(date)))
    lm(ret ~ ewretd + lagewretd, my_dat, subset = rn < 61) %>% 
      coef %>% 
      as.list %>% 
      as_data_frame
  }
}

# dplyr option (Takes ~ 2 hours)
models_dplyr <- years %>% 
  group_by(fyear, permco) %>%
  do(get_coefs(.$permco, .$fyear, .$n_cum))

# Remove NA's
models_dplyr <- filter(models_dplyr, ewretd != "NA" | lagewretd != "NA")
models_dplyr$sum <- models_dplyr$ewretd + models_dplyr$lagewretd

# Write out to save
write.csv(models_dplyr, "prerank_betas.csv")

# Read prerank
models_dplyr <- read_csv("prerank_betas.csv")


# Merge with crsp data set
merge <- select(crsp, permco, portfo, me, ret, fyear, month)
prerank <- inner_join(models_dplyr, merge, by = "permco")

# Sum ewretd and lagewretd to get pre-beta
prerank_betas <- prerank %>% 
  group_by(permco) %>% 
  summarize(pre_beta = mean(sum), ret = mean(ret), me = mean(me), ewr = mean(ewretd))

# Rank pre-betas and me
prerank_betas$beta_rank=cut(prerank_betas$pre_beta, breaks=quantile(prerank_betas$pre_beta, probs=seq(0,1,1/10), na.rm=T),labels=F)
prerank_betas$portfo=cut(prerank_betas$me, breaks=quantile(prerank_betas$me,probs=seq(0,1,1/10),na.rm=T),labels=F)

prerank_betas <- filter(prerank_betas, beta_rank != "NA" & portfo != "NA")

# Build data frame for pre-ranking betas

df <- prerank_betas %>%
  group_by(portfo, beta_rank) %>%
  summarise(mer = mean(ewr))

df <- prerank_betas %>%
  group_by(beta_rank) %>%
  summarise(mer = mean(ewr))
df
  
table1a <- read_csv("/home/john/Dropbox/UHM/Classes/Fin 701 - International Finance Theory/Replication/Table1_A.csv")
table1a

# Table 1B - Post Ranking Betas

# Function to get coef
get_postcoefs <- function(portfo){
    my_dat <- prerank_betas %>%
      filter(portfo == portfo) %>%
    lm(ret ~ ewr, my_dat) %>% 
      coef %>% 
      as.list %>% 
      as_data_frame
}


postrank <- prerank_betas %>%
  group_by(portfo) %>%
  do(get_postcoefs(.$portfo))

postrank <- prerank_betas
postbetas <- data.frame(LowB = numeric(),
                        B2 = numeric(),
                        B3 = numeric(),
                        B4 = numeric(),
                        B5 = numeric(),
                        B6 = numeric(),
                        B7 = numeric(),
                        B8 = numeric(),
                        B9 = numeric(),
                        B10 = numeric())

size <- data.frame(LowB = numeric(),
                        B2 = numeric(),
                        B3 = numeric(),
                        B4 = numeric(),
                        B5 = numeric(),
                        B6 = numeric(),
                        B7 = numeric(),
                        B8 = numeric(),
                        B9 = numeric(),
                        B10 = numeric())

# Get Post Rank Betas
for (i in unique(postrank$portfo)){
  for (j in unique(postrank$beta_rank)) {
    frame <- filter(postrank, portfo == i & beta_rank == j)
    postbetas[[i,j]] <- as.numeric(lm(ret ~ ewr, data = frame)$coefficients[2]*100)
  }
}

# All post rank betas for portfolio rank
for (i in unique(postrank$portfo)){
  frame <- filter(postrank, portfo == i)
  betas[[i]] <- lm(ret ~ ewr, data = frame)$coefficients[2]
}
                                   

# Get all post rank betas for beta rank
for (i in unique(postrank$beta_rank)){
    frame <- filter(postrank, beta_rank == i)
    betas[[i]] <- lm(ret ~ ewr, data = frame)$coefficients[2]
  }

# Get all betas for portfo data frame
for (i in unique(postrank$portfo)){
    frame <- filter(postrank, portfo == i & beta_rank == j)
    size[[i,j]] <- mean(log(frame$me))
  }

# Get all betas for beta data frame
for (i in unique(postrank$beta_rank)){
    frame <- filter(postrank, portfo == i & beta_rank == j)
    size[[i,j]] <- mean(log(frame$me))
  }
}

portsize <- postrank %>%
  group_by(portfo) %>%
  summarize(sizeme = mean(log(me)))
  
portsize <- postrank %>%
  group_by(beta_rank) %>%
  summarize(sizeme = mean(log(me)))

### NOT FINISHED
#----------------------------------------
# (**) CRSP and Compustat Data Merge
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
