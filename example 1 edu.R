# This code calculates point estimates and their 95% confidence intervals for
# the worst-case bounds in Example 1 (Table 4) in
# Identification and Inference for Welfare Gains without Unconfoundedness
# Written by: Undral Byambadalai, Boston University 

rm(list=ls())

# Read data
# data from Abadie, Angrist, Imbens (2002)
jtpa_aai = read.table('jtpa.tab', header = TRUE)
colnames(jtpa_aai)[1] <- "recid" #record ids
colnames(jtpa_aai)[3] <- "Z" # random assignment
colnames(jtpa_aai)[4] <- "D"  # program enrollment
jtpa_aai <- jtpa_aai[-c(2,5:19)]  # remove other covariates
# data from Kitagawa, Tetenov (2018)
jtpa_kt= read.table('jtpa_kt.tab', header = TRUE)
jtpa_kt <- jtpa_kt[-c(2)] # remove "D"
jtpa <- merge(jtpa_aai, jtpa_kt, by="recid")

# Set seed
set.seed(100)

# Setup
y_under <- 0
y_bar <- 160000

#Function to get 95% CI
CI <- function(beta, V){c(round(beta-1.96*sqrt(V)/sqrt(n), digits = 0), 
                          round(beta+1.96*sqrt(V)/sqrt(n), digits = 0))}

# Cross fitting: Split data 
n <- nrow(jtpa)
L1=sort(sample(1:n,n/2))
L2=setdiff(1:n,L1)
data_L1 <-jtpa[L1,]
data_L2 <-jtpa[L2,]

### First step
# Fold 1
L1_eyd1x <- mean(data_L1$earnings[data_L1$edu==12 & data_L1$D==1])
L1_eyd0x <- mean(data_L1$earnings[data_L1$edu==12 & data_L1$D==0])
L1_pdx <- mean(data_L1$D[data_L1$edu==12])

# Fold 2
L2_eyd1x <- mean(data_L2$earnings[data_L2$edu==12 & data_L2$D==1])
L2_eyd0x <- mean(data_L2$earnings[data_L2$edu==12 & data_L2$D==0])
L2_pdx <- mean(data_L2$D[data_L2$edu==12])

### Second step
# prep - L1
L1_delta_under <- (L1_eyd1x -y_bar)*L1_pdx+ (y_under-L1_eyd0x)*(1-L1_pdx)
L1_delta_bar <- (L1_eyd1x-y_under)*L1_pdx+ (y_bar-L1_eyd0x)*(1-L1_pdx)
L1_term2 <- (L1_eyd1x+L1_eyd0x-(y_under+y_bar))*(1-L1_pdx)
L1_term3 <- (L1_eyd1x+L1_eyd0x-(y_under+y_bar))*(0-L1_pdx)
L1_term4 <- (data_L2$earnings[data_L2$edu==12 & data_L2$D==1]-L1_eyd1x)
L1_term5 <- -(data_L2$earnings[data_L2$edu==12 & data_L2$D==0]-L1_eyd0x)

# prep - L2
L2_delta_under <- (L2_eyd1x -y_bar)*L2_pdx+ (y_under-L2_eyd0x)*(1-L2_pdx)
L2_delta_bar <- (L2_eyd1x-y_under)*L2_pdx+ (y_bar-L2_eyd0x)*(1-L2_pdx)
L2_term2 <- (L2_eyd1x+L2_eyd0x-(y_under+y_bar))*(1-L2_pdx)
L2_term3 <- (L2_eyd1x+L2_eyd0x-(y_under+y_bar))*(0-L2_pdx)
L2_term4 <- (data_L1$earnings[data_L1$edu==12 & data_L1$D==1]-L2_eyd1x)
L2_term5 <- -(data_L1$earnings[data_L1$edu==12 & data_L1$D==0]-L2_eyd0x)

#  lower bound beta
beta_low_wc<- 1/n*(sum(L1_delta_under+L1_term2+L1_term4) + sum(L1_delta_under+L1_term3+ L1_term5) 
                       +sum(L2_delta_under+L2_term2+L2_term4) + sum(L2_delta_under+L2_term3+L2_term5))

# lower bound variance
V_low_wc <- 1/n*(sum((L1_delta_under-beta_low_wc+L1_term2+L1_term4)^2)+sum((L1_delta_under-beta_low_wc+L1_term3+L1_term5)^2)
                    + sum((L2_delta_under-beta_low_wc+L2_term2+L2_term4)^2)+sum((L2_delta_under-beta_low_wc+L2_term3+L2_term5)^2)
                    + sum(data_L1$edu!=12)*(0-beta_low_wc)^2 +sum(data_L2$edu!=12)*(0-beta_low_wc)^2)

# CI
CI_low_wc <- CI(beta_low_wc, V_low_wc)

# upper bound beta
beta_up_wc<- 1/n*(sum(L1_delta_bar+L1_term2+L1_term4) + sum(L1_delta_bar+L1_term3+ L1_term5) 
                   +sum(L2_delta_bar+L2_term2+L2_term4) + sum(L2_delta_bar+L2_term3+L2_term5))

# upper bound variance
V_up_wc <- 1/n*(sum((L1_delta_bar-beta_up_wc+L1_term2+L1_term4)^2)+sum((L1_delta_bar-beta_up_wc+L1_term3+L1_term5)^2)
                 + sum((L2_delta_bar-beta_up_wc+L2_term2+L2_term4)^2)+sum((L2_delta_bar-beta_up_wc+L2_term3+L2_term5)^2)
                 + sum(data_L1$edu!=12)*(0-beta_up_wc)^2 +sum(data_L2$edu!=12)*(0-beta_up_wc)^2)
# CI
CI_up_wc <- CI(beta_up_wc, V_up_wc)