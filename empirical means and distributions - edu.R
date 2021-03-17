# This code calculates the empirical means and distributions when X is education (Table 7) in
# Identification and Inference for Welfare Gains without Unconfoundedness
# Written by: Undral Byambadalai, Boston University 

install.packages("stargazer")
library(stargazer)

# read data
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

# Functions to calculate empirical means and distributions
# sample size
ss <- function(x){round(sum(jtpa$edu==x), digits = 3)}
# P(X)
px <- function(x){round(mean(jtpa$edu==x), digits = 3)}
# E[Y|X=x]
y <- function(x){round(mean(jtpa$earnings[jtpa$edu==x]), digits = 0)} 
# E[Y|D=1, X=x]
yhat1 <- function(x){ round(mean(jtpa$earnings[jtpa$D==1 & jtpa$edu==x]), digits = 0)}
# E[Y|D=0, X=x]
yhat0 <- function(x){ round(mean(jtpa$earnings[jtpa$D==0 & jtpa$edu==x]), digits = 0)} 
# P[D=1|X=x]=E[D|X=x]
phat <- function(x){round(mean(jtpa$D[jtpa$edu==x]), digits = 3)} 
# E[Y|D=1, X=x, Z=1]
yhat_iv11 <- function(x){ if (sum(jtpa$earnings[jtpa$D==1 & jtpa$Z==1 & jtpa$edu==x])==0) return (0)
  else return(round(mean(jtpa$earnings[jtpa$D==1 & jtpa$Z==1 & jtpa$edu==x]), digits = 0))}
# E[Y|D=1, X=x, Z=0]
yhat_iv10 <- function(x){ if (sum(jtpa$earnings[jtpa$D==1 & jtpa$Z==0 & jtpa$edu==x])==0) return (0)
  else return(round(mean(jtpa$earnings[jtpa$D==1 & jtpa$Z==0 & jtpa$edu==x]), digits = 0))}
# E[Y|D=0, X=x, Z=1]
yhat_iv01 <- function(x){ if (sum(jtpa$earnings[jtpa$D==0 & jtpa$Z==1 & jtpa$edu==x])==0) return (0)
  else return(round(mean(jtpa$earnings[jtpa$D==0 & jtpa$Z==1 & jtpa$edu==x]), digits = 0))}
# E[Y|D=0, X=x, Z=0]
yhat_iv00 <- function(x){ if (sum(jtpa$earnings[jtpa$D==0 & jtpa$Z==0 & jtpa$edu==x])==0) return (0)
  else return(round(mean(jtpa$earnings[jtpa$D==0 & jtpa$Z==0 & jtpa$edu==x]), digits = 0))}
# E[Y|D=1, X=x, Z=0]
yhat_iv10 <- function(x){ if (sum(jtpa$earnings[jtpa$D==1 & jtpa$Z==0 & jtpa$edu==x])==0) return (0)
  else return(round(mean(jtpa$earnings[jtpa$D==1 & jtpa$Z==0 & jtpa$edu==x]), digits = 0))}
# P[D=1|X=x, Z=1]=E[D|X=x, Z=1]
phat_iv1 <- function(x){round(mean(jtpa$D[jtpa$Z==1 &jtpa$edu==x], na.rm = TRUE), digits = 3)}
# P[D=1|X=x, Z=0]=E[D|X=x, Z=0]
phat_iv0 <- function(x){round(mean(jtpa$D[jtpa$Z==0 &jtpa$edu==x], na.rm = TRUE), digits = 3)}

# Obtain empirical means and distributions
edu <- c(7:18)
ss_edu <- lapply(edu,ss)
px_edu <- lapply(edu, px)
y_edu <- lapply(edu, y)
yhat1_edu <- lapply(edu, yhat1)
yhat0_edu <- lapply(edu, yhat0)
phat_edu <- lapply(edu, phat)
yhat_iv11_edu <- lapply(edu, yhat_iv11)
yhat_iv10_edu <- lapply(edu, yhat_iv10)
yhat_iv01_edu <- lapply(edu, yhat_iv01)
yhat_iv00_edu <- lapply(edu, yhat_iv00)
phat_iv1_edu <- lapply(edu, phat_iv1)
phat_iv0_edu <- lapply(edu, phat_iv0)
distribution_x <- cbind(edu, ss_edu, px_edu, y_edu, yhat1_edu, yhat0_edu, phat_edu, yhat_iv11_edu, yhat_iv10_edu, yhat_iv01_edu, yhat_iv00_edu, phat_iv1_edu, phat_iv0_edu) 
# Get LaTeX code to create a table with 'stargazer' package
stargazer(t(distribution_x))
