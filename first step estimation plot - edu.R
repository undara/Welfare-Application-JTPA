# This code plots figures for the first step estimation when X is education (Figures 7 & 8) in
# Identification and Inference for Welfare Gains without Unconfoundedness
# Written by: Undral Byambadalai, Boston University 

rm(list=ls()) 

library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)
library(plyr)

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

# Split data to treated and non-treated
jtpa_d1 <- subset(jtpa, D==1)
jtpa_d0 <- subset(jtpa, D==0)

# Define theme
My_Theme = theme(
  axis.title = element_text(size = 16),
  legend.title= element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.text= element_text(size = 14)
)
# X axis is education
education <- seq(7,18,1)

# E[Y|D=1, X]
eyd1x <- function(x){mean(jtpa_d1$earnings[jtpa_d1$edu==x])}
yhatd1x <- unlist(lapply(education, eyd1x))
ggplot()+annotate("point", x = education, y = yhatd1x, colour = "navy", size=5, shape=17)+
  scale_x_continuous(breaks = seq(7,18,1))+ylim(0,25000)+
  theme_bw() +theme(legend.position = 'none') +theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Years of education", y="Post-program 30-month earnings")+My_Theme

# E[Y|D=0, X]
eyd0x <- function(x){mean(jtpa_d0$earnings[jtpa_d0$edu==x])}
yhatd0x <- unlist(lapply(education, eyd0x))
ggplot()+annotate("point", x = education, y = yhatd0x, colour = "navy", size=5, shape=17)+
  scale_x_continuous(breaks = seq(7,18,1))+ylim(0,25000)+
  theme_bw() +theme(legend.position = 'none') +theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Years of education", y="Post-program 30-month earnings")+My_Theme

# P(D=1|X)
pdx <- function(x){mean(jtpa$D[jtpa$edu==x])}
phatx <- unlist(lapply(education, pdx))
ggplot() + annotate("point", x = education, y = phatx, colour = "navy", size=5, shape=17)+
 scale_x_continuous(breaks = seq(7,18,1))+ ylim(0,1)+
  theme_bw()+theme(legend.position = 'none')+theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Years of education", y="Program participation")+ My_Theme


