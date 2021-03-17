# This code plots figures for the hypothetical policies considered in Example 2 (Figure 9)
# and first step estimation when X is education and pre program earnings (Figures 10 & 11) in
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

# E[Y|D=1, X1=prevearn, X2= education]
jtpa_d1$yhatd1x <- predict(lm(data=jtpa_d1, earnings~poly(prevearn,edu,degree=2)))
ggplot(jtpa_d1, aes(x=prevearn, y=earnings, color=factor(edu)))+geom_point(alpha=0.5)+
  geom_line(aes(y=yhatd1x),size=1)+ylim(0,80000)+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Pre-program annual earnings", y="Post-program 30-month earnings", color='Education')+My_Theme

# E[Y|D=0, X1=prevearn, X2= education]
jtpa_d0$yhatd0x <- predict(lm(data=jtpa_d0, earnings~poly(prevearn,edu,degree=2)))
ggplot(jtpa_d0, aes(x=prevearn, y=earnings, color=factor(edu)))+geom_point(alpha=0.5)+
  geom_line(aes(y=yhatd0x),size=1)+ylim(0,80000)+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Pre-program annual earnings", y="Post-program 30-month earnings", color='Education')+My_Theme

## P[D=1|X1=prevearn, X2= education]
jtpa$pdx <- predict(glm(data=jtpa, D~poly(prevearn,edu,degree=2),family = binomial), type = "response")
ggplot(jtpa, aes(x=prevearn, y=D, color=factor(edu)))+geom_point(alpha=0.5)+
  geom_line(aes(y=pdx),size=1)+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Pre-program annual earnings", y="Program participation", color='Education')+My_Theme

## Kitagawa & Tetenov (2018) quadrant rule policies
AA <- data.frame(xyTable(jtpa$edu, jtpa$prevearn))
colnames(AA) <- c('edu', 'prevearn', 'number')
ggplot(AA, aes(x=edu, y=prevearn, size=number))+
  annotate("point", x = c(12,15), y = c(19670, 19670), colour = "purple", size=5, shape=18)+
  annotate("rect", xmin = c(7,7), xmax =c(12,15), ymin=c(0,0), ymax=c(19670, 19670), color = "purple", fill= "blue",alpha=0.2 )+
  annotate("text", x = c(12,15), y = c(21000, 21000), label = c("Policy 2", "Policy 1") , size=5, color="black", fontface="bold")+
  geom_point(color="navy")+scale_size(range = c(0, 18))+scale_x_continuous(breaks = seq(7,18,1))+ylim(0,25000)+
  theme_bw() +theme(legend.position = 'none') +theme(plot.title = element_text(hjust = 0.5))+
  labs(title="", x="Years of education", y="Pre-program annual earnings")+ My_Theme



