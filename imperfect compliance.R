# This code calculates the joint distribution of D and Z (Table 3) in
# Identification and Inference for Welfare Gains without Unconfoundedness
# Written by: Undral Byambadalai, Boston University 

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

# Obtain joint distribution of Z and D
sum(jtpa$Z==1 & jtpa$D==1)
sum(jtpa$Z==0 & jtpa$D==1)
sum(jtpa$D==1)
sum(jtpa$Z==1 & jtpa$D==0)
sum(jtpa$Z==0 & jtpa$D==0)
sum(jtpa$D==0)
sum(jtpa$Z==1)
sum(jtpa$Z==0)