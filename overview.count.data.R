# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("G:/DATA/Common_scripts/R_startup_script.R") #Thuis using Maxtor HDD
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# source("E:/DATA/Common_scripts/R_startup_script.R") #Dell using Maxtor HDD
# source("D:/DATA/Common_scripts/R_startup_script.R") #Dell internal data

# setwd(paste(wd.base, [DATA DRIVE SUBFOLDER], sep=''))

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# data<-df
# treatment.col<-'treatment'
# count.col<-'first_landing_on'
# count.col<-'choice'
# Reformat data
# test<-overview.count.data(data = df.all.s, treatment.col = treatment.col, count.col = count.col)

# Define function
overview.count.data<-function(data, treatment.col, count.col){
  #process input
  dat<-data
  
  treatms<-unique(dat[,treatment.col])
  print(paste('Found',length(treatms),'treatments:'))
  print(treatms)
  
  counts<-unique(dat[,count.col])
  print(paste('Found',length(counts),'possibilities in the count.col:'))
  print(counts)
  
  #make overview table
  overview<-as.data.frame(table(dat[,treatment.col], dat[,count.col]))
  overview<-tidyr::spread(overview,Var2,Freq)
  colnames(overview)<-c('Treatment',colnames(overview)[2:ncol(overview)])
  overview$n<-rowSums(overview[2:ncol(overview)])
  
  return(overview)
}