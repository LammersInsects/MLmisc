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

# Reformat data

# Define function
duplicates<-function(dataframe, columns=c(2:4)){
  #TODO Here the script should check that there is enough columns to perform the paste with
  #if not, check for duplicates on a single vector?
  cln<-columns
  if(length(columns)>1){
    txt<-do.call(paste,dataframe[,cln])
  } else {
    txt<-dataframe[,cln]
  }
  cnt<-as.data.frame(table(txt))
  dupl<-cnt[cnt$Freq!=1,]
  test<-txt %in% dupl$txt
  res<-dataframe[test,]
  print('Full list of all duplicates is returned. Call this list with unique() to get the unique duplicates')
  return(res)
}
# res<-duplicates(dataframe = rbind(df,df[2:4,]), columns = c(2:4))
# unique(res)

