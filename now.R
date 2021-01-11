# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

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
now<-function(precision='seconds'){
  #if precision is desired, change R glocal options
  if(precision=='milliseconds'){
    options(digits.secs=3)
  }
  if(precision=='microseconds'){
    options(digits.secs=6)
  }
  
  #Get the current time and break it up
  broken<-breakdown.string(as.character(Sys.time()))
  #Extract only numbers to get a timestamp
  string<-paste(broken$vect[broken$num], collapse = "")
  
  #restore default 
  options(digits.secs=0)
  
  return(string)
}

# Explore and plot data

