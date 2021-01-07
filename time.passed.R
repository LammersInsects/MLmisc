# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# source("E:/DATA/Common_scripts/R_startup_script.R") #Dell using Maxtor HDD
# source("D:/DATA/Common_scripts/R_startup_script.R") #Dell internal data

# setwd(paste(wd.base, [DATA DRIVE SUBFOLDER], sep=''))

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# start<-now()
# end<-now()
# time.passed(start, end)

# Reformat data

# Define function
time.passed<-function(start, end, return.it=F){
  tp<-strptime(x = end, format = '%Y%m%d%H%M%S') - strptime(x = start, format = '%Y%m%d%H%M%S')
  print(tp)
  if(return.it){
    return(tp)
  }
}


# Explore and plot data

