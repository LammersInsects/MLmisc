# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga



# Load data and packages
# NA?

load.reference.tables<-function(){
  temp.wd<-getwd()
  setwd(paste(wd.base, 'Common_scripts/reference_tables', sep=''))
  
  files<-list.files(getwd())
  files<-files[grep('.R',files)]
  print(paste('The following',length(files),'reference tables are loaded:'))
  print(files)
  for(f in files){source(f)}
  #   source(paste(wd.base,'Common_scripts/reference_tables/units.R',sep=''))
  setwd(temp.wd)
}

