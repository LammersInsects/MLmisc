# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga

load.db.package<-function(){
  temp.wd<-getwd()
  setwd(paste(wd.base, 'Common_scripts/database_package', sep=''))
  
  # Load data and packages
  debugging<-F
  if(!require('xlsx')){install.packages("xlsx")}
  library("xlsx")
  
  #Load users that have clearance
  source('users.R')
  
  #Load the registry building functions
  source('db.new.R')
  source('db.compare.db.R')
  source('db.multicol.import.R')
  source('db.registry.R')
  source('db.translate.R')
  source('db.verify.R') #should be able to take IDs and field names? Only takes IDs now
  source('db.remove.R')
  source('db.mask.R')
  source('db.last.records.R')
  
  #Load the registry to spreadsheet function
  source('db.build.R')
  
  #Load function to generate a new input file
  source('db.new.input.file.R')
  
  #Load the db reformatting function(s)
  # source('db.reorder.R')  #could be one that does the canonical correspondance and sorts items diagonally
  
  #Load functions used internally
  source('db.is.registry.R')
  source('db.compress.R')
  # source('db.input.qc.R')
  #the following functions are used throughout but should already be loaded
  # source('../emptyvalues.R') 
  # source('../trailingspace.R')
  # source('../read.date.format.R')
  # source('../breakdown.string.R')
  # source('../decode.R')
  
  #Load database analysis function(s)
  source('db.summary.R')
  source('db.history.R')
  source('db.missing.R')
  source('db.files.R')
  source('db.findrecords.R')
  
  setwd(temp.wd)
  
  
  # Debugging code
  if(debugging){
    #Load some meaningless registry data
    pot<-read.table('pot.csv',header=T,sep=';')
    pot.new<-read.table('pot.new.csv',header=T,sep=';')
    
    #These lines are required for running the db.registry scipt manually
    existing.data.registry=F
    new.records = pot.new
    filename='debugging'
    register<-db.registry(new.records = pot.new)
    
    
    existing.data.registry = pot
    register<-db.registry(existing.data.registry = pot,
                          new.records = pot.new)
    rm(existing.data.registry)
    rm(new.records)
    rm(filename)
    
    #And now for db.build
    registry<-register  
    include.date=F
    db<-db.build(register, include.date=F)
    
    #And the history of the registry
    db.history(register)
    db.summary(register)
    rm(registry)
    print('')
  }
  
}