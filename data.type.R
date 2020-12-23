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
# testdata<-c(-79,-1.1,-0.9,-0.1,0,1,0.1,0.9,1.1,79,'a','A','%','1%','0','1','0,1','0,9','1,1','79','2020-05-23',NA,'')

# Reformat data
# to.test<-testdata

# output<-data.type(testdata)
# output

# Define function

data.type<-function(vector.to.test, quiet=F){
  
  to.test<-vector.to.test
  
  #Here the format needs to forced into a vector if it's a (named) list or similar
  #TODO: untested!
  to.test<-unname(unlist(c(to.test)))
  
  if(!quiet){
    print('Dates are assumed to have R-friendly formatting, i.e. %Y-%m-%d, otherwise they are not recognized')
  }
  
  # ow <- options("warn")
  options(warn=-1)
  
  #test for all kinds of data types
  analysis<-data.frame(input=to.test,
                       #integer = !is.na(lapply(to.test,as.integer)),
                       numeric = !is.na(lapply(to.test,as.numeric)),
                       dot = grepl('.',to.test,fixed = T),
                       comma = grepl(',',to.test,fixed = T),
                       negative = as.numeric(to.test)<0,
                       positive = as.numeric(to.test)>0,
                       fraction = as.numeric(to.test)>=0 & as.numeric(to.test)<=1,
                       character = !is.na(lapply(to.test,as.character)),
                       date = !is.na(lapply(to.test,as.Date,format='%Y-%m-%d', origin='1970-01-01')),
                       empty = unlist(lapply(to.test,emptyvalues))
  )
  
  # options(warn=ow)
  options(warn=0)
  
  #take the row and col sums to collect the number of hits
  analysis<-cbind(analysis,rowSums(analysis[,2:ncol(analysis)], na.rm = T))
  analysis<-rbind(c(NA,colSums(analysis[2:ncol(analysis)], na.rm = T)),analysis) #the top row has the colSums
  colnames(analysis)<-c(colnames(analysis)[1:(ncol(analysis)-1)],'rowSums')
  if(!quiet){
    print(analysis)
  }
  
  #calculate scores for the input data types 
  output<-as.integer(analysis[1,])/length(to.test)
  names(output)<-names(analysis)
  output[1]<-length(to.test)
  output<-signif(output, digits = nchar(length(to.test)))
  
  return(output)
}



# Explore and plot data

