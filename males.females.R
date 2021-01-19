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

# Load test data 
# xxmxxf<-c("169m47f", NA, "21m4f", "182m63f", "2m", "15f", "105m18f", NA, "169m60f", "[many]", "0m24f", "0m0f")
# males.females(xxmxxf = xxmxxf)

# Reformat data

# Define function
males.females<-function(xxmxxf){
  #check for NAs input
  test<-sum(emptyvalues(xxmxxf))
  if(test>0){
    print(paste('WARNING:',test,'input values are NA'))
  }
  
  #check if the values are of format xxmxxf
  broken<-sapply(xxmxxf[!emptyvalues(xxmxxf)],elements,n=1)
  check<-rbind(sapply(sapply(broken,`%in%`,c('m')),sum)==1, #check whether the occurrence of m in string is 1
               sapply(sapply(broken,`%in%`,c('f')),sum)==1) #same for f
  if(sum(colSums(check)!=2)>0){
    print(paste('WARNING:',sum(colSums(check)!=2),'input value do not specify males and females in format "xxmxxf":'))
    print(xxmxxf[!is.na(xxmxxf)][colSums(check)!=2])
  }
  
  males<-as.integer(sapply(strsplit(xxmxxf, 'm'),head,n=1)) #gives a warning message if the string doesn't have an 'm'
  print('If this gives a warning about introduced NAs, check input format. You should already have been warned.')
  females<-sapply(strsplit(xxmxxf, 'm'),tail,n=1)
  females<-as.integer(gsub('f','',females))
  
  return(cbind(males,females))
}


# Explore and plot data

