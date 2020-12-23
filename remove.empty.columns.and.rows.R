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
remove.empty.columns.and.rows<-function(dataframe, do.columns=T, do.rows=T){
  df<-dataframe
  
  if(nrow(df)>1){ #if there is only one row, the df collapses to a vector
    
    if(do.columns){#remove empty columns
      df<-df[,!colSums(sapply(df,emptyvalues))==nrow(df)]
    }
    
    if(do.rows){
      #remove empty rows
      df<-df[!rowSums(sapply(df,emptyvalues))==ncol(df),]
    }
    
  } else { #so here we basically deal with a named vector
    df<-df[!sapply(df,emptyvalues)]
  }
  
  return(df)
}

# Explore and plot data

