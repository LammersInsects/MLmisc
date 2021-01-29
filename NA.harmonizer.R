# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
NA.harmonizer<-function(dataframe){
  if(class(dataframe)!='data.frame'){
    stop('ERROR: This function is written for data frames only. Functionality for other data formats is not tested so not recommended.')
  }
  df<-dataframe
  #NAs typed into Excel are often not recognised, so replace
  df[sapply(df,function(x){as.character(x)=='NA' & !is.na(x)})]<-NA 
  return(df)
}

# Explore and plot data

