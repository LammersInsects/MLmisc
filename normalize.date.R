# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
normalize.date<-function(date.vector){
  if(class(date.vector)!='Date'){
    print('WARNING: This function is meant to normalize data towards the earliest date. Functionality with other vectors is untested!')
  }
  norm.date.vector<-as.integer(date.vector-min(date.vector))
  return(norm.date.vector)
}