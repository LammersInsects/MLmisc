# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
now<-function(precision='seconds'){ #default precision
  #if precision is desired, change R glocal options
  if(precision=='milliseconds' | precision=='millisec' | precision=='ms'){
    options(digits.secs=3)
  } else if(precision=='microseconds' | precision=='microsec' | precision=='us'){
    options(digits.secs=6)
  } else {
    options(digits.secs=0)
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

