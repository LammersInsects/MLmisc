# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Test data
# res<-c()
# for(i in 1:100){
#   Sys.sleep(0.09)
#   res<-rbind(res,c(i,now(precision = 'us')))
# }
# table(nchar(res[,2]))

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
  
  #Convert to numeric
  string<-as.numeric(string)
  
  #restore default
  options(digits.secs=0)
  
  return(string)
}
