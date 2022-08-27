# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2022. Released under the terms of the GNU General Public License v3.

# Load data
# number<-4
# quantity<-'3 gram'

# number<-c(2.1, 3, 4)
# quantity<-c('is 3.5 gram','3.5 gram','4 gram')

# Reformat data
# number.x.quantity(number = number, quantity = quantity)

# Define function
number.x.quantity<-function(number, #one or more numbers, float or integer
                            quantity #one or more quantities, i.e. number + unit
){
  #checks on input
  if(!is.numeric(number)){
    cat(warn('WARNING: Coercing <number> to numeric\n'))
    number<-as.numeric(number)
  }
  if(length(number)!=length(quantity)){
    cat(error('ERROR: <number> and <quantity> have unequal lengths !\n'))
    stop()
  }
  
  #decode quantities
  datatype<-decode(quantity)
  value<-decode(quantity, target = 'number')
  unit<-decode(quantity, target = 'unit')
  
  #do math
  res<-paste(number*value, unit)
  
  #clean up NAs
  test<-grep('NA',res)
  if(length(test)>0){
    cat(warn('WARNING:',length(test),'NAs have been introduced\n'))
    res[test]<-NA
  }
  
  return(res)
}
