# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2022. Released under the terms of the GNU General Public License v3.

# Define function
trim.string<-function(character.vector, max.length = 100L, append.with = '[...]'){
  #checks before starting
  if(!is.character(character.vector)){
    stop('ERROR: Variable character.vector must be of class character')
  }
  if(!is.numeric(max.length)){
    stop('ERROR: Variable max.length must be an integer')
  } else {
    max.length<-as.integer(max.length)
  }
  
  #test whether each value is longer than the desired length
  test<-sapply(lapply(character.vector,nchar),`>`,max.length)
  
  #if any empty values, then fill those with FALSE
  test[emptyvalues(test)]<-F
  
  #produce output
  result<-character.vector
  if(any(test)){
    #trim any characters after the max.length and append with value given in append.with
    result[test]<-paste(sapply(character.vector,substr,1,max.length)[test],append.with,sep='')
  }
  
  #return it
  return(result)
}
