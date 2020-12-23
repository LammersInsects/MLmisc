# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a string and returns a vector of all elements of that string of a given length

# Source https://stackoverflow.com/questions/2247045/chopping-a-string-into-a-vector-of-fixed-width-character-elements
# - MichaelChirico Jun 17 '15 at 22:01
# Modified by me

elements<-function(x, #input string
                   n=1  #length of elements
){
  if(length(x)>1){
    stop('The script only works on individual strings, not vectors')
  }
  if(is.na(x)){
    return(NA)
  } else {
    result<-substring(x,seq(1,nchar(x),n),seq(n,nchar(x),n))
    if(nchar(x)%%n>0){
      print(paste('Warning: last',nchar(x)%%n>0,'characters are omitted!'))
    }
    return(result)
  }
}
