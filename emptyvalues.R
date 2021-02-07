# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a vector and returns a same-sized vector with logical values indicating which values are empty

emptyvalues<-function(x){
  y<-is.na(x) | lapply(x,nchar)==0
  return(y)
}
