# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a vector, removes all whitespaces at the start of the elements, and returns
# a same-sized vector with the values without starting spaces

startingspace<-function(x){
  y<-x
  z<-lapply(y,function(v){
    reverse<-intToUtf8(rev(utf8ToInt(v))) #flip the word
    v<-trailingspace(reverse) #remove trailing space
    # startspace<-ifelse(is.na(startspace),F,startspace)
    v<-intToUtf8(rev(utf8ToInt(v))) #flip it back
  })
  return(unlist(z))
}

