# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a vector, checks for trailing whitespaces, and returns a same-sized vector with the values without trailing spaces

trailingspace<-function(x){
  y<-x
  trailing.space<-lapply(x,function(v){substr(v,nchar(v),nchar(v)+1)})==' '
  trailing.space<-ifelse(is.na(trailing.space),F,trailing.space)
  y[trailing.space]<-sapply(x[trailing.space],function(f){substr(f,1,nchar(f)-1)})
  return(unlist(y))
}