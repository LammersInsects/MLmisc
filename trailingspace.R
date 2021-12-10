# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a vector, checks for trailing whitespaces, and returns a same-sized vector with the values without trailing spaces

trailingspace<-function(x,
                        apply.to.string.with.only.whitespace=T,
                        quiet=T){
  y<-x
  trailing.space<-lapply(x,function(v){substr(v,nchar(v),nchar(v)+1)})==' ' #test whether the last character is a space
  trailing.space<-ifelse(is.na(trailing.space),F,trailing.space) #if a value is NA, than include FALSE in the test vector
  all.space<-unlist(lapply(lapply(strsplit(as.character(x),''),`==`,' '),all))
  
  #for values with a trailing space, rm last character
  if(apply.to.string.with.only.whitespace){ #unless we want to keep them in strings where there is no other characters
    to.do<-trailing.space
  } else {
    to.do<-trailing.space & !all.space
  }
  y[to.do]<-lapply(x[to.do],function(f){substr(f,1,nchar(f)-1)})
  if(!quiet){
    cat('Removed',sum(to.do),'trailing space(s)\n')
  }
  
  return(unlist(y))
}
