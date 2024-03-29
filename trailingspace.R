# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a vector, checks for trailing whitespaces,
#  and returns a same-sized vector with the values without trailing spaces

trailingspace<-function(x,
                        last.character.only=F,
                        apply.to.string.with.only.whitespace=T,
                        quiet=T){
  y<-x
  datatype<-class(x)
  x<-as.character(x)
  all.space<-unlist(lapply(lapply(strsplit(as.character(x),''),`==`,' '),all))
  all.space[emptyvalues(x)]<-F
  
  if(last.character.only){
    trailing.space<-lapply(x,function(v){substr(v,nchar(v),nchar(v)+1)})==' ' #test whether the last character is a space
    trailing.space<-ifelse(is.na(trailing.space),F,trailing.space) #if a value is NA, than include FALSE in the test vector
    
    #for values with a trailing space, rm last character
    if(apply.to.string.with.only.whitespace){ #unless we want to keep them in strings where there is no other characters
      to.do<-trailing.space
    } else {
      to.do<-trailing.space & !all.space
    }
    
    y[to.do]<-lapply(x[to.do],function(f){substr(f,1,nchar(f)-1)})
    
  } else { #remove any number of trailing spaces
    x.notempty<-!emptyvalues(x)
    lengths<-lapply(x[x.notempty], nchar) #store lengths of the input strings
    positions<-lapply(nchar(x[x.notempty]),`:`,1) #create reverse vectors for each of the strings
    broken<-lapply(x[x.notempty],elements) #break up the strings into separate elements
    broken.rev<-lapply(broken,rev) #reverse those
    not.spaces<-lapply(broken,`!=`,' ') #test which elements are not spaces
    starts<-mapply(`[`,positions,not.spaces, SIMPLIFY = F) #positions of the non-spaces in the strings
    starts<-lapply(starts,tail,1) #take the lowest position
    starts[all.space[x.notempty]]<-lengths[all.space[x.notempty]] #strings with only spaces have empty values, fill them
    to.do<-sum(unlist(starts)!=1) #calculate how many lines are edited
    keep<-mapply(`:`,starts,lengths, SIMPLIFY = F) #store all the character positions to keep
    res<-mapply(`[`,broken.rev,keep, SIMPLIFY = F) #extract those from the stored elements of the strings
    res<-lapply(res,rev) #revert them back to the normal string order
    y[x.notempty]<-lapply(res,paste,collapse='') #collapse the characters back into strings
    
    if(apply.to.string.with.only.whitespace){
      y[all.space]<-'' #space-only strings still consist of a single space, remove that
    } else {
      y[all.space]<-x[all.space] #space-only strings are reduced to a single space, restore to original length
      to.do<-to.do - sum(all.space)
    }
  }
  
  if(!quiet){
    cat('Removed trailing space(s) from',sum(to.do),'lines\n')
  }
  
  #Format output
  y<-unlist(y)
  class(y)<-datatype
  return(y)
}
