# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# This function takes a vector and returns a same-sized vector with logical values indicating which values are empty

decode.workhorse<-function(x,
                           target='datatype'){
  
  #Check before we start #should no longer be necessary
  if(length(x)>1){
    print('ERROR: Input is longer than 1. Use decode.multiple() instead!')
    stop()
  }
  
  #TODO instead of what's done here in an old version, apply breakdown.string() 
  
  #Define groups of character types
  L<-c(LETTERS,letters)
  N<-as.character(0:9)
  P<-c('.',',')
  S<-c(' ','_')
  
  #Break down the input
  full<-x
  vect<-elements(full,1)
  
  n<-vect %in% N
  l<-vect %in% L
  p<-vect %in% P
  s<-vect %in% S
  breakdown<-as.data.frame(cbind(n,l,p,s))
  breakdown<-cbind(vect,breakdown)
  breakdown$o<-ifelse(rowSums(breakdown[,2:5])>0,F,T)
  
  #Analyze breakdown
  anyN<-any(n)
  anyL<-any(l)
  anyP<-any(p)
  anyS<-any(s)
  anyO<-any(breakdown$o)
  
  sumany<-sum(anyN,anyL,anyP,anyS,anyO)
  tr<-colnames(breakdown[,2:6])
  
  #Data types
  possibilities<-c('character','integer','numeric with one other type','numeric complex','non-numeric complex','character with extra','MORE?')
  datatype<-'not recognised yet' #default return
  
  #Start producing output
  if(sumany==1){
    if(anyN){
      datatype<-'integer'
    } else {
      datatype<-'character'
    }
  } else {
    if(sumany==2){
      if(anyN){
        datatype<-'numeric with one other type'
      } else {
        datatype<-'character with extra'
      }
    } else {
      if(anyN){
        datatype<-'numeric complex'
      } else {
        datatype<-'non-numeric complex'
      }
    }
  }
  
  if(!datatype %in% possibilities[1:2]){
    pattern<-T
  } else {
    pattern<-F
  }
  
  if(pattern==T){
    transl.vect<-sapply(1:length(vect),function(x){tr[breakdown[x,2:6]==T]})
    transl<-paste(transl.vect,sep='',collapse='')
    
    matrix<-data.frame(original=vect,transl=transl.vect)    
    matrix$test<-lapply(1:length(vect),function(x){matrix$transl[x]!=matrix$transl[x-1]})
    matrix$test<-matrix$test==T
    matrix$test[1]<-T
    breaks.vect<-seq(1,length(vect),1)[matrix$test]
    breaks<-paste(breaks.vect,collapse='|')
    
    patt.vect<-matrix$transl[matrix$test==T]
    patt<-paste(patt.vect,collapse='')    
    patt.noS<-gsub('s','',patt)
    
    splitted.vect<-substring(full,breaks.vect,c((breaks.vect-1)[2:length(breaks.vect)],length(vect)))
    decoded<-data.frame(splitted=splitted.vect,pattern=patt.vect)
    splitted<-paste(splitted.vect,collapse='|')    
  } else {
    transl<-'not required'
    breaks<-'not required'
    patt<-datatype
    patt.noS<-datatype
    splitted<-full
  }
  
  number<-NA #default return
  unit<-NA #default return
  
  if(datatype %in% c('integer','numeric','numeric decimal')){
    number<-as.numeric(full)
  }
  if(pattern){
    if(patt.noS=='npn'){
      datatype<-'numeric decimal'
      number<-as.numeric(paste(decoded$splitted[decoded$pattern=='n'],collapse='.'))
    }
    if(patt.noS=='npnl'){
      datatype<-'numeric decimal with unit'
      number<-as.numeric(paste(decoded$splitted[decoded$pattern=='n'],collapse='.'))
      unit<-decoded$splitted[decoded$pattern=='l']
    }
    if(patt.noS=='nl'){
      datatype<-'integer with unit'
      number<-as.numeric(decoded$splitted[decoded$pattern=='n'])
      unit<-decoded$splitted[decoded$pattern=='l']
    }
    if(patt.noS=='ll'){ #from this one and lower the number of l might vary!!! not incorporated yet, e.g. llnl
      datatype<-'character with space'
    }
    if(patt.noS=='lnl'){
      datatype<-'character-integer-unit'
      number<-as.numeric(decoded$splitted[decoded$pattern=='n'])
      unit.pos<-grep(number,decoded$splitted)+1
      unit<-decoded$splitted[unit.pos]
      if(unit==' '){
        unit.pos<-unit.pos+1
        unit<-decoded$splitted[unit.pos]
      }
    }
    if(datatype %in% c('not recognised yet','numeric complex','non-numeric complex')){
      not.recognised.old<-read.table(paste(wd.base,'Common_scripts/database_package/patterns.not.recognised.log',sep=''),header=T,sep=';',quote='"')
      not.recognised<-data.frame(start=full,
                                 datatype=datatype,
                                 transl=transl,
                                 breaks=breaks,
                                 pattern=pattern,
                                 patt.noS=patt.noS,
                                 splitted=splitted)
      not.recognised<-rbind(not.recognised.old,not.recognised)
      not.recognised<-unique(not.recognised)
      write.table(not.recognised,file=paste(wd.base,'Common_scripts/database_package/patterns.not.recognised.log',sep=''),row.names=F,sep=';')
    }
    #     if(patt.noS=='llln'){
    #       datatype<-'this needs work'
    #     }
    #     if(patt.noS=='llpn'){
    #       datatype<-'this needs work'
    #     }
  }
  
  #Return any requested output
  if(target=='datatype'){return(datatype)}
  if(target=='translation'){return(transl)}
  if(target=='breaks'){return(breaks)}
  if(target=='pattern'){return(patt)}
  if(target=='pattern.no.spaces'){return(patt.noS)}
  if(target=='splitted'){return(splitted)}
  if(target=='number'){return(number)}
  if(target=='unit'){return(unit)}
}


decode.multiple<-function(x,
                          target='datatype'){
  
  #   print("Function can give multiple output types. Common requests are 'datatype', 'number' and 'unit'.")
  
  output<-sapply(x,decode.workhorse,target=target)
  return(output)
}

decode<-function(x,
                 target='datatype'){
  if(length(x)>1){
    output<-decode.multiple(x, target = target)
  } else {
    output<-decode.workhorse(x, target = target)
  }
  return(output)
}
