# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.


# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Define a function
breakdown.string<-function(string, debugging = F){
  #test whether the input is not NA. If NA, output NA.
  if(is.na(string)){
    return(NA)
  } else {
    
    full<-string
    vect<-elements(full,1)
    
    #define all kinds of character sets
    L<-c(LETTERS,letters)
    N<-as.character(0:9)
    P<-c('.',',') #numeric punctuation
    S<-c(' ','_') #space characters
    seps<-c(':',';','-','/') #often-used separators etc
    maths<-c('+','-','*','/','=','!','%','^')
    spec<-c('!','#','$','€','&')
    brac<-c('(',')','[',']','{','}')
    
    if(debugging){
      l<-ls(envir = environment()) #trying to automatically grab all test results and nothing else
      l<-l[sapply(l, function(x) {is.character(get(x, envir=environment()))})] #all character vectors
      l<-l[grep('full',l, invert = T)]; l<-l[grep('vect',l, invert = T)] #but not full or vect
      chars<-(unlist(sapply(l,get, envir=environment())))
      counts<-as.data.frame(table(chars))
      print('These characters have more than one listed meaning:')
      print(counts[counts$Freq>1,])
    }
    
    #test for all possible groups, save as logical vector
    num<-vect %in% N
    lett<-vect %in% L
    punc<-vect %in% P
    space<-vect %in% S
    sep<-vect %in% seps
    math<-vect %in% maths
    special<-vect %in% spec
    brackets<-vect %in% brac
    
    l<-ls(envir = environment()) #trying to automatically grab all test results and nothing else
    l<-l[sapply(l, function(x) {is.logical(get(x, envir=environment()))})] #all logical vectors
    l<-l[grep('debugging',l,invert = T)] #during debugging it's annoying that this variable is also logical
    breakdown<-as.data.frame(cbind(sapply(l,get, envir=environment())))
    # breakdown<-as.data.frame(cbind(num,lett,punc,space,sep,special,brackets)) #old version with selection by hand
    breakdown<-cbind(vect,breakdown)
    breakdown$other<-ifelse(rowSums(breakdown[,2:ncol(breakdown)])>0,F,T) #other characters
    return(breakdown)
  }
}
