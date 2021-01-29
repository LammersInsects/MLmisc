# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
# x=df.par$Nrs; prefix='I'
# full.ranges(df.par$Nrs, split='-', prefix='I')
full.ranges<-function(x, split='-', prefix=F, suffix=F){
  pref<-''
  suff<-''
  if(prefix==F){
    pref<-'tmp'
  }
  if(suffix==F){
    suff<-'tmp'
  }
  if(pref %in% 0:9 | suff %in% 0:9){
    print('ERROR: Given prefix or suffix is a number.')
    print('The function cannot handle those, as these numbers are likely to occur in the IDs as well')
    stop()
  }
  nrs.split<-strsplit(x,split=split,fixed=F)
  if(prefix!=F){
    nrs.split<-lapply(X = nrs.split, gsub, pattern=prefix, replacement='')
    pref<-prefix
  } else {
    pref<-''
  }
  if(suffix!=F){
    nrs.split<-lapply(X = nrs.split, gsub, pattern=suffix, replacement='')
    suff<-suffix
  } else {
    suff<-''
  }
  nrs.split<-lapply(X = nrs.split,as.integer)
  from<-lapply(nrs.split,`[`,1)
  to<-lapply(nrs.split,`[`,2)
  todo<-!is.na(to)
  ranges<-mapply(seq,from[todo],to[todo])
  ranges<-lapply(1:length(ranges),function(x){paste(pref,ranges[[x]],suff,sep='')})
  y<-data.frame(x=x)
  y$ranges[!todo]<-x[!todo]
  y$ranges[todo]<-ranges
  return(y$ranges)
}
