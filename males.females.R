# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load test data
xxmxxf<-c("169m47f", "21m4f", "2m63f", "2m", "15f", "105m18f60u", "169m60u", '14f5u', '18u', NA, "[many]", '5m0f',"0m24f", "0m0f")
males.females(xxmxxf = xxmxxf)

# Reformat data

# Define function
males.females<-function(xxmxxf){
  #check for NAs input
  test<-sum(emptyvalues(xxmxxf))
  if(test>0){
    print(paste('WARNING:',test,'input values are NA'))
  }
  
  #check if the values are of format xxmxxfxxu
  broken<-sapply(xxmxxf[!emptyvalues(xxmxxf)],elements,n=1)
  check<-data.frame(m=sapply(sapply(broken,`%in%`,c('m')),sum), #check whether the occurrence of m in string is 1
                    f=sapply(sapply(broken,`%in%`,c('f')),sum), #same for f
                    u=sapply(sapply(broken,`%in%`,c('u')),sum), #same for u
                    num=sapply(sapply(broken,`%in%`,0:9),sum), #and for numbers
                    other=sapply(sapply(broken,`%in%`,c(letters,LETTERS)[-c(6,13,21)]),sum), #also check for other letters
                    len=sapply(broken, length)) #length of the strings
  check$test.mfu<-check$m<=1 & check$f<=1 & check$u<=1
  check$test.other<-check$other ==0
  check$test.len<-check$len-check$num-check$m-check$f-check$u ==0
  check$result<-rowSums(check[,grep('test',colnames(check))]) !=3
  if(sum(check$result)>0){
    print(paste('WARNING:',sum(check$result),'input value(s) do not specify males and females in format [xxmxxfxxu]:'))
    print(xxmxxf[!is.na(xxmxxf)][check$result])
  }
  
  #extract numbers of males, females and unknown
  #first extract the positions of m, f and u in the string
  positions<-data.frame(len=check$len)
  positions$m[check$m>0]<-unlist(mapply(`[`,
                                        sapply(check$len,seq,from=1,),
                                        sapply(broken, `==`, 'm')))
  positions$f[check$f>0]<-unlist(mapply(`[`,
                                        sapply(check$len,seq,from=1,),
                                        sapply(broken, `==`, 'f')))
  positions$u[check$u>0]<-unlist(mapply(`[`,
                                        sapply(check$len,seq,from=1,),
                                        sapply(broken, `==`, 'u')))
  positions$min<-apply(positions[,2:4], 1, min, na.rm=T)
  positions$max<-apply(positions[,2:4], 1, max, na.rm=T)
  positions$mid<-apply(positions[,2:6], 1, function(x){
    res<-which(!x[1:3] %in% x[4:5])
    res<-res[!res %in% (1:3)[is.na(x[1:3])]]
    res<-x[1:3][res]
    return(res)
  })
  positions$ascend<-apply(positions[,2:4], 1, function(x){
    res<-diff(x[!is.na(x)])
    res<-all(res>0)
    return(res)
  })
  
  #TODO or assign letters to numbers?
  
  #TODO produce output
  
  return(cbind(males,females))
}


# Explore and plot data

