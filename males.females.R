# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load test data
# xxmxxf<-c('169m47f', '21m4f', '2m63f', '2m', '15f', '105m18f60u', '169m60u', '14f5u', '18u', NA, '[many]',
#           '5m0f','0m24f', '0m0f', '5uf', '6b')
# cbind(xxmxxf,males.females(xxmxxf = xxmxxf))

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
  check$test.num.mfu<-rowSums(check[,c('m','f','u')]) <= check$num
  check$result<-rowSums(check[,grep('test',colnames(check))]) !=4
  if(sum(check$result)>0){
    print(paste('WARNING:',sum(check$result),'input value(s) do not specify males and females in format [xxmxxfxxu]:'))
    print(xxmxxf[!is.na(xxmxxf)][check$result])
  }
  
  # extract numbers of males, females and unknown
  #first extract the positions of m, f and u in the string
  positions<-data.frame(len=check$len, m=NA, f=NA, u=NA)
  positions$m[check$m>0]<-unlist(mapply(`[`,
                                        sapply(check$len,seq,from=1,),
                                        sapply(broken, `==`, 'm')))
  positions$f[check$f>0]<-unlist(mapply(`[`,
                                        sapply(check$len,seq,from=1,),
                                        sapply(broken, `==`, 'f')))
  positions$u[check$u>0]<-unlist(mapply(`[`,
                                        sapply(check$len,seq,from=1,),
                                        sapply(broken, `==`, 'u')))
  #store which ones are the minimum, maximum, and middle one
  options(warn=-1)
  positions$min<-apply(positions[,2:4], 1, min, na.rm=T)
  positions$max<-apply(positions[,2:4], 1, max, na.rm=T)
  options(warn=0)
  positions[rowSums(is.na(positions[,2:4]))==3,c('min','max')]<-NA #if all m, f, and u are NA, then min and max should be NA
  positions$min[positions$max==positions$min]<-NA #if max==min, replace min with NA
  positions$mid<-apply(positions[,2:6], 1, function(x){
    res<-which(!x[1:3] %in% x[4:5])
    res<-res[!res %in% (1:3)[is.na(x[1:3])]]
    res<-unlist(x[1:3], use.names = F)[res]
    if(length(res)==0){res<-NA}
    return(res)
  })
  positions$ascend<-apply(positions[,2:4], 1, function(x){ #may not be necessary anymore
    res<-diff(x[!is.na(x)])
    res<-all(res>0)
    return(res)
  })
  
  #assign letters to numbers
  numbers.to.sex<-broken
  #assign last letter to all previous numbers
  #this also removes everything beyond the max
  for(i in seq(1,length(numbers.to.sex))[!is.na(positions$max)]){ #for only the lines that actually have a max value
    res<-c(rep(unlist(broken[i])[[positions$max[i]]], positions$max[i])) #repeat the last letter as often as its position
    res<-c(res,rep('x',length(broken[[i]]) - length(res))) #append with NA to length of original text
    numbers.to.sex[i]<-list(res)
  }
  #overwrite with mid letter (if present) to all previous numbers
  for(i in seq(1,length(numbers.to.sex))[!is.na(positions$mid)]){ #for only the lines that actually have a mid value
    res<-c(rep(unlist(broken[i])[[positions$mid[i]]], positions$mid[i]), #repeat mid letter as many times as its position
           numbers.to.sex[[i]][(positions$mid[i]+1):length(numbers.to.sex[[i]])]) #append with other letters already stored
    numbers.to.sex[i]<-list(res)
  }
  #overwrite with first letter (if present) to all previous numbers
  for(i in seq(1,length(numbers.to.sex))[!is.na(positions$min)]){ #for only the lines that actually have a min value
    res<-c(rep(unlist(broken[i])[[positions$min[i]]], positions$min[i]), #repeat min letter as many times as its position
           numbers.to.sex[[i]][(positions$min[i]+1):length(numbers.to.sex[[i]])]) #append with other letters already stored
    numbers.to.sex[i]<-list(res)
  }
  
  #extract numbers
  males<-rep(NA,length(xxmxxf))
  females<-rep(NA,length(xxmxxf))
  unknown<-rep(NA,length(xxmxxf))
  males[!emptyvalues(xxmxxf)]<-sapply(mapply(`[`, broken, sapply(numbers.to.sex,`==`,'m')), paste, collapse='')
  females[!emptyvalues(xxmxxf)]<-sapply(mapply(`[`, broken, sapply(numbers.to.sex,`==`,'f')), paste, collapse='')
  unknown[!emptyvalues(xxmxxf)]<-sapply(mapply(`[`, broken, sapply(numbers.to.sex,`==`,'u')), paste, collapse='')
  options(warn=-1)
  males<-as.integer(gsub('m','',males))
  females<-as.integer(gsub('f','',females))
  unknown<-as.integer(gsub('u','',unknown))
  options(warn=0)
  
  #combine them
  result<-data.frame(m=males,f=females,u=unknown)
  result[check$result,]<-NA
  
  return(result)
}
