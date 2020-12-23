# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

#Version 1.0
# splitstring<-function(dataframe, column, primary.sep=';', secondary.sep='='){
#   # print("Example: splitstring(dataframe=vcf, column='INFO', primary.sep=';', secondary.sep='=')")
#   output<-dataframe
#   for (i in 1:nrow(dataframe)){
#     list<-unlist( lapply(dataframe[i,column],strsplit,primary.sep) )
#     for (j in list){
#       output[ i, strsplit(j, secondary.sep)[[1]][1] ] <- strsplit(j, secondary.sep)[[1]][2]
#     }
#     if(i%%1000==0){print(paste('Processing line',i,'of',nrow(dataframe)))}
#     if(i==nrow(df)){print('Done!')}
#   }
#   return(output)
# }

#Version 2.0
splitstring<-function(dataframe, column, targets='all', primary.sep=';', secondary.sep='='){
  # dataframe=test[1,]
  # column='INFO'
  
  splitted<-sapply(1:nrow(dataframe), function(x){
    lst<-unlist( lapply(dataframe[x,column],strsplit,primary.sep) )
    splitted<-sapply(1:length(lst), function(y){
      col<-strsplit(lst[y], secondary.sep)[[1]][1] 
      if(col=='INDEL'){ #this is handy when reading vcf files
        val<-1
      } else {
        val<-strsplit(lst[y], secondary.sep)[[1]][2]
      }
      names(val)<-col
      return(val)
    })
    return(unlist(splitted))
  })
  
  if(is.list(splitted)){
    new.cols<-unique(unlist(sapply(splitted,names)))
  } else {
    new.cols<-rownames(splitted)
  }
  
  if(targets[1]=='all'){
    targets<-new.cols
  }
  
  if(all(targets %in% new.cols)){
    output<-sapply(1:length(targets), function(x){
      if(is.list(splitted)){
        values<-sapply(splitted,`[`,targets[x])
      } else {
        values<-splitted[x]
      }
      values<-as.data.frame(values)
      colnames(values)<-targets[x]
      return(values)
    })
  } else {
    print('ERROR: Target column(s) not found in data')
    stop()
  }
  
  return(as.data.frame(output))
  
}
