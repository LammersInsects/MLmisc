# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# data<-df
# treatment.col<-'treatment'
# count.col<-'first_landing_on'
# count.col<-'choice'
# Reformat data
# test<-overview.count.data(data = df.all.s, treatment.col = treatment.col, count.col = count.col)

# Define function
overview.count.data<-function(data, treatment.col, count.col){
  #process input
  dat<-data
  
  treatms<-unique(dat[,treatment.col])
  print(paste('Found',length(treatms),'treatments:'))
  print(treatms)
  
  counts<-unique(dat[,count.col])
  print(paste('Found',length(counts),'possibilities in the count.col:'))
  print(counts)
  
  #make overview table
  overview<-as.data.frame(table(dat[,treatment.col], dat[,count.col]))
  overview<-tidyr::spread(overview,Var2,Freq)
  colnames(overview)<-c('Treatment',colnames(overview)[2:ncol(overview)])
  overview$n<-rowSums(overview[2:ncol(overview)])
  
  return(overview)
}
