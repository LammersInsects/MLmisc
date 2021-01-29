# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# df<-read.table('D:/DATA/Projects_home/Takenlijst/Database/20201203.Takenlijst.registry.backup', sep=';', header=T)
# df[grep('Austr',df$Wat),]

# Reformat data
# for(i in 1:length(df$Wat)){
#   to.test<-df$Wat[i]
#   tt<-try(strsplit(to.test,nchar(to.test)))
#   res<-ifelse(is(tt,'try-error'),"There was a warning or an error","OK")
#   if(res!='OK'){
#     print(paste(to.test, class(tt), res))
#   }
# }
# x<-df$Wat
# df[detect.encoding.issues(df$Wat),]

# Define function
detect.encoding.issues<-function(x){
  if(class(x)!='character'){
    print('WARNING: This function has only been tested on character vectors')
  } 
  unlist(lapply(x, function(i){
    #try to count number of characters of the string, it will fail if there is an encoding issue, giving class 'try-error'
    tt<-class(try(nchar(i), silent = T))=='try-error' 
    return(tt)
  }))
}

# Explore and plot data
