# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
duplicates<-function(dataframe, columns=c(2:4)){
  #TODO Here the script should check that there is enough columns to perform the paste with
  #if not, check for duplicates on a single vector?
  cln<-columns
  if(length(columns)>1){
    txt<-do.call(paste,dataframe[,cln])
  } else {
    txt<-dataframe[,cln]
  }
  cnt<-as.data.frame(table(txt))
  dupl<-cnt[cnt$Freq!=1,]
  test<-txt %in% dupl$txt
  res<-dataframe[test,]
  print('Full list of all duplicates is returned. Call this list with unique() to get the unique duplicates')
  return(res)
}
# res<-duplicates(dataframe = rbind(df,df[2:4,]), columns = c(2:4))
# unique(res)

