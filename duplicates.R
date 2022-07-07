# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
duplicates<-function(dataframe, columns=c(2:4), invert=F){
  #checks before the start
  if(!class(dataframe)=='data.frame'){
    stop('The function only works for objects of class data.frame!')
    #TODO, how to use the function for duplicates on a single vector?
  }
  if(!is.integer(columns)){
    stop('Variable columns must be one or more integers!')
  }
  if(ncol(dataframe)<max(columns)){
    stop('There are less columns in the dataframe than that you ask me to assess!')
  }
  if(!class(invert)=='logical'){
    stop('Variable invert must be set to TRUE or FALSE!')
  }
  
  #assess columns
  cln<-columns
  if(length(columns)>1){
    txt<-do.call(paste,dataframe[,cln])
  } else {
    txt<-dataframe[,cln]
  }
  cnt<-as.data.frame(table(txt))
  
  #store duplicates (or singletons if requested)
  dupl<-cnt[cnt$Freq!=1,]
  test<-txt %in% dupl$txt
  if(invert){
    test<-!test
    print('Full list of all singletons is returned.')
  } else {
    print('Full list of all duplicates is returned. Call this list with unique() to get the unique duplicates')
  }
  res<-dataframe[test,]
  
  #return output
  return(res)
}
# res<-duplicates(dataframe = rbind(df,df[2:4,]), columns = c(2:4))
# unique(res)

