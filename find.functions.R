# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# source("E:/DATA/Common_scripts/R_startup_script.R") #Dell using Maxtor HDD
# source("D:/DATA/Common_scripts/R_startup_script.R") #Dell internal data

# setwd(paste(wd.base, [DATA DRIVE SUBFOLDER], sep=''))

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# find.functions(list.of.funs)
# character.vector<-list.of.funs

# Reformat data

# Define function
find.functions<-function(character.vector, quiet=F, return.unknown=F){
  #check input
  if(!is.character(character.vector)){
    stop('This funcion only accepts character vectors')
  }
  
  #detect which of these are known functions
  load.find.functions()
  
  #convert and reorganize findings
  res.findfun<-sapply(character.vector,find_funs)
  df.res<-as.data.frame(t(res.findfun))
  
  #extract the most basic package where the function name is found
  df.res$last.res<-mapply(`[`, df.res[,1], sapply(df.res[,1],length)) 
  
  #test which do not match any known function
  res<-df.res$last.res=='No_results_found'
  
  if(!quiet){
  print('These input strings were found to be functions from known packages:')
  print(rownames(df.res)[!res])
  
  print("These input strings do not match any known packages' functions:")
  print(rownames(df.res)[res])
  }
  
  return(rownames(df.res)[res])
}
# Explore and plot data

