# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
scatterplot.all.combinations<-function(dataframe, factor.col=NA, quiet=F){
  #TODO row.names could also be a source of factor names, test for that?!
  
  #Check input
  if(is.data.frame(dataframe)){
    df<-dataframe
  } else {
    stop('ERROR: Input data frame must be a dataframe object!')
  }
  if(!is.na(factor.col)){
    #TODO here check that the column does work to categorize data
    fc<-df[,factor.col]
    df<-df[,grep(factor.col,colnames(df),invert=T)]
    #Check that the input data is numeric
    test<-sapply(df,is.numeric)
  } else {
    if(!quiet){print('No factor.col has been defined')}
    #Check that the input data is numeric
    test<-sapply(df,is.numeric)
  }
  if(all(test)){
    #all the input data columns are numeric
  } else {
    print(paste('WARNING: Not all input data columns are numeric! Omitting ',length(test)-sum(test),
                'non-numeric columns with these column names:',sep=''))
    print(colnames(df)[!test])
    df<-df[,test]
  }
  
  #create a matrix with all the combinations of column names that should be plotted against each other
  combinations<-combn(colnames(df),2)
  
  #Plot all those combinations
  done<-sapply(1:ncol(combinations), function(x){
    plot(df[,combinations[,x][1]] ~ df[,combinations[,x][2]], xlab=combinations[,x][1], ylab=combinations[,x][2])
  })
  print(paste('Made ',length(done),' scatter plots.', sep=''))
}

# Explore and plot data

