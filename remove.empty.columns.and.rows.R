# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
remove.empty.columns.and.rows<-function(dataframe, do.columns=T, do.rows=T){
  df<-dataframe
  
  if(nrow(df)>1){ #if there is only one row, the df collapses to a vector
    
    if(do.columns){#remove empty columns
      df<-df[,!colSums(sapply(df,emptyvalues))==nrow(df)]
    }
    
    if(do.rows){
      #remove empty rows
      df<-df[!rowSums(sapply(df,emptyvalues))==ncol(df),]
    }
    
  } else { #so here we basically deal with a named vector
    df<-df[!sapply(df,emptyvalues)]
  }
  
  return(df)
}

# Explore and plot data

