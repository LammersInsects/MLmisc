# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# start<-now()
# end<-now()
# time.passed(start, end)

# Reformat data

# Define function
time.passed<-function(start, end, return.it=F){ #start and end should be timestamps as produced by now()
  start.n<-nchar(start)
  end.n<-nchar(end)
  if(min(start.n,end.n)<14){
    stop('ERROR: Number of characters of input indicates precision is above the seconds. Is the input produced by MLmisc::now()?')
  } else if(min(start.n,end.n)==14){ #Precision should be in seconds
    
  } else if(min(start.n,end.n)>14){ #Precision in decimal seconds
    
  }
  
  #default for seconds precision
  tp<-difftime(strptime(x = end, format = '%Y%m%d%H%M%S'), strptime(x = start, format = '%Y%m%d%H%M%S'))
  
  if(start.n<20){ #microsecond precision has 20 characters
    start<-paste(as.numeric(start),paste(rep(0,20-start.n),collapse=''),sep='') #add as many zeros as required
    #the as.numeric doesn't help to remove the unwanted precision. Instead the precision should be removed in now()
  }
  if(end.n<20){ #microsecond precision has 20 characters
    end<-paste(as.numeric(end),paste(rep(0,20-end.n),collapse=''),sep='') #add as many zeros as required
  }
  
  if((as.numeric(end)-as.numeric(start))<60000000){ #when something takes less than a minute,
    tp<-(as.numeric(end)-as.numeric(start))/1000000 #report output in decimal seconds
    tp<-round(tp,digits=min(start.n,end.n)-14) #strip any decimals that are below the input precision
    tp<-as.difftime(tp, units = 'secs')
  } #else use default difftime
  
  print(paste('Time passed:',format(tp)))
  if(return.it){
    return(tp)
  }
}


# Explore and plot data

