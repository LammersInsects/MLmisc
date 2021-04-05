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
  if(nchar(start)<20){ #microsecond precision has 20 characters
    start<-paste(start,paste(rep(0,20-nchar(start)),collapse=''),sep='') #add as many zeros as required
  }
  if(nchar(end)<20){ #microsecond precision has 20 characters
    end<-paste(end,paste(rep(0,20-nchar(end)),collapse=''),sep='') #add as many zeros as required
  }
  tp<-difftime(strptime(x = end, format = '%Y%m%d%H%M%S'), strptime(x = start, format = '%Y%m%d%H%M%S'))
  if((as.numeric(end)-as.numeric(start))<60000000){ #when something takes less than a minute,
    tp<-(as.numeric(end)-as.numeric(start))/1000000 #report output in decimal seconds
    tp<-as.difftime(tp, units = 'secs')
  }
  print(paste('Time passed:',format(tp)))
  if(return.it){
    return(tp)
  }
}


# Explore and plot data

