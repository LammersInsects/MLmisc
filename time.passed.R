# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Test data
# start<-now(); Sys.sleep(2.5); end<-now(); time.passed(start, end)
# start<-now(precision = 'ms'); Sys.sleep(2.5); end<-now(precision = 'ms'); time.passed(start, end)
# start<-now(precision = 'us'); Sys.sleep(2.5); end<-now(precision = 'us'); time.passed(start, end)

# Define function
time.passed<-function(start, end, return.it=F){ #start and end should be timestamps as produced by now()
  #normalize date-times
  times<-strptime(c(end,start), format='%Y%m%d%H%M%S')
  ds<-options("digits.secs")
  
  #assess decimal seconds
  start.n<-nchar(start)
  end.n<-nchar(end)
  precision<-min(start.n,end.n)-14
  if(precision<0){
    stop('ERROR: Number of characters of input indicates precision is above the seconds. Is the input produced by MLmisc::now()?')
  } else if(precision==0){ #Precision should be in seconds
    options("digits.secs"=0)
  } else if(precision>0){ #Precision in decimal seconds
    options("digits.secs"=precision)
    times.s<-as.numeric(substr(c(end,start),15,c(end.n,start.n)))/10^precision
    times<-times+times.s
  }
  
  #calculate exact time difference
  tp<-difftime(times[1], times[2])
  
  #report result
  cat('Time passed:',format(tp),'\n')
  
  #reset digits
  options(ds)
  
  if(return.it){
    return(tp)
  }
}
