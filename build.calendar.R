# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
build.calendar<-function(begin.date,end.date){ #in R date format
  
  dates<-as.Date(begin.date:end.date, origin = "1970-01-01") #make a vector of all dates from begin to end
  calendar<-data.frame(Day=strftime(dates,'%A'), #build the basic calendar with weekdays and dates
                       Date=dates)
  
  calendar<-data.frame(Year='',Month='',Weeknr='',calendar) #add columns for overview
  #fill the year, month and weeknumber columns:
  calendar[substring(calendar$Date,5)=='-01-01','Year']<-strftime(calendar[substring(calendar$Date,5)=='-01-01','Date'],'%Y') 
  calendar[1,'Year']<-strftime(calendar[1,'Date'],'%Y') 
  calendar[substring(calendar$Date,8)=='-01','Month']<-strftime(calendar[substring(calendar$Date,8)=='-01','Date'],'%B')
  calendar[1,'Month']<-strftime(calendar[1,'Date'],'%B') 
  calendar[calendar$Day=='Monday','Weeknr']<-strftime(calendar[calendar$Day=='Monday','Date'],'%V')
  calendar[1,'Weeknr']<-strftime(calendar[1,'Date'],'%V') 
  
  #return output
  return(calendar)
}
