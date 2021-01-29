# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data and test
# df<-read.table(paste(wd.base,'Projects_home/Stokerij/database-input/db_recepten_ingredienten_pc-invoer.csv',sep=''),header=T,sep=';')
# df<-read.table('D:/GoogleDrive/Taken_records_new.csv',header=T,sep=';',fill=T,quote='"')
# x<-df[,1]
# x<-gsub('.','',x,fixed=T)
# read.date.format(x)
# input.vector<-x
# x<-c('20190904','20191203', # a vector of common date formats
#      '04092019','03122019',
#      '2019-09-04','2019-12-03',
#      '2019/09/04','2019/12/03',
#      '04-09-2019','03-12-2019',
#      '04.09.2019','03.12.2019',
#      '04/09/2019','03/12/2019',
#      '4-9-2019','3-12-2019',
#      '4/9/2019','3/12/2019',
#      '4-9-19','3-12-19',
#      '4/9/19','3/12/19'
#      )
# for(i in x){print (i); read.date.format(i)}

# Define a function
read.date.format<-function(input.vector){
  
  # Remove trailing spaces and empty values
  input.vector<-trailingspace(input.vector)
  empty<-emptyvalues(input.vector)
  if(sum(empty)>0){
    print(paste('WARNING: Some values are empty.',sum(empty),'of',length(input.vector),'values are NA or empty text strings.'))
  }
  x<-input.vector[!empty]
  
  # Test length of the input
  nch<-nchar(x)
  if(all(nch==10)){
    print('All input is 10 characters, continue...')
  } else {
    if(all(nch %in% 8:10)){
      print('Input inlcudes lengths of 8-9 characters, not included in script yet')
      #this would require a further analysis of the breakdown, 
      #or automatic reformatting the input:
        #if 9 characters, try the script with an extra zero at 1, 4, 6, or 9 position
        #if 8 characters, it could be due to no separators, or two missing zeros
      stop()
    } else {
      print('ERROR: Number of characters of one or more input values cannot be a date')
      stop()
    }
  }
  
  # Load separators and numbers
  seps<-c('.','/','-')
  print('All separators must be the same. Currently only accepting these separators:')
  print(seps)
  nrs<-0:9
  
  # Break down the input
  # breakdown.string(x[1])
  x.broken<-sapply(x,breakdown.string)
  
  # Here I could run some extra analyses on the broken down input
  #e.g. nrow==8 and all(space/sep==F) could mean a date without separators
  
  # Test whether the 3rd and 6th character are all the same and all in seps (year=last)
  # x.broken['vect',2:3] #before the comma is the columns I select, after the comma value id of the input
  third<-sapply(x.broken['vect',], `[`, 3)
  sixth<-sapply(x.broken['vect',], `[`, 6)
  if(all(third %in% seps & sixth %in% seps)){
    print('3rd and 6th are separator: year is last')
    yearlast<-T
    if(all(unname(third)==third[[1]] & unname(sixth)==third[[1]])){
      sep<-third[[1]]
      print(paste('All separators are the same, namely: [',sep,']',sep=''))
    } else {
      print('ERROR: separators are not equal among input')
      print(third)
      print(sixth)
      stop()
    }
  } else {
    # Alternatively, test whether the 5th and 8th character are all the same and all in seps (year=first)
    fifth<-sapply(x.broken['vect',], `[`, 5)
    eigth<-sapply(x.broken['vect',], `[`, 8)
    if(all(fifth %in% seps & sixth %in% seps)){
      print('3rd and 6th are separator: year is first')
      yearlast<-T
      if(all(unname(fifth)==fifth[[1]] & unname(eigth)==fifth[[1]])){
        sep<-fifth[[1]]
        print(paste('All separators are the same, namely:',sep))
      } else {
        print('ERROR: separators are not equal among input')
        print(fifth)
        print(eigth)
        stop()
      }
    } else {
      print('ERROR: Format not recognized. Might vary among input, or is no date')
      stop()
    }
  }
  
  # All other characters in nrs?
  if(yearlast){
    nonsep<-sapply(x.broken['vect',], `[`, c(1,2,4,5,7,8,9,10))
    if(all(nonsep %in% nrs)){
      print('All other input characters in the strings are numbers')
    } else {
      print('ERROR: not all other input characters are numbers')
      print(nonsep)
      stop()
    }
  } else {
    nonsep<-sapply(x.broken['vect',], `[`, c(1,2,3,4,6,7,9,10))
    if(all(nonsep %in% nrs)){
      print('All other input characters in the strings are numbers')
    } else {
      print('ERROR: not all other input characters are numbers')
      print(nonsep)
      stop()
    }
  }
  
  # Which one is the day? (i.e. >12) If unsure, give warning, and put dmY or Ymd depending on tests above
  if(yearlast){
    day<-as.numeric(substr(x,1,2))
    month<-as.numeric(substr(x,4,5))
    if(all(day<13 & month<13)){
      print('WARNING: Both the suspected day and month never go above 12. Date format is set to d-m-Y')
      daymid<-F
    } else {
      if(any(day>12) & any(month>12)){
        print('ERROR: both the day and month go above 12')
        stop()
      } else {
        if(any(day>12)){
          daymid<-F
          print('Day is not in the middle')
        } else {
          daymid<-T
          print('Day is in the middle')
        }
      }
    }
  } else {
    day<-as.numeric(substr(x,9,10))
    month<-as.numeric(substr(x,6,7))
    if(all(day<13 & month<13)){
      print('WARNING: Both the suspected day and month never go above 12. Date format is set to d-m-Y')
      daymid<-F
    } else {
      if(any(day>12) & any(month>12)){
        print('ERROR: both the day and month go above 12')
        stop()
      } else {
        if(any(day>12)){
          daymid<-F
          print('Day is not in the middle')
        } else {
          daymid<-T
          print('Day is in the middle')
        }
      }
    }
  }
  
  # Construct the date format string
  if(yearlast){
    if(daymid){
      form<-paste('%m',sep,'%d',sep,'%Y',sep='')
    } else {
      form<-paste('%d',sep,'%m',sep,'%Y',sep='')
    }
  } else {
    if(daymid){
      form<-paste('%Y',sep,'%d',sep,'%m',sep='')
    } else {
      form<-paste('%Y',sep,'%m',sep,'%d',sep='')
    }
  }
  
  # Test the chosen format with as.Date. If successful, set success to TRUE
  test<-is.na(as.Date(x,format=form))
  if(all(test==F)){
    success<-T
    print(paste('Input conversion to R-friendly date format SUCCESSFUL. Date format input is',form,'and output format is %Y-%m-%d'))
  } else {
    success<-F
    print('The date conversion was unsuccessful despite all intermediate checks')
  }
  
  # Reconstruct length of the input vector
  output.vector<-input.vector
  output.vector[!empty]<-x
  
  if(success){
    output.vector<-as.Date(output.vector,format=form)
    return(output.vector)}
}
