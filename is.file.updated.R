# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Define function
is.file.updated<-function(full.file.path, seconds.before.now=300){
  
  #output is alwways true except when any of the tests fails
  output<-T
  
  #does the file exist at all?
  if(!file.exists(full.file.path)){
    cat('File does not exist!\n')
    output<-F
  }
  
  #is the file actually a folder?
  if(file.info(full.file.path)$isdir){
    cat('WARNING: File is a folder!\n')
  }
  
  #is the file empty?
  if(!file.info(full.file.path)$size>0){
    cat('File is empty!\n')
    output<-F
  }
  
  #check date and time: less than a given number of seconds old?
  if(!file.info(full.file.path)$mtime > (Sys.time() - seconds.before.now)){
    cat("The file's last modification is more than ",seconds.before.now," seconds ago!",sep='',"\n")
    output<-F
  }
  
  return(output)
}
