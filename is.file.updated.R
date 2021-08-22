# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Define function
is.file.updated<-function(full.file.path){
  
  #does the file exist at all?
  if(!file.exists(full.file.path)){
    stop('File does not exist!')
  }
  
  #is the file actually a folder?
  if(file.info(full.file.path)$isdir){
    stop('File is a folder!')
  }
  
  #is the file empty?
  if(!file.info(full.file.path)$size>0){
    stop('File is empty!')
  }
  
  #check date and time: less than 5 minutes old?
  if(!file.info(full.file.path)$mtime > (Sys.time() - 300)){
    stop("The file's last modification is more than 5 minutes ago!")
  }
  
  return()
}