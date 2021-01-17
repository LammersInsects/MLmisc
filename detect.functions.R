# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# source("E:/DATA/Common_scripts/R_startup_script.R") #Dell using Maxtor HDD
# source("D:/DATA/Common_scripts/R_startup_script.R") #Dell internal data

# setwd(paste(wd.base, [DATA DRIVE SUBFOLDER], sep=''))

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# files<-list.files()
# list.of.funs<-detect.functions(files[31])
# text.split[grep('result',text.split, fixed = T)]

# Reformat data

# Define function
detect.functions<-function(function.to.check, 
                           type = 'file'){ #file or function
  if(type=='file'){
    #read the file
    ftext<-readLines(con = function.to.check)
  } else if(type=='function'){
    #read the function
    ftext<-decode.workhorse
  } else { #if it's not an R script, abort
    stop('This function is only meant to read R scripts, either saved as *.R or loaded in the Global Environment.')
  }
  
  #exclude empty lines
  ftext<-ftext[!emptyvalues(ftext)]
  
  #for each line, exclude hashed-out parts
  text.nohash<-sapply(ftext,strsplit,'[#]')
  text.nohash<-sapply(text.nohash,`[`,1)
  
  #for each line, detect opening brackets
  text.split<-sapply(text.nohash,strsplit,'[() -]')
  components<-unique(unname(unlist(text.split)))
  
  #exclude patterns that cannot be in a function name
  components.s<-components[grep('[,"${}:=<>/\\%+-]', components, perl = T, invert = T)] #special characters
  components.s<-components.s[!emptyvalues(components.s)] #empty components
  components.s<-components.s[!sapply(sapply(sapply(components.s,elements,1),`%in%`,0:9),all)] #integers
  components.s<-components.s[grep('[', components.s, fixed = T, invert = T)] #subset brackets
  
  return(components.s)
}

# Explore and plot data

