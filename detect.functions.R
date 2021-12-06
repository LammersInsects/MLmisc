# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data
# files<-list.files()
# files<-files[grep('.R',files, fixed = T)]
# for(f in files){
#   print(f)
#   list.of.funs<-detect.functions(f)
#   if(!is.null(list.of.funs[1])){
#     if(!is.na(list.of.funs[1])){
#       find.functions(list.of.funs)
#     }
#   }
# }
# function.to.check<-files[1]
# for(f in functions){
#   print(f)
#   list.of.funs<-detect.functions(f, type = 'function')
#   if(!is.null(list.of.funs[1])){
#     if(!is.na(list.of.funs[1])){
#       find.functions(list.of.funs)
#     }
#   }
# }
# detect.functions(function.to.check = 'full.ranges', type = 'function')

# text.split[grep('result',text.split, fixed = T)]

# Reformat data

# Define function
detect.functions<-function(function.to.check,
                           type = 'file'){ #file or function
  if(type=='file'){
    if(substr(function.to.check,nchar(function.to.check)-1,nchar(function.to.check))=='.R'){
      #read the file
      ftext<-readLines(con = function.to.check, warn = F)
    } else {
      stop('File extension is not .R')
    }
  } else if(type=='function'){
    #read the function text
    ftext<-as.character(body(function.to.check))
  } else { #if it's not an R script, abort
    stop('This function is only meant to read R scripts, either saved as *.R or loaded in the Global Environment.')
  }
  
  #exclude empty lines
  ftext<-ftext[!emptyvalues(ftext)]
  
  #for each line, exclude hashed-out parts
  text.nohash<-sapply(ftext,strsplit,'[#]')
  text.nohash<-sapply(text.nohash,`[`,1)
  
  #check whether there is anything at all
  if(all(sapply(text.nohash, emptyvalues))){
    print('No unhashed text found in script')
    return(NA)
  } else {
    
    #for each line, split at all kinds of breaks
    text.split<-sapply(text.nohash,strsplit,'[,"${}!:;=<>/\\%*+ -]')
    
    #subset those that only have an opening bracket, the key definition of a function
    text.split.s<-mapply(`[`, text.split, sapply(text.split,grep,pattern='(',fixed=T))
    text.split.s<-unname(unlist(text.split.s))
    
    #split at opening brackets
    list.of.potential.funs<-sapply(text.split.s,strsplit,'[(]')
    if(length(list.of.potential.funs)==0){
      print('No functions found')
      return(NA)
    } else {
      
      #all except the last of a line are functions
      list.of.potential.funs.s<-mapply(`[`, list.of.potential.funs, -sapply(list.of.potential.funs,length))
      list.of.funs<-unique(unname(unlist(list.of.potential.funs.s)))
      
      #sometimes a function has some angular bracket, what's before it is not part of a function name
      list.of.funs<-unique(unname(unlist(sapply(sapply(list.of.funs, strsplit, '[', fixed=T), tail, n=1))))
      
      # #exclude patterns that cannot be in a function name
      # components.s<-components[grep('[,"${}:=<>/\\%+-]', components, perl = T, invert = T)] #special characters
      # components.s<-components.s[!emptyvalues(components.s)] #empty components
      # components.s<-components.s[!sapply(sapply(sapply(components.s,elements,1),`%in%`,0:9),all)] #integers
      # components.s<-components.s[grep('[', components.s, fixed = T, invert = T)] #subset brackets
      
      return(list.of.funs)
    }
  }
}

# Explore and plot data

