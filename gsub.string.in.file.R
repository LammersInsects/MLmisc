# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Define function
gsub.string.in.file<-function(full.file.name,
                              pattern,
                              replacement,
                              ignore.case=F,
                              perl=F,
                              fixed=F,
                              useBytes=F,
                              overwrite=F,
                              return.it=F,
                              quiet=F){
  #read the lines of the file
  if(file.exists(full.file.name)){
    #read the file
    ftext<-readLines(con = full.file.name, warn = F)
  } else {
    cat('File',full.file.name,'does not exist\n')
    stop()
  }
  
  #detect trailing spaces and remove them
  ftext.gsub<-gsub(pattern = pattern, replacement = replacement, x = ftext,
                   ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
  
  #write trimmed file
  if(overwrite){
    writeLines(ftext.gsub, full.file.name, useBytes = T)
    if(is.file.updated(full.file.name, seconds.before.now = 10)){
      if(!quiet){
        cat('Successfully replaced [',pattern,'] to [',replacement,'] in file',full.file.name,'\n')
      }
    } else {
      cat('WARNING: An error may have occurred!\n')
    }
  }
  
  #return text as a vector
  if(return.it){
    return(ftext.gsub)
  }
}
