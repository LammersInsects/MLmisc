# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Define function
trim.EOL.whitespace.from.file<-function(full.file.name,
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
  ftext.trimmed<-trailingspace(ftext, last.character.only = F, apply.to.string.with.only.whitespace = F, quiet = F)
  
  #write trimmed file
  if(overwrite){
    writeLines(ftext.trimmed, full.file.name, useBytes = T)
    if(is.file.updated(full.file.name, seconds.before.now = 10)){
      if(!quiet){
        cat('File is successfully trimmed of all trailing spaces\n')
      }
    } else {
      cat('WARNING: An error may have occurred!\n')
    }
  }
  
  #return text as a vector
  if(return.it){
    return(ftext.trimmed)
  }
}
