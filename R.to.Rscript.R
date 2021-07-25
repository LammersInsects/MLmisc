# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Define function
R.to.Rscript<-function(R.file){
  #run a number of checks
  if(!file.exists(R.file)){
    stop('File does not exist!')
  }
  if(file.info(R.file)$isdir){
    stop('File is a folder!')
  }
  if(!file.info(R.file)$size>0){
    stop('File is empty!')
  }
  if(!tail(unlist(strsplit(R.file, '.', fixed = T)), n = 1) %in% c('r','R')){
    stop('File extension of the targeted script is not .r or .R')
  }
  
  #read the file
  script<-readLines(con = R.file)
  
  #change every drive name whenever it is used
  for(drive in 1:length(LETTERS)){
    script<-gsub(pattern = paste(LETTERS[drive],":/",sep=''),
                 replacement = paste("/mnt/",letters[drive],"/",sep=''),
                 x = script, fixed = T)
  }
  
  #change other specific strings
  #TODO allow flexible input using a 2-column table
  script<-gsub(pattern = '/R_startup_script.R', #Whenever my R startup script is called, call a Rscript-friendly version
               replacement = '/systeembeheer/crontab/R_startup_script.Rscript',
               x = script, fixed = T)
  script<-gsub(pattern = "user = 'MarkLammers'",
               replacement = "user = 'auto-update'",
               x = script, fixed = T)
  
  #export the script into the same folder 
  dest<-paste(R.file,'script',sep='')
  print(paste('Converted R script is saved as ',dest,sep=''))
  writeLines(text = script, con = dest, useBytes = T)
}
