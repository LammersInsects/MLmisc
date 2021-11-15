# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

github2R<-function(github.full.link){
  #test whether it's a https URL
  if(!substr(github.full.link, 1, 5)=='https'){
    cat('The link should start with <https> !')
  }
  
  #convert the link to the raw page link
  result<-gsub('github.com','raw.githubusercontent.com',github.full.link)
  result<-gsub('blob/','',result)
  
  #load the script into R
  source(result)
  
  #check success
  #TODO
}
  
#Example
# github.full.link<-'https://github.com/LammersInsects/MLmisc/blob/main/duplicates.R'
# github2R(github.full.link)
# result should be 'https://raw.githubusercontent.com/LammersInsects/MLmisc/main/duplicates.R'
