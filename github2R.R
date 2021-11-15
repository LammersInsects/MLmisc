# https://www.r-bloggers.com/source_https-sourcing-an-r-script-from-github-over-https/
# November 24, 2011
# By Tony Breyal
# revised version: https://stackoverflow.com/questions/35720660/how-to-use-an-r-script-from-github
# and getURL error fixed with https://stackoverflow.com/questions/21237469/r-specify-ssl-version-in-rcurl-geturl-statement

# install.packages('RCurl')
source_https <- function(URL, unlink.tmp.certs = FALSE) {
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(URL, followlocation = TRUE, cainfo = "cacert.pem", ssl.verifypeer=TRUE, sslversion=4L)
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

# Example
# source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R",
#              "https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/htmlToText/htmlToText.R")

# source_https("https://raw.githubusercontent.com/LammersInsects/R_magic/master/impact2whisky.R")
# rm(impact2whisky)

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
github2R(github.full.link)
#result should be 'https://raw.githubusercontent.com/LammersInsects/MLmisc/main/duplicates.R'
