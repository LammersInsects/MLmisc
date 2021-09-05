# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Define function
#Solution by eyjo at https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
#also include all other given solutions from that page

havingIP <- function(){
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage[grep("127.0.0.1", ipmessage, invert = T)]))
}

is_online <- function(site="http://example.com/") {
  tryCatch({
    readLines(site,n=1)
    TRUE
  },
  warning = function(w) invokeRestart("muffleWarning"),
  error = function(e) FALSE)
}

test.internet<-function(){
  if(!require(curl)){
    havingIP() & is_online()
  } else {
    havingIP() & curl::has_internet()
  }
}
