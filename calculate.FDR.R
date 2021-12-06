# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
# x<-5
# n<-100
# p<-0.05
# calculate.FDR(x, n, p)
calculate.FDR<-function(x, n, p){
  b<- 1- factorial(n) / (factorial(n-x)*factorial(x)) * p^x * (1-p)^(n-x) #probability of getting exactly x hits?
  pr<-1- (1-p)^n #probability of at least one false discovery
  fdr<-if(x>0){p*(n-x) / (p*n +x)} #actually the pFDR, assuming n is sufficiently large
  return(fdr)
}
