# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function for permutations (of all elements) without repetitions
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}
# perm(1:4)

#Source: https://www.r-bloggers.com/learning-r-permutations-and-combinations-with-base-r/
#for permutations with repetitions, use for example base::expand.grid(rep(list(0:9), 3))
#for combinations without repetitions, use combn(1:49, 6)
