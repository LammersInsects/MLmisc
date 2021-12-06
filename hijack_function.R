#########################################################
### Change the default of a parameter of a function ###
#########################################################

# Source: http://www.r-bloggers.com/hijacking-r-functions-changing-default-arguments/
# August 19, 2014, by tylerrinker
# Compilation by Mark Lammers, 22 September 2015


## Option 1: case-specific, just one parameter, fast
# .data.frame <- function(..., stringsAsFactors = FALSE) {
#   data.frame(..., stringsAsFactors = stringsAsFactors)
# }
  
## Option 2: more elaborate, can change multiple parameters
hijack <- function(FUN, ...){
  .FUN <- FUN
  args <- list(...)
  invisible(lapply(seq_along(args), function(i) {
    formals(.FUN)[[names(args)[i]]] <<- args[[i]]
  }))
  .FUN
}
.data.frame <- hijack(data.frame, stringsAsFactors = FALSE, quote='"')
  

## To test:
# dat <- .data.frame(x1 = 1:3, x2 = c("a", "b", "c"))
# str(dat)

