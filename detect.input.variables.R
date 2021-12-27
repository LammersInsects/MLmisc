# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Define function
detect.input.variables<-function(function.to.check,
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
    #extract the line numbers of all that say "function("
    start<-grep("function(", text.nohash, fixed = T)
    output<-c()
    #test whether any functions are defined at all
    if(length(start)==0){
      cat(function.to.check,'does not contain any functions\n')
    } else {
      #extract all lines that contain closing brackets
      potential.ends<-grep("{", text.nohash, fixed = T)
      
      #for each of the functions defined in the file...
      for(ln in 1:length(start)){
        func<-text.nohash[[start[ln]]]
        #test whether it is a try function definition
        start.func<-unlist(gregexpr('function(',func,fixed=T))
        start.arrow<-unlist(gregexpr('<-',func,fixed=T))
        if(start.func-start.arrow<5){
          #extract function name
          func.name<-unlist(strsplit(func, '<-', fixed = T))[1]
          func.name<-trailingspace(func.name)
          #find the first closing bracket
          end<-potential.ends[potential.ends-start[ln]>=0][1]
          #extract lines with variables
          vars<-names(text.nohash[start[ln]:end])
          #drop the function call
          vars<-strsplit(vars, 'function(', fixed = T)
          vars<-mapply(`[`, vars, sapply(vars, length))
          #drop all starting spaces
          vars<-paste(' ',vars,' ',sep='') #first add a starting and trailing space to all lines that will all get removed later
          spaces<-gregexpr(' ',vars)
          for(i in 1:length(spaces)){
            nrs<-unlist(spaces[i])
            vars[i]<-substring(vars[i],nrs[diff(nrs)!=1][1]+1)
          }
          vars<-trailingspace(vars)
          #extract all the comments
          comments.pos<-sapply(gregexpr('#', vars),`[`,1)
          comments<-unname(mapply(substring, vars, comments.pos))
          comments[comments.pos==-1]<-''
          #drop the function close
          # closed<-sapply(gregexpr('{', vars, fixed = T),`[`,1)
          # vars[closed==2]<-'' #TODO this will lead to issues as the { is not always on the second position
          #drop all the comments
          vars<-ifelse(comments.pos==-1,vars,substr(vars,1,comments.pos-1))
          vars<-trailingspace(vars)
          #lines without variables should not be included from the comments
          comments<-comments[!sapply(vars,emptyvalues)]
          #check whether multiple variables are on a single line
          names(vars)<-comments
          vars<-strsplit(vars,',')
          if(all(sapply(vars,length)!=1)){
            cat('Some lines in',func.name,' contain more than one variable\n')
          }
          vars<-unlist(vars)
          #separate variables from defaults
          default<-strsplit(vars, '=', fixed = T)
          vars<-sapply(default,`[`,1)
          default<-sapply(default,`[`,2)
          #create table to append to output
          found<-data.frame(func=func.name,
                            variables=unname(vars),
                            default=default,
                            comments=names(vars),
                            row.names = NULL)
          #drop the function close
          found<-found[!found$variables %in% c('){'),]
          
          #append it
          output<-rbind(output,found)
        }
      }
    }
    #return complete output
    return(output)
  }
}

