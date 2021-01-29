# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 
# df<-read.table('Aphaereta_longevity_exp1.csv', sep=';', header=T)

# Reformat data
# data<-df
# start.exp<-'05.07.2019'

#test function
# result<-convert.death.data.R(data = df, start.exp = '05.07.2019')
# result<-convert.death.data.R(data = df, start.exp = '05.07.2019', death.count.columns=c('males', 'females'))
# result<-convert.death.data.R(data = df, start.exp = '04.07.2019') 
# rm(result)

# Define function
convert.death.data.R<-function(data, #the data frame with death counts
                               start.exp, #give the starting date of the experiment as a string
                               id.column='ID', #should be good if default data sheet was used
                               date.column='Date', #should be good if default data sheet was used
                               death.count.columns=c('males', 'females', 'unkn.') #should be good if default data sheet was used
                               ){
  df<-data
  id.col<-id.column
  date.col<-date.column
  
  #convert date format
  print('Converting date formats using read.date.format.R > both should give SUCCESSFUL as output...')
  print('---> Dates in data frame:')
  df$Date<-read.date.format(df[,date.col])
  print('---> Date of start.exp:')
  start.exp<-read.date.format(start.exp)
  first.record<-min(df$Date)
  if(first.record<start.exp){
    print('ERROR: First records are older than the given start of the experiment')
    stop()
  }
  
  #sort the data frame
  df$ID<-df[,id.col]
  df<-df[order(df$Date,df$ID),]
  
  #get all IDs and dates
  ids<-unique(df$ID)
  dates<-unique(c(start.exp,unique(df$Date)))
  
  #for each record, calculate time since start
  df$day.last.alive<-normalize.date(c(df$Date,start.exp))[1:length(df$Date)]
  
  #get sample sizes
  overview<-data.frame(ID=ids,
                       t(sapply(ids,function(id){
    colSums(df[df$ID==id,death.count.columns])
  })))
  overview<-merge(overview, summarySE(data = df, measurevar = 'day.last.alive', groupvars = 'ID'), by='ID')
  
  #build a survival table
  cumul<-merge(ids, dates)
  colnames(cumul)<-c('ID', 'Date')
  cumul<-cumul[order(cumul$Date,cumul$ID),]
  cumul[,death.count.columns]<-0
  cumul$deaths<-F
  cumul[cumul$Date<=first.record,death.count.columns]<-overview[,death.count.columns]
  
  for(line in 1:nrow(df)){
    record<-df[line,]
    new.counts<-cumul[cumul$ID==record$ID & cumul$Date==record$Date,death.count.columns] - 
      record[,death.count.columns]
    cumul[cumul$ID==record$ID & cumul$Date>record$Date,death.count.columns] <- new.counts
    cumul[cumul$ID==record$ID & cumul$Date==record$Date,'deaths']<-T
  }
  
  # for(i in ids){
  #   print(cumul[cumul$ID==i,])
  # }
  # df[df$ID=='AL9',]
  
  #output
  return(cumul)
}

# some stuff to move to a plotting script
# plot(x=range(result$Date), y=range(result[,death.count.columns]), type='n')
# for(id in unique(result$ID)){
#   df.s<-result[result$ID==id,]
#   for(col in 1:length(death.count.columns)){
#     lines(x = df.s$Date, y = df.s[,death.count.columns[col]], type='b')
#   }
# }  
