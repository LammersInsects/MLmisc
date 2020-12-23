# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.


# ToDo
# - make save.plots.as.pdfs as save.plots=c('no','pdf','png')

survival.analysis<-function(dataframe,     # Line 1: Essentials
                            filename='results',
                            subexperiment.col=F,   # Line 2: Define columns - required when different from default
                            Treatment.col='Treatment', 
                            Timevar=F,  #is assumed to be measured in days
                            Survival.col=F, #survival rate
                            Mortality.col=F, #mortality rate
                            Samplesize.col=F,
                            Surviving.col=F, #aggregate data
                            Dead.col=F, #aggregate data
                            Survived.col=F, #individual-based and binary
                            Died.col=F, #individual-based and binary
                            Bodysize.col=F, #individual-based
                            plot.results=T,   # Line 3: Tell the function which parts to run
                            save.plots.as.pdfs=F, #make as c('no','pdf','png')
                            do.stats=T,
                            randomeffect.col=F,  #models taking this into account are unfinished
                            control.treatment=F,   # Line 4: Optional - additional analysis of deviation from controls
                            analysis.of.residuals=F,
                            return.df=F
){
  
  #   filename='debugging'
  if(filename=='debugging'){
    subexperiment.col=F   # Line 2: Define columns - required when different from default
    Treatment.col='Treatment' 
    Timevar=F  #is assumed to be measured in days
    Survival.col=F #survival rate
    Mortality.col=F #mortality rate
    Samplesize.col=F
    Surviving.col=F #aggregate data
    Dead.col=F #aggregate data
    Survived.col=F #individual-based and binary
    Died.col=F #individual-based and binary
    Bodysize.col=F #individual-based
    plot.results=T   # Line 3: Tell the function which parts to run
    save.plots.as.pdfs=F
    do.stats=T
    randomeffect.col=F  #models taking this into account are unfinished
    control.treatment=F   # Line 4: Optional - additional analysis of deviation from controls
    analysis.of.residuals=F
    return.df=F
  }
  
  print(paste('Running',filename))
  
  #Load packages
  print('Note: The pipeline will install all dependencies on the first run. This might take a while.')
  options(warn = -1)
  if (!require(car)) {install.packages('car')} 
  library(car)
  if (!require(sciplot)) {install.packages('sciplot')} 
  library(sciplot)
  if (!require(ggplot2)) {install.packages('ggplot2')} 
  library(ggplot2)
  if (!require(RColorBrewer)) {install.packages('RColorBrewer')} 
  library(RColorBrewer)
  if (!require(nlme)) {install.packages('nlme')} 
  library(nlme)
  if(!require(multcomp)){install.packages('multcomp')}
  library('multcomp')
  options(warn = 0)
  
  
  ### Check what kind of data frame we have ###
  df<-dataframe
  print('Survival and Mortality are expected as rates between 0 and 1 and require specification of sample sizes;')
  print('Surviving and Dead are expected as aggregate data and should be integers specifying how many survived/died;')
  print('Survived and Died are expected as individual-based data using binary variables (0 or 1) specifying if an individual died or not')
  print('Input cannot logically contain a combination of these 3 data types! (although aggregate and rates might?)')
  
  ## But first we need te check some other data properties and store input independent of type of data frame 
  
  # Check for empty columns and remove these
  df.withemptycol<-ncol(df)
  df<-df[!sapply(df, function(x) all(x == "" || is.na(x)))]
  df.ncol<-ncol(df)
  if(df.withemptycol!=df.ncol){
    print(paste(df.withemptycol-df.ncol,'empty columns found. These are removed from the data set'))
  } else {
    print('No empty columns found')
  }
  
  # Check for rows with NAs in bodysize and remove these lines 
  df.NA<-df[0,]
  if(Bodysize.col!=F){
    if(is.numeric(df[,Bodysize.col])){
      df.withNA<-df
      df.NA<-subset(df,is.na(rowSums(df[,c(Survived.col,Bodysize.col)])))
    } else {
      print('WARNING: non-numeric Bodysize.col!'); stop()
    }
  } 
  df.NA<-rbind(df.NA,df[sapply(df[,Treatment.col],nchar)==0,])
  if(nrow(df.NA)>0){
    print(paste(nrow(df.NA),'rows with NAs found. These are removed from the analysis:'))
    print(df.NA)
  } else {
    print('No NAs found: All rows are complete')
  }
  df.noNA<-subset(df,!rownames(df) %in% rownames(df.NA)) #removes all records that have NAs in those columns
  df<-df.noNA
  
  
  # Store treatment specifications
  df$treatment<-df[,Treatment.col]
  if(control.treatment!=F){
    if(!control.treatment %in% df$treatment){
      print('WARNING: control.treatment is not found in the Treatment column!'); stop()
    } else {
      df$treatment<-gsub(control.treatment,'.Control',df$treatment)
    }
  } else {
    print('WARNING! control.treatment is not specified. Should it?')
  }
  
  # Check for presence of experiment substructure (e.g. RNAi_Target, Strain, Block, ...) 
  if(subexperiment.col==F){
    print('No experiment substructure found')
    groups<-'groupdummyvar'
    nr.groups<-1
    df$groupstructure<-'groupdummyvar'
  } else {
    groups<-sort(unique(df[,subexperiment.col]))
    nr.groups<-length(groups)
    print(paste(nr.groups, 'groups found as substructure named <',subexperiment.col,'> in data set:'))
    print(paste(groups))
    if(is.numeric(df[,subexperiment.col])){
      print(paste('Treating subexperiment column named <',subexperiment.col,'> as numeric (unless coerced by statistical functions).'))
      print('    If incorrect, change input dataframe')
    } else{
      print(paste('Treating subexperiment column named <',subexperiment.col,'> as factor. If incorrect, change input dataframe'))
    }
    df$groupstructure<-df[,subexperiment.col]
  }
  
  # Check for the presence of random effects
  if(randomeffect.col==F){
    print('No random effects found')
    randomeffect<-'randomeffectdummyvar'
    nr.randomeffects<-1
    df$randomeffect<-'randomeffectdummyvar'
  } else {
    if(is.numeric(df[,randomeffect.col])){
      print(paste('Treating randomeffect column named <',randomeffect.col,'> as numeric. If incorrect, change input dataframe'))
    } else{
      print(paste('Treating randomeffect column named <',randomeffect.col,'> as factor. If incorrect, change input dataframe'))
    }
    randomeffects<-sort(unique(df[,randomeffect.col]))
    nr.randomeffects<-length(randomeffects)
    df$randomeffect<-df[,randomeffect.col]
  }
  
  # Is there individual-based data?
  if(Survived.col!=F){ #i.e. there is survived data
    if(all(names(table(df[,Survived.col]))==c(0,1))){
      print('Individual-based data found: A column specifying survival is specified')
      df.long<-data.frame(treatment=df$treatment,
                          groupstructure=df$groupstructure,
                          randomeffect=df$randomeffect,
                          survived=df[,Survived.col],
                          died=ifelse(df[,Survived.col]==1,0,1))
      if(Bodysize.col!=F){df.long$bodysize<-df[,Bodysize.col]}
      generate.individualbased<-F #Here we flag that we do not need to build an individual-based data frame
      generate.aggregate<-T #Here we flag that we need to build an aggregate data frame
      generate.rates<-T #Here we flag that we need to build a rates data frame
    } else {
      print('ERROR: Individual-based data is found, but the column specifying it is not binary'); stop()
    }
  } else {
    if(Died.col!=F){ #i.e. there is died data
      if(all(names(table(df[,Died.col]))==c(0,1))){
        print('Individual-based data found: A column specifying death is specified')
        df.long<-data.frame(treatment=df$treatment,
                            groupstructure=df$groupstructure,
                            randomeffect=df$randomeffect,
                            died=df[,Died.col],
                            survived=ifelse(df[,Died.col]==1,0,1))
        if(Bodysize.col!=F){df.long$bodysize<-df[,Bodysize.col]}
        generate.individualbased<-F #Here we flag that we do not need to build an individual-based data frame
        generate.aggregate<-T #Here we flag that we need to build an aggregate data frame
        generate.rates<-T #Here we flag that we need to build a rates data frame
      } else {
        print('ERROR: Individual-based data is found, but the column specifying it is not binary'); stop()
      }
    } else { #i.e. there is neither survived nor died data
      generate.individualbased<-T #Here we decide to build an individual-based data frame
    }
  }
  
  #Is the input data aggregate data?
  if(Surviving.col!=F){ #i.e. there is survived data
    if(is.integer(df[,Surviving.col])){
      print('Aggregate data found: A column specifying survival is specified')
      if(Samplesize.col!=F){
        if(is.numeric(df[,Samplesize.col])){
          df$samplesize<-df[,Samplesize.col]
        } else {
          print('WARNING: non-numeric Samplesize.col!'); stop()
        }
      }
      df.ag<-data.frame(treatment=df$treatment,
                        groupstructure=df$groupstructure,
                        randomeffect=df$randomeffect,
                        samplesize=df$samplesize,
                        surviving=df[,Surviving.col],
                        dead=df[,Samplesize.col]-df[,Surviving.col])
      generate.individualbased<-T #Here we flag that we need to build an individual-based data frame
      generate.aggregate<-F #Here we flag that we do not need to build an aggregate data frame
      generate.rates<-T #Here we flag that we need to build a rates data frame
    } else {
      print('ERROR: Aggregate data is found, but the column specifying it is not integers'); stop()
    }
  } else {
    if(Dead.col!=F){ #i.e. there is death data
      if(is.integer(df[,Dead.col])){
        print('Aggregate data found: A column specifying death is specified')
        if(Samplesize.col!=F){
          if(is.numeric(df[,Samplesize.col])){
            df$samplesize<-df[,Samplesize.col]
          } else {
            print('WARNING: non-numeric Samplesize.col!'); stop()
          }
        }
        df.ag<-data.frame(treatment=df$treatment,
                          groupstructure=df$groupstructure,
                          randomeffect=df$randomeffect,
                          samplesize=df$samplesize,
                          dead=df[,Dead.col],
                          surviving=df[,Samplesize.col]-df[,Dead.col])
        generate.individualbased<-T #Here we flag that we need to build an individual-based data frame
        generate.aggregate<-F #Here we flag that we do not need to build an aggregate data frame
        generate.rates<-T #Here we flag that we need to build a rates data frame
      } else {
        print('ERROR: Aggregate data is found, but the column specifying it is not integers'); stop()
      }
    } else { #i.e. there is neither survived nor died data
      generate.aggregate<-T #Here we decide to build an aggregate data frame
    }
  }
  
  #Is the input data a set of rates?  THIS PART IS NOT VERFIED!!!
  if(Survival.col!=F){ #i.e. there is survived data
    if(is.numeric(df[,Survival.col])){ #SHOULD ALSO CHECK IT IS BETWEEN 0 AND 1!!!
      print('Rates data found: A column specifying survival is specified')
      df.rates<-df
      df.rates$survival<-df.rates[,Survival.col]
      df.rates$mortality<-1-df.rates$survival
      generate.individualbased<-T #Here we flag that we need to build an individual-based data frame
      generate.aggregate<-T #Here we flag that we need to build an aggregate data frame
      generate.rates<-F #Here we flag that we do not need to build a rates data frame
    } else {
      print('ERROR: Rates data is found, but the column specifying it is not numeric'); stop()
    }
  } else {
    if(Mortality.col!=F){ #i.e. there is mortality data
      if(is.numeric(df[,Mortality.col])){ #SHOULD ALSO CHECK IT IS BETWEEN 0 AND 1!!!
        print('Rates data found: A column specifying mortality is specified')
        df.rates<-df
        df.rates$mortality<-df.rates[,Mortality.col]
        df.rates$survival<-1-df.rates$mortality
        generate.individualbased<-T #Here we flag that we need to build an individual-based data frame
        generate.aggregate<-T #Here we flag that we need to build an aggregate data frame
        generate.rates<-F #Here we flag that we do not need to build a rates data frame
      } else {
        print('ERROR: Rates data is found, but the column specifying it is not numeric'); stop()
      }
    } else { #i.e. there is neither survived nor died data
      generate.rates<-T #Here we decide to build a rates data frame
    }
  }
  
  # Check success of any of the 3 checks above
  if(any(!generate.individualbased,!generate.aggregate,!generate.rates)){
    print('Input successfully read, continue...')
  } else {
    print('ERROR! Neither kind of data found. Please specify input properly'); stop()
  }
  
  ### Construct data frames ###
  # Construct individual-based data frame // UNTESTed IS HOW NAs ARE HANDLED!!!
  # FOR NOW I ASSUME THAT EITHER INDIVIDUAL-BASED OR AGGREGATE DATA IS PROVIDED
  if(generate.individualbased){
    #     df.long<-data.frame(treatment=rep(df.ag$treatment,each=df.ag$samplesize[1]),
    #                         groupstructure=rep(df.ag$groupstructure,each=df.ag$samplesize[1]),
    #                         randomeffect=rep(df.ag$randomeffect,each=df.ag$samplesize[1]),
    #                         survived=c(rep(1,each=df.ag$surviving),rep(0,each=(df.ag$samplesize-df.ag$surviving))))
    df.long<-data.frame(treatment=rep(df.ag$treatment[1],df.ag$samplesize[1]),
                        groupstructure=rep(df.ag$groupstructure[1],df.ag$samplesize[1]),
                        randomeffect=rep(df.ag$randomeffect[1],df.ag$samplesize[1]),
                        survived=c(rep(1,df.ag$surviving[1]),rep(0,df.ag$samplesize[1]-df.ag$surviving[1])))
    for(i in 2:nrow(df.ag)){
      df.long.extension<-data.frame(treatment=rep(df.ag$treatment[i],df.ag$samplesize[i]),
                                    groupstructure=rep(df.ag$groupstructure[i],df.ag$samplesize[i]), 
                                    randomeffect=rep(df.ag$randomeffect[i],df.ag$samplesize[i]),
                                    survived=c(rep(1,df.ag$surviving[i]),rep(0,df.ag$samplesize[i]-df.ag$surviving[i])))
      df.long<-rbind(df.long,df.long.extension)
      rm(df.long.extension)
    }
  } else {
    #Here I could put stuff that is done above in the checks block
  }
  df.long$died<-ifelse(df.long$survived==1,0,1)
  write.table(df.long,paste(today,filename,'individual-based.csv',sep='_'),sep=';',row.names=F)
  
  # Construct aggregate data frame and check data type for essential columns
  if(generate.aggregate){
    df.ag.n<-aggregate(df.long[c('survived')], df.long[c('treatment','groupstructure','randomeffect')], length)
    df.ag<-aggregate(df.long[c('survived','died')], df.long[c('treatment','groupstructure','randomeffect')], sum)
    colnames(df.ag)<-gsub('survived','surviving',colnames(df.ag))
    df.ag$samplesize<-df.ag.n$survived
    rm(df.ag.n)
    write.table(df.ag,paste(today,filename,'aggregate.csv',sep='_'),sep=';',row.names=F)
  } else {
    #Here I could put stuff that is done above in the checks block
  }
  
  # Construct rates data frame
  if(generate.rates){
    df.rates<-df.ag[,c('treatment','groupstructure','randomeffect','samplesize')]
    df.rates$survival<-df.ag$surviving/df.ag$samplesize
    df.rates$mortality<-1-df.rates$survival
    write.table(df.rates,paste(today,filename,'rates.csv',sep='_'),sep=';',row.names=F)
  } else {
    #Here I could put stuff that is done above in the checks block
  }
  
  # A so far unused block of how to handle time-sampled data
  #   if(Timevar!=F){ #i.e. there is a column specifying the number of days fed or when an individual died
  #     print(paste('Time variable called',Timevar,'found'))
  #     if(is.numeric(df[,Timevar])){
  #       df$Treatment<-paste(df[,Timevar],df[,Treatment.col],sep='d')
  #       df$Timevar<-df[,Timevar]
  #     } else {
  #       print('WARNING: non-numeric Timevar!'); stop()
  #     }
  #   } else {
  #     df$Treatment<-df[,Treatment.col]
  #   }
  #   
  
  #Check Mortality data: exclude negative Mortality // THIS BLOCK IS KEPT AS A REMINDER THAT CHECKS FOR IMPOSSIBLE DATA MIGHT NEED TO BE RUN
  #   impossible<-subset(df,df$Mortality<0 | df$Mortality>1) #Check for impossible data (i.e. negative mortality and remove these)
  #   if(nrow(impossible)>0){
  #     print(paste(nrow(impossible),'impossible rows of data found (negative Mortality). These are removed before analysis:'))
  #     print(impossible)
  #   } else {
  #     print('All rows have Mortality rates between 0 and 1')
  #   }
  #   df<-subset(df,df$Mortality>=0 & df$Mortality<=1)
  
  #Assign colors to groups
  groups.key<-data.frame(color=1:nr.groups, Group=groups)
  df.long$color.Groups<-groups.key$col[match(df.long$groupstructure,groups.key$Group)]
  df.ag$color.Groups<-groups.key$col[match(df.ag$groupstructure,groups.key$Group)]
  df.rates$color.Groups<-groups.key$col[match(df.rates$groupstructure,groups.key$Group)]
  
  #Treatment overview
  treatments<-sort(unique(df.long$treatment))
  nr.treatments<-length(treatments)
  print(paste(nr.treatments, 'treatments found:'))
  samplesize<-table(df.long[,c('groupstructure','treatment')]) #This does not work well when the input data is aggregate data like Survived/Dead
  print(samplesize)
  
  #Assign colors to treatments
  treatments.key<-data.frame(color=1:nr.treatments, Treatment=treatments)
  df.long$color.Treatment<-treatments.key$col[match(df.long$treatment,treatments.key$Treatment)]
  df.ag$color.Treatment<-treatments.key$col[match(df.ag$treatment,treatments.key$Treatment)]
  df.rates$color.Treatment<-treatments.key$col[match(df.rates$treatment,treatments.key$Treatment)]
  
  ##################################
  # Summary statistics of data set #
  ##################################
  
  MORTALITY<-summarySE(df.rates,measurevar = 'mortality', groupvars = c('treatment'))
  print('Summary of complete data set:')
  print(MORTALITY)
  print(paste('Total number of animals in data set is',nrow(df.long)))
  #   if(Timevar!=F){
  #     MORTALITY.time<-summarySE(df,measurevar = 'Mortality', groupvars = c('FeedingTreatment','Timevar'))
  #   }
  if(Bodysize.col==F){ 
    print('No body size data found')
  } else { 
    BODYSIZE<-summarySE(data = df.long, measurevar = 'bodysize', groupvars = c('treatment','groupstructure'))
    print(BODYSIZE)
    bodysize.minmax<-c(min(df.long$bodysize,na.rm=T),max(df.long$bodysize,na.rm=T))
  }
  
  #############
  # Plot data #
  #############
  if(plot.results){
    #     dev.off()
    options(warn = -1)
    par(mfrow=c(1,1))
    
    #Bodysize  data    
    if(Bodysize.col!=F){ 
      if(save.plots.as.pdfs){
        pdf(paste(today,'_',filename,'_Bodysize.density.plot.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
      }
      bodysize.density<-ggplot(df.long, aes(bodysize, fill = treatment)) + geom_density(alpha = 0.2) +#Density plot of Bodysize per treatment
        ggtitle(paste(today,filename))
      plot(bodysize.density)
      bodysize.density<-ggplot(df.long, aes(bodysize, fill = as.factor(survived))) + geom_density(alpha = 0.2) +#Density plot of Bodysize per treatment
        ggtitle(paste(today,filename))
      plot(bodysize.density)
      
      if(save.plots.as.pdfs){ dev.off() }
    }
    
    
    #All plots related to survival, in one file
    if(save.plots.as.pdfs){
      pdf(paste(today,'_',filename,'_Plots.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
#       png(paste(today,'_',filename,'_Plots.png',sep=''),width=11.69,height=8.27, units='in',res=120) # A4 landscape
    }
    
    #     options(warn = -1)
    
    #Set layout of graphic output and pre-calculate some graphic parameters
    old.par <- par(mfrow=c(2,3)) #ADAPT IF NECESSARY
    
    for (i in treatments){      
      title<-i #ifelse(nr.groups>1,i,NA)
      df.s<-subset(df.rates,df.rates$treatment==i)      
      colorset<-brewer.pal(nr.treatments+1,'Set1')
      colorset<-paste(colorset,'80',sep='')
      #       df.s.b<-subset(df.s,df.s$groupstructure==groups[1])
      #       barplot(df.s.b$mortality,col=colorset[1],names.arg=df.s.b$treatment,
      #               ylab='Mortality',
      #               ylim=c(0,1))
      #       for (j in 2:nr.groups){ 
      #         df.s.b<-subset(df.s,df.s$groupstructure==groups[j])
      #         barplot(df.s.b$mortality,col=colorset[j],#names.arg=paste(df.s.b$treatment,df.s.b$Block),
      #                 ylim=c(0,1),add=T)
      #       }
      barplot(df.s$mortality,col=colorset,names.arg=df.s$groupstructure,
              ylab='Mortality', xlab=subexperiment.col, main=title,
              ylim=c(0,1))
    }
    par(mfrow=c(1,1))
    mtext(paste(today,filename),3,3)
    
    options(warn = 0)
    par(mfrow=c(1,1))
    
    if(save.plots.as.pdfs){ dev.off() }
    
    
    #     barplot(df.rates$mortality,col=colorset,names.arg=df.rates$treatment,
    #             ylab='Mortality', xlab='Treatment', main='Adult mortality',
    #             ylim=c(0,1))
    
    if(nr.groups>1){
      sciplot::bargraph.CI(x.factor=treatment, response=mortality, group=groupstructure, data=df.rates,           
                           err.width=0.02, ylim=c(0,1),legend=T,
                           ylab='Mortality',xlab='Treatment',main=filename)
    }
    
    if(Bodysize.col!=F){ #i.e. there is Bodysize data
      boxplot(data=df.long, bodysize~treatment*as.factor(died), 
              main=filename, xlab="Treatment", ylab="Bodysize [um]",
              ylim=bodysize.minmax, las=2,
              col=c('grey30',treatments.key$color[2:nrow(treatments.key)]))
      
      #       sciplot::bargraph.CI(x.factor=treatment, response=groupstructure, group=died, data=BODYSIZE,
      #                            ci.fun=c(BODYSIZE$se,-BODYSIZE$se), #UNTESTED
      #                            err.width=0.02, ylim=c(0,1),legend=T,
      #                            ylab='Mortality',xlab='Treatment',main='Pupae')
      
      #Scatterplot of Mortality by Bodysize (UNTESTED)
      #     
      #       plot(data=df.long, survived~bodysize, col=df.long$color.Treatment, pch=df.long$color.Treatment,
      # #            xlim=bodysize.minmax, 
      #            ylim=bodysize.minmax, 
      #            main=title, xlab="Bodysize [um]", ylab="Mortality")
      #       legend("topleft",legend=treatments.key$Treatment,col=treatments.key$color,pch=treatments.key$color)
      #       for (j in unique(df.s$Treatment)){
      #         df.s.t<-subset(df.s,df.s$Treatment==j)
      #         m<-lm(data=df.s.t, Mortality~Bodysize)
      #         #           print(summary(m))
      #         abline(m, col=df.s.t$color.Treatment[1])
      #       }
      
      #       }
    }      
    
    #Plots for time-sampled data (UNTESTED)
    #     if(Timevar!=F){
    #       if(save.plots.as.pdfs){
    #         pdf(paste(today,'_',filename,'_Time.plots.pdf',sep=''),width=6,height=6) 
    #       }
    #       #http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
    #       pd <- position_dodge(0.1) # move them .05 to the left and right
    #       
    #       time.plot.mortality<-ggplot(MORTALITY.time, aes(x=Timevar, y=Mortality, colour=FeedingTreatment, group=FeedingTreatment)) + 
    #         geom_errorbar(aes(ymin=Mortality-se, ymax=Mortality+se), colour="black", width=.1, position=pd) +
    #         geom_line(position=pd) +
    #         geom_point(position=pd, size=4, shape=21, fill="white") + # 21 is filled circle
    #         xlab("Time [days]") +
    #         ylab("Mortality") +
    #         scale_colour_hue(name="Treatment",    # Legend label, use darker colors
    #                          labels=unique(df$FeedingTreatment),
    #                          l=40) +                    # Use darker colors, lightness=40
    #         ggtitle(paste(today,filename)) +
    #         expand_limits(y=0, x=c(0,10)) +                        # Expand y range
    #         scale_y_continuous(c(0,1)) + #     breaks=pretty(survival.minmax,n=10)) +     
    #         scale_x_continuous(breaks=pretty(c(min(df$Day),max(df$Day)+1),n=10)) +  
    #         theme_bw() +
    #         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
    #         theme(legend.justification=c(1,0),
    #               legend.position=c(1,0))               # Position legend in bottom right
    #       
    #       plot(time.plot.mortality)
    #       
    #       if(save.plots.as.pdfs){ dev.off() }
    #     }
  }
  
  ##############
  # Statistics #
  ##############
  if(do.stats){
    #Correlation between Bodysize.col and FFDW (measures for body size)
    if(Bodysize.col!=F){ #i.e. there is Bodysize.col data
      m1<-lm(bodysize~as.factor(survived)*treatment, data=df.long)
      print(anova(m1))
      
      if(randomeffect.col!=F){
        m2<-lm(bodysize~as.factor(survived)*treatment*randomeffect, data=df.long)
        print(anova(m2))
        
        m.lme<-lme(bodysize~as.factor(survived)*treatment, data=df.long, random=~1|randomeffect, na.action=na.omit, method="ML")
        print(anova(m.lme))
        #         print(summary(m.lme))
        print(paste("AIC of mixed-effects model =", AIC(m.lme)))
        #         print(Anova(m.lme))
        #         res<-residuals(m.lme) ; fit<-fitted(m.lme) ; plot(fit,res)
      }
    } else {
      
      if(nr.groups>1){
        m.glm<-glm(died~treatment*groupstructure, family=quasibinomial, data=df.long)
        print(Anova(m.glm))
        #       print(summary(m.glm))
        print(paste("AIC of GLM =", AIC(m.glm))) #not possible for binomial?
        #         res<-residuals(m.glm) ; fit<-fitted(m.glm) ; plot(fit,res)
        #         plot(m.glm)
        
        df.long$interaction<-interaction(df.long$treatment,df.long$groupstructure)
        m.glm<-glm(died~interaction, family=quasibinomial, data=df.long)
        #         print(Anova(m.glm))
        Dunnet <- glht(m.glm, linfct=mcp(interaction="Dunnett"))
        print(summary(Dunnet))
        Tukey <- glht(m.glm, linfct=mcp(interaction="Tukey"))
        print(summary(Tukey))
      } else {
        df.long$treatment<-as.factor(df.long$treatment)
        m.glm<-glm(died~treatment, family=quasibinomial, data=df.long)
        print(Anova(m.glm))
        #       print(summary(m.glm))
        print(paste("AIC of GLM =", AIC(m.glm))) #not possible for binomial?
        #         res<-residuals(m.glm) ; fit<-fitted(m.glm) ; plot(fit,res)
        #         plot(m.glm)
        #         print(Anova(m.glm))
        Dunnet <- glht(m.glm, linfct=mcp(treatment="Dunnett"))
        print(summary(Dunnet))
        Tukey <- glht(m.glm, linfct=mcp(treatment="Tukey"))
        print(summary(Tukey))
      }
      
      if(randomeffect.col!=F){
        m.lme<-lme(died~treatment, data=df.long, random=~1|randomeffect, na.action=na.omit, method="ML")
        print(anova(m.lme))
        #         print(summary(m.lme))
        print(paste("AIC of mixed-effects model =", AIC(m.lme)))
        #         print(Anova(m.lme))
        #         res<-residuals(m.lme) ; fit<-fitted(m.lme) ; plot(fit,res)
      }
    }
  }
  
  #################
  # Final remarks #
  #################
  
  if(return.df){
    print('After running the full script the resulting data frame looks like this. Best to be stored in a df using "output<-"')
    return(df.long)
  }
  
}




