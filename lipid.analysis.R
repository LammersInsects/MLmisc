# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; mark.lammers@vu.nl
# (c) 2018. Released under the terms of the GNU General Public License v3.

# ToDo
# - [VERY IMPORTANT] Sophisticted stats:
#   - Investigate correlation between Tibia length and FFDW
#   - Build a model where all relevant factors are coded as covariate if required
#   - Build GLM(M)
# - [VERY IMPORTANT] Check use of subexperiment.col throughout
# - [important] Add sample size to plots
# - [not important] Add statistical significance to plots
# - [not important] Save plots in /plots/ subfolder, if R can make that when not present yet
# - [important, to be tested ASAP] With 3 treatments the boxplots and plots use different colors! (see L. clavipes plots)
# - Does not accomodate more than one factor as a treatment: What if time*treatment structure? And what if there is more than 2 treatments? Not all works

lipid.analysis<-function(dataframe, # Line 1: Essentials
                         filename='results',
                         subexperiment.col=F, # Line 2: Define columns - required when different from default
                         Treatment.col='Treatment',
                         Timevar=F,  #is assumed to be measured in days
                         Dryweight.col='Dry_weight',
                         FFDW.col='Fat.free_dw',
                         Tibia.col=F,
                         plot.results=T, # Line 3: Tell the function which parts to run
                         save.plots.as.pdfs=F,
                         do.stats=T,
                         random.effect.col=F, #models taking this into account are unfinished
                         control.treatment='.Control', # Line 4: Optional - additional analysis of deviation from controls
                         analysis.of.residuals=F,
                         return.df=F
){
  
  #   filename='debugging'
  if(filename=='debugging'){
    #For debugging (equals to the default values)
    subexperiment.col=F   # Line 2: Define columns - required when different from default
    Treatment.col='Treatment'
    Timevar=F  #is assumed to be measured in days
    Dryweight.col='Dry_weight'
    FFDW.col='Fat.free_dw'
    Tibia.col=F
    plot.results=T   # Line 3: Tell the function which parts to run
    save.plots.as.pdfs=F
    do.stats=T
    random.effect.col=F  #models taking this into account are unfinished
    control.treatment='.control'   # Line 4: Optional - additional analysis of deviation from controls
    analysis.of.residuals=F
    return.df=F
  }
  
  #Load packages
  print('Note: The pipeline will install all dependencies on the first run. This might take a while.')
  options(warn = -1)
  if (!require(car)) {install.packages('car')}
  library(car)
  if (!require(ggplot2)) {install.packages('ggplot2')}
  library(ggplot2)
  if (!require(RColorBrewer)) {install.packages('RColorBrewer')}
  library(RColorBrewer)
  if (!require(nlme)) {install.packages('nlme')}
  library(nlme)
  if(!require(multcomp)){install.packages('multcomp')}
  library('multcomp')
  options(warn = 0)
  
  #Construct data frame and check data type for essential columns
  df<-dataframe
  if(Timevar!=F){ #i.e. there is a column specifying the number of days fed
    print(paste('Time variable called',Timevar,'found'))
    if(is.numeric(df[,Timevar])){
      df$Treatment<-paste(df[,Timevar],df[,Treatment.col],sep='d')
      df$FeedingTreatment<-df[,Treatment.col]
      df$Timevar<-df[,Timevar]
    } else {
      print('WARNING: non-numeric Timevar!'); stop()
    }
  } else {
    df$Treatment<-df[,Treatment.col]
    df$FeedingTreatment<-df[,Treatment.col]
  }
  if(is.numeric(df[,Dryweight.col])){
    df$Dry_weight<-df[,Dryweight.col]
  } else {
    print('WARNING: non-numeric Dryweight.col!'); stop()
  }
  if(is.numeric(df[,FFDW.col])){
    df$Fat.free_dw<-df[,FFDW.col]
  } else {
    print('WARNING: non-numeric FFDW.col!'); stop()
  }
  if(Tibia.col!=F){
    if(is.numeric(df[,Tibia.col])){
      df$Tibia<-df[,Tibia.col]
    } else {
      print('WARNING: non-numeric Tibia column!'); stop()
    }
  }
  if(random.effect.col!=F){
    print(paste('Random effect column called',random.effect.col,'is specified'))
    print('So far only included in the Dunnett posthoc test for deviation of expected lipid levels!')
    df$Random<-df[,random.effect.col]
  }
  df$Lipids<-df[,Dryweight.col]-df[,FFDW.col]
  
  #Check for empty columns and remove these
  #TODO here could use remove.empty.columns()
  df.withemptycol<-ncol(df)
  df<-df[!sapply(df, function(x) all(x == "" || is.na(x)))]
  df.ncol<-ncol(df)
  if(df.withemptycol!=df.ncol){
    print(paste(df.withemptycol-df.ncol,'empty columns found. These are removed from the data set'))
  } else {
    print('No empty columns found')
  }
  
  #Check for rows with NAs and remove these lines
  df.withNA<-df
  df.NA<-subset(df,is.na(rowSums(df[,c(Dryweight.col,FFDW.col)])))
  if(nrow(df.NA)>0){
    print(paste(nrow(df.NA),'rows with NAs found. These are removed from the analysis:'))
    print(df.NA)
  } else {
    print('No NAs found: All rows are complete')
  }
  df.noNA<-subset(df,!is.na(rowSums(df[,c(Dryweight.col,FFDW.col)]))) #removes all records that have NAs in those columns
  df<-df.noNA
  
  #Check Lipid data: exclude negative lipid levels
  impossible<-subset(df,df$Lipids<=0) #Check for impossible data (i.e. negative lipids and remove these)
  if(nrow(impossible)>0){
    print(paste(nrow(impossible),'impossible rows of data found (negative lipids). These are removed before analysis:'))
    print(impossible)
  } else {
    print('All rows have lipid levels above 0 [ug]')
  }
  df<-subset(df,df$Lipids>0)
  
  #Check Lipid data: give a warning for unrealistic lipid levels - anything below 5 and above 30% is suspicious
  df$perc.lipid<-df$Lipids/df[,Dryweight.col]*100
  outliers<-subset(df,df$perc.lipid<5 | df$perc.lipid>30)
  if(nrow(outliers)>0){
    print(paste('WARNING!',nrow(outliers), 'lines with exreme lipid levels (<5% or >30%) found!'))
    #   print('This is all of them:')
    #   print(outliers)
    print('These are NOT removed from the data set')
  } else {
    print('All lipid levels are between 5 and 30%')
  }
  
  ##Check for presence of experiment substructure (e.g. RNAi_Target, Strain, Block, ...)
  if(subexperiment.col==F){
    #         subexperiment.col<-'groupdummyvar'
    #     df[,subexperiment.col]<-'groupdummyvar'
    print('No experiment substructure found')
    groups<-'groupdummyvar'
    nr.groups<-1
    df$groupstructure<-'groupdummyvar'
  } else {
    if(control.treatment!=F){ df[,subexperiment.col]<-gsub(control.treatment,'.Control',df[,subexperiment.col])}
    groups<-sort(unique(df[,subexperiment.col]))
    nr.groups<-length(groups)
    print(paste(nr.groups, 'groups found as substructure in data set:', '(Name of substructure =', subexperiment.col, ')'))
    print(paste(groups))
    df$groupstructure<-df[,subexperiment.col]
  }
  
  #Assign colors to groups
  groups.key<-data.frame(color=1:nr.groups, Group=groups)
  df$color.Groups<-groups.key$col[match(df$groupstructure,groups.key$Group)]
  
  #Treatment overview
  treatments<-sort(unique(df$Treatment))
  nr.treatments<-length(treatments)
  if('Died'==treatments[1]){
    treatments<-treatments[c(2:nr.treatments,1)]
  }
  print(paste(nr.treatments, 'treatments found:'))
  samplesize<-table(df[,c('groupstructure','Treatment')])
  print(samplesize)
  
  #Assign colors to treatments
  treatments.key<-data.frame(color=1:nr.treatments, Treatment=treatments)
  df$color.Treatment<-treatments.key$col[match(df$Treatment,treatments.key$Treatment)]
  
  ##################################
  # Summary statistics of data set #
  ##################################
  
  LIPIDS<-summarySE(df,measurevar = 'Lipids', groupvars = c('groupstructure','Treatment'))
  FFDW<-summarySE(df,measurevar = 'Fat.free_dw', groupvars = c('groupstructure','Treatment'))
  print('Summary of complete data set:')
  print(LIPIDS)
  print(FFDW)
  
  if(Timevar!=F){
    LIPIDS.time<-summarySE(df,measurevar = 'Lipids', groupvars = c('FeedingTreatment','Timevar'))
    FFDW.time<-summarySE(df,measurevar = 'Fat.free_dw', groupvars = c('FeedingTreatment','Timevar'))
  }
  
  #############
  # Plot data #
  #############
  if(plot.results){
    dev.off()
    par(mfrow=c(1,1))
    
    #Density plot of lipids per treatment
    if(save.plots.as.pdfs){
      pdf(paste(today,'_',filename,'_Lipid.density.plot.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
    }
    
    lipids.density<-ggplot(df, aes(Lipids, fill = Treatment)) + geom_density(alpha = 0.2) + #Density plot of Lipids per treatment
      ggtitle(paste(today,filename))
    plot(lipids.density)
    
    if(save.plots.as.pdfs){ dev.off() }
    
    #Tibia length data
    if(Tibia.col==F){
      print('No tibia length data found')
    } else {
      if(save.plots.as.pdfs){
        pdf(paste(today,'_',filename,'_Hist.tibia.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
      }
      # Overlapped histogram - Colored (blue and red)
      #     colorset<-brewer.pal(nr.treatments+1,'Set1')
      #     colorset<-paste(colorset,'80',sep='')
      #     print('Plotting histogram of tibia lengths')
      #     hist(subset(df[,Tibia.col],df$Treatment==treatments[1]),
      #          breaks=pretty(min(df$Tibia, na.rm=T):max(df$Tibia, na.rm=T),n=10), col=colorset[1],
      #          main='Histogram of Tibia Lengths', xlab='Tibia length [um]')
      #     for (i in 2:length(treatments)){
      #       hist(subset(df[,Tibia.col],df$Treatment==treatments[i]),
      #            breaks=pretty(min(df$Tibia, na.rm=T):max(df$Tibia, na.rm=T),n=10),col=colorset[i], add=T)
      #     }
      #     mtext(paste(today,filename),3,3)
      tibia.density<-ggplot(df, aes(Tibia, fill = groupstructure)) + geom_density(alpha = 0.2) +#Density plot of Tibia per treatment
        ggtitle(paste(today,filename))
      plot(tibia.density)
      tibia.density<-ggplot(df, aes(Tibia, fill = Treatment)) + geom_density(alpha = 0.2) +#Density plot of Tibia per treatment
        ggtitle(paste(today,filename))
      plot(tibia.density)
      
      if(save.plots.as.pdfs){ dev.off() }
    }
    
    
    #All plots related to lipid levels, in one file
    if(save.plots.as.pdfs){
      pdf(paste(today,'_',filename,'_Plots.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
    }
    
    options(warn = -1)
    
    for (i in groups){
      #Set layout of graphic output and pre-calculate some graphic parameters
      if(Tibia.col!=F){ #i.e. there is Tibia data
        old.par <- par(mfrow=c(1,4))
        tibia.minmax<-c(min(df$Tibia,na.rm=T),max(df$Tibia,na.rm=T))
      } else {
        old.par <- par(mfrow=c(1,2))
      }
      title<-ifelse(nr.groups>1,i,NA)
      ffdw.minmax<-c(min(df$Fat.free_dw,na.rm=T),max(df$Fat.free_dw,na.rm=T))
      lipids.minmax<-c(min(df$Lipids,na.rm=T),max(df$Lipids,na.rm=T))
      
      # if(samplesize[i,treatments[1]]>1 & samplesize[i,treatments[2]]>1){
      #This ONLY works well with 2 treatments! What if more than 2 treatments? Why is this necessary? For now disabled.
      
      df.s<-subset(df,df$groupstructure==i)
      
      #Boxplot of lipids per treatment
      boxplot(data=df.s, Lipids~Treatment,
              main=title, xlab="Treatment", ylab="Lipids [ug]",
              ylim=c(0,lipids.minmax[2]),
              col=c('grey30',treatments.key$color[2:nrow(treatments.key)]))
      
      #Scatterplot of lipids by FFDW
      plot(data=df.s, Lipids~Fat.free_dw, col=df.s$color.Treatment, pch=df.s$color.Treatment,
           xlim=ffdw.minmax, ylim=c(0,lipids.minmax[2]),
           main=title, xlab="Fat free dry weight [ug]", ylab="Lipids [ug]")
      legend("topleft",legend=treatments.key$Treatment,col=treatments.key$color,pch=treatments.key$color)
      for (j in unique(df.s$Treatment)){
        df.s.t<-subset(df.s,df.s$Treatment==j)
        m<-lm(data=df.s.t, Lipids~Fat.free_dw)
        #         print(summary(m))
        abline(m, col=df.s.t$color.Treatment[1])
      }
      
      if(Tibia.col!=F){ #i.e. there is Tibia data
        
        #Scatterplot of lipids by tibia
        plot(data=df.s, Lipids~Tibia, col=df.s$color.Treatment, pch=df.s$color.Treatment,
             xlim=tibia.minmax, ylim=lipids.minmax,
             main=title, xlab="Tibia [um]", ylab="Lipids [ug]")
        legend("topleft",legend=treatments.key$Treatment,col=treatments.key$color,pch=treatments.key$color)
        for (j in unique(df.s$Treatment)){
          df.s.t<-subset(df.s,df.s$Treatment==j)
          m<-lm(data=df.s.t, Lipids~Tibia)
          #           print(summary(m))
          abline(m, col=df.s.t$color.Treatment[1])
        }
        
        #Scatterplot of FFDW by Tibia
        plot(data=df.s, Fat.free_dw~Tibia,col=df.s$color.Treatment, pch=df.s$color.Treatment,
             xlim=tibia.minmax, ylim=ffdw.minmax,
             main=title, xlab="Tibia [um]", ylab="Fat free dry weight [ug]")
        legend("topleft",legend=treatments.key$Treatment,col=treatments.key$color,pch=treatments.key$color)
        for (j in unique(df.s$Treatment)){
          df.s.t<-subset(df.s,df.s$Treatment==j)
          m<-lm(data=df.s.t, Fat.free_dw~Tibia)
          #           print(summary(m))
          abline(m, col=df.s.t$color.Treatment[1])
        }
      }
      #     }
      par(mfrow=c(1,1))
      mtext(paste(today,filename),3,3)
      
      # Boxplots for both lipids and FFDW side by side
      old.par <- par(mfrow=c(1,2))
      #Boxplot of lipids per treatment
      boxplot(data=df.s, Lipids~Treatment,
              main=title, xlab="Treatment", ylab="Lipids [ug]",
              ylim=lipids.minmax,
              col=c('grey30',treatments.key$color[2:nrow(treatments.key)]))
      #Boxplot of FFDW per treatment
      boxplot(data=df.s, Fat.free_dw~Treatment,
              main=title, xlab="Treatment", ylab="Fat-free dry-weight [ug]",
              ylim=ffdw.minmax,
              col=c('grey30',treatments.key$color[2:nrow(treatments.key)]))
      par(mfrow=c(1,1))
      mtext(paste(today,filename),3,3)
      
    }
    
    options(warn = 0)
    #   par(old.par)
    par(mfrow=c(1,1))
    
    if(save.plots.as.pdfs){ dev.off() }
    
    #   scatterplot(df$Lipids~df$Fat.free_dw | df$Treatment,boxplots='xy') # REMOVED
    
    #Plots for time-sampled data
    if(Timevar!=F){
      if(save.plots.as.pdfs){
        pdf(paste(today,'_',filename,'_Time.plots.pdf',sep=''),width=6,height=6)
      }
      #http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
      pd <- position_dodge(0.1) # move them .05 to the left and right
      # Black error bars - notice the mapping of 'group=supp' -- without it, the error bars won't be dodged!
      #     ggplot(LIPIDS.time, aes(x=Timevar, y=Lipids, colour=FeedingTreatment, group=FeedingTreatment)) +
      #       geom_errorbar(aes(ymin=Lipids-se, ymax=Lipids+se), colour="black", width=.1, position=pd) +
      #       geom_line(position=pd) +
      #       geom_point(position=pd, size=3)
      
      time.plot.lipids<-ggplot(LIPIDS.time, aes(x=Timevar, y=Lipids, colour=FeedingTreatment, group=FeedingTreatment)) +
        geom_errorbar(aes(ymin=Lipids-se, ymax=Lipids+se), colour="black", width=.1, position=pd) +
        geom_line(position=pd) +
        geom_point(position=pd, size=4, shape=21, fill="white") + # 21 is filled circle
        xlab("Time [days]") +
        ylab("Lipids [ug]") +
        scale_colour_hue(name="Treatment",    # Legend label, use darker colors
                         labels=unique(df$FeedingTreatment),
                         l=40) +                    # Use darker colors, lightness=40
        ggtitle(paste(today,filename)) +
        expand_limits(y=0, x=c(0,10)) +                        # Expand y range
        scale_y_continuous(breaks=pretty(lipids.minmax,n=10)) +
        scale_x_continuous(breaks=pretty(c(min(df$Day),max(df$Day)+1),n=10)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
        theme(legend.justification=c(1,0),
              legend.position=c(1,0))               # Position legend in bottom right
      
      time.plot.ffdw<-ggplot(FFDW.time, aes(x=Timevar, y=Fat.free_dw, colour=FeedingTreatment, group=FeedingTreatment)) +
        geom_errorbar(aes(ymin=Fat.free_dw-se, ymax=Fat.free_dw+se), colour="black", width=.1, position=pd) +
        geom_line(position=pd) +
        geom_point(position=pd, size=4, shape=21, fill="white") + # 21 is filled circle
        xlab("Time [days]") +
        ylab("Fat-free dry weight [ug]") +
        scale_colour_hue(name="Treatment",    # Legend label, use darker colors
                         labels=unique(df$FeedingTreatment),
                         l=40) +                    # Use darker colors, lightness=40
        ggtitle(paste(today,filename)) +
        expand_limits(y=0, x=c(0,10)) +                        # Expand x&y range
        scale_y_continuous(breaks=pretty(ffdw.minmax,n=10)) +
        scale_x_continuous(breaks=pretty(c(min(df$Day),max(df$Day)+1),n=10)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
        theme(legend.justification=c(1,0),
              legend.position=c(1,0))               # Position legend in bottom right
      
      plot(time.plot.lipids)
      plot(time.plot.ffdw)
      
      if(save.plots.as.pdfs){ dev.off() }
    }
  }
  
  ##############
  # Statistics #
  ##############
  if(do.stats){
    #Correlation between Tibia and FFDW (measures for body size)
    if(Tibia.col!=F){ #i.e. there is Tibia data
      if(Timevar!=F){ #i.e. there is a Time variable
        m<-lm(data=df, formula=Tibia~df$FeedingTreatment*df$groupstructure*df$Timevar)
      } else {
        if(nr.groups>1){
          m<-lm(data=df, formula=Tibia~df$FeedingTreatment*df$groupstructure)
        } else {
          m<-lm(data=df, formula=Tibia~df$FeedingTreatment)
        }
      }
      print('Testing for differences in body size between treatments')
      print(anova(m))
    }
    
    #Check normal distribution of lipids data
    #Each treatment separately
    #   for(i in treatments){
    #     df.s<-subset(df,df$Treatment==i)
    #     bc<-boxCox(data=df.s,df.s$Lipids~df.s$Tibia)
    #     lambda<-bc$x[which.max(bc$y)]
    #     print(paste('Lambda of Box-Cox transformation is estimated as',lambda))
    #     par(mfrow=c(2,2))
    #     hist(df.s$Lipids,breaks=20, main='Untransformed')
    #     #   hist(log(df$Lipids,base=exp(1)),breaks=20)
    #     hist((df.s$Lipids^lambda-1)/lambda,breaks=20, main='Box-Cox transformed')
    #     qqnorm(df.s$Lipids)
    #     qqnorm((df.s$Lipids^lambda-1)/lambda)
    #     par(mfrow=c(1,1))
    #   }
    #   #All data together
    #   bc<-boxCox(data=df,df$Lipids~df$Treatment+df$Tibia)
    #   lambda<-bc$x[which.max(bc$y)]
    #   print(paste('Lambda of Box-Cox transformation is estimated as',lambda))
    #   par(mfrow=c(2,2))
    #   hist(df$Lipids,breaks=20, main='Untransformed')
    #   #   hist(log(df$Lipids,base=exp(1)),breaks=20)
    #   # hist(1/df$Lipids,breaks=20)
    #   hist((df$Lipids^lambda-1)/lambda,breaks=20, main='Box-Cox transformed')
    #   qqnorm(df$Lipids)
    #   qqnorm((df$Lipids^lambda-1)/lambda)
    #   par(mfrow=c(1,1))
    #   #All data together for Tibia
    #   bc<-boxCox(data=df,df$Tibia~df$Treatment)
    #   lambda<-bc$x[which.max(bc$y)]
    #   print(paste('Lambda of Box-Cox transformation is estimated as',lambda))
    #   par(mfrow=c(2,2))
    #   hist(df$Tibia,breaks=20, main='Untransformed')
    #   # hist(log(df$Tibia,base=exp(1)),breaks=20)
    #   # hist(1/df$Lipids,breaks=20)
    #   hist((df$Tibia^lambda-1)/lambda,breaks=20, main='Box-Cox transformed')
    #   qqnorm(df$Tibia)
    #   qqnorm((df$Tibia^lambda-1)/lambda)
    #   par(mfrow=c(1,1))
    
    #Check Homogeneity of variances
    levene.lipids<-leveneTest(y=df$Lipids, group=as.factor(df$Treatment))
    print(levene.lipids)
    
    #ANCOVAs for lipids and FFDW
    if(Timevar!=F){ #i.e. there is a Time variable
      if(nr.groups>1){ #i.e. there is groups structure
        if(Tibia.col!=F){ #i.e. there is Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Tibia+df$groupstructure*df$FeedingTreatment*df$Timevar)
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~df$Tibia+df$groupstructure*df$FeedingTreatment*df$Timevar)
          print('Using Tibia as covariate')
        } else { #i.e. there is no Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Fat.free_dw+df$groupstructure*df$FeedingTreatment*df$Timevar) #Use FFDW as covariate
          print('Using FFDW as a covariate for lipid levels')
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~df$groupstructure*df$FeedingTreatment*df$Timevar) #Collapses to an ANOVA
          print('Remember: without Tibia data the ANCOVA for FFDW does not have a covariate (=ANOVA)!')
        }
        
      } else { #i.e. there is no groups structure
        if(Tibia.col!=F){ #i.e. there is Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Tibia+df$FeedingTreatment*df$Timevar)
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~+df$Tibia+df$FeedingTreatment*df$Timevar)
          print('Using Tibia as covariate')
        } else { #i.e. there is no Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Fat.free_dw+df$FeedingTreatment*df$Timevar)
          print('Using FFDW as a covariate for lipid level because no Tibia data is found. Validity should be checked!')
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~df$FeedingTreatment*df$Timevar) #Collapses to an ANOVA
          print('Remember: without Tibia data the ANCOVA for FFDW does not have a covariate (=ANOVA)!')
        }
      }
    } else { #i.e. there is no Time variable
      if(nr.groups>1){ #i.e. there is groups structure
        if(Tibia.col!=F){ #i.e. there is Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Tibia+df$groupstructure*df$Treatment)
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~df$Tibia+df$groupstructure*df$Treatment)
          print('Using Tibia as covariate')
        } else { #i.e. there is no Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Fat.free_dw+df$groupstructure*df$Treatment) #Use FFDW as covariate
          print('Using FFDW as a covariate for lipid levels')
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~df$groupstructure*df$Treatment) #Collapses to an ANOVA
          print('Remember: without Tibia data the ANCOVA for FFDW does not have a covariate (=ANOVA)!')
        }
        
      } else { #i.e. there is no groups structure
        if(Tibia.col!=F){ #i.e. there is Tibia data
          #The covariate goes first (and there is no interaction)! If you do not do this in order, you will get different results.
          #See https://www.datanovia.com/en/lessons/ancova-in-r/
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Tibia+df$Treatment)
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~+df$Tibia+df$Treatment)
          print('Using Tibia as covariate')
        } else { #i.e. there is no Tibia data
          ancova.lipids<-aov(data=df, formula=df$Lipids~df$Fat.free_dw+df$Treatment)
          print('Using FFDW as a covariate for lipid level because no Tibia data is found. Validity should be checked!')
          ancova.ffdw<-aov(data=df, formula=df$Fat.free_dw~df$Treatment) #Collapses to an ANOVA
          print('Remember: without Tibia data the ANCOVA for FFDW does not have a covariate (=ANOVA)!')
        }
      }
    }
    print('ANCOVA for lipids:')
    print(summary(ancova.lipids))
    print('ANCOVA for FFDW:')
    print(summary(ancova.ffdw))
    #See also https://towardsdatascience.com/anovas-three-types-of-estimating-sums-of-squares-don-t-make-the-wrong-choice-91107c77a27a
    #for type 1, 2 and 3 sum of squares. Here we use type 1 because we first want to correct for tibia-effects
    
    #Check residuals of the ANCOVAs
    par(mfrow=c(2,2))
    df$res.lipids<-resid(ancova.lipids)
    plot(df$Lipids,df$res.lipids,col=df$color.Groups,main='Colored by group')
    plot(df$Lipids,df$res.lipids,col=df$color.Treatment,main='Colored by treatment')
    
    df$res.ffdw<-resid(ancova.ffdw)
    plot(df$Fat.free_dw,df$res.ffdw,col=df$color.Groups,main='Colored by group')
    plot(df$Fat.free_dw,df$res.ffdw,col=df$color.Treatment,main='Colored by treatment')
    par(mfrow=c(1,1))
    mtext(paste(today,filename),3,3)
    
    #GLM
    #ToDo
    #   glm.lipids<-glm(data=df, formula=df$Lipids~df$Treatment+df$Tibia)
    #   print(summary(glm.lipids))
    #   plot(df$Lipids,resid(glm.lipids),col=df$color.Treatment)
    #   
    #   #Mixed-effects model
    #   m4Tot <- lme(data=df, Lipids ~ Treatment + Tibia, random =~1|Number, 
    #                na.action = na.omit, method= "ML")
    #   anova(m4Tot)
    #   m4tot <- glht(m4Tot, mcp(Treat ="Tukey")) ## Or Dunnett when the two control treatments are combined?
    #   summary(m4tot)
    
    
    
    # Calculate residuals of the data from a model fitted to the controls 
    if(analysis.of.residuals){
      if(control.treatment==F){
        print('ERROR: control.treatment must be specified for analysis of residuals'); stop()
      } else {
        if(Tibia.col==F){
          print('ERROR: analysis.of.residuals is hard-coded to use Tibia data as covariate'); stop()
        } else {
          
          if(save.plots.as.pdfs){
            pdf(paste(today,'_',filename,'_Residuals.Plots.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
          }
          
          old.par <- par(mfrow=c(2,2))
          #Lipids
          for (i in treatments){ #do for all treatments
            df.s<-subset(df,df$Treatment==i)
            df.m<-subset(df.s,df.s$groupstructure=='.Control')
            m.lipids<-lm(data=df.m,Lipids~Tibia)
            df.s$expected<-predict.lm(m.lipids,df.s)
            plot((df.s$Lipids-df.s$expected)~df.s$Tibia,col=df.s$color.Groups,pch=df.s$color.Groups,
                 main=paste('Deviation from expectation at\n',i,'for Lipids'))
            legend("topleft",legend=groups.key$Group,fill=groups.key$color)
            for (j in unique(df.s$groupstructure)){
              df.s.t<-subset(df.s,df.s$groupstructure==j)
              m<-lm((df.s.t$Lipids-df.s.t$expected)~df.s.t$Tibia)
              #               print(paste('Using lm to test for difference from control in lipids at',i,'for',j))
              #               print(summary(m))
              abline(m, col=df.s.t$color.Groups[1])
            }
          }
          # plot(df$Lipids~df$Tibia,type='n')
          # legend("topright",legend=ktreatments.key$Treatment,fill=treatments.key$color)
          #Fat free dry weight
          for (i in unique(df$Treatment)){ #do for all treatments
            df.s<-subset(df,df$Treatment==i)
            df.m<-subset(df.s,df.s$groupstructure=='.Control')
            m.ffdw<-lm(data=df.m, Fat.free_dw~Tibia)
            df.s$expected<-predict.lm(m.ffdw,df.s)
            plot((df.s$Fat.free_dw-df.s$expected)~df.s$Tibia,col=df.s$color.Groups,pch=df.s$color.Groups,
                 main=paste('Deviation from expectation at\n',i,'for Fat-free dry-weight'))
            legend("topleft",legend=groups.key$Group,fill=groups.key$color)
            for (j in unique(df.s$groupstructure)){
              df.s.t<-subset(df.s,df.s$groupstructure==j)
              m<-lm((df.s.t$Fat.free_dw-df.s.t$expected)~df.s.t$Tibia)
              #               print(paste('Using lm to test for difference from control in FFDW at',i,'for',j))
              #         print(summary(m))
              abline(m, col=df.s.t$color.Groups[1])
            }
          }
          
          par(old.par)
          mtext(paste(today,filename),3,3)
          
          if(save.plots.as.pdfs){ dev.off() }
          
          #Plot boxplot of deviation from expectation (=residual from model predicted on control)
          if(save.plots.as.pdfs){
            pdf(paste(today,'_',filename,'_Boxplot.Residuals.pdf',sep=''),width=11.69,height=8.27) # A4 landscape
          }
          
          groups.key$col<-c('grey30',2,3,3,3,'yellow')   #For now hard-coded for RNAi data analysis
          old.par <- par(mfrow=c(2,5))
          #Lipids
          for (i in unique(df$Treatment)){ #do for all treatments
            df.s<-subset(df,df$Treatment==i)
            df.m<-subset(df.s,df.s$groupstructure=='.Control')
            print('Linear model with Dunnett posthoc')
            m.lipids<-lm(data=df.m,Lipids~Tibia)
            df.m.summ<-summarySE(df.m,measurevar = 'Lipids', groupvars = 'Treatment')
            
            df.s$expected<-predict.lm(m.lipids,df.s)
            boxplot((df.s$Lipids-df.s$expected)~df.s$groupstructure,col=groups.key$col,
                    main=ifelse(i=='Em','Emergence','Fed 1 wk'),
                    ylab=paste('Deviation from expectated lipids content [ug]'))
            abline(0,0,lty=2,col='grey40')
            print(paste('Mean lipid levels of controls for treatment <',i,'> is',df.m.summ$Lipids,'with SE',df.m.summ$se))
            print('Model fit for Lipids predicted by Tibia')
            print(summary(m.lipids))
            qqnorm(resid(m.lipids))
            
            df.s$groupstructure<-as.factor(df.s$groupstructure)
            m<-lm(data=df.s,(Lipids-expected)~groupstructure)
            print('Model fit for deviation of expected Lipids as predicted by Tibia')
            print(summary(m))
            print(anova(m))
            qqnorm(resid(m))
            Dunnett <- glht(m, linfct=mcp(groupstructure="Dunnett"))
            print('Dunnett posthoc for deviation of expected Lipids as predicted by Tibia')
            print(summary(Dunnett))
            #             
            #             if(random.effect.col!=F){
            #               print('Mixed-effects model with Dunnett posthoc')
            #               m2 <- lme(data=df.s, (Lipids-expected)~groupstructure, random =~1|Random, na.action = na.omit, method= "ML")
            #               print(summary(m2))
            #               print(anova(m2))
            #               qqnorm(resid(m2))
            #               Dunnett.random <- glht(m2, mcp(groupstructure="Dunnett")) ## Or Dunnett when the two control treatments are combined?
            #               print(summary(Dunnett.random))
            #               
            #               print('Model with random effect as factor with Dunnett posthoc')
            #               df.s$interactionoffactors<-interaction(df.s$groupstructure,df.s$Random)
            #               m3<-lm(data=df.s,(Lipids-expected)~interactionoffactors)
            #               print(summary(m3))
            #               print(anova(m3))
            #               qqnorm(resid(m3))
            #               Dunnett.random <- glht(m3, mcp(interactionoffactors="Dunnett")) ## Or Dunnett when the two control treatments are combined?
            #               print(summary(Dunnett.random))
            #               
            #             }
          }
          #Fat free dry weight
          #           for (i in unique(df$Treatment)){ #do for all treatments
          #             df.s<-subset(df,df$Treatment==i)
          #             df.m<-subset(df.s,df.s$groupstructure=='.Control')
          #             m.ffdw<-lm(data=df.m, Fat.free_dw~Tibia)
          #             df.m.summ<-summarySE(df.m,measurevar = 'Fat.free_dw', groupvars = 'Treatment')
          #             print(paste('Mean FFDW of controls for treatment <',i,'> is',df.m.summ$Fat.free_dw,'with SE',df.m.summ$se))
          #             df.s$expected<-predict.lm(m.ffdw,df.s)
          #             boxplot((df.s$Fat.free_dw-df.s$expected)~df.s$groupstructure,col=groups.key$col,
          #                     main=paste('Deviation from expectation at\n',i,'for Fat-free dry-weight'))
          #             m<-lm(data=df.s,(Lipids-expected)~groupstructure)
          #             print(summary(m))
          #           }
          par(old.par)
          
          if(save.plots.as.pdfs){ dev.off() }
          
        }        
      }
    }
    
    #Here analyse the lipid levels in percentages, a method for dealing with high heteroscedasticity
    #TODO
    
  }
  if(return.df){
    print('After running the full script the resulting data frame looks like this. Best to be stored in a df using "output<-"')
    return(df)
  }
}
