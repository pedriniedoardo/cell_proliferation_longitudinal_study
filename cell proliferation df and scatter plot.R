setwd("C:/Users/edoardo/Dropbox/Seattle/seattle experiments/cell proliferation/june 2016 stiching/R")
remove(list = ls())
#define the files with the data
path<-paste0(getwd(),"/data")
files<-dir(path)

#######################################################
#check that all the files have the same order of labels
  library(purrr)
  #load all the label from the files
  a<-data.frame(map(files,function(x){
    df<-read.csv(paste0(path,"/",x),header = T,sep = "\t",stringsAsFactors = F)
    #df<-as.data.frame(lapply(FUN = as.character,X = df),stringsAsFactors = FALSE)
    #extrapolate only the info of interest
    df$Label
  }),stringsAsFactors = F)
  #split the df by row in order to check the labels at the same position in each file
  b<-split(a,f=1:241)
  #identify the unique label by row
  c<-map(b,function(x){
    unique(unlist(x))
  })
  #check if there is some row with more than one label
  sum(lengths(c)!=1)
#all the file have the same order for the labels
#################################################################

#################################################################
#load all the data in one single df
  df<-data.frame(map(files,function(x){
    df<-read.csv(paste0(path,"/",x),header = T,sep = "\t",stringsAsFactors = F)
    #df<-as.data.frame(lapply(FUN = as.character,X = df),stringsAsFactors = FALSE)
    #extrapolate only the info of interest
    df$X.Area
  }),stringsAsFactors = F)
  
  #add the column with the name of the well
  TotDf<-data.frame(unlist(c),df,stringsAsFactors = F)
  
  #place a more appropriate label name (column)
  colname<-paste0(t(as.data.frame(strsplit(files,fixed = T,split = " "),stringsAsFactors = FALSE))[,2],"_",
                  t(as.data.frame(strsplit(files,fixed = T,split = " "),stringsAsFactors = FALSE))[,1])
  colnames(TotDf)<-c("well ID",colname)
  
  #make the wells as number in order to be reordered
  labels<-as.numeric(t(as.data.frame(strsplit(TotDf[,1],fixed = T,split = ".")))[,1])
  TotDf$`well ID`<-labels
  
  #reorder the df
  TotDf_ord<-TotDf[order(TotDf$`well ID`),]
  
  #add the factor variable to define the group of the well
  fac<-c("ctrl",rep("10",80),rep("04",80),rep("01",80))
  TotDf_ord_un<-TotDf_ord[-1]
  TotDf_ord_un$well_ID<-TotDf_ord$`well ID`
  TotDf_ord_un$factor<-fac
  #the unormalized df
  write.csv(TotDf_ord_un,paste0(getwd(),"/","area_not_normalized2.csv"),row.names = FALSE)
  
#####################################################################

#####################################################################  
#analyze the data
  #look at the distribution of the area
  library(ggplot2)
  library(tidyr)
  df2<-gather(data = TotDf_ord_un,key = day,value = area,-c(factor,well_ID))
  
  #replace the variable name with a number to make the scatter plot
  days<-rep(2:(length(files)+1),each=241)
  df2$day<-days
  #add the log2 of the area
  df2$log2<-log2(df2$area)
  
  #look at the data
  library(gridExtra)
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  #raw data
  stot<-ggplot(df2,aes(x=day,y=area,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette)+ylim(c(0,100))+labs(x="#days",y="% area cells")
  s1<-ggplot(df2[df2$factor=="01",],aes(x=day,y=area,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[1])+ylim(c(0,100))+labs(x="#days",y="% area cells")
  s4<-ggplot(df2[df2$factor=="04",],aes(x=day,y=area,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[2])+ylim(c(0,100))+labs(x="#days",y="% area cells")
  s10<-ggplot(df2[df2$factor=="10",],aes(x=day,y=area,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[3])+ylim(c(0,100))+labs(x="#days",y="% area cells")
  grid.arrange(stot,s1,s4,s10,ncol=2)
  #log transformed data
  stot_log<-ggplot(df2,aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette)+ylim(c(0,7))+labs(x="#days",y="log2 %area cells")
  s1_log<-ggplot(df2[df2$factor=="01",],aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[1])+ylim(c(0,7))+labs(x="#days",y="log2 % area cells")
  s4_log<-ggplot(df2[df2$factor=="04",],aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[2])+ylim(c(0,7))+labs(x="#days",y="log2 % area cells")
  s10_log<-ggplot(df2[df2$factor=="10",],aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[3])+ylim(c(0,7))+labs(x="#days",y="log2 % area cells")
  grid.arrange(stot_log,s1_log,s4_log,s10_log,ncol=2)
  
  #identify the "04" wells slow
  #df2[df2$factor=="04"&df2$day==15&df2$log2<4,]
  #identify a fast one factor "04"
  #x<-df2[df2$factor=="04"&df2$log2>4&df2$log2<4.2,]
  #x[order(x$day),]
  
  #plot after filtration
  df4<-df2
  #remove the first three days
  df4<-df4[df4$day>4,]
  #remove the data above 5.7
  df5<-df4[df4$log2<=5.7,]
  #remove the factor control
  df5<-df5[df5$factor!="ctrl",]
  #replot the data after filtering
  stot_slope<-ggplot(df5,aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette)+ylim(c(1,6))+labs(x="#days",y="log2 % area cells")+xlim(c(5,21))
  s1_slope<-ggplot(df5[df5$factor=="01",],aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[1])+ylim(c(1,6))+labs(x="#days",y="log2 % area cells")+xlim(c(5,21))
  s4_slope<-ggplot(df5[df5$factor=="04",],aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[2])+ylim(c(1,6))+labs(x="#days",y="log2 % area cells")+xlim(c(5,21))
  s10_slope<-ggplot(df5[df5$factor=="10",],aes(x=day,y=log2,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[3])+ylim(c(1,6))+labs(x="#days",y="log2 % area cells")+xlim(c(5,21))
  grid.arrange(stot_slope,s1_slope,s4_slope,s10_slope,ncol=2)
  
  #save the log in the matrix form
  TotDf_log<-log2(TotDf_ord_un[,1:(ncol(TotDf_ord_un)-2)])
  #do not consider the first three columns
  TotDf_log<-TotDf_log[4:ncol(TotDf_log)]
  #remove the log2 data above 5.7 
  TotDf_log[TotDf_log>5.7]<-NA
  #define the slope for each row
  slope<-unname(apply(TotDf_log,MARGIN = 1,FUN = function(x) lm(unlist(x)~c(1:ncol(TotDf_log)))[[1]][2]))
  df_slope<-cbind.data.frame("slope"=slope,"well_ID"=TotDf_ord_un$well_ID,"factor"=TotDf_ord_un$factor)
  #make ANOVA over factor
  anova<-aov(slope~factor,data = df_slope[2:nrow(df_slope),])
  summary(anova)
  #Tukey
  TukeyHSD(anova)
  #basic table of the results
  table<-as.data.frame(as.list(aggregate(slope~factor,data = df_slope[2:nrow(df_slope),],FUN =function(x) c("slope"=mean(x),"sd"=sd(x)))))
  colnames(table)<-c("factor","slope","sd")
  #summary table of the parameters, define the df for each group
  v<-as.character(table$factor)
  table$df<-sapply(v,FUN = function(x){
    length(unique(df_slope$well_ID[df_slope$factor==x]))-1
  })
  #summary table of the parameters, define the critical value of the t statistic for each df (95% two tails)
  table$crit_value<-abs(qt(0.025,table$df))
  #summary table of the parameters, define the se
  table$se<-table$sd/sqrt(table$df+1)
  #summary table of the parameters, define the 95% CI
  table$lower<-table$slope-(table$crit_value*table$se)
  table$upper<-table$slope+(table$crit_value*table$se)
  write.csv(table,paste0(getwd(),"/","table_mean_CI.csv"),row.names = FALSE)
  
  #produce the boxplot and arrange the images
  box<-ggplot(df_slope[2:nrow(df_slope),],aes(factor,slope))+geom_boxplot()
  grid.arrange(stot_slope,s1_slope,s4_slope,s10_slope,box,layout_matrix=rbind(c(1,2,5),
                                                                              c(3,4,5)))
