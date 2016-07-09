setwd("C:/Users/edoardo/Desktop/seattle experiments/cell proliferation/june 2016 stiching/R")
remove(list = ls())
#read the dfs
path<-paste0(getwd(),"/data")
files<-dir(path)
for(i in files){
  print(i)
  df<-read.csv(paste0(path,"/",i),header = T,sep = "\t")
  #df<-as.data.frame(lapply(FUN = as.character,X = df),stringsAsFactors = FALSE)
  #extrapolate only the info of interest
  Data<-cbind.data.frame(df$Label,df$X.Area)
  assign(i,Data)
}
TotDf<-{}
indexis<-{}
#setup the total df
for(i in 1:length(files)){
  print(i)
  df<-get(files[i])
  df$`df$Label`<-as.character(df$`df$Label`)
  if(i==1){
    TotDf<-df
  }else{
    #verify that the order of the names is the same
    index<-match(TotDf[,1],df[,1])
    TotDf<-cbind.data.frame(TotDf,df[index,2])
    indexis<-cbind(indexis,index)
    }
}
#test if all the indexis are the same for all the files
sum(apply(X = indexis,MARGIN = 1,FUN =function(x) length(unique(x))>1))

##############################################################################################################################
#changing of the area measured at the corner of the plate (beads don't grow)
plot(2:(ncol(TotDf[1,][-1])+1),unlist(TotDf[1,][-1]),xlab = "#day",ylab = "raw area",main = "area of the corner well (beads)")
####
#let's check if the problem is evaporation 
#(this well whas meant to be use as a normalizer of the area to correct the different acquisition differences)


##############################################################################################################################
#place a more appropriate label name (column)
colname<-paste0(t(as.data.frame(strsplit(files,fixed = T,split = " "),stringsAsFactors = FALSE))[,2],"_",
                t(as.data.frame(strsplit(files,fixed = T,split = " "),stringsAsFactors = FALSE))[,1])
colnames(TotDf)<-c("well ID",colname)
#split and rearrange the labels of the df (sample/row)
labels<-as.numeric(t(as.data.frame(strsplit(TotDf[,1],fixed = T,split = ".")))[,1])
TotDf$`well ID`<-labels
#reorder the df per labels
TotDf_ord<-TotDf[order(TotDf$`well ID`),]
#add the factors
#fac<-c("ctrl",rep("10",80),rep("04",80),rep("01",80))
#TotDf_ord$factor<-fac
#####################################################################################################################
#I notced that the control change in the time, 
#I might be interested in normalize all the area base on the first time
#after seeing the behaviour of the corner I think it might provide a bias in the analysis
n<-unlist(TotDf[1,2:ncol(TotDf)]/TotDf_ord[1,2])
TotDf_ord_n<-TotDf_ord[-1]
TotDf_ord_n<-TotDf_ord_n/n
#re-add the labels
TotDf_ord_n$well_ID<-TotDf_ord$`well ID`
#add the factors
fac<-c("ctrl",rep("10",80),rep("04",80),rep("01",80))
TotDf_ord_n$factor<-fac
#the normalized df
write.csv(TotDf_ord_n,paste0(getwd(),"/","area_normalized.csv"),row.names = FALSE)
####################################################################################################################
#fix the label also in the unnormalized df
TotDf_ord_un<-TotDf_ord[-1]
TotDf_ord_un$well_ID<-TotDf_ord$`well ID`
TotDf_ord_un$factor<-fac
#the unormalized df
write.csv(TotDf_ord_un,paste0(getwd(),"/","area_not_normalized.csv"),row.names = FALSE)


#look at the distribution of the area
library(ggplot2)
library(reshape)
df<-melt(TotDf_ord_n,id.vars = c("factor","well_ID"))
df2<-melt(TotDf_ord_un,id.vars = c("factor","well_ID"))
#replace the variable name with a number to make the scatter plot
days<-rep(2:(length(files)+1),each=241)
df$variable<-days
df2$variable<-days
st<-ggplot(df,aes(x=variable,y=value,color=as.factor(factor),group=well_ID))+geom_point()+geom_line()
st2<-ggplot(df2,aes(x=variable,y=value,color=as.factor(factor),group=well_ID))+geom_point()+geom_line()

library(gridExtra)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

stot<-ggplot(df2,aes(x=variable,y=value,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette)+ylim(c(0,100))+labs(x="#days",y="% area cells")
s1<-ggplot(df2[df$factor=="01",],aes(x=variable,y=value,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[1])+ylim(c(0,100))+labs(x="#days",y="% area cells")
s4<-ggplot(df2[df$factor=="04",],aes(x=variable,y=value,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[2])+ylim(c(0,100))+labs(x="#days",y="% area cells")
s10<-ggplot(df2[df$factor=="10",],aes(x=variable,y=value,group=well_ID,colour=factor))+geom_point()+geom_line()+scale_color_manual(values = cbbPalette[3])+ylim(c(0,100))+labs(x="#days",y="% area cells")
grid.arrange(stot,s1,s4,s10,ncol=2)
