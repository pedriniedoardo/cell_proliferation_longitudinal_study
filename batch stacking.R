#move to the other directory and pick up the files
DirPhoto<-"C:/Users/edoardo/Desktop/seattle experiments/cell proliferation/june 2016 stiching/photo"
lis<-dir(DirPhoto,pattern = "stich")[-1]
input<-"C:/Users/edoardo/Desktop/prova stack"
output<-"C:/Users/edoardo/Desktop/batch stack"
for(l in lis){
  print(l)
  origin<-paste0(DirPhoto,"/",l)
  i=which(lis==l)
  setwd(origin)
  file.copy(from = dir(origin,pattern = ".tif"),to = input ,overwrite = TRUE,recursive = TRUE)
  setwd(input)
  a<-dir()
  b<-t(as.data.frame(strsplit(a,split = ".",fixed = T)))
  NewName<-paste0(b[,1],"_",i,".",b[,2])
  #rename process
  file.rename(from = a,to = NewName)
  #copy the new files in the new directory
  file.copy(from = dir(input),to = output ,overwrite = TRUE,recursive = FALSE)
  #remove the original files
  file.remove(dir(input))
}
