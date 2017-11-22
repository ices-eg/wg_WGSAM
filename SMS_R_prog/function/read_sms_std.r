#Function to read sms.std
Read.SMS.std<-function(dir=data.path) {
  a<-read.table(file=file.path(dir,"sms.std"),skip=1) 
  tmp<-data.frame(index=a$V1,name=a$V2, value=a$V3, CV.round=round(a$V4/a$V3*100,2), std=a$V4)
  tmp$new<-!duplicated(tmp$name)
  for (i in (1:dim(tmp)[[1]])) if (tmp[i,'new']) tmp[i,'no']<-1 else tmp[i,'no']<-tmp[i-1,'no']+1
  tmp$new<-NULL
  b<-read.table(file.path(dir,"par_exp.out"),comment.char = "#",header=T) 
  b$new=!duplicated(b$par)
  for (i in (1:dim(b)[[1]])) if (b[i,'new']) b[i,'no']<-1 else b[i,'no']<-b[i-1,'no']+1
  b$new<-NULL
  
  tmp<-merge(x=tmp,y=b,by.x=c("name",'no'),by.y=c("par","no"),all.x=T)
  
  if (SMS.control@no.species==1)  tmp<-subset(tmp,select=c(-prey,-predator))
  #if (SMS.control@no.areas==1)  tmp<-subset(tmp,select=c(-area))
  
  return(tmp)
}

#Read.SMS.std()
