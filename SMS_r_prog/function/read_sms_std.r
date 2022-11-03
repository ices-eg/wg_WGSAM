#Function to read sms.std
Read.SMS.std<-function(dir=data.path,stdFile="sms.std",gfile='gradient.dat',excludeNotUsed=TRUE,rec_scale=TRUE,remove=NULL) {
  a<-read.table(file=file.path(dir,stdFile),skip=1) 
  a<-data.frame(index=a$V1,name=a$V2, value=a$V3,  std=a$V4)
  a$name<-as.character(a$name)
 
  b<-read.table(file.path(dir,"par_exp.out"),comment.char = "#",header=T) 
  b<-merge(x=a,y=b,by.x=c('index'),by.y=c("parNo"),all.x=T)
  
  if (!is.null(remove)) for (x in remove) b<-b[-grep(x,b$name),]
  
 # sort(unique(b$par))
  
  # prepare for merge with gradients where available (some parameters (log_F_a_ini) is not in the gradient file?)
  b$parG<-paste0(b$par,"(",b$idx1,")")
  
  if (rec_scale) {
    r<-read.table(file.path(dir,"rec_scale.out"),comment.char = "#",header=T) 
    b<-merge(x=b,y=r,by='species',all.x=TRUE)
    b[b$name=='log_rec','value']<- b[b$name=='log_rec','value']+ b[b$name=='log_rec','rec_scale']
    b[b$name=='log_rec_older','value']<- b[b$name=='log_rec_older','value']+ b[b$name=='log_rec_older','rec_scale']
  
    b$rec_scale<-NULL
  }
  
  b$CV.round=round(b$std/b$value*100,2)
  
  f<-b$par=="log_rec"
  b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],",",b[f,"idx2"],")")
  
  f<-b$par=="log_rec_older"
  b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],",",b[f,"idx2"],")")

  f<-b$par=="F_y_ini"
  b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],",",b[f,"idx2"],")")
  
  # fejl
  f<-b$par=="F_q_ini"
  b[f,"parG"]<-paste0(b[f,'par'],"[",b[f,"idx1"],"](",b[f,"idx2"],")")

  
  f<-b$par=="qq_ini"
  head(b[f,])
  b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],",",b[f,"idx2"],")")
  head(b[f,])
   
  f<-b$par=="qq_s2_ini"
  b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],",",b[f,"idx2"],")")
  
  #f<-b$par=="SSB_R_alfa"
  #head(b[f,])
  #b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],")")
  
  #f<-b$par=="SSB_R_beta"
  #head(b[f,])
  #b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],")")
 

  f<-b$par=="catch_s2_ini"
  b[f,"parG"]<-paste0(b[f,'par'],"[",b[f,"idx1"],"](",b[f,"idx2"],",",b[f,"idx3"],")")

  #f<-b$par=="SSB_R_s2_ini"
  #head(b[f,])
  #b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],")")
 
  #f<-b$par=="vulnera"
  #head(b[f,])
  #b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],")")
  
  #f<-b$par=="init_stl_other_suit_slope"
  #head(b[f,])
  #b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],")")

  #f<-b$par=="init_season_overlap"
  #head(b[f,])
  #b[f,"parG"]<-paste0(b[f,'par'],"(",b[f,"idx1"],")")
  
  g<-Read.gradient.dat(gfile=gfile)
  g$no<-NULL

  bg<-merge(x=b,y=g,by.y=c('ParName'),by.x=c('parG'),all.x=TRUE)
  

  bg$check<-(bg$Value-bg$value)**2
  subset(bg,check>1E-3)
  bg<-subset(bg,select=c(-idx1,-idx2,-idx3))
  
  mis_gra<-subset(bg,is.na(Gradient))
  unique(mis_gra$par)

  bg$Value<-bg$check<-NULL
  #bg$parG<<-NULL
  if (SMS.control@no.species==1)  bg<-subset(bg,select=c(-prey,-predator))
  bg<-bg[order(bg$index),]
  #if (SMS.control@no.areas==1)  tmp<-subset(tmp,select=c(-area))
  
  if (!('used' %in% colnames(bg))) bg$used<-'used'
  if (excludeNotUsed ) bg<-subset(bg,used=='used')
  return(bg)
}

#Read.SMS.std()
