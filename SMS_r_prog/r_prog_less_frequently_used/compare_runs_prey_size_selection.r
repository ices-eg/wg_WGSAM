
first.x.on.plot<-1
last.x.on.plot<-8

incl.sp<-c(17,18,20)                      # species number to be included.
incl.sp<-c(1)                      # species number to be included.

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only

by.den<-0.01

if (F) {
  dirs<-  c('Baltic_logn_logn','Baltic_beta_logn' )       # directories with output to compare
  labels<-c('lognorm size','beta size')  # labes for each scenario (directory with data)
  
  dirs<-  c('Baltic_logn_logn','Baltic_logn_diri','Baltic_beta_logn','Baltic_beta_diri')       # directories with output to compare
  labels<-c('lognorm size, lognorm','lognorm size, Dirichlet','Beta size, lognorm','Beta size, Dirichlet' )  # labes for each scenario (directory with data)
  
  dirs<-  c('NS_4_7_Mac_beta5_diri','NS_4_7_Mac_beta5_logn',"NS_4_7_MAC_logn_diri_limit" ,"NS_4_7_MAC_logn_logn",'NS_4_7_Mac_beta6_logn','NS_4_7_Mac_beta6_diri' )       # directories with output to compare
  labels<-c('beta size, Dirichlet','beta size, lognormal','lognorm size, Dirichlet','lognorm size, lognorm','beta unimodal, logn','beta unimodal, diri' )  # labes for each scenario (directory with data)
   
  dirs<-  c("NS_4_7_MAC_no_adj", "NS_4_7_MAC", "NS_4_7_MAC_free" , "NS_4_7_MAC_100" ,"NS_4_7_MAC_test"  )       # directories with output to compare
  labels<-c('a) no adjustment', 'b) adjusted input','bb) estimate L50 and SR', 'd) L50 fixed at 100 mm','e) test')  # labes for each scenario (directory with data)
  
     
  dirs<-  c("NS_4_7_MAC_81dist_size","NS_4_7_MAC_81dist_size_fixed","NS_4_7_MAC_81dist_size_mesh","NS_4_7_MAC_91dist_size_mesh")       # directories with output to compare
  labels<-c("log-normal","log-normal, fixed parameters","log-normal, mesh","log-normal, 91 mesh")  # labes for each scenario (directory with data)
  
     
  dirs<-  c("NS_4_7_MAC_81dist","NS_4_7_MAC_81dist_size","NS_4_7_MAC_81dist_size_fixed","NS_4_7_MAC_81dist_size_mesh")       # directories with output to compare
  labels<-c("a) uniform size selction","b) size selection, free parameters","c) size selction, fixed parameters","d) b) and mesh selction for ALK")  # labes for each scenario (directory with data)
  
     
  dirs<-  c("NS_paper_size","NS_paper_size_fixed","NS_paper_size_mesh")       # directories with output to compare
  labels<-c("log-normal","log-normal, fixed parameters","log-normal, mesh selection")  # labes for each scenario (directory with data)
  
}

######################
 
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 


Init.function() # get SMS.contol object  including sp.names
a<-0
for (dir in dirs) {
 
 file<-file.path(root,dir,'size_pref.out')
 size<-read.table(file,comment.char = "#",header=T)
 a<-a+1
 size<-data.frame(size,dirs=labels[a])
 if (dir==dirs[1]) {sizes<-size; npr<-dim(size)[1];} else sizes<-rbind(sizes,size)
 if (dir==dirs[1]) {
   file<-file.path(root,dir,'min_max_size_pref.out')
   mm<-scan(file)
   min.size<-matrix(data=mm,ncol=nsp-first.VPA+1,nrow=npr,byrow=TRUE)
   dimnames(min.size)[2]<-list(sp.names[first.VPA:nsp])
   dimnames(min.size)[1]<-list(sp.names[1:npr])
   max.size<-matrix(data=mm[(1+length(min.size)):(2*length(min.size))],ncol=nsp-first.VPA+1,nrow=npr,byrow=TRUE)
   dimnames(max.size)<-dimnames(min.size)
   min.size<-apply(min.size,1,min)
   max.size<-apply(max.size,1,max)
   range.size<-min.size # copy structure
 }
}


sizes<-subset(sizes,size.model %in% c(1,3,5,6) & species.n %in% incl.sp)

for (a in (1:dim(sizes)[1])) {
  ratio<-sizes[a,"size.ratio"]
  vars<-sizes[a,"size.var"]
 # var.right<-sizes[a,"size.var.right"]
  model<- sizes[a,"size.model"]
  species<-sizes[a,"species.n"]
  dirss<-sizes[a,"dirs"]
  
  if (model==1) {
    xx<-seq(first.x.on.plot,last.x.on.plot,by=by.den)
    len=length(xx)
    b<-data.frame(x=xx,y=exp(-(xx-ratio)^2/(2.0*vars)),Species=rep(sp.names[species],len),dirs=rep(dirss,len))
    b<-subset(b,x>=log(min.size[species]) & x<=log(max.size[species]))
  }
   else if (model==3) {   # Gamma
    xx<-seq(first.x.on.plot,last.x.on.plot,by=by.den)
    len=length(xx)
    b<-data.frame(x=xx,y=dgamma(xx,shape=ratio,scale=vars),Species=rep(sp.names[species],len),dirs=rep(dirss,len))
    b<-subset(b,x>=log(min.size[species]) & x<=log(max.size[species]))
  }

  else if (model==5 | model==6) {
     min.s=log(min.size[species]);
     max.s=log(max.size[species]);
     # adjust to avoid outer bounds in beta distribution [0;1] 
    range.size[species]= 1.001*(max.s-min.s);
    min.s= 0.999*min.s;
  # range.size[species]=max.s-min.s;
 
    xx<-seq(0,1,by=by.den/10)
    len=length(xx)
    yy<-dbeta(xx,ratio,vars)
    xx<-min.s+range.size[species]*xx
    b<-data.frame(x=xx,y=yy,Species=rep(sp.names[species],len),dirs=rep(dirss,len))
    b<-subset(b,x>=min.s & x<=max.s)
  }  
  if (a==1) ab<-rbind(b) else ab<-rbind(ab,b)
}

 
 print(xyplot(y~x|Species*dirs,data=subset(ab,y<2.5),type='l',lwd=2,col=1,transparent=F,
  layout=c(2,3),
       xlab='log(predator weight / prey weight)',ylab='Size preference'))
 ####

nox<-2; noy<-3;
#cleanup()

newplot(dev="screen",nox,noy)
by(ab,list(ab$Species),function(x) {
#plot(x$x,y$y,
a<-subset(x,dirs==dirs[1])
plot(a$x,a$y,type='l',col=1,xlab="log(predator weight / prey weight)",ylab="size preference",
     xlim=c(first.x.on.plot,last.x.on.plot),ylim=c(0,1),main=a[1,'Species'] )
for (i in (2:length(dirs))) {
   a<-subset(x,dirs==labels[i])
  lines(a$x,a$y,type='l',col=i,lty=i,lwd=2)
}  

})



#for the paper;
#cleanup()
trellis.device(device = "windows", 
               color = F, width=9, height=17,pointsize = 12,
               new = TRUE, retain = FALSE)
               
 print(   xyplot(y~x|Species,group=dirs, data=ab,type='a',lwd=2,lty=c(9,1,2),
    layout=c(1,3),  xlab='log(predator weight / prey weight)',ylab='Size preference',
     strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.7),
     scales = list(x = list( cex=1), y= list(cex=1),alternating = 1)))
    
  