
cleanup()
incl.sp<-c(16:18,21:24)                      # species number to be included. Numbers or "all"
#incl.sp<-"all"
#incl.sp<-c(3,4)

first.pch<-0    # first pch symbol
first.color<-1   # first color

#dirs<-  c("NS_paper_uniform","NS_paper_uniform_confined","NS_paper_size","NS_paper_size_fixed","NS_paper_size_mesh")       # directories with output to compare
#labels<-c("1) Uniform","2) Uniform, confined","3) log-normal","4) log-normal, fixed parameters","5) log-normal, mesh selection")  # labes for each scenario (directory with data)
 
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 


Init.function() # get SMS.contol object  including sp.names
for (dir in dirs) {  
  cat(dir,'\n')
  a<-Read.SMS.std(dir=file.path(root,dir))
  a$dir<-dir
  a$label<-labels[ which(dirs==dir)]
 if (dir==dirs[1]) aa<-a else aa<-rbind(aa,a)
}


bb<-subset(aa,species %in% incl.sp)
years<-c(1975,1995,2005,2015:2019)
bb<-subset(bb,year %in% years)

###################
# SSB  or avg_F or recruitment
 
#bb<-subset(bb,name=='hist_SSB')
bb<-subset(bb,name=='avg_F')
#bb<-subset(bb,name=='rec_sd')

bb$Species<-sp.names[bb$species]
#bb$fam<-'multi'
#bb[grep('Single',bb$label),"fam"]<-'single'

round(tapply(bb$std/bb$value*100,list(bb$label,bb$year,bb$Species),sum),2)

round(cbind( tapply(bb$std/bb$value*100,list(bb$label,bb$Species),mean),tapply(bb$std/bb$value*100,list(bb$label),mean)),2)

round(tapply(bb$std,list(bb$label,bb$Species),mean),2)

round(tapply(bb$std/bb$value*100,list(bb$fam),mean),2)



###################
# prey Vulnerability

bb<-subset(aa,name=='vulnera')
bb$Prey<-sp.names[bb$prey]
bb$Predator<-sp.names[bb$predator]

tapply(bb$CV.round,list(bb$Predator,bb$Prey,bb$label),sum)
################################

###################
#Preadtion mortality
 
bb<-subset(aa,name=='M2_sd0')
bb<-subset(aa,name=='M2_sd1')
#bb<-subset(aa,name=='M2_sd2')

bb$Species<-sp.names[bb$species]
bb$fam<-'all'
bb[grep('Uniform',bb$label),"fam"]<-'uniform'
bb[grep('log',bb$label),"fam"]<-'log-normal'

round(tapply(bb$std/bb$value*100,list(bb$Species,bb$year,bb$label),sum),0)
round(tapply(bb$std/bb$value*100,list(bb$label,bb$year,bb$Species),sum),1)


round(cbind( tapply(bb$std/bb$value*100,list(bb$label,bb$Species),mean),tapply(bb$std/bb$value*100,list(bb$label),mean)),2)


round(tapply(bb$std/bb$value*100,list(bb$fam),mean),2)
