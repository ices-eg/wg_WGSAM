exOneSp<-function(my.sp.name='Mackerel',my.sp=20,read.fleet=T,doRun=F) {

 SMS.dat<-read.FLSMS.control()

 ############
  my.sp.VPA<-my.sp-first.VPA+1
  #  runs are made in a separate dirictory

  my.sp.dir<-paste('SS',my.sp.name,sep='_')
    scenario.dir<-file.path(root,my.sp.dir)
  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
  dir.create(scenario.dir,showWarnings = FALSE)

 
 
 old<-SMS.dat
 ny<-new("FLSMS.control")


ny@first.year<-old@first.year
ny@first.year.model<-old@first.year.model
ny@last.year<-old@last.year
ny@last.year.model<-old@last.year.model
ny@last.season<-old@last.season
ny@last.season.last.year<-old@last.season.last.year
ny@species.names<-old@species.names[my.sp]

ny@first.age<-old@first.age
ny@rec.season<-old@rec.season
ny@species.info[]<- old@species.info[my.sp,]

ny@max.age.all<-ny@species.info[,'last-age']


nyAges<-ny@max.age.all-ny@first.age+1
#cat(nyAges,'\n')

ny@beta.cor[]<-old@beta.cor[my.sp.VPA]
ny@SSB.R.year.first[]<-old@SSB.R.year.first[my.sp.VPA]
ny@SSB.R.year.last[]<-old@SSB.R.year.last[my.sp.VPA]
ny@obj.func.weight[]<-old@obj.func.weight[my.sp,]


ny@discard<-old@discard[[my.sp.VPA]]
ny@combined.catches<-old@combined.catches[my.sp.VPA]
ny@seasonal.catch.s2<-old@seasonal.catch.s2[my.sp.VPA]

ny@catch.s2.group<-list(old@catch.s2.group[[my.sp.VPA]])
ny@catch.season.age<-list(old@catch.season.age[[my.sp.VPA]])
ny@avg.F.ages[]<-old@avg.F.ages[my.sp.VPA,]
ny@min.catch[]<-old@min.catch[my.sp.VPA]
ny@catch.sep.year<-list(old@catch.sep.year[[my.sp.VPA]])

ny@catch.spline.year<- list(old@catch.spline.year[[my.sp.VPA]])
ny@zero.catch.year.season<-old@zero.catch.year.season
ny@zero.catch.season.age<-old@zero.catch.season.age
ny@fix.F.factor<-old@fix.F.factor[my.sp.VPA]
ny@est.calc.sigma<-old@est.calc.sigma
ny@read.HCR<-old@read.HCR

 write.FLSMS.control(ny,file=file.path(scenario.dir,'SMS.dat'),path=scenario.dir, writeSpNames=T)
 
 if (read.fleet) {
  SMS.indices<<-SMS2FLIndices(SMS.dat)
  summary(SMS.indices)
 }

f2 <- function(x) {
  #print(x@name)
  if (substr(x@name,1,3)=='Whg')  return(x) 
 }

used<-FLIndices();fl<-0
for (i in (1:length(SMS.indices))) {
   a<-SMS.indices[[i]]
   if (substr(a@name,1,3)== my.sp.name) {
     fl<-fl+1
     used[[fl]]<-a 
   }
}
    

FLIndices2SMS(out.path=scenario.dir,indices=used,control=ny)



 SMS.control<-read.FLSMS.control()

la<-SMS.control@max.age.all
fa<-SMS.control@first.age
years<-c(1,1)
years[1]<-SMS.control@first.year
years[2]<-SMS.control@last.year
ny<-years[2]-years[1]+1
npr<-sum(SMS.control@species.info[,'predator']>=1)
nsp<-SMS.control@no.species
nq<-SMS.control@last.season
noAreas<-SMS.control@no.areas

#############  catch data




tr_sp<-function(inp.file='canum.in',path=NULL) {
  vari<-scan(file.path(data.path,inp.file),comment.char='#')
  vari<-head(vari,-1)
  if (inp.file=='west.in')vari<-vari[((first.VPA-1)*noAreas*ny*(la-fa+1)*nq+1):length(vari)] 
  b<-expand.grid(sub_area=1:noAreas,species.n=first.VPA:nsp,year=years[1]:years[2],quarter=1:nq,age=fa:la)
  b<-b[order(b$sub_area,b$species.n,b$year,b$quarter,b$age),]
  
  b$vari<-vari
  b<-droplevels(subset(b,species.n==my.sp))
  b<-tapply(b$vari,list(b$year,b$quarter,b$age),sum)
  round(ftable(b),0)
  cat('#\n',file=file.path(path,inp.file),append=F)
  y<-0
  for (year in (years[1]:years[2])) {
     y<-y+1
    write.table(b[y,,1:nyAges],row.names = F,col.names = F,file=file.path(path,inp.file),append=T)
  }
  cat(' -999  # check\n',file=file.path(path,inp.file),append=T)
}
 
tr_sp(inp.file='canum.in',path=file.path(root,my.sp.dir))
tr_sp(inp.file='weca.in',path=file.path(root,my.sp.dir))
tr_sp(inp.file='natmor.in',path=file.path(root,my.sp.dir))
tr_sp(inp.file='natmor1.in',path=file.path(root,my.sp.dir))
tr_sp(inp.file='propmat.in',path=file.path(root,my.sp.dir))
tr_sp(inp.file='proportion_landed.in',path=file.path(root,my.sp.dir))
tr_sp(inp.file='west.in',path=file.path(root,my.sp.dir))


tr_sp2<-function(inp.file='zero_catch_season_ages.in',path=NULL) {
  vari<-scan(file.path(data.path,inp.file),comment.char='#')
  vari<-head(vari,-1)
  b<-expand.grid(sub_area=1:noAreas,species.n=first.VPA:nsp,quarter=1:nq,age=fa:la)
  b<-b[order(b$sub_area,b$species.n,b$quarter,b$age),]
  b$vari<-vari
  b<-droplevels(subset(b,species.n==my.sp))
  b<-tapply(b$vari,list(b$quarter,b$age),sum)
  write.table(b,row.names = F,col.names = F,file=file.path(path,inp.file),append=F)
  cat(' -999  # check\n',file=file.path(path,inp.file),append=T)
}
 
tr_sp2(inp.file='zero_catch_season_ages.in',path=file.path(root,my.sp.dir))


tr_sp3<-function(inp.file='zero_catch_year_season.in',path=NULL) {
  vari<-scan(file.path(data.path,inp.file),comment.char='#')
  vari<-head(vari,-1)
  b<-expand.grid(sub_area=1:noAreas,species.n=first.VPA:nsp,year=years[1]:years[2],quarter=1:nq)
  b<-b[order(b$sub_area,b$species.n,b$year,b$quarter),]
  b$vari<-vari
  b<-droplevels(subset(b,species.n==my.sp))
  b<-tapply(b$vari,list(b$year,b$quarter),sum)
  write.table(b,row.names = F,col.names = F,file=file.path(path,inp.file),append=F)
  cat(' -999  # check\n',file=file.path(path,inp.file),append=T)
}
 
tr_sp3(inp.file='zero_catch_year_season.in',path=file.path(root,my.sp.dir))

tr_sp4<-function(inp.file='recruitment_years.in',path=NULL) {
  vari<-scan(file.path(data.path,inp.file),comment.char='#')
  vari<-head(vari,-1)
  b<-expand.grid(sub_area=1:noAreas,species.n=first.VPA:nsp,year=years[1]:years[2])
  b<-b[order(b$sub_area,b$species.n,b$year),]
  b$vari<-vari
  b<-droplevels(subset(b,species.n==my.sp))
  b<-tapply(b$vari,list(b$year),sum)
  write.table(b,row.names = F,col.names = F,file=file.path(path,inp.file),append=F)
  cat(' -999  # check\n',file=file.path(path,inp.file),append=T)
}
 
tr_sp4(inp.file='recruitment_years.in',path=file.path(root,my.sp.dir))



a<-readLines(con = file.path(data.path,'F_q_ini.in'))
a<-a[grep( toupper(my.sp.name),a)]

writeLines(a,con= file.path(root,my.sp.dir,'F_q_ini.in'))
cat(' -999  # check\n',file=file.path(root,my.sp.dir,'F_q_ini.in'),append=T)
 
  SMS.files.single<-c("area_names.in","just_one.in","reference_points.in","cp.bat",
                      "proportion_M_and_F_before_spawning.in",'sms.exe')

  for (from.file in SMS.files.single) {
    file.copy(file.path(data.path,from.file), file.path(scenario.dir,from.file), overwrite = TRUE)
  }
 
 sms.do<-file.path(scenario.dir,'do_run.bat')
 
 if (doRun) {
    cat(paste('cd ', scenario.dir,'\n'),file=sms.do)
    cat(paste(file.path(scenario.dir,"sms.exe")," -nox \n",sep=""),file=sms.do,append=TRUE)
    
     command<-paste('"',sms.do,'"',sep='')
    system(command,show.output.on.console =T)
    
   
 }
}


if (FALSE) { 
  
  exOneSp(my.sp.name='Cod',my.sp=18,read.fleet=T)
  exOneSp(my.sp.name='Whg',my.sp=19,read.fleet=F)
  exOneSp(my.sp.name='Had',my.sp=20,read.fleet=F)
  exOneSp(my.sp.name='Pok',my.sp=21,read.fleet=F)
  exOneSp(my.sp.name='Her',my.sp=22,read.fleet=F)
  exOneSp(my.sp.name='Nsa',my.sp=23,read.fleet=F)
  exOneSp(my.sp.name='Ssa',my.sp=24,read.fleet=F)
  exOneSp(my.sp.name='Nop',my.sp=25,read.fleet=F)
  exOneSp(my.sp.name='Spr',my.sp=26,read.fleet=F)
  exOneSp(my.sp.name='Ple',my.sp=27,read.fleet=F)
  exOneSp(my.sp.name='Sol',my.sp=28,read.fleet=F)
} 

exOneSp(my.sp.name='Nop',my.sp=24,read.fleet=T)

#  scenario.dir<-file.path(root,'SS_COD')
  