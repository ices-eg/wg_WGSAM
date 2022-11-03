# the directory with our input data on list format
#list.data.path<-file.path(root,"data_northSea")
#list.data.path<-file.path(root,"data_baltic")

## See the end of this file for more options


##  input files  ###############

# VPA_bi01.in: species, year, quarter, age,  WSEA, PROPMAT ,M, M1, PROP_M2
# VPA_bi02.in: species ,year, quarter, age,  WSEA, N
# VPA_ca01.in: species, year, quarter, age,  CATCHN,WCATCH,PROP_CAT

# mean_l.dat:  species, year, quarter, age ,SMS_area, age, mean_l

# consum.dat:    spno species year quarter ageSMS_area CONSUM
# stockdist.dat:

# ALK_stom_list.dat  optional wih addition of stomMark variable
# ALK_all_list.dat   optional wih addition of stomMark variable
# stomcon_list.dat   optional wih addition of stomMark variable

#########################################################################################

  #trans.stomach        # transform stomach data from list input format to SMS format
  #relevant if trans.stom = TRUE
  #stom.first           # stomcon for inserted (first length class) values
  #stom.mid             # stomcon for inserted (mid length class) values
  #stom.last            # stomcon for inserted (last length class) values
  #stom.exp            # stomcon for inserted ("expanded" length class) values
  #stom.min.abs         # absolut minimum value for stom con for inserted values
  #delete.tails         # Detelete tails (first and last "observations")
  #inserted.haul.no.propor         # proportion of number hauls in case of invented values (first, mid and last values)
  #max.other.food           # delete strata with more than max.other.food% oher food, use 100 for no deletion
  #formatted.output              # make a nice table output (takes some minutes) or fast and unformatted

SMS.data.transform<-function( list.data.path=" ",stomMark=NULL,trans.bio=FALSE, trans.catch=FALSE,
    trans.meanL=FALSE,  trans.meanL.from.weight=FALSE, trans.stockDist=FALSE,
    trans.stomach=FALSE, trans.stomach.number=FALSE, use.stom.meanl.ALK=FALSE,
    trans.ALK.stomach=FALSE, trans.ALK.all=FALSE, trans.other=FALSE, trans.Consum=FALSE,
    stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.exp=1E-06, stom.min.abs=1E-08, delete.tails=TRUE,
    inserted.haul.no.propor=1.0,      
    max.other.food=100,
    formatted.output=TRUE,selected.years=NULL,year.q=NULL,min.pred.length=0) {

if (FALSE) {  #test
  list.data.path=file.path(finalExchangeDir);
  trans.bio=T; trans.catch=T;
  trans.meanL=T;  trans.meanL.from.weight=FALSE;trans.stockDist=FALSE; trans.stomach.number<-FALSE
  trans.stomach=T; use.stom.meanl.ALK=FALSE; trans.ALK.stomach=T; trans.other=T; trans.Consum=F; trans.ALK.all=T;
  stom.first=1E-06; stom.mid=1E-06; stom.last=1E-06; stom.min.abs=1E-06; delete.tails=TRUE;
  inserted.haul.no.propor=1.0; 
  formatted.output=T; selected.years=selected.years; year.q=year.q;min.pred.length=0; max.other.food=100
  stom.first=1E-06; stom.mid=1E-06; stom.last=1E-06; stom.exp=1E-06; stom.min.abs=1E-08; delete.tails=TRUE;
  inserted.haul.no.propor=1.0;
  list.data.path<-file.path(finalExchangeDir)
}  
  
  
if (max.other.food!=100) stop("Sorry, a value different from 100 cannot be used for max.other.food")

if (is.null(selected.years) & is.null(year.q)) stop("you have to specify selected.years or year.q.")
if (!is.null(selected.years) & !is.null(year.q)) stop("you cannot specify both selected.years and year.q")

species.no.prey<-data.frame(species=code.name,prey.no=0:(length(code.name)-1) )
species.no.pred<-data.frame(species=code.name,pred.no=0:(length(code.name)-1) )

maximum.age.all.species<-SMS.control@max.age.all 
years<-c(1,1)
years[1]<-SMS.control@first.year
years[2]<-SMS.control@last.year
npr<-sum(SMS.control@species.info[,'predator'])
no_areas<- SMS.control@no.areas
area.names<-Read.area.names()
#SMS_areas<-as.character(1:no_areas)
SMS_areas<-(1:no_areas)

checksum<-function(file='a'){
  cat("-999 # Checksum",file=file,append=TRUE)
}

select.ALK<-function(a){   #function for selection of ALK data
 if (!('ALK.MeanL' %in% names(a))) a$ALK.MeanL<-0
 if (is.null(year.q)) {
   b<-subset(a, (year %in% selected.years)   & (prey %in% code.name.prey),
           select=c(SMS_area,year,quarter,prey, prey.age,prey.size.class,prey.size,ALK.MeanL,ALK))
 } else {          
    a$year.qq<-paste(a$year,'q',a$quarter,sep='')
    b<-subset(a, (year.qq %in% year.q)   & (prey %in% code.name.prey),
           select=c(SMS_area,year,quarter,prey, prey.age,prey.size.class,prey.size,ALK.MeanL,ALK))
 }           
 merge(b,species.no.prey,by.x="prey",by.y="species")                           
}


select.ALK.all<-function(a){   #function for selection of ALK data
 if (is.null(year.q)) {
   b<-subset(a,  (prey %in% code.name),
           select=c(SMS_area,year,quarter,prey, prey.age,prey.size.class,prey.size,ALK.MeanL,ALK))
 } else {          
    a$year.qq<-paste(a$year,'q',a$quarter,sep='')
    b<-subset(a, (prey %in% code.name),
           select=c(SMS_area,year,quarter,prey, prey.age,prey.size.class,prey.size,ALK.MeanL,ALK))
 }           
 merge(b,species.no.prey,by.x="prey",by.y="species")                           
}

#######

select.stom<-function(a){   #function for selection of stomach data
 if (is.null(year.q)) {
   if (!trans.stomach.number)  { a$used.prey.number<-0; a$calc.prey.number<-0 }
   b<-subset(a,(year %in% selected.years)  & (pred %in% code.name.pred) 
              & (haul.no>=min.stomach.sampled.in.stratum) & (pred.mean.length>=min.pred.length)
              & ((type %in% c('first','last','exp') & !delete.tails) | type %in% c('obs','mid')),
        select=c(SMS_area,year,quarter,pred,pred.size,pred.size.class,pred.mean.length,
                 prey,prey.size,prey.size.class,prey.mean.length,type,
                 stomcon,mean.weight,haul.no,haul.no.scaled, calc.prey.number,used.prey.number))
  } else {
    a$year.qq<-paste(a$year,'q',a$quarter,sep='')
    b<-subset(a,(year.qq %in% year.q)  & (pred %in% code.name.pred) 
              & (haul.no>=min.stomach.sampled.in.stratum) 
              & ((type %in% c('first','last','exp') & !delete.tails) | type %in% c('obs','mid')),
        select=c(SMS_area,year,quarter,pred,pred.size,pred.size.class,pred.mean.length,
                 prey,prey.size,prey.size.class,prey.mean.length,type,
                 stomcon,mean.weight,haul.no,haul.no.scaled,calc.prey.number,used.prey.number))
  } 
  
  #cat('\ntest1\n');print(head(b))

  non.prey<-subset(b,!(prey %in% code.name.prey),
                 select=c(SMS_area,year,quarter,pred,pred.size,prey,stomcon))

 if (dim(non.prey)[1]>0) {
   print(paste(unique(non.prey$prey)," to other food"))
   b<-subset(b,(prey %in% code.name.prey))
   non.prey[,'prey']<-'OTH'
   oth<-tapply(non.prey$stomcon,list(non.prey$SMS_area,non.prey$year,non.prey$quarter,non.prey$pred,non.prey$pred.size,non.prey$prey),sum)
   a<-subset(arr2df(oth),stomobs>0)
   names(a)<-list('SMS_area','year','quarter','pred','pred.size','prey','stomcon.new')
   c<-merge(b,a,all.x=TRUE)
   c[!is.na(c$stomcon.new),'stomcon']<-c[!is.na(c$stomcon.new),'stomcon']+c[!is.na(c$stomcon.new),'stomcon.new']
   b<-subset(c,select=-stomcon.new)
 
 }   

 b<-merge(b,species.no.pred,by.x="pred",by.y="species")    
 b<-merge(b,species.no.prey,by.x="prey",by.y="species")                           

 if (max.other.food<100) {
    
    # calculate relative stomach contents, 
    a<-by(b,list(a$SMS_area,b$year,b$quarter,b$pred,b$pred.size.class),function(x) sum(x$stomcon))
    a<-subset(arr2df(a),stomobs>0)
    names(a)<-list('SMS_area','year','quarter','pred','pred.size.class','sum')
    
    b<-merge(b,a)
    b$stomcon<-b$stomcon/b$sum;
  
    
    # extract strata with more than x% other food
    a<-subset(b,prey.no==0 & stomcon>(max.other.food/100))
    a<-data.frame(year=a$year,quarter=a$quarter,pred.no=a$pred.no,pred.size.class=a$pred.size.class,del=TRUE)
    
    b<-merge(b,a,all=TRUE)
    cat("\nDeleted strata:\n")
    print(a)    
    b<-b[is.na(b$del),]
 }
 #cat('\ntest2\n');print(head(b))
 b
}


#######


##########################################################################

trans.4M.SMS.MEANL.from.Weight<-function(){
cat("trans.4M.SMS.MEANL.from.Weight\n")
file<-file.path(list.data.path,'VPA_bi01.in') 
s<-read.csv(file,header=TRUE)
s<-subset(s,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name),
   select = c(year,quarter,species,age,WSEA))

file<-file.path(list.data.path,'length_weight_relations.dat') 
lw<-read.table(file,header=TRUE)
lw<-subset(lw, (species %in% code.name), select = c(quarter,species,A,B))

s<- merge(s,lw)                           
s$mean_l<-(s$WSEA/s$A)^(1/s$B)

transf<-function(s,file.name,dig) {
  CC<<-tapply(s$mean_l,list(s$year,s$quarter,s$species,s$age),sum)
  out<-file.path(data.path,file.name)
  unlink(out)

  for (sp in code.name[2:(nsp+1)]) {
      print(sp)
      out1<<-CC[,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<--1
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
      }
  }
  checksum(file=out)
}

transf(s,"lsea.in",0)
} # end function definition
if (trans.meanL.from.weight) trans.4M.SMS.MEANL.from.Weight()
#################################################################


trans.4M.SMS.MEANL<-function(){
cat("trans.4M.SMS.MEANL\n")
file<-file.path(list.data.path,'mean_l.dat') 
s<-read.csv(file)
if (is.null(s[1,'SMS_area'])) s$SMS_area<-1

s<-subset(s,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name),
       select = c(SMS_area,year,quarter,species,age,mean_l))
transf<-function(s,file.name,dig) {

  CC<<-tapply(s$mean_l,list(s$SMS_area,s$year,s$quarter,s$species,s$age),sum)
  #print(dim(CC))
  #print(dimnames(CC))
  out<-file.path(data.path,file.name)
  unlink(out)
  
  for (d in (SMS_areas)) {
    cat(paste("# Area",d,area.names[d],"\n"),file=out,append=TRUE)
    
    for (sp in code.name[2:(nsp+1)]) {
      cat('Meanl ',sp,'\n')
      out1<<-CC[d,,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<--1
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
      }
    }
  }
  checksum(file=out)
}

transf(s,"lsea.in",0)
} # end function definition
if (trans.meanL) trans.4M.SMS.MEANL()



trans.stock.distribution<-function(){
cat("trans.stock.distribution\n")
file<-file.path(list.data.path,'stockdist.dat')
s<-read.table(file,header=TRUE)
s<-subset(s,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name),
       select = c(SMS_area,year,quarter,species,age,proportion))

transf<-function(s,file.name,dig) {

  CC<<-tapply(s$proportion,list(s$SMS_area,s$year,s$quarter,s$species,s$age),sum)
  out<-file.path(data.path,file.name)
  unlink(out)
  for (d in (SMS_areas)) {
    cat(paste("# Area",d,area.names[d],"\n"),file=out,append=TRUE)
    for (sp in code.name[2:(nsp+1)]) {
      print(sp)
      out1<<-CC[d,,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<- 0
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
      }
    }
  }
  checksum(file=out)
}

transf(s,"Stock_distribution.in",3)
} # end function definition
if (trans.stockDist) trans.stock.distribution()

#################################################################
trans.4M.SMS.CONSUM<-function(){
cat("trans.4M.SMS.CONSUM\n")
file<-file.path(list.data.path,'Consum.dat') 
s<-read.csv(file)
s<-subset(s,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name.pred),
   select = c(SMS_area,year,quarter,species,age,CONSUM))

transf<-function(s,file.name,dig) {
  CC<<-tapply(s$CONSUM,list(s$SMS_area,s$year,s$quarter,s$species,s$age),sum)
  out<-file.path(data.path,file.name)
  unlink(out)
 for (d in (SMS_areas)) {
  cat(paste("# Area",d,area.names[d],"\n"),file=out,append=TRUE)
  for (sp in code.name.pred) {
      print(sp)
      out1<<-CC[d,,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<- 0
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
      }
    }
 }
  checksum(file=out)
 }

transf(s,"consum.in",3)
} # end function definition
#################################################################
if (trans.Consum) trans.4M.SMS.CONSUM()

#################################################################
#transf Other predator N and WSEA data 

trans.4M.SMS.OTHER<-function(){
cat("trans.4M.SMS.OTHER\n")

file<-file.path(list.data.path,'VPA_Bi01.IN') 
s<-read.csv(file,header=TRUE)
s<-subset(s,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name),
 select = c(year,quarter,species,age,WSEA))

file<-file.path(list.data.path,'VPA_Bi02.IN') 
s2<-read.csv(file,header=TRUE)

s2<-subset(s2,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name),
         select = c(year,quarter,species,age,WSEA))

s3<-rbind(s,s2)

transf<-function(s,file.name,dig) {
  # s<-s3; file.name="west.in";dig=6
  SS<<-s
  CC<<-tapply(s$WSEA,list(s$year,s$quarter,s$species,s$age),sum)
  out<-file.path(data.path,file.name)
  unlink(out)
   for (sp in code.name[2:(nsp+1)]) {
      cat(sp,'\n')
      out1<<-CC[,,sp,]
      for (y in years[1]:years[2]){
        cat(y,'\n')
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<--1
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
      }
   }
  checksum(file=out)
}
if (first.VPA>1)transf(s3,"west.in",6)
WW<<-CC

file<-file.path(list.data.path,'VPA_Bi02.IN') 
s2<-read.csv(file,header=TRUE)
s2<-subset(s2,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name),
        select = c(year,quarter,species,age,N))

fill<-data.frame(   year=rep(years[1],maximum.age.all.species+1),
                    quarter=rep(1,maximum.age.all.species+1),
                    species=rep('OTH',maximum.age.all.species+1),
                    age=seq(0,maximum.age.all.species),
                    N=rep(1,maximum.age.all.species+1))
s2<-rbind(s2,fill)
 
transf<-function(s,file.name,dig) {
  CC<<-tapply(s$N,list(s$year,s$quarter,s$species,s$age),sum)
  out<-file.path(data.path,file.name)
  unlink(out)
  for (sp in code.name[2:(first.VPA)]) {
      print(sp)
      out1<<-CC[,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<--1
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
      }
  }
  checksum(file=out)
 }
if (first.VPA>1)transf(s2,"other_pred_N.in",0)
} # end function definition
#################################################################
if (trans.other) trans.4M.SMS.OTHER()

#################################################################
#transf data from file with VPA biological data input

trans.4M.SMS.bio<-function(){
cat("trans.4M.SMS.bio\n")
file<-file.path(list.data.path,'VPA_Bi01.IN')
s<-read.csv(file,header=TRUE)
s<-subset(s,age<=maximum.age.all.species & year>=years[1] & year<=years[2] & (species %in% code.name))

#s[is.na(s$WSEA),'WSEA']<- -99
#CC<<-tapply(s$WSEA,list(s$year,s$quarter,s$species,s$age),sum)
#summary(s)

attach(s)

transf<-function(item,file.name,dig) {
  CC<<-tapply(item,list(year,quarter,species,age),sum)
  out<-file.path(data.path,file.name)
  unlink(out)

  for (sp in code.name[(first.VPA+1):(nsp+1)]) {
  #cat(sp,'\n')
      out1<<-CC[,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<--1
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
      }
  }
  checksum(file=out)
}

if (first.VPA==1) transf(WSEA,"west.in",6)

cat('doing M\n')
transf(M,"natmor.in",4)
cat('doing M1\n')
transf(M1,"natmor1.in",4)
cat('doing Propmat\n')
transf(PROPMAT,"propmat.in",4)
cat('doing Prop N\n')
transf(PROP_M2,"n_proportion_m2.in",2)
} # end function definition
#################################################################

if (trans.bio) trans.4M.SMS.bio()


#################################################################
#transform data from file with VPA catch  data input
trans.4M.SMS.catch<-function(){
cat("trans.4M.SMS.catch\n")
file<-file.path(list.data.path,'VPA_Ca01.IN')
s<-read.csv(file,header=TRUE)

a<-data.frame(species=code.name[2:(nsp+1)],first.age=rep(fa,nsp),last.age=SMS.control@species.info[,'last-age'],plus=SMS.control@species.info[,'+group'])

b<-merge(s,a)
bbb<<-b
c<-data.frame(species=b$species,year=b$year,quarter=b$quarter,
   age=ifelse(b$plus==1,ifelse(b$age>b$last.age,b$last.age,b$age),b$age),
   Catchn=ifelse(b$age>b$last.age, ifelse(b$plus==1,b$CATCHN,0),b$CATCHN),
   WCatch=b$WCATCH,PROP_CAT=b$PROP_CAT)

#c[c$species=='SAN' & c$quarter==1,'quarter']<-2
#c[c$species=='SAN' & c$quarter==4,'quarter']<-3

CN<-tapply(c$Catchn,list(c$year,c$quarter,c$species,c$age),sum)
# CW<-tapply(c$Catchn*c$WCatch,list(c$year,c$quarter,c$species,c$age),sum)
#CW<-CW/CN
CW<-tapply(c$WCatch,list(c$year,c$quarter,c$species,c$age),sum)

PROP_CAT<-tapply(c$PROP_CAT,list(c$year,c$quarter,c$species,c$age),sum)
PROP_CAT[PROP_CAT>1]<-1

transf<-function(item,file.name,dig) {
  out<-file.path(data.path,file.name)
  unlink(out)
  if (file.name=="canum.in") {
    cat(paste("#Data extracted from file ",file,"\n"),file=out,append=TRUE)
    cat(paste("#Date ", date(), '\n'),file=out,append=TRUE)
    cat(paste("#The file includes catch numbers at age",'\n'),file=out,append=TRUE)
    cat(paste("# if catch number=0  is applied for all ages in a year-season Zero fishing is assumed","\n"),file=out,append=TRUE)
    cat(paste("# if catch number<0  is catch numbers are assumed unknown (NA)","\n"),file=out,append=TRUE)
  }
  for (sp in code.name[(first.VPA+1):(nsp+1)]) {
   # cat(sp,'\n')
   # cat('dim(out1) ',dim(out1),'\n')
      out1<-item[,,sp,]
      for (y in years[1]:years[2]){
        out2<-out1[as.character(y),,]
        out2[is.na(out2)]<-0
        cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
        write.table(formatC(out2,digits=dig,width=12,format = "f"),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
    }
  }
  checksum(file=out)
}
transf(CW,"weca.in",4)
transf(CN,"canum.in",2)
transf(PROP_CAT,"proportion_landed.in",2)
}
if (trans.catch) trans.4M.SMS.catch()
##################################################################################



trans.4M.SMS.stomach<-function(){

cat("trans.4M.SMS.stomach\n")
if (is.null(stomMark)) filen<-'ALK_stom_list.dat' else  filen<-paste0('ALK_stom_list',stomMark,'.dat')
file<-file.path(list.data.path,filen) 
cat(file,'\n')
lak<-read.table(file,header=TRUE)
lak<-select.ALK(lak)

lak$ALK<-lak$ALK/100
if (is.null(stomMark)) filen<-'stomcon_list.dat' else  filen<-paste0('stomcon_list',stomMark,'.dat')
file<-file.path(list.data.path,filen) 
stom<-read.table(file,header=TRUE,na.strings=".")
if (any(is.na(stom))) stop(paste("program halted: file ", filen," includes missing data"))


if (use.stom.meanl.ALK) stom$prey.mean.length<-stom$prey.mean.length.ALK

if (is.null(var.groups.size)) stom$var.groups<-stom$pred.size else stom$var.groups<-var.groups[match(stom$pred.size,var.groups.size)]
a<-aggregate(haul.no~SMS_area+pred+var.groups,max,data=stom)
b<-names(a)
names(a)<- c("SMS_area","pred",    "var.groups",   "max.N" )
stom<-merge(stom,a)
stom$haul.no.scaled<-round(stom$haul.no/stom$max.N*100)
stom[stom$haul.no.scaled<min.stom.groups,"haul.no.scaled"]<-min.stom.groups
# hist(stom$haul.no)
# summary(stom$haul.no)


stom<-select.stom(stom)
#print(summary(stom))
# print(subset(stom,is.na(stomcon)))

# adjust invented observations (first,mid and last) 
stom[stom$type=='first','stomcon']<-stom.first
stom[stom$type=='mid','stomcon']<-stom.mid
stom[stom$type=='last','stomcon']<-stom.last
stom[stom$type=='exp','stomcon']<-stom.exp

stom[stom$type!='obs' & stom$stomcon<stom.min.abs,'stomcon']<-stom.min.abs

stom[stom$type!='obs','haul.no']<-stom[stom$type!='obs','haul.no']*inserted.haul.no.propor
stom[stom$type!='obs','haul.no.scaled']<-stom[stom$type!='obs','haul.no.scaled']*inserted.haul.no.propor

stom$type.no<-0
stom[stom$type=='obs','type.no']<-1
stom[stom$type=='mid','type.no']<-2
stom[stom$type=='first','type.no']<-3
stom[stom$type=='last','type.no']<-3
stom[stom$type=='exp','type.no']<-4


# recalculate to relative stomach contents, summing up to 1
a<-by(stom,list(stom$SMS_area,stom$year,stom$quarter,stom$pred,stom$pred.size.class),function(x) sum(x$stomcon))
a<-subset(arr2df(a),stomobs>0)
names(a)<-list('SMS_area','year','quarter','pred','pred.size.class','sum')

stom<-merge(stom,a)
stom$stomcon<-stom$stomcon/stom$sum;
#cat('\ntest3\n');print(head(stom))
  
#check existence of ALK keys for all prey sizes
a<-unique(data.frame(SMS_area=lak$SMS_area,year=lak$year,quarter=lak$quarter,species=lak$prey,size=lak$prey.size.class,a="a"))
# cat('\nLAK\n');print(head(a,20)) # remove

b2<-unique(data.frame(MS_area=stom$SMS_area,year=stom$year,quarter=stom$quarter,species=stom$prey,size=stom$prey.size.class))
b3<-b2
b3<-unique(subset(b3,species!='OTH'))
b3$key<-paste(b3$SMS_area,b3$year,b3$quarter,b3$species,formatC(b3$size,wid=2,flag="0"))
s<-order(b3$key)
b4<-b3[s,]
b4<-subset(b4,select=-key)
b4$b<-"b"
# cat('\nSTOM\n'); print(head(b4,20))  # remove
check<-merge(a,b4,all=TRUE)

mis.alk<-subset(check,is.na(a))
if (dim(mis.alk)[1]>0) {
  cat('ERROR:   Missing ALK prey size class\n')
  print(mis.alk)
  print(check)
  stop('program stop')
} else cat("Data are OK\n")


###############################################################################


tot<-stom<-subset(stom,select=-sum)

tot$year.quarter<-paste(tot$year,tot$quarter)
tot$year.quarter.area<-paste(tot$year.quarter,formatC(tot$SMS_area,wid = 2, flag = "0"))
tot$year.quarter.area.pred<-paste(tot$year.quarter.area,formatC(tot$pred.no,wid = 2, flag = "0"))
tot$year.quarter.area.pred.predL<-paste(tot$year.quarter.area.pred,formatC(tot$pred.size.class,wid = 2, flag = "0"))
tot$year.quarter.area.pred.predL.prey<-paste(tot$year.quarter.area.pred.predL,formatC(tot$prey.no,wid = 2, flag = "0"))
tot$year.quarter.area.pred.predL.prey.preyL<-paste(tot$year.quarter.area.pred.predL.prey,formatC(tot$prey.size.class,wid = 2, flag = "0"))

b<-subset(tot,select=c(year,pred.size,pred.size.class))
a<-paste(b$year,formatC(b$pred.size.class,wid = 2, flag = "0"),formatC(b$pred.size,wid = 4, flag = "0"))

bb1<-subset(b[!duplicated(a),],select=c(year,pred.size,pred.size.class))
names(bb1)<-c('year','size','size.no')

  
b<-subset(tot,prey.no!=0,select=c(year,pred.size,pred.size.class,prey.size,prey.size.class))

a<-paste(b$year,formatC(b$prey.size.class,wid = 2, flag = "0"),formatC(b$prey.size,wid = 4, flag = "0"))

bb2<-subset(b[!duplicated(a),],select=c(year,prey.size,prey.size.class))
names(bb2)<-c('year','size','size.no')
bb<-rbind(bb1,bb2)
key<-paste(bb$year,formatC(bb$size,wid = 4, flag = "0"),formatC(bb$size.no,wid = 2, flag = "0"))
bb<-bb[!duplicated(key),]
key<-order(paste(bb$year,formatC(bb$size,wid = 4, flag = "0"),formatC(bb$size.no,wid = 2, flag = "0")))
bb<-bb[key,]

key<-order(tot$year.quarter.area.pred.predL.prey.preyL)
tot<-tot[key,]
tot$first.year<-!duplicated(tot$year)
tot$first.year.quarter<-!duplicated(tot$year.quarter)
tot$first.year.quarter.area<-!duplicated(tot$year.quarter.area)
tot$first.year.quarter.area.pred<-!duplicated(tot$year.quarter.area.pred)
tot$first.year.quarter.area.pred.predL<-!duplicated(tot$year.quarter.area.pred.predL)
tot$first.year.quarter.area.pred.predL.prey<-!duplicated(tot$year.quarter.area.pred.predL.prey)
tot$first.year.quarter.area.pred.predL.prey.preyL<-!duplicated(tot$year.quarter.area.pred.predL.prey.preyL)
tot$last.prey<-c(tot$first.year.quarter.area.pred.predL.prey[2:dim(tot)[1]],TRUE)

pred.size.class.format<-subset(tot, first.year.quarter.area.pred.predL==T,select=c(year, quarter, SMS_area, pred.no,pred.size.class, pred.size))
# cat('\ntest 20:\n') ;print(pred.size.class.format)

line<-'##############################################################\n'

out<-file.path(data.path,'stom_struc_at_length.in')
unlink(out)
cat(line,file=out,append=TRUE)
cat(paste("#Data extracted from file ",out,"\n"),file=out,append=TRUE)
cat(paste("#Date ", date(), '\n'),file=out,append=TRUE)
cat(paste("#stom.first ",stom.first,"  #  stomcon for inserted (first length class) values\n"),file=out,append=TRUE)
cat(paste("#stom.mid ",stom.mid," # stomcon for inserted (mid length class) values\n"),file=out,append=TRUE)
cat(paste("#stom.last ",stom.last,"  # stomcon for inserted (last length class) values\n"),file=out,append=TRUE)
cat(paste("#stom.exp ",stom.exp,"  # stomcon for inserted (expanded species and length class) values\n"),file=out,append=TRUE)
cat(paste("#stom.min.abs",stom.min.abs, "   # absolut minimum value for stom con for inserted values\n"),file=out,append=TRUE)
cat(paste("#delete.tails",delete.tails, "   # artifictal data deleted or not\n"),file=out,append=TRUE)
cat(paste("#inserted.haul.no.propor ",inserted.haul.no.propor," # proportion of number hauls in case of invented values (first, mid and last values)\n"),file=out,append=TRUE)

cat(line,file=out,append=TRUE)
cat(paste("# File for stomach contents data by length groups of predators and preys\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
cat(paste("# n_stl_y:  number of years with stomach data\n",length(unique(tot$year)),"\n"),file=out,append=TRUE)

cat(paste("# n_stl_yq:  number of year, quarter combinations with stomach data\n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter)),"\n"),file=out,append=TRUE)

cat(paste("# n_stl_yqd:  number of year, quarter, area combinations with stomach data\n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter.area)),"\n"),file=out,append=TRUE)

cat(paste("# n_stl_yqdp:  number of year, quarter, area, predator combinations with stomach data\n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter.area.pred)),"\n"),file=out,append=TRUE)

cat(paste("# n_stl_yqdpl:  number of year, quarter, area, predator, predator length combinations with stomach data\n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter.area.pred.predL)),"\n"),file=out,append=TRUE)

cat(paste("# n_stl_yqdplp:  number of year, quarter, area, predator, predator length, prey  combinations with stomach data\n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter.area.pred.predL.prey)),"\n"),file=out,append=TRUE)
 
aa<-tot[!duplicated(tot$year.quarter),]
aa$index<-seq(1,dim(aa)[1])
min<-tapply(aa$index,list(aa$year),min)
max<-tapply(aa$index,list(aa$year),max)

cat(line,file=out,append=TRUE)
cat(paste("# stl_Y: year name, first and last year-quarter index\n"),file=out,append=TRUE)
yy<-as.numeric(unlist(dimnames(min)))
for (i in (1:dim(min))) cat(paste(yy[i],min[i],max[i],"\n"),file=out,append=TRUE)


aa<-tot[!duplicated(tot$year.quarter.area),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area))
min<-tapply(aa$index,list(aa$year,aa$quarter),min)
max<-tapply(aa$index,list(aa$year,aa$quarter),max)
yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
cat(paste("# stl_yq: quarter name, first and last quarter-area index\n"),file=out,append=TRUE)
for (j in (1:length(yy))) for(i in (1:length(qq))) if (!is.na(min[j,i]) & !is.na(max[j,i])) cat(paste(qq[i],min[j,i],max[j,i],"\n"),file=out,append=TRUE)
 
cat(paste("# stl_yqd: area name, and first and last year-quarter-species-age index\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.pred),]
aa$index<-seq(1,dim(aa)[1])
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
pp<-as.numeric(unlist(dimnames(min)[3]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(pp))) {
  if (!is.na(min[j,i,k]) & !is.na(max[j,i,k])) cat(paste(pp[k],min[j,i,k],max[j,i,k],"\n"),file=out,append=TRUE)
}
  
cat(line,file=out,append=TRUE)
cat(paste("# stl_yqdp: predator name, and first and last area-pred index\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.pred.predL),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area,pred.no))
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$pred.no),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$pred.no),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
dd<-as.numeric(unlist(dimnames(min)[3]))
pp<-as.numeric(unlist(dimnames(min)[4]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(dd))) for(l in (1:length(pp))) {
  if (!is.na(min[j,i,k,l]) & !is.na(max[j,i,k,l])) cat(paste(pp[l],min[j,i,k,l],max[j,i,k,l],"\n"),file=out,append=TRUE)
}

 
cat(line,file=out,append=TRUE)
cat(paste("# stl_yqdpl: predator length name, first and last predator-length-prey index, and predator length\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.pred.predL.prey),]
#cat ('\ntest 7:\n'); summary(aa);print(head(aa))
aa$index<-seq(1,dim(aa)[1])
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$pred.no,aa$pred.size.class),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$pred.no,aa$pred.size.class),max)

b<-cbind(expand.grid(dimnames(min)),as.vector(min),as.vector(max))
colnames(b)<-c('year','quarter','SMS_area','pred.no','size.no','min.index','max.index')
b<-as.data.frame(b)
b<-subset(b,!is.na(max.index))


pred.size.class.format$size.no<-pred.size.class.format$pred.size.class; pred.size.class.format$pred.size.class<-NULL
pred.size.class.format$size<-pred.size.class.format$pred.size; pred.size.class.format$pred.size<-NULL
#print(head(b))
#print(head(pred.size.class.format))
b<-merge(b,pred.size.class.format,all.x=TRUE,sort=FALSE)   

key<-order(paste(b$year,b$quarter,b$SMS_area,formatC(as.numeric(b$pred.no),wid=2,flag='0'),formatC(as.numeric(b$size.no),wid=2,flag='0')))
b<-b[key,]

b<-subset(b,select=c('size.no','min.index','max.index','size'))
#cat ('\ntest 8:\n'); print(head(b,15))
write.table(b,file=out,append=TRUE, quote = FALSE, row.names = FALSE,col.names = FALSE)

cat(line,file=out,append=TRUE)
cat(paste("# stl_yqdplp: prey name, first and last year-quarter-predator-length-prey-length\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.pred.predL.prey.preyL),]
aa<-subset(aa,select=c('year','quarter','SMS_area','pred','pred.no','pred.size','pred.size.class','prey','prey.no','prey.size','prey.size.class'))
min<-tapply(aa$prey.size.class,list(aa$year,aa$quarter,aa$SMS_area,aa$pred.no,aa$pred.size.class,aa$prey.no),min)
max<-tapply(aa$prey.size.class,list(aa$year,aa$quarter,aa$SMS_area,aa$pred.no,aa$pred.size.class,aa$prey.no),max)

b<-cbind(expand.grid(dimnames(min)),as.vector(min),as.vector(max))
colnames(b)<-c('year','quarter','SMS_area','pred.no','pred.size','prey','min.index','max.index')
b<-as.data.frame(b)
b<-subset(b,!is.na(max.index))
key<-order(paste(b$year,b$quarter,b$SMS_area,formatC(as.numeric(b$pred.no),wid = 2, flag = "0"),formatC(as.numeric(b$pred.size),wid = 2, flag = "0"),formatC(as.numeric(b$prey),wid = 2, flag = "0")))
b<-b[key,]
b<-subset(b,select=c('prey','min.index','max.index'))
write.table(b,file=out,append=TRUE, quote = FALSE, row.names = FALSE,col.names = FALSE)
cat(paste("#\n -999.0 # Check sum \n"),file=out,append=TRUE)

nfiles<-8
out<-c(
    file.path(data.path,'/stomcon_at_length.in'),            # 1
    file.path(data.path,'/stomlen_at_length.in'),            # 2
    file.path(data.path,'/stomweight_at_length.in'),         # 3
    file.path(data.path,'/N_haul_at_length.in'),             # 4
    file.path(data.path,'/N_haul_at_length_scaled.in'),      # 5
    file.path(data.path,'/stomnumber_at_length.in'),         # 6
    file.path(data.path,'/stomtype_at_length.in'),           # 7
    file.path(data.path,'/stom_pred_length_at_sizecl.in')    # 8
    )
for (i in 1:nfiles) unlink(out[i])
for (i in 1:nfiles) cat(line,file=out[i])
cat(paste("# relative stomach contents weight  by year, quarter, area, predator, predator length, prey and prey length\n"),file=out[1],append=TRUE)
cat(paste("# Length of preys by year, quarter, area, predator, predator length, prey and prey length\n"),file=out[2],append=TRUE)
cat(paste("# Mean weight at length of  preys by year, quarter, area, predator, predator length, prey and prey length\n"),file=out[3],append=TRUE)
cat(paste("# Number of samples by year, quarter, area, predator, predator length, prey and prey length\n"),file=out[4],append=TRUE)
cat(paste("# SCALED Number of samples by year, quarter, area, predator, predator length, prey and prey length\n"),file=out[5],append=TRUE)
cat(paste("# Number at length of preys by year, quarter, area, predator, predator length, prey and prey length\n"),file=out[6],append=TRUE)
cat(paste("# Type of data of preys by year, quarter, area, predator, predator length, prey and prey length\n",
          "# 1=Observed data\n",
          "# 2=(not observed) data within the observed size range (=fill in)\n",
          "# 3=(not observed) data outside an observed size range. One obs below and one above (=tails)\n",
          "# 4=(not observed) data for the full size range of a prey species irrespective of predator size (=expansion) \n"),file=out[7],append=TRUE)
cat(paste("# Mean length per predator size class\n"),file=out[8],append=TRUE)


code.slow.but.formatted.output<-function() {
for (i in (1:dim(tot)[1])) {
 if (tot$first.year[i]) {
   for (j in 1:nfiles) cat(line,file=out[j],append=TRUE)
   for (j in (1:nfiles)) cat(paste("# year:",tot$year[i],"\n"),file=out[j],append=TRUE)
   cat(paste(' stom year:',tot$year[i],'\n'))
 }
 if (tot$first.year.quarter[i]) {
   for (j in 1:nfiles) cat(line,file=out[j],append=TRUE)
   for (j in (1:nfiles))cat(paste("# ",tot$year[i]," quarter:",tot$quarter[i],"\n"),file=out[j],append=TRUE)
 }
 if (tot$first.year.quarter.area[i]) {
   for (j in 1:nfiles) cat(line,file=out[j],append=TRUE)
   for (j in (1:nfiles))cat(paste("# ",tot$year[i]," quarter:",tot$quarter[i]," area:",tot$SMS_area[i],"\n"),file=out[j],append=TRUE)
 }
 if (tot$first.year.quarter.area.pred[i]){
   for (j in 1:nfiles) cat(line,file=out[j],append=TRUE) 
   for (j in (1:nfiles)) cat(paste("# predator:",tot$pred[i]," no:",tot$pred.no[i],"\n"),file=out[j],append=TRUE)
 }
 if (tot$first.year.quarter.area.pred.predL[i]) {
    for (j in (1:7)) cat(paste("# predator size:",tot$pred.size[i]," class:",tot$pred.size.class[i],"\n"),file=out[j],append=TRUE)
    cat(paste(formatC(tot$pred.mean.length[i],format="d",width=6),'\n'),file=out[8],append=TRUE)
 }

 if (tot$first.year.quarter.area.pred.predL.prey[i]){
   for (j in (1:7)) cat(paste("# prey:",tot$prey[i]," no:",tot$prey.no[i]," first size:",tot$prey.size[i],"\n"),file=out[j],append=TRUE)
 }
 cat(formatC(tot$stomcon[i],format="f",dig=8,width=12),file=out[1],append=TRUE)
 cat(formatC(tot$prey.mean.length[i],format="d",width=6),file=out[2],append=TRUE)
 cat(formatC(tot$mean.weight[i],format="f",dig=5,width=9),file=out[3],append=TRUE)
 cat(formatC(tot$haul.no[i],format="d",width=5),file=out[4],append=TRUE)
 cat(formatC(tot$haul.no.scaled[i],format="d",width=5),file=out[5],append=TRUE)
 cat(formatC(tot$used.prey.number[i],format="d",width=6),file=out[6],append=TRUE)
 cat(formatC(tot$type.no[i],format="d",width=6),file=out[7],append=TRUE)

 if (tot$last.prey[i])for (j in (1:7)) cat('\n',file=out[j],append=TRUE)
}  
for (j in (1:8)) cat(paste("#\n -999.0 # Check sum \n"),file=out[j],append=TRUE)
} # end code.slow.but.formatted.output


execution.effective<-function() {
  write.table(formatC(tot$stomcon,format="f",dig=8,width=12),file=out[1],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot$prey.mean.length,format="d",width=6),file=out[2],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot$mean.weight,format="f",dig=5,width=9),file=out[3],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot$haul.no,format="d",width=5),file=out[4],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot$haul.no.scaled,format="d",width=5),file=out[5],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot$used.prey.number,format="d",width=5),file=out[6],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot$type.no,format="d",width=5),file=out[7],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(formatC(tot[tot$first.year.quarter.pred.predL,'pred.mean.length'],format="d",width=6),file=out[8],append=TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  for (j in (1:8)) cat(paste("#\n -999.0 # Check sum \n"),file=out[j],append=TRUE)
  }

if (formatted.output) code.slow.but.formatted.output() else execution.effective()

}

if (trans.stomach) trans.4M.SMS.stomach()

#######################################################################################

trans.4M.SMS.ALK<-function(){
cat("trans.4M.SMS.ALK\n")
if (is.null(stomMark)) filen<-'ALK_stom_list.dat' else  filen<-paste0('ALK_stom_list',stomMark,'.dat')
file<-file.path(list.data.path,filen) 

lak<-read.table(file,header=TRUE)

lak<-select.ALK(lak)

lak$ALK=lak$ALK/100

tot<-lak
tot$year.quarter<-paste(tot$year,tot$quarter)
tot$year.quarter.area<-paste(tot$year,tot$quarter,formatC(tot$SMS_area,wid = 2, flag = "0"))
tot$year.quarter.area.prey<-paste(tot$year.quarter.area,formatC(tot$prey.no,wid = 2, flag = "0"))
tot$year.quarter.area.prey.age<-paste(tot$year.quarter.area.prey,formatC(tot$prey.age,wid = 2, flag = "0"))
tot$year.quarter.area.prey.age.size<-paste(tot$year.quarter.area.prey.age,formatC(tot$prey.size.class,wid = 2, flag = "0"))

key<-order(tot$year.quarter.area.prey.age.size)
tot<-tot[key,]
tot$first.year.quarter              <-!duplicated(tot$year.quarter)
tot$first.year.quarter.area         <-!duplicated(tot$year.quarter.area)
tot$first.year.quarter.area.prey    <-!duplicated(tot$year.quarter.area.prey)
tot$first.year.quarter.area.prey.age<-!duplicated(tot$year.quarter.area.prey.age)
tot$last.age<-c(tot$first.year.quarter.area.prey.age[2:dim(tot)[1]],TRUE)


line<-'##############################################################\n'

############## Print ALK ##############
out<-file.path(data.path,'alk_stom.in')
unlink(out)
cat(line,file=out,append=TRUE)
cat(paste("# File for splitting age-group on length classess (ALK). Used for likelihood for stomach data\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
cat(paste("# n_ALK_y:  number of years with ALK\n", length(unique(tot$year))," \n"),file=out,append=TRUE)
cat(paste("# n_ALK_yq:  number of year, quarter combinations with ALK \n",length(unique(tot$year.quarter)),"\n"),file=out,append=TRUE)
cat(paste("# n_ALK_yqd:  number of year, quarter area combinations with ALK \n",length(unique(tot$year.quarter.area)),"\n"),file=out,append=TRUE)
cat(paste("# n_ALK_yqds:  number of year, quarter area species combinations with ALK \n"),file=out,append=TRUE)
cat(length(unique(tot$year.quarter.area.prey)),file=out,append=TRUE)
cat(paste("\n# n_ALK_yqdsa:  number of year, quarter, area species, age combinations with ALK \n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter.area.prey.age)),"\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)

cat(paste("# Year name, and first and last year-quarter index\n# ALK_y_name if_ALK_y il_ALK_y\n"),file=out,append=TRUE)

aa<-tot[!duplicated(tot$year.quarter),]
aa$index<-seq(1,dim(aa)[1])
min<-tapply(aa$index,list(aa$year),min)
max<-tapply(aa$index,list(aa$year),max)
cat(paste("# stl_y: year name, first and last year-quarter index\n"),file=out,append=TRUE)
yy<-as.numeric(unlist(dimnames(min)))
for (i in (1:dim(min))) cat(paste(yy[i],min[i],max[i],"\n"),file=out,append=TRUE)

aa<-tot[!duplicated(tot$year.quarter.area),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area))
min<-tapply(aa$index,list(aa$year,aa$quarter),min)
max<-tapply(aa$index,list(aa$year,aa$quarter),max)
yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
cat(paste("# stl_yq: quarter name, first and last quarter-area index\n"),file=out,append=TRUE)
for (j in (1:length(yy))) for(i in (1:length(qq))) if (!is.na(min[j,i]) & !is.na(max[j,i])) cat(paste(qq[i],min[j,i],max[j,i],"\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)

cat(paste("# stl_yqd: area name, and first and last year-quarter-species-age index\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.prey),]
aa$index<-seq(1,dim(aa)[1])
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
pp<-as.numeric(unlist(dimnames(min)[3]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(pp))) {
  if (!is.na(min[j,i,k]) & !is.na(max[j,i,k])) cat(paste(pp[k],min[j,i,k],max[j,i,k],"\n"),file=out,append=TRUE)
}

cat(line,file=out,append=TRUE)
cat(paste("# stl_yqdp: species name, and first and last area-species index\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.prey.age),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area,prey.no))
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$prey.no),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$prey.no),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
dd<-as.numeric(unlist(dimnames(min)[3]))
pp<-as.numeric(unlist(dimnames(min)[4]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(dd))) for(l in (1:length(pp))) {
  if (!is.na(min[j,i,k,l]) & !is.na(max[j,i,k,l])) cat(paste(pp[l],min[j,i,k,l],max[j,i,k,l],"\n"),file=out,append=TRUE)
}

cat(line,file=out,append=TRUE)
cat(paste("# species age name, and first and last species-age index\n"),file=out,append=TRUE)
cat(paste("# ALK_yqasa_name if_ALK_yqasa il_ALK_yqasa\n"),file=out,append=TRUE)
min<-tapply(tot$prey.size.class,list(tot$year,tot$quarter,tot$SMS_area,tot$prey.no,tot$prey.age),min)
max<-tapply(tot$prey.size.class,list(tot$year,tot$quarter,tot$SMS_area,tot$prey.no,tot$prey.age),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
dd<-as.numeric(unlist(dimnames(min)[3]))
pp<-as.numeric(unlist(dimnames(min)[4]))
pa<-as.numeric(unlist(dimnames(min)[5]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(dd))) for(l in (1:length(pp))) for(m in (1:length(pa))){
  if (!is.na(min[j,i,k,l,m]) & !is.na(max[j,i,k,l,m])) cat(paste(pa[m],min[j,i,k,l,m],max[j,i,k,l,m],"\n"),file=out,append=TRUE)
}


cat(line,file=out,append=TRUE)
cat(paste("# ALK by year, quarter, species, age and length\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
if (formatted.output) {
 for (i in (1:dim(tot)[1])) {
    if (tot$first.year.quarter[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# year:",tot$year[i],"Quarter:",tot$quarter[i],'\n'),file=out,append=TRUE)
        cat(paste('ALK output, ALKS year:',tot$year[i]," Quarter:",tot$quarter[i],'\n'))
    }
   if (tot$first.year.quarter.area[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# SMS_area:",tot$SMS_area[i],'\n'),file=out,append=TRUE)
        cat(line,file=out,append=TRUE)
    }

    if (tot$first.year.quarter.area.prey[i]) cat(paste("# species:",tot$prey[i],'\n'),file=out,append=TRUE)
    if (tot$first.year.quarter.area.prey.age[i]) cat(paste("# age:",tot$prey.age[i],"first size:",tot$prey.size.class[i],tot$prey.size[i],'\n'),file=out,append=TRUE)
    cat(formatC(tot$ALK[i],format="f",dig=5,width=8),file=out,append=TRUE)
    if (tot$last.age[i]) cat('\n',file=out,append=TRUE)
  } 
} else {
    cat(formatC(tot$ALK,format="f",dig=5,width=8),file=out,append=TRUE)
}


####
# print mean length at size class

cat(line,file=out,append=TRUE)
cat(paste("# mean length (mm) by year, quarter, species, age and length\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
if (formatted.output) {
 for (i in (1:dim(tot)[1])) {
    if (tot$first.year.quarter[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# year:",tot$year[i],"Quarter:",tot$quarter[i],'\n'),file=out,append=TRUE)
        cat(paste('ALK-mean length output, ALKS year:',tot$year[i]," Quarter:",tot$quarter[i],'\n'))
    }
   if (tot$first.year.quarter.area[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# SMS_area:",tot$SMS_area[i],'\n'),file=out,append=TRUE)
        cat(line,file=out,append=TRUE)
    }

    if (tot$first.year.quarter.area.prey[i]) cat(paste("# species:",tot$prey[i],'\n'),file=out,append=TRUE)
    if (tot$first.year.quarter.area.prey.age[i]) cat(paste("# age:",tot$prey.age[i],"first size:",tot$prey.size.class[i],tot$prey.size[i],'\n'),file=out,append=TRUE)
    cat(formatC(tot$ALK.MeanL[i],format="f",dig=0,width=6),file=out,append=TRUE)
    if (tot$last.age[i]) cat('\n',file=out,append=TRUE)
  } 
} else {
    cat(formatC(tot$ALK.MeanL,format="f",dig=0,width=6),file=out,append=TRUE)
}
 cat(paste("#\n -999.0 # Check sum \n"),file=out,append=TRUE)

}

 

trans.4M.SMS.ALK.all<-function(){
cat("trans.4M.SMS.ALK.all\n")
if (is.null(stomMark)) filen<-'ALK_all_list.dat' else  filen<-paste0('ALK_all_list',stomMark,'.dat')
file<-file.path(list.data.path,filen) 
lak<-read.table(file,header=TRUE)

lak<-select.ALK.all(lak)

lak$ALK=lak$ALK/100

tot<-lak
tot$year.quarter<-                   paste(tot$year,tot$quarter)
tot$year.quarter.area<-              paste(tot$year.quarter,formatC(tot$SMS_area,wid = 2, flag = "0"))
tot$year.quarter.area.prey<-         paste(tot$year.quarter.area,formatC(tot$prey.no,wid = 2, flag = "0"))
tot$year.quarter.area.prey.age<-     paste(tot$year.quarter.area.prey,formatC(tot$prey.age,wid = 2, flag = "0"))
tot$year.quarter.area.prey.age.size<-paste(tot$year.quarter.area.prey.age,formatC(tot$prey.size.class,wid = 2, flag = "0"))

key<-order(tot$year.quarter.area.prey.age.size)
tot<-tot[key,]
#print(dim(tot))
#print(tot)

tot$first.year.quarter<-!duplicated(tot$year.quarter)
tot$first.year.quarter.area<-!duplicated(tot$year.quarter.area)
tot$first.year.quarter.area.prey<-!duplicated(tot$year.quarter.area.prey)
tot$first.year.quarter.area.prey.age<-!duplicated(tot$year.quarter.area.prey.age)
tot$last.age<-c(tot$first.year.quarter.area.prey.age[2:dim(tot)[1]],TRUE)

print(sum(tot$first.year.quarter.area.prey.age))

line<-'##############################################################\n'

############## Print ALKS ##############
out<-file.path(data.path,'alk_all.in')
unlink(out)
cat(line,file=out,append=TRUE)
cat(paste("# File for splitting age-group on length classess (ALK). Used for calculation of M2 values\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
cat(paste("# n_ALKS_y:  number of years with ALK\n", length(unique(tot$year))," \n"),file=out,append=TRUE)
cat(paste("# n_ALKS_yq:  number of year, quarter combinations with ALK \n",length(unique(tot$year.quarter)),"\n"),file=out,append=TRUE)
cat(paste("# n_ALKS_yqa:  number of year, quarter, area combinations with ALK \n",length(unique(tot$year.quarter.area)),"\n"),file=out,append=TRUE)
cat(paste("# n_ALKS_yqas:  number of year, quarter, area, species combinations with ALK \n"),file=out,append=TRUE)
cat(length(unique(tot$year.quarter.area.prey)),file=out,append=TRUE)
cat(paste("\n# n_ALKS_yqasa:  number of year, quarter, area, species, age combinations with ALK \n"),file=out,append=TRUE)
cat(paste(length(unique(tot$year.quarter.area.prey.age)),"\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)

cat(paste("# Year name, and first and last year-quarter index\n# ALKS_y_name if_ALKS_y il_ALKS_y\n"),file=out,append=TRUE)

aa<-tot[!duplicated(tot$year.quarter),]
aa$index<-seq(1,dim(aa)[1])
min<-tapply(aa$index,list(aa$year),min)
max<-tapply(aa$index,list(aa$year),max)
cat(paste("# stl_y: year name, first and last year-quarter index\n"),file=out,append=TRUE)
yy<-as.numeric(unlist(dimnames(min)))
for (i in (1:dim(min))) cat(paste(yy[i],min[i],max[i],"\n"),file=out,append=TRUE)


aa<-tot[!duplicated(tot$year.quarter.area),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area))
min<-tapply(aa$index,list(aa$year,aa$quarter),min)
max<-tapply(aa$index,list(aa$year,aa$quarter),max)
yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
cat(paste("# stl_yq: quarter name, first and last quarter-area index\n"),file=out,append=TRUE)
for (j in (1:length(yy))) for(i in (1:length(qq))) if (!is.na(min[j,i]) & !is.na(max[j,i])) cat(paste(qq[i],min[j,i],max[j,i],"\n"),file=out,append=TRUE)


aa<-tot[!duplicated(tot$year.quarter.area.prey),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area))
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area),max)
yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
dd<-as.numeric(unlist(dimnames(min)[3]))
cat(paste("# stl_yqa: area name, first and last area-species index\n"),file=out,append=TRUE)
for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(dd))) {
  if (!is.na(min[j,i,k]) & !is.na(max[j,i,k])) cat(paste(dd[k],min[j,i,k],max[j,i,k],"\n"),file=out,append=TRUE)
}

cat(line,file=out,append=TRUE)
cat(paste("#  species name, and first and last area-species index\n"),file=out,append=TRUE)
aa<-tot[!duplicated(tot$year.quarter.area.prey.age),]
aa$index<-seq(1,dim(aa)[1])
aa<-subset(aa,select=c(index,year,quarter,SMS_area,prey.no))
min<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$prey.no),min)
max<-tapply(aa$index,list(aa$year,aa$quarter,aa$SMS_area,aa$prey.no),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
dd<-as.numeric(unlist(dimnames(min)[3]))
pp<-as.numeric(unlist(dimnames(min)[4]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(dd))) for(l in (1:length(pp))) {
  if (!is.na(min[j,i,k,l]) & !is.na(max[j,i,k,l])) cat(paste(pp[l],min[j,i,k,l],max[j,i,k,l],"\n"),file=out,append=TRUE)
}

cat(line,file=out,append=TRUE)
cat(paste("# species age name, and first and last species-age index\n"),file=out,append=TRUE)
cat(paste("# ALKS_yqasa_name if_ALKS_yqasa il_ALKS_yqasa\n"),file=out,append=TRUE)
min<-tapply(tot$prey.size.class,list(tot$year,tot$quarter,tot$SMS_area,tot$prey.no,tot$prey.age),min)
max<-tapply(tot$prey.size.class,list(tot$year,tot$quarter,tot$SMS_area,tot$prey.no,tot$prey.age),max)

yy<-as.numeric(unlist(dimnames(min)[1]))
qq<-as.numeric(unlist(dimnames(min)[2]))
dd<-as.numeric(unlist(dimnames(min)[3]))
pp<-as.numeric(unlist(dimnames(min)[4]))
pa<-as.numeric(unlist(dimnames(min)[5]))

for (j in (1:length(yy))) for(i in (1:length(qq))) for(k in (1:length(dd))) for(l in (1:length(pp))) for(m in (1:length(pa))){
  if (!is.na(min[j,i,k,l,m]) & !is.na(max[j,i,k,l,m])) cat(paste(pa[m],min[j,i,k,l,m],max[j,i,k,l,m],"\n"),file=out,append=TRUE)
}


cat(line,file=out,append=TRUE)
cat(paste("# ALK by year, quarter, species, age and length\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
if (formatted.output) {
 for (i in (1:dim(tot)[1])) {
    if (tot$first.year.quarter[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# year:",tot$year[i],"Quarter:",tot$quarter[i],'\n'),file=out,append=TRUE)
        cat(paste('ALK output, ALKS year:',tot$year[i]," Quarter:",tot$quarter[i],'\n'))
    }
   if (tot$first.year.quarter.area[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# SMS_area:",tot$SMS_area[i],'\n'),file=out,append=TRUE)
        cat(line,file=out,append=TRUE)
    }

    if (tot$first.year.quarter.area.prey[i]) cat(paste("# species:",tot$prey[i],'\n'),file=out,append=TRUE)
    if (tot$first.year.quarter.area.prey.age[i]) cat(paste("# age:",tot$prey.age[i],"first size:",tot$prey.size.class[i],tot$prey.size[i],'\n'),file=out,append=TRUE)
    cat(formatC(tot$ALK[i],format="f",dig=5,width=8),file=out,append=TRUE)
    if (tot$last.age[i]) cat('\n',file=out,append=TRUE)
  } 
} else {
    cat(formatC(tot$ALK,format="f",dig=5,width=8),file=out,append=TRUE)
}
####
# print mean length at size class

cat(line,file=out,append=TRUE)
cat(paste("# Size (mm) by year, quarter, species, age and length\n"),file=out,append=TRUE)
cat(line,file=out,append=TRUE)
if (formatted.output) {
 for (i in (1:dim(tot)[1])) {
    if (tot$first.year.quarter[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# year:",tot$year[i],"Quarter:",tot$quarter[i],'\n'),file=out,append=TRUE)
        cat(paste('mean length output, ALKS year:',tot$year[i]," Quarter:",tot$quarter[i],'\n'))
    }
    if (tot$first.year.quarter.area[i]) {
        cat(line,file=out,append=TRUE)
        cat(paste("# SMS_area:",tot$SMS_area[i],'\n'),file=out,append=TRUE)
        cat(line,file=out,append=TRUE)
    }

    if (tot$first.year.quarter.area.prey[i]) cat(paste("# species:",tot$prey[i],'\n'),file=out,append=TRUE)
    if (tot$first.year.quarter.area.prey.age[i]) cat(paste("# age:",tot$prey.age[i],"first size:",tot$prey.size.class[i],tot$prey.size[i],'\n'),file=out,append=TRUE)
    cat(formatC(tot$ALK.MeanL[i],format="f",dig=0,width=6),file=out,append=TRUE)
    if (tot$last.age[i]) cat('\n',file=out,append=TRUE)
  } 
} else {
    cat(formatC(tot$ALK.MeanL,format="f",dig=0,width=6),file=out,append=TRUE)
}
cat(paste("#\n -999.0 # Check sum \n"),file=out,append=TRUE)
}


if (trans.ALK.all) trans.4M.SMS.ALK.all()  
if (trans.ALK.stomach) trans.4M.SMS.ALK()
 
}


# ICES/4M code for species in the same order as given variable name in species_names.in

########################################################################################
