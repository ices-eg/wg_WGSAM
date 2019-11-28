calc_proportion_landed<-function(sp='COD',Species='X') {
  load(file.path(root,"data_NorthSea","2012-data","stocks",paste(sp,".Rdata",sep='')))    # Data from Clara
  a<-stock@landings.n/stock@catch.n
  a<-subset(as.data.frame(a),select=c( age,year,data))
  a$SP<-sp
  a$Species<-Species
  return(a)

}

a<-rbind(
  calc_proportion_landed(sp='COD',Species="17 Cod"),
  calc_proportion_landed(sp='HAD',Species="19 Haddock"),
  calc_proportion_landed(sp='WHG',Species="18 Whiting"),
  calc_proportion_landed(sp='POK',Species="20 Saithe"),
  calc_proportion_landed(sp='PLE',Species="25 Plaice")
)
head(a)

# use mean landingsrate for the year befor 1989
whg<-subset(a,Species=='18 Whiting' & year> 1990 & year<1995)
whg<-aggregate(data~ age +SP+Species,mean,data=whg)
yy<-data.frame(year=1963:1989)
whg<-merge(whg,yy)
a<-rbind(a,whg)

SMS<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))
prop<-expand.grid(age=SMS@first.age:SMS@max.age.all, quarter=1:SMS@last.season , year=SMS@first.year:SMS@last.year,
                   Species=paste(first.VPA:SMS@no.species,SMS@species.names[first.VPA:SMS@no.species]),prop=1)

aa<-merge(x=prop,y=a, all.x = T)
aa[!is.na(aa$data),'prop']<-aa[!is.na(aa$data),'data']

summary(aa)
bb<-tapply(aa$prop,list(aa$quarter,aa$age,aa$year,aa$Species),sum)

out<-file.path(data.path,"proportion_landed.in")
cat("# Proportion landed\n", file=out,append=F)
for (sp in (paste(first.VPA:SMS@no.species,SMS@species.names[first.VPA:SMS@no.species]) )) for (y in (SMS@first.year:SMS@last.year)) {
   cat("# ",sp,y, "\n",file=out,append=T)
   write.table(round(bb[,,as.character(y),as.character(sp)],digits=4),file=out, col.names=F,row.names=F,append=T)
}

