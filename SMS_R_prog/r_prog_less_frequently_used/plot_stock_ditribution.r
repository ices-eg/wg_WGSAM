Use.OP.stock.dist<-T

la<-SMS.control@max.age.all
fa<-SMS.control@first.age
years<-c(1,1)
years[1]<-SMS.control@first.year
years[2]<-SMS.control@last.year
ny<-years[2]-years[1]+1
if (Use.OP.stock.dist) {
  years[1]<-0
  years[2]<-0
  ny<-1
}
npr<-sum(SMS.control@species.info[,'predator']>=1)
nsp<-SMS.control@no.species
nq<-SMS.control@last.season
noAreas<-SMS.control@no.areas
area.names<-Read.area.names()

nox<-3;
noy<-3;

######################

noxy<-nox*noy
if (Use.OP.stock.dist) {
  l<-scan(file.path(data.path,'OP_Stock_distribution.in'),comment.char='#')
  b<-expand.grid(quarter=1:nq,area=1:noAreas,species.n=1:nsp,year=years[1]:years[2],age=fa:la)
}
if (!Use.OP.stock.dist) {
  l<-scan(file.path(data.path,'Stock_distribution.in'),comment.char='#')
}
b<-expand.grid(area=1:noAreas,species.n=1:nsp,year=years[1]:years[2],quarter=1:nq,age=fa:la)
b$species<-sp.names[b$species.n]

if (Use.OP.stock.dist) b<-b[order(b$quarter,b$area,b$species.n,b$year,b$age),] else b<-b[order(b$area,b$species.n,b$year,b$quarter,b$age),]

b<-data.frame(b,dist=l)
b<-subset(b,select=c(year,species,species.n,age,quarter,area,dist))

b$maxAge<-SMS.control@species.info[b$species.n,'last-age']
b<-subset(b,(age<=maxAge) & (quarter<SMS.control@rec.season & age<=(noxy-1)) | (quarter>=SMS.control@rec.season & age<=(noxy-2)))
b<-droplevels(subset(b,!(age==fa & quarter<SMS.control@rec.season)))
head(b)

#ftable(tapply(b$dist,list(b$species,b$quarter,b$age,b$year,b$area),sum))

palette(rainbow(noAreas))
cleanup()

if (!Use.OP.stock.dist) by(b,list(b$species.n,b$quarter),function(x){
  by(x,list(x$species.n,x$quarter),function(x){
    newplot(dev='screen',nox=nox,noy=noy)
    par(mar=c(3,3,3,1)) # c(bottom, left, top, right)
    plot.new();
    legend(x=0,y=1,rev(area.names),fill=rev(2:(noAreas+1)),cex=1.5,col=rev(2:(noAreas+1)),ncol=1,title=paste(x[1,'species'],"Q:",x[1,'quarter']))

    by(x,list(x$age),function(x){
      a<-tapply(x$dist,list(x$area,x$year),sum)
      barplot(a,main=paste('Age',x[1,'age']),col=2:(noAreas+1))
    })
  })
})

#ftable(tapply(x$dist,list(x$age,x$area,x$quarter),sum))
b<-droplevels(subset(b,age<=noxy-2))
if (Use.OP.stock.dist) by(b,list(b$species.n),function(x){
    x$quarter<-paste("Q",x$quarter,sep='')
    newplot(dev='screen',nox=nox,noy=noy)
    par(mar=c(3,3,3,1)) # c(bottom, left, top, right)
    plot.new();
    legend(x=0,y=1,rev(area.names),fill=rev(2:(noAreas+1)),cex=1.5,col=rev(2:(noAreas+1)),ncol=1,title=paste(x[1,'species']))

    by(x,list(x$age),function(x){
      a<-tapply(x$dist,list(x$area,x$quarter),sum)
      barplot(a,main=paste('Age',x[1,'age']),col=2:(noAreas+1))
    })
})

