
#Function to read and plot survey residuals
# parameter start.year: first year on X-axis, default=0 (defined from data)
# parameter end.year: end year on X-axis, default=0 (defined from data)
# 
# use over.all.max to set the maximum size of the reference buble. A value of 0 scales bubles individually  


over.all.max<-6  # use over.all.max different from 0 to set bublesize  
over.all.max<-2.5

use.ref.dot<-TRUE


plot.survey.residuals<-function(dev,nox=1,noy=1,Portrait=T,start.year=0,end.year=0,use.ref.dot=TRUE,add.title=TRUE,over.all.max=1.5) {

fleet.names<-Read.fleet.names()

file<-file.path(data.path,'fleet_info.dat') 
finfo<-scan(file,comment.char = "#") 

file<-file.path(data.path,'survey_residuals.out') 
res<-scan(file,sep=',',comment.char = "#") 

res[res==-99.9]<-NA
a<-res
a[a>99]<-NA
max.buble<-max(abs(a),na.rm=TRUE)

Init.function() # get SMS.contol object  including sp.names

nsp<-nsp-first.VPA+1

ii<-nsp+2       #counter in finfo file
ir<-1       #counter in residual file
nox.noy<-nox*noy
plot.no<-0

years<-rep(0,2)
ages<-rep(0,2)

for (sp in 1:nsp) {

  nf<-finfo[sp+1] #no. of fleets
  print(paste("species:",sp,"Number of fleets:",nf))
    sp.name<-sp.names[sp]

  for (f in 1:nf) {

    years[1]<-finfo[ii]
    ii<-ii+1
    years[2]<-finfo[ii]
    ii<-ii+1
    alfa<-finfo[ii]
    ii<-ii+1
    beta<-finfo[ii]
    ii<-ii+1
    if (alfa==0 && beta==0 && years[2]>SMS.control@last.year.model) years[2]<-SMS.control@last.year.model+1  else years[2]<-min(years[2],SMS.control@last.year.model)

    ages[1]<-finfo[ii]
    ii<-ii+1
    ages[2]<-finfo[ii]
 
    print(paste("sp:",sp,"  fleet:",f,"  years:",years[1],years[2],"  ages:",ages[1],ages[2]))

    nyr<-years[2]-years[1]+1
    nag<-ages[2]-ages[1]+1
    plot.no<-plot.no+1

    if (plot.no%%nox.noy==0 || f==1){
     newplot(dev,nox,noy,Portrait=Portrait,filename=paste(sp,f,plot.no))
      par(mar=c(3,4,3,2))
      if (dev=="wmf") par(mar=c(2,4,2,2))
      plot.no<-0
    }



    tmp<-matrix(res[ir:(ir+(nag+3)*nyr-1)],ncol=nyr,nrow=nag+3)
    tmp<-matrix(tmp[4:(nag+3),],ncol=nyr,nrow=nag)  
    tmp[tmp==-99.99]<-0
     print(tmp)
    xpos <- years[1]:years[2]
    ypos <- ages[1]:ages[2]
    title<- paste(sp.name," fleet:",f,sep="")
    title<- fleet.names[sp,f]
    #title<- paste(sp.name,": ",fleet.names[f],sep="")

    if (length(ypos)==1) {
      
        tmp2<-tmp
        tmp2[]<-0
        if (ages[1]>=1) {
          tmp<-rbind(tmp2,tmp,tmp2)
          ypos <- (ages[1]-1):(ages[2]+1)
        }
        if (ages[1]==0) {
          tmp<-rbind(tmp,tmp2)
          ypos <- (ages[1]):(ages[2]+1)
        }      
    }

    if (over.all.max>0) residplot(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year,maxn=over.all.max)
    else residplot(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year)

    
    ii<-ii+5
    ir<-ir+nyr*(nag+3)
  }
}

cat("Max buble size=",max.buble,'\n')
cat("log(Survey observed CPUE) - log(expected CPUE). 'Red' positive, 'White' negative\n")
if (dev !='screen') cleanup()
}
