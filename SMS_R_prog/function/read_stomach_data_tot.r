#Function to read stomach related data
Read.stomach.data.tot<-function(dir=data.path)
{

file<-file.path(dir,'stom_struc_at_length.in')
s<-scan(file,skip=3,what=integer(),comment.char = "#")

n.y<-s[1]
n.yq<-s[2]
n.yqp<-s[3]
n.yqpl<-s[4]
n.yqplp<-s[5]

i<-6
j<-i+n.y*3-1
stl.y<-matrix(s[i:j],ncol=n.y,nrow=3)

i<-j+1
j<-i+n.yq*3-1
stl.yq<-matrix(s[i:j],ncol=n.yq,nrow=3)

i<-j+1
j<-i+n.yqp*3-1
stl.yqp<-matrix(s[i:j],ncol=n.yqp,nrow=3)

i<-j+1
j<-i+n.yqpl*4-1
stl.yqpl<-matrix(s[i:j],ncol=n.yqpl,nrow=4)

file<-paste(dir,'stl_avail_food.out',sep="")
avail<-scan(file,skip=0,comment.char = "#")

nobs<-length(avail)
Year<-rep(0.0,nobs)
Quarter<-rep(0.0,nobs)
Predator<-rep(0.0,nobs)
Pred.no<-rep(0.0,nobs)
Predator.Length<-rep(0.0,nobs)
Avail<-rep(0.0,nobs)



i<-0
m<-10.0
for (iy in 1:n.y) {
  year=stl.y[1,iy] 
  for (iq in stl.y[2,iy]:stl.y[3,iy]) {
   quarter<-stl.yq[1,iq]
    for (ipred in stl.yq[2,iq]:stl.yq[3,iq]) {
      pred.no<-stl.yqp[1,ipred]
      pred<-sp.name(pred.no)
       for (ipred.l in stl.yqp[2,ipred]:stl.yqp[3,ipred]) {
         pred.l=stl.yqpl[4,ipred.l]
         i<-i+1
         Year[i]<-year
         Quarter[i]<-quarter
         Predator[i]<-pred
         Pred.no[i]<-pred.no
         Predator.Length[i]<-pred.l
         Avail[i]<-avail[i]
      
      }
    }
  }
}
 
Read.stomach.data.tot<-data.frame(Year,Quarter,Predator,Pred.no,Predator.Length,
                              Avail)
}

#tot.stom<-Read.stomach.data.tot()
