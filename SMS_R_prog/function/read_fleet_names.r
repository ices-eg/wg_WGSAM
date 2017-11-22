
Read.fleet.names<-function(dir=data.path)
{
    file<-file.path(dir,'fleet_names.in')
    s<-readLines(file, n=1000)
    s<-gsub('_',' ',s)
    s<-sub('[[:space:]]+$', '', s)


    file<-file.path(dir,'fleet_info.dat')
    finfo<-scan(file,skip=3,comment.char = "#") 
    
    years<-rep(0,2)
    ages<-rep(0,3)
    
    max.fleet<-0
    for (sp in 1:nsp) {
    nf<-finfo[sp] #no. of fleets
    if (max.fleet<nf) max.fleet<-nf
    #print(paste("species:",sp,"Number of fleets:",nf))
    }
    fl<-matrix('                       ',nrow=nsp,ncol=max.fleet)
    i<-1
    for (sp in 1:nsp) {
      nf<-finfo[sp] #no. of fleets
      for (f in (1:nf)) {
        fl[sp,f]<-s[i]
        i<-i+1
      }
    }

  fl

}
