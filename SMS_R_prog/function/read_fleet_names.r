
Read.fleet.names<-function(dir=data.path)
{
    file<-file.path(dir,'fleet_names.in')
    s<-readLines(file, n=1000)
    s<-gsub('_',' ',s)
    s<-sub('[[:space:]]+$', '', s)
    s<-s[substr(s,1,1)!='#']

    file<-file.path(dir,'fleet_info.dat')
    finfo<-scan(file,skip=3,comment.char = "#") 
    
    years<-rep(0,2)
    ages<-rep(0,3)
    
    max.fleet<-0
    for (sp in first.VPA:nsp) {
    nf<-finfo[sp-first.VPA+1] #no. of fleets
    if (max.fleet<nf) max.fleet<-nf
    #print(paste("species:",sp,"Number of fleets:",nf))
    }
    fl<-matrix('                       ',nrow=nsp-first.VPA+1,ncol=max.fleet)
    i<-1
    for (sp in first.VPA:nsp) {
      nf<-finfo[sp-first.VPA+1] #no. of fleets
      for (f in (1:nf)) {
        fl[sp-first.VPA+1,f]<-s[i]
        i<-i+1
      }
    }

  rownames(fl)<-sp.names[first.VPA:nsp]  
  colnames(fl)<-paste('fleet',1:max.fleet)
  fl
}

#Read.fleet.names()


