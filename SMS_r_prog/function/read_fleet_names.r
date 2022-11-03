
Read.fleet.names<-function(dir=data.path)
{
    file<-file.path(dir,'fleet_names.in')
    s<-readLines(file, n=1000)
    s<-gsub('_',' ',s)
    s<-sub('[[:space:]]+$', '', s)
    s<-s[substr(s,1,1)!='#']

    file<-file.path(dir,'fleet_info.dat')
    finfo<-scan(file,skip=0,comment.char = "#") 
    finfo<-tail(finfo,-1)
    years<-rep(0,2)
    ages<-rep(0,3)
    n<-nsp-first.VPA+1
    max.fleet<-max(finfo[1:n])

    fl<-matrix('                       ',nrow=n,ncol=max.fleet)
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

readFleetInfo<-function(){
  
  fleet.names<-Read.fleet.names()
  
  file<-file.path(data.path,'fleet_info.dat') 
  finfo<-scan(file,comment.char = "#") 
  
  Init.function() # get SMS.contol object  including sp.names
  
  nsp<-nsp-first.VPA+1
  
  ii<-nsp+2       #counter in finfo file
  ir<-1       #counter in residual file
  
  years<-rep(0,2)
  ages<-rep(0,2)
  out<-NULL
  
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
      tmp<-data.frame(species=sp+first.VPA-1,fleet=f,fleetName=fleet.names[sp,f],age=ages[1]:ages[2])
      if (sp==1 & f==1) out<-tmp else out<-rbind(out,tmp)
      #print(paste("sp:",sp,"  fleet:",f,"  years:",years[1],years[2],"  ages:",ages[1],ages[2], ' ii:',ii))
      ii<-ii+5
    }}
  return(out)
}

