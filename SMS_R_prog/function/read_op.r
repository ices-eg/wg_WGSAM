
#Function to read rep file
Read.OP.rep<-function(dir=data.path,infile='op.rep')
{
  file<-file.path(dir,infile)
  s<-readLines(file)
  s<-suppressWarnings(as.numeric(unlist(strsplit(s,' '))))
  s<-s[!is.na(s)]
  s<-list(objFunc=s[1],npar=s[2],maxGrad=s[3],Penalty=s[4],Ftarget=s[5:(5+first.VPA-1)],Penalty.sp=tail(s,first.VPA-1))
  return(s)
}
  


#Function to read par file
Read.OP.par<-function(dir=data.path,infile='op_optim_par.out')
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}

#Function to read summary data
Read.OP.condensed<-function(dir=data.path,infile='op_condensed.out')
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}

#Function to read variable size and growth other pred
Read.op.other.sp.var<-function(dir=data.path,infile='op_other_sp_var.out')
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Species.n],s)
}


#Function to read summary data
Read.OP.community.indicator<-function(dir=data.path,infile='op_indicator_system.out')
{
  file<-file.path(dir,infile)
  read.table(file,header=TRUE)
}

Read.OP.other<-function(dir=data.path,infile="op_other_sp.out")
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Species.n],s)
}

#Read refence points from file reference_points.in 

Read.reference.points.OP<-function(dir=data.path){
    a<-scan(file.path(dir,"op_reference_points.in"),comment.char = "#",quiet = TRUE)
    b<-matrix(a,ncol=4,byrow=TRUE)
    colnames(b)<-c("Flim","Fpa","Blim","Bpa")   
    rownames(b)<-sp.names[first.VPA:nsp]
    b
}
