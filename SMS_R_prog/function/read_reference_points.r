#Read refence points from file reference_points.in 

Read.reference.points<-function(dir=data.path,read.init.function=TRUE){
    if (read.init.function) Init.function()
    
    a<-scan(file.path(dir,"reference_points.in"),comment.char = "#",quiet = TRUE)
    b<-matrix(a,nrow=nsp,ncol=4,byrow=TRUE)
    colnames(b)<-c("Flim","Fpa","Blim","Bpa")   
    rownames(b)<-sp.names[1:nsp]
    b
}


Read.reference.points.OP<-function(dir=data.path,read.init.function=TRUE){
    if (read.init.function) Init.function()
    
    a<-scan(file.path(dir,"OP_reference_points.in"),comment.char = "#",quiet = TRUE)
    b<-matrix(a,ncol=4,byrow=TRUE)
    colnames(b)<-c("Flim","Fpa","Blim","Bpa")   
    rownames(b)<-sp.names[first.VPA:nsp]
    b
}
