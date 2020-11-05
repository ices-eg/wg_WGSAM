Read.consum_ab<-function(dir=data.path)
{
    file<-file.path(dir,'consum_ab.in')
    a<-scan(file,comment.char = "#") 
    a<-a[-length(a)]  # drop check sum
    b<-expand.grid(ab=c('a','b'),Quarter=1:4,Species.n=1:npr)
    b$parameter<-a
    b$Species=sp.names[b$Species.n]
    return(b)
}

#Read.consum_ab()