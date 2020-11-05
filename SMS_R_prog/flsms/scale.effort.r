# functon to scale effort, such that catchability becomes close to one,
# mean.Z is mean Z at age
# N1 is mean stock number at age 1
#
scale.effort<-function(FLIndices,N,mean.Z=1,N1=300000){
    n.ages<-20
    
    N<-rep(NA,n.ages)
    N[1]<-N1*exp(mean.Z)
    N[2]<-N1
    for (i in (3:n.ages)) N[i]<-N[i-1]*exp(-mean.Z)
    j<-1
    fact<-rep(NA,length(FLIndices))
    for (fl in (FLIndices)) {
        catch<-as.vector(yearMeans(fl@catch.n))
        eff<-as.numeric(yearMeans(fl@effort))
        CPUE<-catch/eff
        a1<-fl@range["min"]+1
        a2<-fl@range["max"]+1
        q<-CPUE/N[a1:a2]
        fac<-mean(q)
        i<-1E12
        while (TRUE) if ((fac/i)>1) break else i<-i/10
        fact[j]<-i
        fl@effort<-fl@effort*i
        fl@index<-fl@index/i
        FLIndices[[j]]<-fl
        j<-j+1

    }
    cat("\nScaling factors by index:\n",fact,"\n")
    FLIndices
}
