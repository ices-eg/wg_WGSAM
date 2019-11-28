if (F) {

  NF<-forecast[[1]]$s1.state.sim   # get log N and log F
  exp(colMeans(NF))

  N<-NF[,1:10]
  dimnames(N)[[2]]<-c(1:10 )
  FF<-NF[,11:19]
  dimnames(FF)[[2]]<-c(1:9 )

  a<-3      # select age
  plot(N[,a],FF[,a])
  plot(exp(N[,a]),exp(FF[,a]))
  hist(N[,a])
  hist(exp(N[,a]))

  logN<-apply(N,2,mean)
  logF<-apply(FF,2,mean)

  Nexp1<-exp(logN)

  Nexp2<-apply(exp(N),2,mean)
  Nexp1/ Nexp2
  apply(N,2,sd)    # the difference in mean of a exp(log) becomes higher with higher std, as expected

  # stochastic
  C<-exp(N[,1:9])*(1-exp(-exp(FF)-0.2))/(exp(FF)+0.2)*exp(FF)

  apply(C,2,mean)
  apply(C,2,median)   # stochastic result

  CC<-exp(logN[1:9])*(1-exp(-exp(logF)-0.2))/(exp(logF)+0.2)*exp(logF)     # determenistic

  CC/apply(C,2,median)  # they er pretty much the same, no consistent difference
  sum(CC)/sum(apply(C,2,median))

  N2<- exp(N[,1:9])*exp(-exp(FF)-0.2) #survivors, no birthday
  N2d<-exp(logN[1:9])*exp(-exp(logF)-0.2)    # determenistic
  N2d/apply(N2,2,median) # they er pretty much the same, no consistent difference
  t(t(sum(N2d)/sum(apply(N2,2,median))))

  # just using fixed F
  logF[]<-log(0.3)
  FF[]<-log(0.3)
  C<-exp(N[,1:9])*(1-exp(-exp(FF)-0.2))/(exp(FF)+0.2)*exp(FF)  # stochastic
  CC<-exp(logN[1:9])*(1-exp(-exp(logF)-0.2))/(exp(logF)+0.2)*exp(logF)     # determenistic

  apply(C,2,median)    # stochastic result
  t(t(CC/apply(C,2,median)))    # they er pretty much the same, no consistent difference
  sum(CC)/sum(apply(C,2,median))
}