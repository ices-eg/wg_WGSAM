
#Function to read hindcast age based detailed MCMC data
Read.MCMC.summary.data<-function(dir=data.path)
{
file<-file.path(dir,'summary_MCMC.out')
a<-read.table(file,header=TRUE)
data.frame(Species=sp.names[a$Species.n],a)
}

#Function to read age based detailed MCMC data
Read.MCMC.detailed.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout_N.out')
N<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_F.out')
F<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_M2.out')
m2.exist<-file.exists(file)
if (m2.exist) M2<-read.table(file,header=TRUE)


a<-data.frame(Species=sp.names[N$Species.n],Species.n=N$Species.n, Year=N$Year,
              Quarter=N$Quarter,Age=N$Age, Repetion=N$Repetion, Iteration=N$Iteration,
                                    N=N$N/1000, F=F$F)
if (m2.exist) {
  yy<-unique(a$Year)
  M2<-subset(M2,Year %in% yy,drop=T)
  a$M2<-M2$M2
}
a
}
 
#Function to read hindcast age based detailed MCMC data
Read.SSB_percieved<-function(dir=data.path)
{
file<-file.path(dir,'mcout_SSB_percieved.out')
a<-read.table(file,header=TRUE)
data.frame(Species=sp.names[a$Species.n],a)
}

Read.MCMC.fixed<-function(dir=data.path) {
 file<-file.path(dir,'Predict_fixed.out')
 read.table(file,header=TRUE)
}

Read.MCMC.detailed.M2<-function(dir=data.path,del.zero=F)
{
 file<-file.path(dir,'mcout_M2.out')
 M2<-read.table(file,header=TRUE)
 if (del.zero) M2<-subset(M2,M2>0)
 data.frame(Species=sp.names[M2$Species.n],Species.n=M2$Species.n, Year=M2$Year,
            Quarter=M2$Quarter,Age=M2$Age, Repetion=M2$Repetion, Iteration=M2$Iteration,
                                     M2=M2$M2)
}

Read.MCMC.detailed.M2.N<-function(dir=data.path)
{
 file<-file.path(dir,'mcout_M2.out')
 M2<-read.table(file,header=TRUE)
 file<-file.path(dir,'mcout_N.out')
 N<-read.table(file,header=TRUE)

 data.frame(Species=sp.names[M2$Species.n],Species.n=M2$Species.n, Year=M2$Year,
            Quarter=M2$Quarter,Age=M2$Age, Repetion=M2$Repetion, Iteration=M2$Iteration,
                                     M2=M2$M2,N=N$N)
}

#M2<-Read.MCMC.detailed.M2.N()


#Function to read SSB and rec MCMC data
Read.MCMC.SSB.rec.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout_SSB.out')
SSB<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_TSB.out')
TSB<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_recruit.out')
rec<-read.table(file,header=TRUE)

data.frame(Species=sp.names[SSB$Species.n],Species.n=SSB$Species.n, Year=SSB$Year,
            Repetion=SSB$Repetion, Iteration=SSB$Iteration,SSB=SSB$SSB,TSB=TSB$TSB,recruit=rec$recruit)
}

#Function to read clouse of the fishery MCMC data
Read.MCMC.closure.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout_closure.out')
a<-read.table(file,header=TRUE)

Read.MCMC.closure.data<-data.frame(Species=sp.names[a$Species.n],Species.n=a$Species.n, Year=a$Year,
                                    Repetion=a$Repetion, Iteration=a$Iteration,closure=a$closure)
}


#Function to read clouse of the fishery MCMC data
Read.MCMC.closure.constraints.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout_closure.out')
c<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_constraints.out')
cc<-read.table(file,header=TRUE)

data.frame(Species=sp.names[c$Species.n],Species.n=c$Species.n, Year=c$Year,
      Repetion=c$Repetion, Iteration=c$Iteration,closure=c$closure,constraints=cc$constraints)
}

#Function to read meanF  and Yield data
Read.MCMC.F.yield.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout_mean_F.out')
a<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_mean_F_percieved.out')
c<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_yield.out')
b<-read.table(file,header=TRUE)

Read.MCMC.F.Yield.data<-data.frame(Species=sp.names[a$Species.n],Species.n=a$Species.n, Year=a$Year,
                                    Repetion=a$Repetion, Iteration=a$Iteration,Yield=b$yield,
                                    mean.F=a$mean_F,mean_F_percieved=c$mean_F_percieved)
}


#Function to read SSB, rec, yield and F MCMC data
Read.MCMC.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout_SSB.out')
SSB<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_recruit.out')
rec<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_mean_F.out')
a<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout_yield.out')
b<-read.table(file,header=TRUE)


Read.MCMC.data<-data.frame(Species=sp.names[SSB$Species.n],Species.n=SSB$Species.n, Year=SSB$Year,
                                    Repetion=SSB$Repetion, Iteration=SSB$Iteration,SSB=SSB$SSB,recruit=rec$recruit,
                                    Yield=b$yield,mean.F=a$mean_F)
}

Read.MCMC.eaten_M2<-function(dir=data.path)
{
file<-file.path(dir,'mcout_eaten_M2.out')
a<-read.table(file,header=TRUE)

data.frame(Species=sp.names[a$Species.n],Species.n=a$Species.n, Year=a$Year,
                                    Repetion=a$Repetion, Iteration=a$Iteration,eaten.M2=a$eaten_M2)
}


#Function to read mean and std values of SSB, rec, yield and F MCMC data
Read.MCMC.mean.data<-function(dir=data.path)
{
file<-file.path(dir,'mcout2_average_SSB.out')
SSB<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout2_average_recruit.out')
rec<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout2_average_mean_F.out')
a<-read.table(file,header=TRUE)

file<-file.path(dir,'mcout2_average_yield.out')
b<-read.table(file,header=TRUE)

Read.MCMC.mean.data<-data.frame(Species=sp.names[SSB$Species.n],Species.n=SSB$Species.n, Year=SSB$Year,
                                    SSB=SSB$SSB,SSB.std=SSB$std,
                                    recruit=rec$recruit, recruit.std=rec$std,
                                    Yield=b$yield,Yield.std=b$std,
                                    mean.F=a$mean_F,mean.F.std=a$std)
}


#Function to read Objective function values
Read.MCMC.objective<-function(dir=data.path)
{
file<-file.path(dir,'mcout_SSB.out')
obj<-read.table(file,header=TRUE)
Read.MCMC.objective<-obj
}
