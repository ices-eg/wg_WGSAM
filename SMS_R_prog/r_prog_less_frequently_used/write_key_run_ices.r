a<-Read.summary.data()
write.csv(a,file.path(data.path,'Assess_species.csv'),row.names = F)


a<-Read.other.predator()
write.csv(a,file.path(data.path,'other_Predators.csv'),row.names = F)


a<-read.table(file.path(data.path,'summary_table_raw.out'),header=TRUE)
a<-data.frame(Species=sp.names[a$Species.n],a)
write.csv(a,file.path(data.path,'Assess_summary.csv'),row.names = F)


stom<-Read.stomach.data()
stom<-subset(stom,select=c(Year,Quarter, Predator,Prey,Predator.length.mean,Predator.weight,Predator.weight,Prey.length.mean,Prey.weight,
            N.samples,stom.input,stomcon,stomcon.hat))
write.csv(stom,file.path(data.path,'Stomachs.csv'),row.names = F)



a<-read.table(file.path(data.path,'partial_M2.out'),header=TRUE)
a<-data.frame(Predator=sp.names[a$Predator.n],Prey=sp.names[a$Prey.n],a)
a<-subset(a,select=c(Year,Quarter,Predator,Predator.age, Prey ,  Prey.age ,  Part.M2))

write.csv(a,file.path(data.path,'partial_M2.csv'),row.names = F)

