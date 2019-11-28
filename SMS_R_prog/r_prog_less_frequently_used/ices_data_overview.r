out.path<-file.path(data.path,'NS-keyRun')

dat<-Read.summary.data(extend=T)
write.csv(dat, file = file.path(out.path,'Assessed_species_details_by_age_and_quarter.csv'),row.names = FALSE)


dat<-Read.other.predator()
write.csv(dat, file = file.path(out.path,'Other_predators_details_by_age_and_quarter.csv'),row.names = FALSE)

#############


stom<-Read.stomach.data(read.init.function=T)
stom$rel.stom.contents<-stom$stom.input
stom<-subset(stom,stom.used.all==1,
   select=c(Predator,Prey,Year,Quarter, Predator.no,Predator.length.class,Predator.length,Predator.length.mean,
            Predator.weight,Predator.size,Prey.no,Prey.length.class,
            Prey.length.mean,Prey.weight,Prey.size, N.haul, rel.stom.contents))


write.csv(stom, file = file.path(out.path,'stomach_data.csv'),row.names = FALSE)

#
stom<-Read.stomach.data(read.init.function=T)
write.csv(stom, file = file.path(out.path,'stomach_data.csv'),row.names = FALSE)

