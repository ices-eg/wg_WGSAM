a<-Read.length.weight.relation()


a<-subset(a,Species.n %in% c(10,11,15:27))

subset(a,select=-Species.n)
l<-data.frame(l=(7:100)*10)
a<-droplevels(merge(a,l))

a$w<-a$a* a$l**a$b


#subset(a,Species=='Sprat')

a1<-subset(a,l %in% c(100,200,300,500,1000))
round(tapply(a1$w,list(a1$Species,a1$l),sum),3)


xyplot(w~l,data=a, group=Species,auto.key =   list(space = "right", points = FALSE, lines = TRUE))
#cleanup() 