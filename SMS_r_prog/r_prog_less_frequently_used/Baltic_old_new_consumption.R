cod_new<-read.csv(file.path(root,"Data_baltic","2019-data","cod_w_l.csv"))
head(cod_new)
         
con<-read.csv(file.path(root,exchangeDir,'Cod',"Average QUARTERLY consum param.csv"))
con$species<-'COD'
cod<-as_tibble(cod_new)
con<-as_tibble(con)
a<-left_join(cod,con) %>% filter(year>=year.start & year<=year.stop) %>% mutate(CONSUM=a*(mean_l*0.1)**b /1000)
a
cleanup()
X11()
plot(a$WSEA,a$CONSUM,xlab='Cod weight (kg)',ylab="ration kg/quarter")


#Old data
b<-Read.summary.data(dir=file.path(root,"EB_2012_key_run"),read.init.function=FALSE)
b
b<-subset(b,Species=='Cod',select=c(Species,Year,Quarter,west,ration))
head(b)

X11()
plot(b$west,b$ration,xlab='Cod weight (kg)',ylab="ration kg/quarter")


