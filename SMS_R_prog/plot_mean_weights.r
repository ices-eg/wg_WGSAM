
  cleanup()
  
  a<-Read.summary.data(read.init.function=F)
  
    #b<-filter(a,((Age ==0 & Quarter %in% c(3,4)) |(Age ==1 & Quarter %in% c(1,2,3,4))) & Species=='Herring')
     
  b<-a 
    by(b,list(b$Species),function(x) {
      X11(w=9,h=12)
      print(ggplot(x,aes(Year,west)) +
              theme_bw() +
              geom_point() +
              #geom_smooth(method = "loess") +
              #geom_smooth(method = "lm") +
              #facet_wrap(~ paste0(Species,' Age:',Age,' Q:',Quarter), scale="free_y") +
              facet_wrap(~ paste0(Species,' Age:',Age,' Q:',Quarter)) +
              labs(x="Year", y="Weight",title=""))
    })

 