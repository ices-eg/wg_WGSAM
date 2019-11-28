a<-Read.summary.data()
names(a)

library(tidyverse)

X11()

b<-filter(a,Species %in% c('Herring','Sprat') & ((Age ==0 & Quarter %in% c(3,4)) |(Age ==1 & Quarter %in% c(1,2,3,4))))

cleanup()
by(b,list(b$Species),function(x) {
  X11()
  print(ggplot(x,aes(Year,west)) +
    theme_bw() +
    geom_point() +
    facet_wrap(~ paste0(Species,' Age:',Age,' Q:',Quarter), scale="free_y") +
    labs(x="Year", y="Weight",title=""))
})

aggregate(west~Age+Quarter+Species,mean,data=b)

