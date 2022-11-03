dirs<-c("Baltic-2019-keyRun", "Baltic-2022")  
labels<-c("2019 keyrun","2022 keyrun")

 compare_runs_other_predators(
  dirs=dirs,  # directory files to be compared
  labels=labels,  # output legends
  paper=FALSE,   # graphs on file (paper=TRUE) or on screen (paper=FALSE)
  first.year.on.plot=1975,
  last.year.on.plot=2020,
  nonFish=c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill') #biomass make no sense for non-fish
)
