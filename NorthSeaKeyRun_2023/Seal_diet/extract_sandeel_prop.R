library(dplyr)
library(readxl)
dir <- "NorthSeaKeyRun_2023/Seal_diet/"
datadir <- paste0(dir, "data/")
sps<-  matrix(c(
  "COD",	"Gadus morhua",	'cod',"Cod",
  "WHG",	"Merlangius merlangus", 'whiting',"Whiting",
  "HAD",	"Melanogrammus aeglefinus", "haddock","Haddock",
  "POK",	"Pollachius virens","saithe","Saithe",
  "MAC",	"Scomber scombrus","mackerel","Mackerel",
  "HER",	"Clupea harengus","herring","Herring",
  "SAN",	"Ammodytidae","sandeel",'N.sandeel',
  "NOP",	"Trisopterus esmarkii","norway pout","Nor.pout",
  "SPR",	"Sprattus sprattus","sprat",'Sprat',
  "PLE",  "Pleuronectes platessa","plaice","Plaice",
  "SOL",	"Solea solea","sole","Sole"),
  byrow=T,ncol=4)
colnames(sps)<-c('SMS','latin','species',"Species")
sps<-as.data.frame(sps)

## Diet files
files <- list.files(datadir)
files <- files[grep("Hg cons ", files)]

cons.q.area <- list()
for (fil in files){
  ## Extract total consumption per quarter
  ss<-read_xlsx(path=file.path(datadir,fil),sheet="cons") %>% rename(prey=species.name)
  ss
  cons.q.area[[fil]] <- by(ss,ss$quarter,function(x){
    a<-xtabs(cons.t~prey+region,data=x)
    round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  nameCons <- lapply(cons.q.area[[fil]], rownames)[[1]]
  # Change naming so same as SMS
  nameCons[which(nameCons %in% sps$Species)] <- sort(sps[which(sps$Species %in% nameCons),]$SMS)
  nameCons[which(nameCons=="Norway pout")] <- "NOP"
  nameCons[which(nameCons=="Sandeel")] <- "SAN"
  for (k in 1:length(cons.q.area[[fil]])) rownames(cons.q.area[[fil]][[k]])=nameCons
}



# From consumption of sandeel
# Assumption North = Shetland, Orkney and northern North Sea, South = central North Sea, and southern North Sea
tmp2 <- lapply(cons.q.area, function(y) sapply(y, function(x) x["SAN", c("1","2","3","4")])) # row=region, col=quarter
tmp3 <- lapply(tmp2, function(x) cbind("North" = apply(x[c("1","2"),],2,sum)/apply(x,2,sum), "South" = apply(x[c("3","4"),],2,sum)/apply(x,2,sum)))

# From seal numbers
# Here only North Sea and Orkney
load("NorthSeaKeyRun_2023/Work_seal_numbers_VT/sealN.RData")
tmp <- t(apply(sealN[,c("NorthSea", "Orkney")],1, function(x) x/sum(x)))
rownames(tmp) <- sealN$Year

