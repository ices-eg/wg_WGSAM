source("SMS_r_prog/function/read_l-w_relation.r")

dir <- "NorthSeaKeyRun_2023/Seal_diet/"

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

files <- list.files(dir)
files <- files[grep("Hg cons", files)]

for (fil in files){
  
  ss<-read_xlsx(path=file.path(dir, fil),sheet="cons") %>% rename(prey=species.name)
  ss
  cons.q.area <- by(ss,ss$quarter,function(x){
    a<-xtabs(cons.t~prey+region,data=x)
    round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  
  
  fishLength<-read_xlsx(path=file.path(dir, fil),sheet="Fish_lengths") %>%
    select(region,year,quarter,species=sp.name,l=`fish length`)
  
  fishLength<-left_join(fishLength,sps)
  sp.names <- c("Fulmar", "Guillemot", "Her.Gull", "Kittiwake", "GBB.Gull", "Gannet", "Puffin" ,"Razorbill", "A.radiata", "G.gurnards" ,"W.horse.mac" ,"N.horse.mac" ,"Grey.seal" ,"H.porpoise", "Hake" ,"Cod" ,"Whiting", "Haddock" ,"Saithe", "Mackerel", "Herring", "N.sandeel", "S.sandeel" ,"Nor.pout", "Sprat", "Plaice" ,"Sole")
  nsp <- length(sp.names)
  fishLength<-left_join(fishLength,Read.length.weight.relation(dir=dir))
  
  # length weight relation W=a * Power(L,b)
  # L in mm, W in kg
  # Source Coull, K.K et al. Length/weight relationships for 88 species
  # of fish Encountered in the North East Atlantic
  # Scottish Fisheries Research Report, 43, 1989
  fishLength<-fishLength %>% mutate(w=a*((l*10)^b) )  #in kg
  
  fishLength<-fishLength %>% select(region,year,quarter,l,latin,prey=SMS,Species,w) %>%
    dplyr::mutate_if(is.character,as.factor)
  fishLength<-fishLength %>% mutate(lowpreyl=round(trunc(l)),higpreyl=round(trunc(l)+1))
  
  # fishWperLengthBin<-fishLength %>% group_by(quarter,region,latin,prey,lowpreyl,higpreyl) %>% 
  #   summarize(meanW=mean(w),meanL=mean(l),w=sum(w), nfish=n()) %>% ungroup()
  # 
  # propFishPerLengthBin <- fishWperLengthBin %>% group_by(region,quarter,prey, lowpreyl, higpreyl) %>% summarize(w=sum(w)) %>% group_by(region,quarter, prey) %>% mutate(W_percent=w/sum(w))
  # 
  # propFishPerLengthBin$preyw <- NA
  # for (k in 1:nrow(propFishPerLengthBin)){
  #   propFishPerLengthBin$preyw[k] <- propFishPerLengthBin$W_percent[k]* cons.q.area[[propFishPerLengthBin$quarter[k]]][propFishPerLengthBin$prey[k], propFishPerLengthBin$region[k]]
  # } 
  # 
  # # Checks
  # sum(subset(propFishPerLengthBin, prey=="COD" & region==1 & quarter==1)$preyw)
  # cons.q.area[[1]]["Cod","1"]
  # sum(subset(propFishPerLengthBin, prey=="COD" & quarter==1)$preyw)
  # cons.q.area[[1]]["Cod",5]
  # sum(subset(propFishPerLengthBin, prey=="SAN" & region==3 & quarter==3)$preyw)
  # cons.q.area[[3]]["Sandeel","3"]
  # sum(subset(propFishPerLengthBin, prey=="SAN" & quarter==3)$preyw)
  # cons.q.area[[3]]["Sandeel",5]
  
  # problem if sample missing
  
  ## Aggregate by quarter so that we don't miss samples
  fishWperLengthBin<-fishLength %>% group_by(quarter,latin,prey,lowpreyl,higpreyl) %>% 
    summarize(meanW=mean(w),meanL=mean(l),w=sum(w), nfish=n()) %>% ungroup()
  
  propFishPerLengthBin <- fishWperLengthBin %>% group_by(quarter,prey, lowpreyl, higpreyl) %>% summarize(w=sum(w)) %>% group_by(quarter, prey) %>% mutate(W_percent=w/sum(w))
  
  propFishPerLengthBin$preyw <- NA
  for (k in 1:nrow(propFishPerLengthBin)){
    propFishPerLengthBin$preyw[k] <- propFishPerLengthBin$W_percent[k]* cons.q.area[[propFishPerLengthBin$quarter[k]]][propFishPerLengthBin$prey[k], 5]
  } 
  # Checks
  tmp <- by(propFishPerLengthBin,propFishPerLengthBin$quarter,function(x){
    a<-xtabs(preyw~prey,data=x)
    #"TotalNS"=round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  round(unlist(tmp)-unlist(lapply(cons.q.area, function(x) x[,5])))
  # No samples HER quarter 2 in 2010
  # No samples NOP quarter 3 in 2010
  
  
}