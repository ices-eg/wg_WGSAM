########################################################################
## Analysis grey seal diet data, Vanessa Trijoulet and Morten Vinther ##
########################################################################

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

## Diet files
files <- list.files(dir)
files <- files[grep("Hg cons", files)]

## Total consumption per year (including other food), 1=1985, 2=2002, 3=2010-2011
total_cons <- list()
for (k in 1:3){
  total_cons[[k]] <- read_xlsx(path=file.path(dir, "Total Hg consumption North Sea by quarter.xlsx"),sheet=k)
}

## Loop through files
for (fil in files){
  
  ## Extract total consumption per quarter
  ss<-read_xlsx(path=file.path(dir, fil),sheet="cons") %>% rename(prey=species.name)
  ss
  cons.q.area <- by(ss,ss$quarter,function(x){
    a<-xtabs(cons.t~prey+region,data=x)
    round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  
  
  ## Extract fish length
  fishLength<-read_xlsx(path=file.path(dir, fil),sheet="Fish_lengths") %>%
    select(region,year,quarter,species=sp.name,l=`fish length`)
  
  fishLength<-left_join(fishLength,sps)
  sp.names <- c("Fulmar", "Guillemot", "Her.Gull", "Kittiwake", "GBB.Gull", "Gannet", "Puffin" ,"Razorbill", "A.radiata", "G.gurnards" ,"W.horse.mac" ,"N.horse.mac" ,"Grey.seal" ,"H.porpoise", "Hake" ,"Cod" ,"Whiting", "Haddock" ,"Saithe", "Mackerel", "Herring", "N.sandeel", "S.sandeel" ,"Nor.pout", "Sprat", "Plaice" ,"Sole")
  nsp <- length(sp.names)
  
  ## Attach the length-weight relationship parameters (here same as 2020 keyrun but could be changed if needed, see "Otoliths_porpoise.xlsx")
  fishLength<-left_join(fishLength,Read.length.weight.relation(dir=dir))
  # length weight relation W=a * Power(L,b)
  # L in mm, W in kg
  # Source Coull, K.K et al. Length/weight relationships for 88 species
  # of fish Encountered in the North East Atlantic
  # Scottish Fisheries Research Report, 43, 1989
  
  ## w is weight of fish (estimated from length) and create length bins for fish
  fishLength<-fishLength %>% mutate(w=a*((l*10)^b) )  #in kg
  fishLength<-fishLength %>% select(region,year,quarter,l,latin,prey=SMS,Species,w) %>%
    dplyr::mutate_if(is.character,as.factor)
  fishLength<-fishLength %>% mutate(lowpreyl=round(trunc(l)),higpreyl=round(trunc(l)+1))

  
  # ## For 1985, no samples in region 1, Phil Hammond extrapolated them by assuming diet composition was the same as in region 2
  # ## So need to assume the same length distribution as region 2 here, right? Or not needed because we sum across regions?
  # ## NEEDS CHECKING!!!!!!!!!!!!!!!!!
  # if (length(grep("1985", fil))>0){
  #   tmp <- subset(fishLength, region==2)
  #   tmp$region <- 1
  #   fishLength <- rbind(fishLength, tmp)
  # }
  
  ## Make plot of length distribution per quarter
  library(gridExtra)
  library(ggplot2)
  p <- list()
  for (sp in seq(unique(fishLength$prey))){
    p[[sp]] <- ggplot(data=subset(fishLength, prey==unique(fishLength$prey)[sp]), aes(x=lowpreyl)) +
      geom_histogram() +
      #geom_vline(aes(xintercept=lowpreyl)) + 
      facet_grid(quarter~region) +
      labs(title=unique(fishLength$prey)[sp])
  }
  pdf(paste0("NorthSeaKeyRun_2023/Seal_diet/Length_distribution_diet_perQuarter&Region_", unique(fishLength$year)[1], ".pdf"), onefile = TRUE)
    marrangeGrob(p, nrow=1, ncol=1)
  dev.off()
  
  p <- list()
  for (sp in seq(unique(fishLength$prey))){
    p[[sp]] <- ggplot(data=subset(fishLength, prey==unique(fishLength$prey)[sp]), aes(x=lowpreyl)) +
      geom_histogram() +
      #geom_vline(aes(xintercept=lowpreyl)) + 
      facet_grid(~quarter) +
      labs(title=unique(fishLength$prey)[sp])
  }
  pdf(paste0("NorthSeaKeyRun_2023/Seal_diet/Length_distribution_diet_perQuarter_", unique(fishLength$year)[1], ".pdf"), onefile = TRUE)
  marrangeGrob(p, nrow=1, ncol=1)
  dev.off()
  
  # ## Keep regions in til the end
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
  # problem if samples are missing!
  
  
  
  ## Aggregate weight consumed by quarter so that we have more samples (no more regions)
  fishWperLengthBin<-fishLength %>% group_by(quarter,latin,prey,lowpreyl,higpreyl) %>% 
    summarize(meanW=mean(w),meanL=mean(l),w=sum(w), nfish=n()) %>% ungroup()
  
  ## Estimate weight consumed in terms of proportion per length bin (W_percent)
  propFishPerLengthBin <- fishWperLengthBin %>% group_by(quarter,prey, lowpreyl, higpreyl) %>% summarize(w=sum(w)) %>% group_by(quarter, prey) %>% mutate(W_percent=w/sum(w))
  
  ## Estimate total weight of prey consumed by all grey seals per length bin (preyw) 
  propFishPerLengthBin$preyw <- NA
  for (k in 1:nrow(propFishPerLengthBin)){
    propFishPerLengthBin$preyw[k] <- propFishPerLengthBin$W_percent[k]* cons.q.area[[propFishPerLengthBin$quarter[k]]][propFishPerLengthBin$prey[k], 5]
  }
  
  ## Check all is correct
  tmp <- by(propFishPerLengthBin,propFishPerLengthBin$quarter,function(x){
    a<-xtabs(preyw~prey,data=x)
    #"TotalNS"=round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  round(unlist(tmp)-unlist(lapply(cons.q.area, function(x) x[,5])))
  # No samples HER quarter 2 in 2010
  # No samples NOP quarter 3 in 2010
  
  ## Add other food
  y <- which(files==fil)
  for (q in 1:4){
    tmp <- propFishPerLengthBin[1,]
    tmp$quarter <- q
    tmp$prey <- "OTHER"
    tmp$lowpreyl <- tmp$higpreyl <- tmp$w <- tmp$W_percent <- 0
    tmp$preyw <- unlist(total_cons[[y]][as.character(q),"Sum of cons.t"])-apply(cons.q.area[[q]],2,sum)[5]
    propFishPerLengthBin <- rbind(propFishPerLengthBin, tmp)
  }
  
  
}