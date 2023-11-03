########################################################################
## Analysis grey seal diet data, Vanessa Trijoulet and Morten Vinther ##
########################################################################

library(dplyr)
library(readxl)

source("SMS_r_prog/function/read_l-w_relation.r")

dir <- "NorthSeaKeyRun_2023/Seal_diet/"
datadir <- paste0(dir, "data/")
figdir <- paste0(dir, "figs/")
dir.create(figdir)


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
filesALL <- list.files(datadir)
files <- filesALL[grep("Hg cons ", filesALL)]

## Scat number files
filesScat <- filesALL[grep("OtosScatsQuarterRegion", filesALL)]


## Total consumption per year (including other food), 1=1985, 2=2002, 3=2010-2011
total_cons <- list()
for (k in 1:3){
  total_cons[[k]] <- read_xlsx(path=file.path(datadir, "Total Hg consumption North Sea by quarter.xlsx"),sheet=k)
}


## Loop through files
s <- c() # for final output
for (fil in files){
  
  ## Extract total consumption per quarter
  ss<-read_xlsx(path=file.path(datadir, fil),sheet="cons") %>% rename(prey=species.name)
  ss
  cons.q.area <- by(ss,ss$quarter,function(x){
    a<-xtabs(cons.t~prey+region,data=x)
    round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  
  
  ## Extract fish length
  fishLength<-read_xlsx(path=file.path(datadir, fil),sheet="Fish_lengths") %>%
    select(region,year,quarter,species=sp.name,l=`fish length`)
  
  fishLength<-left_join(fishLength,sps)
  sp.names <- c("Fulmar", "Guillemot", "Her.Gull", "Kittiwake", "GBB.Gull", "Gannet", "Puffin" ,"Razorbill", "A.radiata", "G.gurnards" ,"W.horse.mac" ,"N.horse.mac" ,"Grey.seal" ,"H.porpoise", "Hake" ,"Cod" ,"Whiting", "Haddock" ,"Saithe", "Mackerel", "Herring", "N.sandeel", "S.sandeel" ,"Nor.pout", "Sprat", "Plaice" ,"Sole")
  nsp <- length(sp.names)
  
  ## Attach the length-weight relationship parameters (here same as 2020 keyrun but could be changed if needed, see "Otoliths_porpoise.xlsx")
  fishLength<-left_join(fishLength,Read.length.weight.relation(dir=datadir))
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

  
  ## For 1985, no samples in region 1, Phil Hammond extrapolated them by assuming diet composition was the same as in region 2
  ## So need to assume the same length distribution as region 2 here, right? Or not needed because we sum across regions?
  if (length(grep("1985", fil))>0){
    tmp <- subset(fishLength, region==2)
    tmp$region <- 1
    fishLength <- rbind(fishLength, tmp)
  }
  
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
  pdf(paste0(figdir, "Length_distribution_diet_perQuarter&Region_", unique(fishLength$year)[1], ".pdf"), onefile = TRUE)
    marrangeGrob(print(p), nrow=1, ncol=1)
  dev.off()
  
  p <- list()
  for (sp in seq(unique(fishLength$prey))){
    p[[sp]] <- ggplot(data=subset(fishLength, prey==unique(fishLength$prey)[sp]), aes(x=lowpreyl)) +
      geom_histogram() +
      #geom_vline(aes(xintercept=lowpreyl)) + 
      facet_grid(~quarter) +
      labs(title=unique(fishLength$prey)[sp])
  }
  pdf(paste0(figdir, "Length_distribution_diet_perQuarter_", unique(fishLength$year)[1], ".pdf"), onefile = TRUE)
  marrangeGrob(print(p), nrow=1, ncol=1)
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
  
  ## Sprat not eaten much so remove here and will be added automatically to other food
  species <- as.vector(unique(fishLength$prey))
  species <- species[-which(species=="SPR")]
  fishLength <- subset(fishLength, prey %in% species)
  fishLength$prey <- factor(fishLength$prey) # reset factor
  cons.q.area <- lapply(cons.q.area, function(x) x[-which(rownames(x)=="Sprat"),])
  nameCons <- lapply(cons.q.area, rownames)[[1]]
  # Change naming so same as SMS
  nameCons[which(nameCons %in% sps$Species)] <- sort(sps[which(sps$Species %in% nameCons),]$SMS)
  nameCons[which(nameCons=="Norway pout")] <- "NOP"
  nameCons[which(nameCons=="Sandeel")] <- "SAN"
  for (k in 1:length(cons.q.area)) rownames(cons.q.area[[k]])=nameCons
  
  
  ## Borrow fish length distributions if not enough samples in a quarter (<5)
  ## Keep borrowing between Q1-2 and Q3-4
  numSample <- list()
  fishWperLengthBin<-fishLength %>% group_by(quarter,latin,prey,lowpreyl,higpreyl) %>% 
    summarize(meanW=mean(w),meanL=mean(l),w=sum(w), nfish=n()) %>% ungroup()
  for (sp in unique(fishWperLengthBin$prey)){
    tmp <- c()
    for (q in 1:4) tmp[q] <- nrow(subset(fishWperLengthBin, prey==sp & quarter==q))
    numSample[[sp]] <- tmp
  }
  for (sp in unique(fishLength$prey)){
    q=which(numSample[[sp]]<5)
    borrowq <- q
    if (length(borrowq)!=0) {
      borrowq = sapply(borrowq, function(x) as.numeric(switch(as.character(x), "1" = "2", "2" = "1", "3" = "4", "4" = "3")))
      tmp2 <- c()
      for (i in 1:length(borrowq)){
        tmp <- subset(fishLength, quarter==borrowq[i] & prey==sp)
        tmp$quarter <- q[i]
        tmp2 <- rbind(tmp2,tmp)
      }
      fishLength <- rbind(fishLength, tmp2) # here we keep the original data in and add the borrowed samples
    }
  }
  
  p <- list()
  for (sp in seq(unique(fishLength$prey))){
    p[[sp]] <- ggplot(data=subset(fishLength, prey==unique(fishLength$prey)[sp]), aes(x=lowpreyl)) +
      geom_histogram() +
      #geom_vline(aes(xintercept=lowpreyl)) + 
      facet_grid(~quarter) +
      labs(title=unique(fishLength$prey)[sp])
  }
  pdf(paste0(figdir, "Length_distribution_diet_perQuarter_with_borrowing_", unique(fishLength$year)[1], ".pdf"), onefile = TRUE)
  marrangeGrob(print(p), nrow=1, ncol=1)
  dev.off()
  
  ## Random sampling of the otoliths (scats) so we weight the length/weight distribution with consumption per quarter and region aggregated across prey
  ## Weights by consum per quarter and region
  weightsConsum=sapply(cons.q.area, function(x) apply(x[,1:4],2,sum)/sum(x[,1:4])) # col=quarter, row=region
  
  ## Calculate weighted w
  fishLength$weightedW <- NA
  for (k in 1:nrow(fishLength)){
    fishLength$weightedW[k] <- fishLength$w[k]*weightsConsum[as.character(fishLength$region[k]),as.character(fishLength$quarter[k])]
  }
  
  ## Aggregate weight consumed by quarter so that we have more samples (no more regions)
  fishWperLengthBin<-fishLength %>% group_by(quarter,latin,prey,lowpreyl,higpreyl) %>% 
    summarize(meanW=mean(weightedW),meanL=mean(l),w=sum(weightedW), nfish=n()) %>% ungroup()
  

  ## Estimate weight consumed in terms of proportion per length bin (W_percent)
  propFishPerLengthBin <- fishWperLengthBin %>% group_by(quarter,prey, lowpreyl, higpreyl) %>% summarize(w=sum(w)) %>% group_by(quarter, prey) %>% mutate(W_percent=w/sum(w))
  
  ## Estimate total weight of prey consumed by all grey seals per length bin (preyw) 
  propFishPerLengthBin$preyw <- NA
  for (k in 1:nrow(propFishPerLengthBin)){
    propFishPerLengthBin$preyw[k] <- propFishPerLengthBin$W_percent[k]* cons.q.area[[as.character(propFishPerLengthBin$quarter[k])]][as.character(propFishPerLengthBin$prey[k]), 5]
  }
  
  ## Check all is correct
  tmp <- by(propFishPerLengthBin,propFishPerLengthBin$quarter,function(x){
    a<-xtabs(preyw~prey,data=x)
    #"TotalNS"=round(cbind(a,rowSums(a)),2)
  }) # in tonnes
  if(sum(round(unlist(tmp)-unlist(lapply(cons.q.area, function(x) x[,5])))) !=0) stop ("PROBLEM IN RAISING DATA")

  ## Add other food
  y <- which(files==fil)
  year <- as.numeric(switch(as.character(y), "1" = "1985", "2" = "2002", "3" = "2010"))
  for (q in 1:4){
    tmp <- propFishPerLengthBin[1,]
    tmp$quarter <- q
    tmp$prey <- "OTH"
    tmp$lowpreyl <- tmp$higpreyl <- tmp$w <- tmp$W_percent <- 0
    tmp$preyw <- unlist(total_cons[[y]][as.character(q),"Sum of cons.t"])-apply(cons.q.area[[q]],2,sum)[5]
    propFishPerLengthBin <- rbind(propFishPerLengthBin, tmp)
  }
  propFishPerLengthBin$year <- year
  
  
  ## Make the output for Morten to use (same as before)
  propFishPerLengthBin$dataset='seal data'
  propFishPerLengthBin$record_type='PS'
  propFishPerLengthBin$country='All'
  propFishPerLengthBin$area='1'
  propFishPerLengthBin$lat=55
  propFishPerLengthBin$lon=0
  propFishPerLengthBin$month=(propFishPerLengthBin$quarter-1)*3+1
  propFishPerLengthBin$day=1
  propFishPerLengthBin$haul=propFishPerLengthBin$quarter
  propFishPerLengthBin$station=propFishPerLengthBin$quarter
  propFishPerLengthBin$sample_id=paste('Seal',propFishPerLengthBin$year,propFishPerLengthBin$quarter,sep='_')
  propFishPerLengthBin$ship='xxx'
  propFishPerLengthBin$rectangle='40F0'
  propFishPerLengthBin$pred_name='Halichoerus grypus'
  propFishPerLengthBin$pred_nodc=8111111111
  propFishPerLengthBin$pred_l=1100
  propFishPerLengthBin$pred_ll=1000
  propFishPerLengthBin$pred_lu=1200
  propFishPerLengthBin$CPUE=1
  propFishPerLengthBin$fish_id=paste(propFishPerLengthBin$year,propFishPerLengthBin$quarter,sep='_')
  ## Extract number of scat samples per quarter (to use as information for uncertainty in stomachs)
  scatfil <- filesScat[grep(year, filesScat)]
  numScat <- read.csv(paste0(datadir, scatfil))
  numScat <- subset(numScat, region %in% 1:4)
  numScatPerQ <- xtabs(no.scats~quarter, data=numScat)
  propFishPerLengthBin$n_food=NA
  for (q in 1:4) propFishPerLengthBin$n_food[which(propFishPerLengthBin$quarter==q)] <- numScatPerQ[as.character(q)]
  ## 
  propFishPerLengthBin$n_regur=0
  propFishPerLengthBin$n_empty=0
  propFishPerLengthBin$n_skel=0
  propFishPerLengthBin$n_tot=propFishPerLengthBin$n_food+propFishPerLengthBin$n_empty+propFishPerLengthBin$n_regur+propFishPerLengthBin$n_skel
  propFishPerLengthBin$digest=1
  propFishPerLengthBin$prey_name=propFishPerLengthBin$prey
  propFishPerLengthBin$prey=NULL
  propFishPerLengthBin$prey_w=propFishPerLengthBin$preyw
  propFishPerLengthBin$preyw=NULL
  propFishPerLengthBin$prey_n=NA
  propFishPerLengthBin$nprey=NULL
  propFishPerLengthBin$prey_l=NA
  propFishPerLengthBin$prey_w_meth='r'
  propFishPerLengthBin$prey_ll=propFishPerLengthBin$lowpreyl*10
  propFishPerLengthBin$lowpreyl=NULL
  propFishPerLengthBin$prey_lu=propFishPerLengthBin$higpreyl*10
  propFishPerLengthBin$higpreyl=NULL
  propFishPerLengthBin$w=NULL
  propFishPerLengthBin$W_percent=NULL
  
  propFishPerLengthBin[propFishPerLengthBin$prey_name=='OTH','prey_ll']<-NA
  propFishPerLengthBin[propFishPerLengthBin$prey_name=='OTH','prey_lu']<-NA
  
  save(propFishPerLengthBin, file=paste0(dir, "propFishPerLengthBin_", year, ".RData"))
  
  ## Merge all years
  s <- rbind(s, propFishPerLengthBin)
  
  rm(fishLength, fishWperLengthBin, propFishPerLengthBin, cons.q.area, ss, numSample, tmp, tmp2, p); gc()

}

write.csv(s,file=paste0(dir, 'adjusted_seal_diet.csv'), row.names = FALSE)

