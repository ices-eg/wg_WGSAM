# Sankey diagram
# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(htmlwidgets)

do_sankey <-function(years=1900:2000, out.dir=data.path,incl_catch=FALSE,area_catch=TRUE,width=NULL,height=NULL,margin=NULL,do_show=FALSE,MyPalette=c('red','blue'),excl_sp='none') {

  a<-read_csv(file.path(data.path,"Input_Output","Output","WhoEatsWhom","who_eats_whom_combined.csv"))
  data_long<- a %>% filter(Year %in% years) %>% rename(source=Prey,target=Predator,value=eatenW) 
  
  if (incl_catch) {
    
    #pred_format<-read.csv(file.path(data.path,'pred_format.csv'),header=TRUE)
    pred_format<-read.table(file.path(data.path,'pred_format.dat'),header=TRUE)
    
    if (area_catch) {
      b<-filter(Read.summary.data(), Year %in% years & Species.n>=first.VPA) %>% mutate(target='Fishery',Predator.no= -1, value=CWsum*prop.in /1000,source=Species,Prey.no=Species.n) %>% 
        dplyr::select(target,Year, Predator.no, source,  Prey.no,  value) 
    } else {
      b<-filter(Read.summary.table(), Year %in% years) %>% mutate(target='Fishery',Predator.no= -1, value=Yield/1000,source=Species,Prey.no=Species.n) %>% 
      dplyr::select(target,Year, Predator.no, source,  Prey.no,  value) 
    }

    oc<-Read.other.catch(dir=data.path,read.init.function=FALSE)
    bo<-filter(Read.other.catch(dir=data.path,read.init.function=FALSE), Year %in% years & Species.n<first.VPA) %>% mutate(target='Fishery',Predator.no= -1, value=catch/1000,source=Species,Prey.no=Species.n) %>% 
      dplyr::select(target,Year, Predator.no, source,  Prey.no,  value) 
    
    b<-rbind(b,bo)
    
        
    s<-merge(x=b,y=pred_format,by.x='source',by.y='old',all.x=TRUE)
    s$source<-s$new; s$new<-NULL
    s$Prey.no<-s$newno; s$newno<-NULL
  
    s<-aggregate(value~source +Year+target+Predator.no+Prey.no, data=s,sum)
    data_long<-rbind(data_long,s)  
  }  
  
  data_long<-subset(data_long,!(source %in% excl_sp) & value>0)  
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes<-rbind(unique(data.frame(no=data_long$Predator.no, name=data_long$target)),
               unique(data.frame(no=data_long$Prey.no, name=data_long$source))) %>% unique() %>% 
    #filter(no !=99) %>% 
    arrange(no) %>%dplyr::select(name)
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  data_long$IDsource=match(data_long$source, nodes$name)-1 
  data_long$IDtarget=match(data_long$target, nodes$name)-1
  
  # prepare colour scale
  #ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

  
  col2hex<-function (cname) {
    colMat <- col2rgb(cname)
    rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3, 
    ]/255)
  }
  
  ii<-NULL
  for (i in col2hex(MyPalette)) ii<-paste(ii,paste0('"',i,'"'),sep=',')
  ii
  
  ColourScal <- paste0('d3.scaleOrdinal() .range([', ii,'])')
  
  for (y in sort(unique(data_long$Year))) {
    dat<-subset(data_long,Year==y)
     # Make the Network
     p<-sankeyNetwork(Links = dat, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", 
                  sinksRight=FALSE, 
                  units='kt',width=width,height=height,margin=margin,
                  #colourScale=ColourScal, 
                  nodeWidth=40, fontSize=12, nodePadding=10)
    
     if (do_show) p
  
   # save the widget
     
   saveWidget(p, file=file.path(out.dir, paste0("sankey_",y,".html")))
   library(webshot)
   # you convert it as png
   webshot(file.path(out.dir, paste0("sankey_",y,".html")),file.path(out.dir, paste0("sankey_",y,".png")), vwidth = 1000, vheight = 900)
 }
}

 #
# do_sankey(years=c(1974,2000,2019), out.dir=data.path,  excl_sp=c('Plaice','Sole'),incl_catch=TRUE,area_catch=TRUE,do_show=TRUE) 
  
