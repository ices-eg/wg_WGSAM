transform.condensed<-function(a=condensed,my.area='North Sea') {
  if (my.area=='North Sea') {
    cat("\ntransform.condensed\n")
    if (KeyRunYear==2014) {
      a<-droplevels(subset(a,Species.n<27))
  
      a$Species<-sp.names[a$Species.n]
      a$Fround<- -1
      a[a$Species.n==18,"Fround"]<-a[a$Species.n==18,"COD"]
      a[a$Species.n==19,"Fround"]<-a[a$Species.n==19,"WHG"]
      a[a$Species.n==20,"Fround"]<-a[a$Species.n==20,"HAD"]
      a[a$Species.n==21,"Fround"]<-a[a$Species.n==21,"POK"]
      a[a$Species.n==22,"Fround"]<-a[a$Species.n==22,"HER"]
      a[a$Species.n==23,"Fround"]<-a[a$Species.n==23,"NSA"]
      a[a$Species.n==24,"Fround"]<-a[a$Species.n==24,"SSA"]
      a[a$Species.n==25,"Fround"]<-a[a$Species.n==25,"NOR"]
      a[a$Species.n==26,"Fround"]<-a[a$Species.n==26,"SPR"]
  
      a$COD1<-formatC(a$COD,digits = 3, width = 4, format = "f")
      a$WHG1<-formatC(a$WHG,digits = 3, width = 4, format = "f")
      a$HAD1<-formatC(a$HAD,digits = 3, width = 4, format = "f")
      a$POK1<-formatC(a$POK,digits = 3, width = 4, format = "f")
      a$HER1<-formatC(a$HER,digits = 3, width = 4, format = "f")
      a$NSA1<-formatC(a$NSA,digits = 3, width = 4, format = "f")
      a$SSA1<-formatC(a$SSA,digits = 3, width = 4, format = "f")
      a$NOR1<-formatC(a$NOR,digits = 3, width = 4, format = "f")
      a$SPR1<-formatC(a$SPR,digits = 3, width = 4, format = "f")
    }
    if (KeyRunYear==2017) {
      a<-droplevels(subset(a,Species.n<26))
      
      a$Species<-sp.names[a$Species.n]
      a$Fround<- -1
      a[a$Species.n==16,"Fround"]<-a[a$Species.n==16,"COD"]
      a[a$Species.n==17,"Fround"]<-a[a$Species.n==17,"WHG"]
      a[a$Species.n==18,"Fround"]<-a[a$Species.n==18,"HAD"]
      a[a$Species.n==19,"Fround"]<-a[a$Species.n==19,"POK"]
      a[a$Species.n==20,"Fround"]<-a[a$Species.n==20,"MAC"]
      a[a$Species.n==21,"Fround"]<-a[a$Species.n==21,"HER"]
      a[a$Species.n==22,"Fround"]<-a[a$Species.n==22,"NSA"]
      a[a$Species.n==23,"Fround"]<-a[a$Species.n==23,"SSA"]
      a[a$Species.n==24,"Fround"]<-a[a$Species.n==24,"NOR"]
      a[a$Species.n==25,"Fround"]<-a[a$Species.n==25,"SPR"]
      
      a$COD1<-formatC(a$COD,digits = 3, width = 4, format = "f")
      a$WHG1<-formatC(a$WHG,digits = 3, width = 4, format = "f")
      a$HAD1<-formatC(a$HAD,digits = 3, width = 4, format = "f")
      a$POK1<-formatC(a$POK,digits = 3, width = 4, format = "f")
      a$MAC1<-formatC(a$MAC,digits = 3, width = 4, format = "f")
      a$HER1<-formatC(a$HER,digits = 3, width = 4, format = "f")
      a$NSA1<-formatC(a$NSA,digits = 3, width = 4, format = "f")
      a$SSA1<-formatC(a$SSA,digits = 3, width = 4, format = "f")
      a$NOR1<-formatC(a$NOR,digits = 3, width = 4, format = "f")
      a$SPR1<-formatC(a$SPR,digits = 3, width = 4, format = "f")
    }
    
    
  } else if (my.area=='Baltic Sea') {
    a$Species<-sp.names[a$Species.n]
    a$Fround<- -1
    a[a$Species.n==1,"Fround"]<-a[a$Species.n==1,"COD"]
    a[a$Species.n==2,"Fround"]<-a[a$Species.n==2,"HER"]
    a[a$Species.n==3,"Fround"]<-a[a$Species.n==3,"SPR"]

    a$COD1<-formatC(a$COD,digits = 3, width = 4, format = "f")
    a$HER1<-formatC(a$HER,digits = 3, width = 4, format = "f")
    a$SPR1<-formatC(a$SPR,digits = 3, width = 4, format = "f")
  }
   
    a$probBelowBlim<-a$belowBlim/years.in.average
    sss<-sort(unique(a$Species.n))
    risk<-data.frame(Species.n=sss)
    risk<-data.frame(risk, riskL=riskLevels[risk$Species.n-first.VPA+1]/100)
    a<-merge (a,risk)
    a$riskBlim<-as.numeric(a$belowBlim>a$riskL)

    
    a$probBelowBpa<-a$belowBpa/years.in.average 
    a$riskBpa<-as.numeric(a$belowBpa>a$riskL)
    a$riskL<-NULL
      
  return(a)
}


 
