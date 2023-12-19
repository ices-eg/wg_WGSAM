aa<-file.path("C:","_C_drev","SMS-git","Data_NorthSea","final_input_NS_2023")
keys<-c("SMS_area","year","quarter","pred","pred.size","prey","prey.size", 'type')

readDiet<-function(diet="stomcon_list_FishStomach_Boots_haul.dat",lab='_a') {
  a<-read.table(file=file.path(aa,diet),sep=' ',header=T) %>%
    mutate(calc.prey.number=NULL,used.prey.number=NULL, prey.mean.length.ALK=NULL,pred.mean.length=NULL,prey.mean.length=NULL, pred.size.class=NULL,
           prey.size.class=NULL,pred.no=NULL,prey.no=NULL, haul.prey.no=NULL) %>% as_tibble()
  an<-names(a)

  an[!(an %in% keys)]<- paste0(an[!(an %in% keys)],lab)
  an
  names(a)<-an
  return(a)
}



a<-readDiet(diet="stomcon_list_Simple_0001_haul_as_observed.dat",lab='_a')
b<-readDiet(diet="stomcon_list_Simple_0001_haul_hb_as_observed.dat",lab='_b')


a<-readDiet(diet="stomcon_list_Simple_0001_haul_as_observed.dat",lab='_a')
b<-readDiet(diet="stomcon_list_Boots_0500_haul_hb_as_observed.dat",lab='_b')


#a<-readDiet(diet="stomcon_list_Boots_0020_haul_mu.dat",lab='_a')
#b<-readDiet(diet="stomcon_list_Boots_0020_haul_mu.dat",lab='_b')

ab<-left_join(a,b)
ab
ab[(ab$stomcon_a != ab$stomcon_b),]

aa<-ab %>%group_by(year,quarter,pred,pred.size,prey) %>% summarize(stom_a=sum(stomcon_a), stom_b=sum(stomcon_b))
aa

a3<-aa %>% filter(year==1991 & quarter==2 & pred=='COD') %>%mutate(ratio=stom_a/stom_b)
a3

round(xtabs(ratio ~ prey+pred.size,data=a3),2)

if (FALSE) {
  aa<-file.path("C:","_C_drev","SMS-git","Data_NorthSea","final_input_NS_2023")

  a<-read.table(file=file.path(aa,"stomcon_list_Boots_0500_haul_hb_as_observed.dat"),header=T)
  a0<-by(a,list(paste(formatC(a$pred.no,w=2,flag='0'),a$pred)),function(x)summary(x$phi))
  round(do.call(rbind,a0),1)

}
