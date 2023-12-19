split_hm<-function(x){
  hom<-subset(x,pred_name=='Trachurus trachurus')

  x<-subset(x,(pred_name=='Trachurus trachurus' & area %in% c(1,2,3)) | (pred_name!='Trachurus trachurus'))

  x[['PRED']]<-x[['PRED']] %>%
    mutate(pred_name=as.character(pred_name),pred_name=if_else(pred_name=='Trachurus trachurus',"W Trachurus trachurus",pred_name))

  hom<-subset(hom,area %in% c(2,4,5,6,7))
  hom[['PRED']]<-hom[['PRED']] %>% mutate(pred_name=as.character(pred_name),pred_name="N Trachurus trachurus")
  hom[['PRED']]$sample_id<-paste('N',hom[['PRED']]$sample_id)
  hom[['PREY']]$sample_id<-paste('N',hom[['PREY']]$sample_id)

  x<-c(x,hom)
  x[['PRED']]<-x[['PRED']] %>% mutate(pred_name=forcats::fct_na_value_to_level(pred_name))
  return(x)
}

