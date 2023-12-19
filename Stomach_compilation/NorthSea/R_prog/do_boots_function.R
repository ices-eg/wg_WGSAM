
# common function for stomach contents  compilation into diet
do_boots<-function(b,keep_prey,do_details=FALSE,testit=FALSE,sz='x') {

  control<-get_control(b)
  #sz<-get_option_string('size_class')



  if (testit) print(checkp(b[['PREY']]))

  ######### allocate partly identified prey species to known prey species
  #                                                            sample ( and by_prey_size=TRUE)
  b<-  redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=sample_id,dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,by_prey_size=TRUE, keep_size=TRUE, remains_to_other = FALSE)
 #b<-  redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=sample_id,dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,by_prey_size=TRUE, remains_to_other = FALSE)

    if (testit) print(checkp(b[['PREY']]))
  if (do_details) do_detailed_output(b,label='redist_unidentified_prey_sp sample',digits=2)



  #                                                            sub_area ( and by_prey_size=TRUE)
  b<-redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=stratum_sub_area,dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,by_prey_size=TRUE, keep_size=TRUE, remains_to_other = FALSE)
  #b<-redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=stratum_sub_area,dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,by_prey_size=TRUE, remains_to_other = FALSE)

    if (do_details) do_detailed_output(b,label='redist_unidentified_prey_sp 1',digits=2)
  if (testit) checkp(b[['PREY']])

    #                                                            sub_area ( and by_prey_size=FALSE)
  b<-redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=stratum_sub_area,dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to, by_prey_size=FALSE,  keep_size=TRUE, remains_to_other = FALSE)
 # b<-redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=stratum_sub_area,dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to, by_prey_size=FALSE,   remains_to_other = FALSE)

  if (do_details) do_detailed_output(b,label='redist_unidentified_prey_sp 1',digits=2)
  if (testit) print(checkp(b[['PREY']]))

  #                                                                 area ( and by_prey_size=TRUE)
  b<-redist_unidentified_prey_sp(s=b,dist_time=stratum_time,dist_area=stratum_area,dist_pred_size=pred_size, do_only=c(1,2,3),
                                 from_to_species=from_to, by_prey_size=TRUE,remains_to_other = TRUE)
  if (do_details) do_detailed_output(b,label='redist_unidentified_prey_sp 2',digits=1)
  if (testit) print(checkp(b[['PREY']]))

  if (FALSE) {
    #                                                 halfyear                                        area
    b<-redist_unidentified_prey_sp(s=b,dist_time=paste(year,ifelse(quarter %in% c(1,2),'S1','S2')),dist_area=stratum_area,dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,remains_to_other = FALSE)
    if (do_details) do_detailed_output(b,label='redist_unidentified_prey_sp 1',digits=3)

    #                                                 halfyear                                        area=all
    b<-redist_unidentified_prey_sp(s=b,dist_time=paste(year,ifelse(quarter %in% c(1,2),'S1','S2')),dist_area='All',dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,remains_to_other = TRUE)
    if (do_details) do_detailed_output(b,label='redist_unidentified_prey_sp 4',digits=1)
  }

  # a<-xtabs(prey_w~prey_name,data=b[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)

  b<-rename_sandeel_outside(b)


  if (FALSE) {
    do_detailed_output(b,label='redist_unidentified_prey_sp done',digits=1)
    a<-make_criteria_set(b) ## see what is requested for detailed output
    sink(attr(ns,'control')@detailed_tst_file,append=TRUE)
    print (select(a[['PRED']],-sample_id,-fish_id,-dataset,-pred_name,-rectangle))
    print (select(a[['PREY']],-sample_id,-fish_id))
    sink()
  }



  b<-edit_control(b,sel_preys=keep_prey)

  b<-group_prey_species(b, keep_prey_names=keep_prey,sum_other_food=TRUE)
  if (do_details) do_detailed_output(b,label='all non prey species to other done',digits=1)

  if (testit) print(checkp(b[['PREY']]))

  if (FALSE) {
    bb<-as.data.frame(b)
    by(bb,list(bb$pred_name),function(x){
    x<-droplevels(x)
    (round(xtabs(prey_w~prey_name+prey_size+pred_name,data=x)))
    })

    by(bb,list(bb$pred_name),function(x){
      x<-droplevels(x)
      round(xtabs(prey_w~prey_name+pred_name,data=x)/sum(x$prey_w)*100,1)
    })

    by(bb,list(bb$pred_size,bb$pred_name),function(x){
      x<-droplevels(x)
      a<-xtabs(prey_w~prey_name+prey_size+pred_name,data=x)
      round(a/sum(a)*100,1)
    })


    qq<-subset(data.frame(as.data.frame(b)),year==1987 & quarter==1 ,select=c(prey_name,prey_size_class,prey_size))
    #sort(unique(paste(qq$prey_size_class,as.character(qq$prey_size))))
  }

  # a<-xtabs(prey_w~prey_name,data=b[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)
  # a<-xtabs(prey_w~prey_size,data=b[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)
  # a<-xtabs(prey_w~prey_size+prey_name,data=b[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)

  ########################################

  ####  Prey length on missing length

  # round(xtabs(prey_w~prey_name+prey_size,data=b[['PREY']]))

  # b[['PRED']]
  # summary(b[['PRED']])

  b[['PREY']]<-filter(b[['PREY']],prey_w>0)

  if (testit) cat('\n####  Prey length on missing length\n')
  if (testit) checkp(b[['PREY']])

  b<-redist_unidentified_prey_lengths(s=b,dist_time=stratum_time,dist_area=stratum_area,dist_pred_size=pred_size,remains_to_other = FALSE)
  if (do_details)  do_detailed_output(b,label='redist_unidentified_prey_length within strata',digits=1)
  if (testit) print(checkp(b[['PREY']]))

  # x<-xtabs(prey_w~prey_name+prey_size,data=b[['PREY']]); round(x/sum(x,na.ram=TRUE)*100,2)

  #                                                                    within all area strata
  b<-redist_unidentified_prey_lengths(s=b,dist_time=stratum_time,dist_area='All',dist_pred_size=pred_size,remains_to_other = TRUE)
  if (do_details)  do_detailed_output(b,label='redist_unidentified_prey_length within all area strata',digits=1)
  if (testit) print(checkp(b[['PREY']]))

  #  within year and           within all strata  HUSK AT ÆNDRE remains_to_other to FALSE if this section is used
  if (FALSE) {
    b<-redist_unidentified_prey_lengths(s=b,dist_time=substr(stratum_time,1,4),dist_area='All',dist_pred_size=pred_size,remains_to_other = TRUE)
    if (do_details) do_detailed_output(b,label='redist_unidentified_prey_length within all area and year strata',digits=1)
    if (testit) print(checkp(b[['PREY']]))
  }
  # kan ikke bruges da size class er forskellig mellem årene         within all years and           withn all strata
  #b<-redist_unidentified_prey_lengths(s=b,dist_time='All',dist_area='All',dist_pred_size=pred_size,remains_to_other = TRUE)
  # if (do_details) do_detailed_output(b,label='redist_unidentified_prey_length nr within all area and year strata, and remains to other',digits=1)


  if (FALSE) { #  use neighbouring predator lengths
    ncut<-3
    unique(cut(as.numeric(substr(b[['PRED']]$pred_size,1,4)),ncut))
    b<-redist_unidentified_prey_lengths(s=b,dist_time='All',dist_area='All',dist_pred_size=cut(as.numeric(substr(pred_size,1,4)),ncut),remains_to_other = TRUE)
  }


  group<-unlist(b[['PRED']][1,'group'])
  #print(group)

  if (group=='Saithe') {
    b<-edit_control(b,
                    calc_sub_strata=list(  # how to calculate diet per rectangle
                      relative_weight=FALSE,       # transform into relative weight before data are compiled
                      weighting_factor=expression(n_tot),
                      # weighting_factor_file=file.path(develop_dir,'Data','W_fac_sub_strata.csv')
                      weighting_factor_file=NA
                    ),
                    calc_strata=list( # how to calculate diet per roundfish area from average stomach content by sub-strata (=rectangle)
                      relative_weight=FALSE,
                      weighting_factor=expression(n_tot),
                      #weighting_factor=expression(1),
                      # weighting_factor_file=file.path(develop_dir,'Data','W_fac_strata.csv')
                      weighting_factor_file=NA
                    ),
                    calc_total=list( # how to calculate diet per total area (in this case that is roundfish area 1)
                      relative_weight=FALSE,
                      weighting_factor=expression(1),
                      # weighting_factor_file=file.path(develop_dir,'Data','W_fac_total.csv')
                      weighting_factor_file=NA
                    )
    )
  }

  ####################################################################
  if (group=='otherRound') {

    s<-subset(b,!(pred_name=='HAD' & stratum_area %in% c("R-4","R-5")))

    #
    s<-edit_control(s,
                    calc_sub_strata=list(
                      relative_weight=FALSE,       # transform into relative weight before data are compiled
                      weighting_factor=expression(n_tot),
                      weighting_factor_file=NA
                    ),
                    calc_strata=list(
                      relative_weight=FALSE,
                      weighting_factor=expression(sqrt(mean_cpue)),
                      weighting_factor_file=NA
                    ),
                    calc_total=list(
                      relative_weight=FALSE,
                      weighting_factor=NA,
                      weighting_factor_file=file.path(config_dir,paste0('remains_weighting_total',sz,'.csv'))
                    )
    )

    # detailed control output
    s<-edit_control(s,
                    detailed_tst_output=FALSE,
                    detailed_tst_file=file.path(output_dir,'remains_diet_calc.txt'),
                    detailed_tst_criteria=list(
                      pred_size=expression(pred_name=='MAC'),
                      pred_size=expression(pred_size=='0300-0400'),
                      stratum_time=expression(stratum_time=='1981-Q3')
                    )
    )
    ###

    a<-make_template_strata_weighting(s,strata=c('sub_strata','strata','total')[3])

    #    xtabs(~stratum_time+pred_name,data=a)

    # weighting factor by ICES roundfish area and predator size  (from SAS Bootsmave)
    # header in file should be: year pred_name	quarter		stratum_area	size	area_size_fac

    fac <- read_csv(file.path(config_dir,"area_fac.csv"),col_types = cols()) %>%
      rename(year=Year,pred_name=Species, quarter=Quarter, stratum_area=area, size=size_class, area_size_fac=fac) %>%
      mutate(year=as.integer(year),quarter=as.integer(quarter),stratum_area=paste('R',stratum_area,sep='-')) %>%
      filter(pred_name !='SAI')
    # fc<-filter(fac,pred_name=='COD' & quarter==1 & year==1981 & size==500)
    # fc; sum(fc$area_size_fac)
    fac[is.na(fac$area_size_fac),'area_size_fac']<-0
    fac[fac$pred_name=='WHI','pred_name']<-'WHG'
    fac[fac$pred_name=='GRE','pred_name']<-'GUR'

    if (FALSE) {
      sort(unique(fac$pred_name)) # no saithe!

      tst<-filter(fac,year==1981 & quarter==1)
      ftable(round(tapply(tst$area_size_fac,list(tst$pred_name,tst$stratum_area,tst$size),sum)*1000))

      tst<-filter(fac,year==1986 & quarter==3)
      ftable(round(tapply(tst$area_size_fac,list(tst$pred_name,tst$stratum_area,tst$size),sum)*1000))

      tst<-filter(fac,pred_name=='RAJ' & quarter==4 & year==1991)
      ftable(round(tapply(tst$area_size_fac,list(tst$year,tst$stratum_area,tst$size),sum)*1000))

      tst<-filter(fac,pred_name=='MAC' & quarter==1)
      ftable(round(tapply(tst$area_size_fac,list(tst$year,tst$stratum_area,tst$size),sum)*1000))

      tst<-filter(fac,pred_name=='MAC' & quarter==3 &year==1981)
      ftable(round(tapply(tst$area_size_fac,list(tst$year,tst$stratum_area,tst$size),sum)*1000))
    }

    l<-read_csv(file.path(config_dir,paste0("length_classes_",sz,".csv")),col_types = cols())
    if (any(l$Species=='ALL')) by_species<-FALSE else by_species<-TRUE
    if (by_species) {
      l<- left_join(fac,l, by = c("pred_name"="Species","size"="l1","year"="year", "quarter"="quarter")) %>%
        filter(!is.na(group)) %>% select(year,quarter,pred_name,stratum_area,area_size_fac,group) %>%
        rename(pred_size=group)
    } else {
      l<-left_join(fac,l, by = c("size"="l1","year"="year", "quarter"="quarter")) %>%
        filter(!is.na(group)) %>% select(year,quarter,pred_name,stratum_area,area_size_fac,group) %>%
        rename(pred_size=group)
    }

    l<-mutate(l,stratum_time=paste0(year,'-Q',quarter),year=NULL,quarter=NULL)
    a<-mutate_if(a,is.factor,as.character)
    a<-left_join(a,l,by = c("pred_name", "stratum_time", "pred_size", "stratum_area")) %>%
      mutate_if(is.character,as.factor)

    mis_l<-droplevels(filter(a,is.na(area_size_fac)))
    #summary(mis_l)
    #xtabs(~stratum_time+pred_name,data=mis_l)

    a<-filter(a,!is.na(area_size_fac)) %>% mutate( w_fac_area=area_size_fac,area_size_fac=NULL)

    if (FALSE) {
      tst<-filter(a,pred_name=='MAC' & stratum_time=='1981-Q3')
      ftable(round(tapply(tst$w_fac_area,list(tst$stratum_area,tst$pred_size),sum)*1000))
      tst<-droplevels(tst)
      ftable(round(tapply(tst$w_fac_area,list(tst$stratum_area,tst$pred_size),sum)*1000))
    }

    write_csv(a,file=file.path(config_dir,paste0("remains_weighting_total",sz,".csv")))

    b<-s
  }


  #######################################################
  ###### Seals and porpoise
  # data for marine mammals are by lumped into one sampling station by year and quarter, such that the diet is the same as the the stomach contents
  if (group=='mammals') {

    #
    b<-edit_control(b,
                    calc_sub_strata=list(
                      relative_weight=FALSE,       # transform into relative weight before data are compiled
                      weighting_factor=expression(1),
                      weighting_factor_file=NA
                    ),
                    calc_strata=list(
                      relative_weight=FALSE,
                      weighting_factor=expression(1),
                      weighting_factor_file=NA
                    ),
                    calc_total=list(
                      relative_weight=FALSE,
                      weighting_factor=expression(1),
                      weighting_factor_file=NA
                    )
    )

    # detailed control output
    b<-edit_control(b,
                    detailed_tst_output=FALSE,
                    detailed_tst_file=file.path(output_dir,'mam_diet_calc.txt'),
                    detailed_tst_criteria=list(
                      pred_size=expression(pred_size=='1000-1200' | pred_size=='1000-1500' ),
                      pred_name=expression(pred_name=='GSE'),
                      stratum_time=expression(stratum_time=='1985-Q3')
                    )
    )

  }
  ################  horse mackerel #######################

  if (group=='horseMackerel') {
    ###### Horse mackerel
    # Data on Horse mackerel are divided into northern HM (H_N) and western HM (H_W)
    #
    #
    # The compilation first calculate the average stomach contents within a rectangle (sub_area) as a simple mean
    # the average of this rectangle mean is then used as population diet.
    #
    # With the new method this translates into
    # 1. weigthed average (by number of stomachs) by rectangle to get sub_strata (rectangel)  mean stomach contents
    # 2. weigthed average (by  mean cpue per rectancle)  by roundfish area  to get average stomach contents by strata (roundfish area)
    # 3. weigthed average  by area (size)  to get population diet

    #
    b<-edit_control(b,
                    calc_sub_strata=list(
                      relative_weight=FALSE,       # transform into relative weight before data are compiled
                      weighting_factor=expression(n_tot),
                      weighting_factor_file=NA
                    ),
                    calc_strata=list(
                      relative_weight=FALSE,
                      weighting_factor=expression(sqrt(mean_cpue)),
                      weighting_factor_file=NA
                    ),
                    calc_total=list(
                      relative_weight=FALSE,
                      weighting_factor=NA,
                      weighting_factor_file=file.path(config_dir,paste0('hom_weighting_total',sz,'.csv'))
                    )
    )
    a<-make_template_strata_weighting(b,strata=c('sub_strata','strata','total')[3])
    w<-read_csv(file.path(config_dir,"roundfish_weight.csv"),col_types = cols())
    a<-left_join(a,w,by = "stratum_area") %>% mutate(w_fac_area=area_fac, area_fac=NULL) # use the same factor for all pred sizes
    write_csv(a,file=file.path(config_dir,paste0('hom_weighting_total',sz,'.csv')))


  }


  ###########  end case specific raising
  #save(b,file='b.rData')

  d<-calc_population_stom(b)

  # summary(d[['PRED']])
  # to keep information on bootstrap replicate
  rep_id<-unlist(b[['PRED']][1,'rep_id'])
  d[['PRED']]$rep_id<-rep_id
  print(rep_id)

  return(d)

}
### end do-boot function  ################
