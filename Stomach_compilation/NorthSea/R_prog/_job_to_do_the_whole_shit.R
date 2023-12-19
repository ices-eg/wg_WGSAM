#pack_root<-file.path("C:","_C_drev")
#package_dir <- file.path(pack_root, "FishStomachs")


#Remove all objects
rm(list = ls())

source(file.path('c:',"_C_drev",'SMS-git','r_prog','init.r'))

### more paths ####
mv_root<-file.path("C:","_C_drev","Stomach_compilation")
NS_root<-file.path(mv_root,"NorthSea")
NorthSea<-file.path(NS_root,"Stom_2023")
porpoise<-file.path(NorthSea,'porpoise')

mammals<-file.path(NorthSea,"mammals")
R_prog<-file.path(NorthSea,'R_prog')
stom_dir<-file.path(mv_root,"Stomachs","NorthSea")
config_dir<-file.path(NorthSea,'config')
output_dir<- file.path(NorthSea,'output')
SAS_dir<- file.path(NorthSea,'from_SAS')
SMS_dir<- file.path(NorthSea,'SMS')
Rdata<-file.path(NorthSea,'Rdata')

DatrasDir<-file.path("C:","MV","Storage","DATRAS","All")

test_dir<-file.path(stom_dir,"FishStomachs_tests")


finalExchangeDir<-file.path('c:','_C_drev',"SMS-git","Data_NorthSea","final_input_NS_2023")
finalExchangeDirOld<-file.path('c:','_C_drev',"SMS-git","Data_NorthSea","final_input_NS_2020")

#setwd(finalExchangeDir)

#### Load libraries #####################################
library(FishStomachs)
library(tidyverse)
library(readxl)
# for nice output tables
#library(knitr)
#library(kableExtra)
#library(FishStomachs)


boot_sample_id_all<-c('sample_id','haul')
prey_prop_all<-c('as_observed','meanBoots','mu')
##################################################

do_it<-function(convert_data=FALSE,
                read_data=FALSE,
                adjust_harborPorpoise=FALSE,
                my_n_boot= 0, min_boots_for_stat=10,
                boot_sample_id=boot_sample_id_all[2],
                yfinal=2013,
                gse_north_south_sandeel=TRUE,
                make_plots=TRUE,
                make_plots_simple=FALSE)

 {

if (FALSE) { #test no bootstrap
  convert_data<-FALSE  # convert exchange format data (used in read_data.R)
  read_data<-FALSE   # read data from exchange format (or from previously made R data)
  adjust_harborPorpoise<-FALSE   # adjust HB stomachs from otolith mass (done in read_data.R)
  if (adjust_harborPorpoise) read_data<-TRUE
  # n_boot is set in species_info.csv
  my_n_boot<- 1   # no of bootstrap replicates
  min_boots_for_stat<-10  # minimum no of replicates to do bootstrap statistics
  boot_sample_id<-boot_sample_id_all[2]
  yfinal<-2013 # maximum year for all stomach data (needed for size classes of Hake and birds not included in data on exchange format)
  make_plots<-FALSE
  make_plots_simple<-FALSE
  make_plots<-FALSE
  gse_north_south_sandeel<-TRUE
}

if (FALSE) { #test bootstrap
    convert_data<-FALSE  # convert exchange format data (used in read_data.R)
    read_data<-FALSE   # read data from exchange format (or from previously made R data)
    adjust_harborPorpoise<-FALSE   # adjust HB stomachs from otolith mass (done in read_data.R)
    if (adjust_harborPorpoise) read_data<-TRUE
    # n_boot is set in species_info.csv
    my_n_boot<- 500   # no of bootstrap replicates
    min_boots_for_stat<-10  # minimum no of replicates to do bootstrap statistics
    boot_sample_id<-boot_sample_id_all[2]
    yfinal<-2013 # maximum year for all stomach data (needed for size classes of Hake and birds not included in data on exchange format)
    make_plots<-FALSE
    make_plots_simple<-FALSE
    make_plots<-FALSE
    gse_north_south_sandeel<-TRUE
  }

  if (adjust_harborPorpoise) read_data<-TRUE
  my_n_boot<-max(my_n_boot,1)

  do_diri<- (my_n_boot >=min_boots_for_stat)

  nb<-formatC(my_n_boot,width=4,flag='0')
  if (do_diri) labRdata<-'Boots' else labRdata<-'Simple'
  labRdata<-paste(labRdata,nb,boot_sample_id,sep='_')
  if (adjust_harborPorpoise) labRdata<-paste(labRdata,'hb',sep='_')
  labRdata


#delete_plots<-TRUE
if (make_plots | make_plots_simple) {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  oo2<-file.path(output_dir,labRdata)
  if (dir.exists(oo2)) unlink(oo2,recursive = TRUE)
  dir.create(oo2)
  output_dir<-oo2
}

birds<-c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ")


# list to organise compilation
compile_options=list(
  years=c(1981,1985,1986,1987,1990,1991,1995,2002,2005,2010,2013),
  split_sandeel=TRUE,
  truncate_sandeel_length=TRUE,
  split_horsemackerel=TRUE,
  bias_correct_energy_armarment=TRUE,
  size_class=c('l_1981','l_1991')[2],           # use the 1981 size classes for all years or use size classes by year
  species_dependent_size_class=FALSE            # make species dependent size classes (TRUE) or use the same length classes for all species (FALSE): You have to use FALSE for the North Sea, as the same sizes classes have not been used for all predators in a given year
)

clean_sink<-function(){
  if (sink.number()>0) for (i in (1:sink.number())) sink()
}


cleanup<-function(){for(i in dev.list()) if (names(dev.off())=='null device') break()}

check1<-function(x,show=FALSE) {
  ss1<-xtabs(prey_w~prey_name,data=x[['PREY']])
  xx<-as.data.frame(x)
  ss2<-xtabs(prey_w~prey_name,data=xx)
  if (show) {
    print(ss1)
    print(ss2)
  } else {
    if (sum(ss1-ss2)==0) print(sum(ss1-ss2)) else{
      print(ss1-ss2)
      print(sum(ss1-ss2))
    }
  }
}

check_pw<-function(x,show=FALSE) {
  ss1<-sum(x[['PREY']]$prey_w,na.rm=TRUE)
  xx<-as.data.frame(x)

  ss2<-xtabs(prey_w~pred_name,data=xx)
  print(ss2/1000)
  print(ss1-sum(xx$prey_w,na.rm=TRUE))
}



get_option_logical<-function(opt){
  if (is.null(compile_options[[opt]])) return(FALSE)
  if (is.logical(compile_options[[opt]])) return(compile_options[[opt]]) else return(FALSE)
}

get_option_string<<-function(opt){
  if (is.null(compile_options[[opt]])) return('something is wrong')
  if (is.character(compile_options[[opt]])) return(compile_options[[opt]]) else return('something is wrong2')
}

get_option_numeric<-function(opt){
  if (is.null(compile_options[[opt]])) return('something is wrong')
  if (is.numeric(compile_options[[opt]])) return(compile_options[[opt]]) else return('something is wrong2')
}


#################################################
info<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols())
sp<-filter(info,predator_sp | other_predator_sp | prey_sp )

my_sp<-(sp  %>% arrange(number))$species
my_pred_sp<-(filter(sp,predator_sp )  %>% arrange(number))$species
my_other_sp<- (filter(sp,other_predator_sp ) %>% arrange(number))$species
my_prey_sp<- (filter(sp,prey_sp ) %>% arrange(number))$species
my_sp

sms_sp<-(sp %>% arrange(number))$code
sms_pred_sp<<-(filter(sp,predator_sp  )  %>% arrange(number))$code
sms_other_sp<- (filter(sp,other_predator_sp ) %>% arrange(number))$code
sms_prey_sp<- (filter(sp,prey_sp ) %>% arrange(number))$code


get_length_used_in_BootsMave <-function() {
  lc<-read_csv(file.path(config_dir,'length_classes_bootsMave.csv'),col_types=cols())
  xtabs(~year_format+length,data=lc)

  yfinal<-max(max(control@years),yfinal)

  lc81<-filter(lc,year_format=='y81') %>% mutate(year_format=NULL)
  lc81<- as_tibble(expand.grid(l=lc81$length,Species=preds)) %>%
    mutate(y1=1981L,y2=1981L,q1=min(control@quarters),q2=max(control@quarters))

  lc85<-filter(lc,year_format=='y85') %>% mutate(year_format=NULL)
  lc85<- as_tibble(expand.grid(l=lc85$length,Species=preds)) %>%
    mutate(y1=1982L,y2=1989L,q1=min(control@quarters),q2=max(control@quarters))
  lc91<-filter(lc,year_format=='y91') %>% mutate(year_format=NULL)
  lc91<- as_tibble(expand.grid(l=lc91$length,Species=preds)) %>%
    mutate(y1=1990L,y2=yfinal,q1=min(control@quarters),q2=max(control@quarters))
  lc<-bind_rows(lc81,lc85,lc91)


  ftable(xtabs(~Species+paste(y1,y2,sep='-')+l,data=lc))
  ifile<-"length_classes_config_bootsMave.csv"
  write_csv(lc,file=file.path(config_dir,ifile))


  len_classes_in<-make_length_classess(inp_dir=config_dir,inp_file=ifile,out_file=NULL,write_output=FALSE,max_l=control@max_l,minus_one= 0)
  return(len_classes_in)
}


##################  North Sea ###########################################################################
if (read_data) source(file.path(R_prog,'read_data.R'),local=TRUE) else load(file=file.path(Rdata,'ns.Rdata'),verbose=TRUE)
# sort(unique(subset(ns,pred_name=="Halichoerus grypus")[['PREY']]$prey_name))

# roundfish area 1 to 7 only
ns<- subset(ns,area %in% c(1:7))

criteria<<-list(
  year=expression(year==1991),
  quarter=expression(quarter==2),
  pred_name=expression(pred_name=="Gadus morhua"),
  fish_id=expression(fish_id=="1921")
)

criteria<<-list(
  year=expression(year==1981),
  quarter=expression(quarter==2),
  pred_name=expression(pred_name=="Pollachius virens"),
  fish_id=expression(fish_id=="14")
)

# print(get_control(ns))

ns<-edit_control(ns,
                 detailed_tst_criteria=criteria,
                 detailed_tst_output=TRUE,
                 detailed_tst_file=file.path(output_dir,'ns.dat')
                 )

# print(get_control(ns),show="detailed_test_output")


a<-make_criteria_set(ns) ## see what is requested for detailed output

sink(attr(ns,'control')@detailed_tst_file)
print (select(a[['PRED']],-sample_id,-fish_id,-dataset,-record_type,-pred_name,-rectangle))
print (select(a[['PREY']],-sample_id,-fish_id))
if (dim(a[['PREY']])[[1]] > 10) print (as.data.frame(select(a[['PREY']],-sample_id,-fish_id)))
sink()

do_detailed_output(ns,to_screen=FALSE,label='after read_exchange',digits=1,by_sample_id = TRUE,write_criteria=TRUE)
do_detailed_output(ns,to_screen=FALSE,label='after read_exchange, relative weight',digits=1,by_sample_id = TRUE,rel_weight=TRUE)


###################### split horsemackerel into areas
if (get_option_logical('split_horsemackerel')) {
  levels(sort(unique(ns[['PRED']]$pred_name)))
  source(file.path(R_prog,'split_hm.R'));
  ns<-split_hm(ns)
  levels(sort(unique(ns[['PRED']]$pred_name)))
}

check1(ns)

# sort(unique(subset(ns,pred_name=="Halichoerus grypus")[['PREY']]$prey_name))


###################### split sandeel as prey into areas
if (get_option_logical('split_sandeel')) {
  source(file.path(R_prog,'split_sandeel.R'));
  ns<-split_sandeel(ns)
  if (gse_north_south_sandeel) ns<-split_sandeel_GSE(x=ns,splitD=north_south_GSE_quarter)
}

# sort(unique(as.character(subset(ns,pred_name=="Halichoerus grypus")[['PREY']]$prey_name)))
# summary(subset(ns,pred_name=="Halichoerus grypus")[['PREY']]$prey_name)

# a<-xtabs(prey_w~prey_name,data=ns[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)


###### use SMS codes for predators
info2<-info %>%  select(code,SMS_species) %>%
      mutate(species=factor(SMS_species,levels=levels(ns[['PRED']]$pred_name)))
levels(sort(unique(ns[['PRED']]$pred_name)))
ns[['PRED']] <- left_join(ns[['PRED']],info2,by=c('pred_name'='species')) %>% mutate(pred_name=fct_na_value_to_level(code),code=NULL,SMS_species=NULL)
levels(sort(unique(ns[['PRED']]$pred_name)))
# summary(ns)

criteria<-list(
  year=expression(year==1991),
  quarter=expression(quarter==2),
  pred_name=expression(pred_name=="COD"),
  fish_id=expression(fish_id=="1921")
)

criteria<-list(
  year=expression(year==1981),
  quarter=expression(quarter==2),
  pred_name=expression(pred_name=="POK"),
  fish_id=expression(fish_id=="14") #14
)

ns<-edit_control(ns,detailed_tst_criteria=criteria)
#make_criteria_set(ns) ## see what is requested for detailed output
do_detailed_output(ns,label='after change to SMS predator code',by_sample_id = TRUE)
do_detailed_output(ns,label='after change to SMS predator code, relative weight',by_sample_id = TRUE,rel_weight=TRUE)


############### add variables. Temporal and spatial strata
ns<-edit_control(ns,haul_id=expression(paste(country,ship,rectangle,year,quarter,month,day,time,station,haul,sep='_')))


#old_ns<-ns
##### Bias corrected, energy contents, armament etc
hour_per_quarter <-2190

if (get_option_logical('bias_correct_energy_armarment') ) {
  ns<-  bias_correct_energy_etc(ns,
                                nodc_group_file=file.path(config_dir,"consum_nodc.csv"),
                                param_file=file.path(config_dir,"parameters.csv"),
                                energy_file=file.path(config_dir,"energy_density_and_armament.csv"),
                                temperature_file=file.path(config_dir,"temperature.csv"),
                                hour_multiplier=hour_per_quarter,pred_length_multiplier=0.1)

  do_detailed_output(ns,label='After bias_correct_energy_armarment',by_sample_id = TRUE)
  do_detailed_output(ns,label='After bias_correct_energy_armarment, relative weight',by_sample_id = TRUE,rel_weight=TRUE)
}

# ##########  make size classes definitions from input


x<-ns[['PRED']]
max_l<- get_control(ns)@max_l
widthl<-4
tst<-select(x,year,quarter,pred_name,pred_ll,pred_lu) %>% distinct() %>%
  mutate(group=paste(formatC(pred_ll, width=widthl,flag='0'),formatC(pred_lu, width=widthl,flag='0'),sep='-')) %>%
  rename(l1=pred_ll,l2=pred_lu)

if (FALSE) by(ns[['PRED']],list(ns[['PRED']]$year),function(x) {
  x$size<- paste(formatC(x$pred_ll, width=4,flag='0'),formatC(x$pred_lu, width=4,flag='0'),sep='-')
  x<-droplevels(x)
  xtabs(~pred_name+size,data=x)
})


if (FALSE) by(ns[['PRED']],list(ns[['PRED']]$pred_name),function(x) {
  x$size<- paste(formatC(x$pred_ll, width=4,flag='0'),formatC(x$pred_lu, width=4,flag='0'),sep='-')
  x<-droplevels(x)
  xtabs(~year+size,data=x)
})


ns<-edit_control(ns,predators=sms_pred_sp)
control<-get_control(ns)

lc<-read_csv(file.path(config_dir,'length_classes.csv'),col_types=cols())
xtabs(~year_format+length,data=lc)

sz<<-get_option_string('size_class')
if (get_option_logical('species_dependent_size_class')) preds<-control@predators else preds='ALL'
sz;preds

yfinal<-max(max(control@years),yfinal)

if (sz=='l_1981') {
  lc<-filter(lc,year_format=='y81') %>% mutate(year_format=NULL)

  lc<- as_tibble(expand.grid(l=lc$length,Species=preds)) %>%
    mutate(y1=min(control@years),y2=yfinal,q1=1L,q2=4L)
} else if (sz=='l_1991') {
  lc81<-filter(lc,year_format=='y81') %>% mutate(year_format=NULL)
  lc81<- as_tibble(expand.grid(l=lc81$length,Species=preds)) %>%
    mutate(y1=1981L,y2=1981L,q1=min(control@quarters),q2=max(control@quarters))
  lc85<-filter(lc,year_format=='y85') %>% mutate(year_format=NULL)
  lc85<- as_tibble(expand.grid(l=lc85$length,Species=preds)) %>%
    mutate(y1=1982L,y2=1989L,q1=min(control@quarters),q2=max(control@quarters))
  lc91<-filter(lc,year_format=='y91') %>% mutate(year_format=NULL)
  lc91<- as_tibble(expand.grid(l=lc91$length,Species=preds)) %>%
    mutate(y1=1990L,y2=yfinal,q1=min(control@quarters),q2=max(control@quarters))
  lc<-bind_rows(lc81,lc85,lc91)
} else stop()

ftable(xtabs(~Species+paste(y1,y2,sep='-')+l,data=lc))

ifile<-paste0("length_classes_config_",sz,".csv")
ofile<-paste0("length_classes_",sz,".csv")
write_csv(lc,file=file.path(config_dir,ifile))

len_classes_in<-make_length_classess(inp_dir=config_dir,inp_file=ifile,out_file=ofile,write_output=TRUE,max_l=control@max_l,minus_one= 0)
if (FALSE) {
  filter(len_classes_in,year==1981 & quarter==1)
  filter(len_classes_in,year==1987 & quarter==1)
  filter(len_classes_in,year==1991 & quarter==1)
}
#tst<-subset(ns,pred_name=='MAC' & year==1981 & quarter==3); xtabs(~pred_ll,data=tst[['PRED']])


### put size classes on predators
ns<-put_size_class_on_predator(ns, len_classes=len_classes_in)

xtabs(~pred_size+year,data=ns[['PRED']])
do_detailed_output(ns,label='After put_size_class_on_predator',by_sample_id = TRUE)

tst<-subset(ns,pred_name=='MAC' & year==1981 & quarter==3); xtabs(~pred_size,data=tst[['PRED']])

#tst<-subset(ns,pred_name=='POK' & year==1981 & quarter==1); xtabs(~pred_size,data=tst[['PRED']])
#tst<-subset(ns,pred_name=='POK' & year==1987 & quarter==3); xtabs(~pred_size,data=tst[['PRED']])

##### size class on prey
ns<-put_size_class_on_prey(stom=ns,  len_classes=len_classes_in)
check1(ns)

do_detailed_output(ns,label='After put_size_class_on_prey',by_sample_id = TRUE)

check1(ns)
check_pw(ns)

a<-xtabs(prey_w~prey_name,data=ns[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)

if (FALSE){
  qq<-subset(data.frame(as.data.frame(ns)),year==1987 & quarter==1  & prey_lu < 9999,select=c(prey_name,year,quarter,prey_size_class,prey_l,prey_ll, prey_lu))
  sort(unique(paste(qq$prey_size_class,qq$prey_ll,qq$prey_lu)))
}



#### Bias corrected, regurgitated stomachs

ns<-bias_correct_regurgitated(ns,delete_just_regurgitated=TRUE,update_n_food_with_n_regur=TRUE,drop_variables=c('n_regur','n_skel','n_empty'))


a<-make_criteria_set(ns) ## see what is requested for detailed output
sink(attr(ns,'control')@detailed_tst_file,append=TRUE)
print (select(a[['PRED']],-sample_id,-fish_id,-dataset,-record_type,-pred_name,-rectangle))
clean_sink()

do_detailed_output(ns,label='After bias_correct_regurgitated',by_sample_id = TRUE)

check_pw(ns) # should be different from 0


####  group prey species

a<-info %>% filter(nodc>0 & prey_sp) %>%
     select(species,nodc)%>%distinct() %>%rename(species_group=species,First=nodc) %>% mutate(Last=First,named=TRUE)

b<-read_csv(file.path(config_dir,"other_food_nodc.csv"),col_types = cols()) %>%mutate(named=FALSE)

NODC_split<-bind_rows(a,b)
NODC_split

#filter(ns[['PREY']],prey_nodc>=9000000000)

ns<-group_prey_species(ns,NODC_split=NODC_split,show_allocations=FALSE )

do_detailed_output(ns,label='After group_prey_species',by_sample_id = TRUE)

# a<-xtabs(prey_w~prey_name,data=ns[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)

# summary(subset(ns,pred_name=="GSE")[['PREY']]$prey_name)
############################
#### aggregate stomachs contents within sample_id and predator and prey size classes
ns<-aggregate_within_sample(ns)



criteria<-list(
  year=expression(year==1991),
  quarter=expression(quarter==2),
  sample_id=expression(sample_id=="Gadus morhua_SCO_y:91_q:2_ID:1921"))

criteria<-list(
  year=expression(year==1981),
  quarter=expression(quarter==2),
  sample_id=expression(sample_id=="Pollachius virens_DEN_y:81_q:2_ID:14"))

ns<-edit_control(ns,detailed_tst_criteria=criteria)

do_detailed_output(ns,label='After aggregate_within_sample',by_sample_id=TRUE,write_criteria=TRUE)

a<-xtabs(prey_w~prey_name,data=ns[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)


############### add variables. Temporal and spatial strata
ns<-edit_control(ns,
                 strata_area_exp=expression(paste('R',area,sep='-')),
                 strata_sub_area_exp=expression(rectangle),
                 strata_time_exp=expression(paste0(year, "-Q", quarter))
                )

print(get_control(ns),show='stratification')

#check1(ns)
#check_pw(ns)

ns<-add_strata(ns)

# summary(subset(ns,pred_name=="GSE")[['PREY']]$prey_name)

do_detailed_output(ns,label='After add_strata',digits=1)

##  average CPUE per rectangle - used later on

cpue<- ns[['PRED']] %>% group_by(pred_name, pred_size,stratum_time, stratum_area, stratum_sub_area) %>%
   summarise(cpue=mean(pred_cpue)) %>% ungroup()

######### select data that will be used for SMS diet data


# %let cod_select=%str(if pred=8791030402 and (pred_l<100 or pred_l=1200 or (pred_l<120 and quarter in ('1','2')) or (quarter='4' and pred_l in (60,70))) then delete;);
# %let had_select=%str(if pred=8791031301 and (pred_l<100 or pred_l=0700 or (pred_l<100 and quarter='1') or (quarter='2' and pred_l<120) or (pred_l=50)) then delete;);
# %let sai_select=%str(if pred=8791030901 and pred_l<300 then delete;);
# %let whg_select=%str(if pred=8791031801 and (pred_l<100 or quarter in ('1','2') and pred_l <80) then delete;);


app<-list(
  COD=paste("pred_name!='COD' | (pred_name=='COD' & stratum_area %in% c('R-1','R-2','R-3','R-4','R-5','R-6','R-7') &",
            "(as.character(pred_size) >='0100-0120' | (as.character(pred_size) >='0150-0200' & quarter %in% c(1,2))) & as.character(pred_size) <'1200-9999' &",
            "(year %in% c(1981,1991) | (year==1990 & quarter==3) | (year %in% c(1985,1986,1987) & quarter %in% c(1,3))))"),

  WHG=paste("pred_name!='WHG' | (pred_name=='WHG' & stratum_area %in% c('R-1','R-2','R-3','R-4','R-5','R-6','R-7') &",
            "((as.character(pred_size) >='0100-0120' & quarter %in% c(1,2)) | (as.character(pred_size) >='0080-0100' & quarter %in% c(3,4))) &",
            "as.character(pred_size) <'0500-0600' &",
            "(year %in% c(1981,1991) | (year==1990 & quarter==3) | (year %in% c(1985,1986,1987) & quarter %in% c(1,3))))"),

  HAD=paste("pred_name!='HAD' | (pred_name=='HAD' & stratum_area %in% c('R-1','R-2','R-3','R-4','R-5','R-6','R-7') &",
            "((as.character(pred_size) >='0100-0120' & quarter %in% c(1,2)) | (as.character(pred_size) >='0120-0150' & quarter %in% c(3,4))) &",
            "as.character(pred_size) <'0700-1000' &",
            "(year %in% c(1981,1991) ))"),

  POK="pred_name!='POK' | (pred_name=='POK' & stratum_area=='R-1' & as.character(pred_size) >='0300-0350' & (year %in% c(1981,1991) | (year %in% c(1986,1987) & quarter==3)))",

  W_H="pred_name!='W_H' |  (year %in% c(1991) & as.character(pred_size) >='0250-0300' & quarter %in% c(3,4))",
  N_H="pred_name!='N_H' |  (year %in% c(1987,1991) & as.character(pred_size) >='0150-0200')"


)



#wm<-subset(ns,pred_name=='W_H' & year==1991  ); xtabs(n_tot~quarter+pred_size,data=wm[['PRED']])


#just cheking syntax
lapply(app,rlang::parse_expr )

compile_options<-  append(compile_options,app)

select_diet<-function(x,select,test=FALSE) {
  sel<-compile_options[[select]]
  x<-eval(rlang::parse_expr(paste("subset(x,",sel,")")))

  if (test) {
    tst<-filter(x[['PRED']],pred_name %in% select)
    print(xtabs(~pred_name+stratum_area,data=tst))
    print(xtabs(~pred_size+stratum_area,data=tst))
    print(xtabs(~pred_size+quarter,data=tst))
    print(xtabs(~year+quarter,data=tst))
  }
  return(x)
}
do.test<-TRUE
ns<-select_diet(ns,'COD',test=do.test)
ns<-select_diet(ns,'WHG',test=do.test)
ns<-select_diet(ns,'HAD',test=do.test)
ns<-select_diet(ns,'POK',test=do.test)
ns<-select_diet(ns,'W_H',test=do.test)
ns<-select_diet(ns,'N_H',test=do.test)

# a<-xtabs(prey_w~prey_name,data=ns[['PREY']]); a<-a/sum(a,na.rm=TRUE)*100; round(a,1)
# summary(subset(ns,pred_name=="GSE")[['PREY']]$prey_name)
ns<-aggregate_within_sample(ns)

#sort(unique(ns[['PREY']]$prey_name))
#names(ns[['PRED']])

# tabs(n_tot~year+pred_size,data=ns[['PRED']])
# xtabs(n_tot~paste(year,pred_name,sep=':')+pred_size,data=ns[['PRED']])
###################################################################################################################################
############## Bootstrapping #####################################

tst<- c('COD','HAD','POK')
tst<-c("GSE","GUR","HBP","N_H","RAJ","W_H","COD","HAD","MAC","POK","WHG")
#

s<-subset(ns,pred_name %in% tst )


#s[['PRED']] %>% select(pred_name,year,quarter) %>% unique()

boot<-info%>% select(number,code,SMS_species,n_boot,group_boot)


boot<-filter(info,predator_sp & code %in% tst) %>% select(number,code,SMS_species,n_boot,group_boot)
sp<-as.character(unique(s[['PRED']]$pred_name))
sp
boot$include<-boot$code %in% sp
boot

boot[boot$n_boot>1,'n_boot']<- my_n_boot


if (!all(boot$include)) {
  print(boot)
  stop("not the same species in boot and STOMobs")
}


############### add variables for bootstrapping

if (boot_sample_id==boot_sample_id_all[1] ){
   s<-edit_control(s, bootstrapping = list(
    boots_id = expression(sample_id),
    boots_strata = expression(paste(year,quarter,pred_name,sep='-'))
  ))
}
if (boot_sample_id==boot_sample_id_all[2] ){
  s<-edit_control(s, bootstrapping = list(
    boots_id = expression(paste(ship,rectangle,year,quarter,station,haul,sep='_')),
    #boots_id = expression(sample_id),
    boots_strata = expression(paste(year,quarter,pred_name,sep='-'))
  ))
}

s<-edit_control(s,detailed_tst_output=FALSE)
print(get_control(s),'detailed_test_output')


print(get_control(s),'bootstrapping')
s<-bootstrap_addVar(s)
#s

group_boot<-unique(boot$group_boot)
group_boot
boot

#s[['PRED']] %>% select(year,quarter,month,day,time,station,haul,sample_id,fish_id,boots_id)


a<-lapply(group_boot,function(group){
 # test group<-group_boot[2]
  b<-filter(boot,group_boot==group)
  ss<-subset(s,pred_name %in% b$code)
  print(ss)
  n_boot<-1:unique(b$n_boot)
  cat(group, b$code,n_boot,'\n')
  bd<-lapply(n_boot,function(x) {y <-bootstrap_data(ss,seed=x,rep_id=x); y[['PRED']]$group<-group; return(y)})
  return(bd)
})
names(a)<-group_boot


from_to <<- make_from_to_species(inp_file=file.path(config_dir,'from_to_species.csv'))
#from_to<-filter(from_to,from_species != "unknown")


#xtabs(~pred_size+year,data=a[[1]][[1]][['PRED']])

# delete not used variables to reduce size
compresBoot<-function(s) {
  s[['PRED']]<-s[['PRED']] %>% dplyr::select(sample_id, pred_name,  year, quarter,  n_food,  pred_size_class,  pred_size,
                                             stratum_time, area,stratum_area, stratum_sub_area, n_tot, pred_l_mean,
                                             pred_cpue,fish_id,boots_id,boots_strata,rep_id, n_boot,group )
  return(s)
}

# summary(subset(a[['mammals']][[1]],pred_name=="GSE")[['PREY']]$prey_name)

a<-lapply(a,function(x1){
  lapply(x1,function(x2) compresBoot(s=x2))
})

# summary(subset(a[['mammals']][[1]],pred_name=="GSE")[['PREY']]$prey_name)
#sort(unique(a[['mammals']][[1]][['PRED']]$sample_id))
source(file.path(R_prog,'split_sandeel.R'));

# all non prey species to other
keep_prey<<-(read_csv(file.path(config_dir,"species_info.csv"),col_types = cols()) %>% filter(prey_sp) %>%select(SMS_species))$SMS_species

# function to do the raising of stomach contents to diet
source(file.path(R_prog,'do_boots_function.R'));

checkp<-function(x) {
  a<-xtabs(prey_w~prey_name+prey_size_class,data=x)
  a<-cbind(a,all=rowSums(a))
  a<-rbind(a,all=colSums(a))
  round(a)
}

# checkp(a[[1]][[1]][['PREY']]) # unknown is there!

#xtabs(~pred_size+year,data=a[[1]][[1]][['PRED']])

# the real thing, takes time
aa<-lapply(a,function(x1){
  lapply(x1,function(x2) do_boots(b=x2,keep_prey,sz=sz,testit=FALSE))
})


# summary(subset(aa[['mammals']][[1]],pred_name=="GSE")[['PREY']]$prey_name)

##

if (FALSE) {
  xtabs(~pred_size+stratum_time,data=aa[['otherRound']][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=aa[["horseMackerel"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=aa[["mammals"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=aa[["Saithe"]][[1]][['PRED']])
}


# post-processsing as done in SAS, Bootsmave, length_stom_con1.sas
source(file.path(R_prog,'manual_edit.R'));

# tst   x<-aa[[1]][[1]]

aaa<-lapply(aa,function(x1){
  lapply(x1,function(x2) post_process(x2))
})

# aaa[[2]][[1]][['PRED']]

if (FALSE) {
  xtabs(~pred_size+stratum_time,data=aaa[['otherRound']][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=aaa[["horseMackerel"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=aaa[["mammals"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=aaa[["Saithe"]][[1]][['PRED']])
}

diet<-lapply(aaa,function(x1){
  lapply(x1,function(x2) from_to_species_diet(x2,pred_from_to=c("code","short"),prey_from_to=c("SMS_species","short"),sp_info_file=file.path(config_dir,'species_info.csv')))
})


if (FALSE) {
  xtabs(~pred_size+stratum_time,data=diet[['otherRound']][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=diet[["horseMackerel"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=diet[["mammals"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=diet[["Saithe"]][[1]][['PRED']])
}

save(diet,compile_options,file=file.path(Rdata,paste0('diet_',labRdata,'.Rdata')))
######################################################


# load(file=file.path(Rdata,paste0('diet_',labRdata,'.Rdata')),verbose=TRUE)
# gse<- diet[['mammals']][[1]] %>% subset(pred_name=='Grey seal' & stratum_time=='2010-Q1');  gse; gse[['PREY']]

# overview
pl<-lapply(diet,function(x){
  cat('\n\n##########################\n')
  print(x[[1]])
  print(get_control(x[[1]]),show=c('bootstrapping','calc_diet'))
})

### plot diet data

if (make_plots_simple) {
  #tst<-diet[[1]][[1]];  plot(tst,show_plot=TRUE,addNstom=TRUE,cut_pred_size=c(1,4),addTitle = TRUE,Ncol=1)

  pl<-lapply(diet,function(x){
    plot(x[[1]],show_plot=FALSE,addNstom=TRUE,cut_pred_size=c(1,4),addTitle = TRUE,Ncol=1)
  })
  #pl[[1]]
  #pl[[3]][[1]]

  out<-pl %>% purrr::discard(rlang::is_null)
  out<-lapply(out,function(x) purrr::discard(x,rlang::is_null))
 out[[1]][[1]]
  # plot (with no prey size) the first (not bootstrapped) data
  ppl<-lapply(out,function(x){
    lapply(x,function(x1){
      dat<-dplyr::mutate_if(x1$data,is.factor,as.character)
      pred_name<-dat[1,'pred_name']
      year<-dat[1,'year']
      cat(paste(pred_name,year),'\n')
      if (length(unique(dat$quarter)) >=4) h<-1000 else h<-600
      png(filename=file.path(output_dir,paste0(pred_name,'_',year,'.png')),width=700,height=h,pointsize=35)
      print(x1)
      cleanup()
    })
  })
}



###  bootstrap statistics
if (FALSE) {
  tst<-diet[[1]]
  str(tst,1)

  tst[[1]][['PRED']]
  print(get_control(tst[[1]]),show=c('model_options','constants','bootstrapping'))
}

#add_missing_boots(bt=tst)
#diet_relative(tst[[1]])

diet2<-lapply(diet,function(x) add_missing_boots(bt=x))


# from absolute to relative stomach contents
diet2<-lapply(diet2,function(x1) lapply(x1,function(x2) diet_relative(x2)))

#special combination where the Dirichlet parameters cannot be estimated (no converge,)
excl_diri<-data.frame(
  year=      c(1985L, 1991L, 1981L, 1981L),
  quarter=   c(1L,       2L,     4L,  1L),
  pred_name=c('Cod', 'Mackerel',"Cod","Whiting"),
  pred_size= c('0700-0800','0250-0300',"0250-0300","0100-0150"))
excl_diri



# test
if (FALSE) {
  tst<-diet2[[1]]
  summary(tst[[1]][['PREY']])
  summary(tst[[1]][['PRED']])
  sort(unique(tst[[1]][['PRED']]$stratum_time))
  subset(tst[[1]][['PRED']],stratum_time=='1990-Q3' )
  bb<-bootsMean(b=tst,pointEst=tst[[1]],by_prey_size=FALSE,do_Diri=do_diri, excl_diri=excl_diri, Diri_min=get_control(tst[[1]])@model_options$min_stom,verbose=TRUE)
  head(bb)
}


diri<-lapply(diet2,function(x) bootsMean(b=x,pointEst=x[[1]],by_prey_size=FALSE,do_Diri = do_diri,excl_diri=excl_diri,Diri_min=get_control(x[[1]])@model_options$min_stom,Diri_max=0.97,verbose=FALSE))
diri

mean_size<-lapply(diet2,function(x) bootsMean(b=x,pointEst=x[[1]],by_prey_size=TRUE,do_Diri = FALSE,Diri_min=get_control(x[[1]])@model_options$min_stom,Diri_max=0.97,verbose=FALSE))
mean_size
mean_size_diri<-lapply(diet2,function(x) bootsMean(b=x,pointEst=x[[1]],by_prey_size=TRUE,do_Diri = TRUE,Diri_min=get_control(x[[1]])@model_options$min_stom,Diri_max=0.97,verbose=FALSE))



save(diet2,diri,mean_size,mean_size_diri,compile_options,file=file.path(Rdata,paste0('diet2_',labRdata,'.Rdata')))
######################################################

# load(file=file.path(Rdata,paste0('diet2_',labRdata,'.Rdata')),verbose=TRUE)

# gse<- diet[['mammals']][[1]] %>% subset(pred_name=='Grey seal' & stratum_time=='2010-Q1');  gse; gse[['PREY']]
# gse<- diet2[['mammals']][[1]] %>% subset(pred_name=='Grey seal' & stratum_time=='2010-Q1');  gse; gse[['PREY']]
if (make_plots & do_diri) {
  d<-do.call(rbind,diri)
  #m<-do.call(rbind,mean_size)
  md<-do.call(rbind,mean_size_diri) %>% filter(!(pred_name %in% c('Grey seal', 'H. porpoise','N.horse mac','W.horse mac'))) %>%
    arrange(year,quarter,pred_name,pred_size,prey_name,prey_size) %>% mutate(prey=NULL) %>% rename(prey=prey_name,pred=pred_name)
  unique(md$pred)

  tst<-filter(md,!is.na(mu) &  !is.na(prey_w) &  (pred!='A.radiata') )
  tsto<-tst %>% filter(prey=='Other')

  x<-ggplot(tsto, aes(x=mu, y=mean_w, group=pred)) +
       geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+ggtitle('Other food')+

  #png(filename=file.path(output_dir,'bias_01.png'),width=800,height=1000,pointsize=35)
   png(filename=file.path(output_dir,'bias_01.png'),height=600)
  print(x);cleanup()

  x<-ggplot(tsto, aes(x=prey_w, y=mu, group=pred)) +
    geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+ggtitle('Other food')+
    labs(x = "non bootstrap weight proportion",y="proportion from alpha",color='Predator',shape='Predator')
  png(filename=file.path(output_dir,'bias_02.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()

  x<-ggplot(tsto, aes(x=prey_w, y=mu)) +
    geom_point()+
    geom_abline(col='red')+ggtitle('Other food')+
    facet_wrap(~pred,scales='free')+
    labs(x = "non bootstrap weight proportion",y="proportion from alpha")
  png(filename=file.path(output_dir,'bias_02b.png'),width=600,height=700,pointsize=25)
  print(x);cleanup()




  x<-ggplot(tsto, aes(x=mu, y=prey_w)) +
    geom_point()+ggtitle('Other food')+
    geom_smooth(col='blue',se=FALSE)+
        geom_abline(col='red')+facet_wrap(~pred)+
    labs(x = "non bootstrap weight proportion",y="Weight proportion from alpha")+
  png(filename=file.path(output_dir,'bias_03.png'),width=600,height=600,pointsize=25)
  print(x);cleanup()


  x<-ggplot(tsto, aes(x=prey_w, y=mean_w)) +
    geom_point()+

    geom_abline(col='red')+facet_wrap(~pred)+ggtitle('Other food')
  png(filename=file.path(output_dir,'bias_04.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()


  x<-ggplot(tsto, aes(x=mu, y=prey_w)) +
    geom_point()+ggtitle('Other food')+
    geom_abline(col='red')+facet_wrap(~pred) +
  labs(x = "non bootstrap weight proportion",y="Weight proportion from alpha")+
  png(filename=file.path(output_dir,'bias_05.png'),width=600,height=600,pointsize=25)
  print(x);cleanup()


  x<-ggplot(tst, aes(x=mu, y=mean_w)) +
    geom_point(aes(shape=prey, color=prey))+
    geom_abline(col='red')+facet_wrap(~pred)
  png(filename=file.path(output_dir,'bias_06.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()

  x<-ggplot(filter(tst,mean_w<0.1 & prey !='Other'), aes(x=mu, y=mean_w)) +
    geom_point(aes(shape=prey, color=prey))+
    geom_abline(col='red')+facet_wrap(~pred)
  png(filename=file.path(output_dir,'bias_07.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()


  x<-ggplot(filter(tst,mean_w<0.2 & prey !='Other'), aes(x=mu, y=mean_w)) +
    geom_point(aes(shape=prey, color=prey))+
    geom_abline(col='red')+facet_wrap(~pred)
  png(filename=file.path(output_dir,'bias_08.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()


  x<-ggplot(filter(tst,mean_w<0.1 & prey !='Other'), aes(x=mu, y=mean_w)) +
    geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+facet_wrap(~prey)
  png(filename=file.path(output_dir,'bias_09.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()


  x<-ggplot(filter(tst,mean_w<0.1 & prey !='Other'), aes(x=prey_w, y=mean_w)) +
    geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+facet_wrap(~prey)
  png(filename=file.path(output_dir,'bias_10.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()

  summary(glm(prey_w~mu+pred,data=filter(tsto,!(is.na(prey_w) | is.na(mu)))))

  x<-ggplot(filter(tst,prey_w<0.2 & prey !='Other' ), aes(x=prey_w, y=mu)) +
    geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+facet_wrap(~prey)+
    labs(x = "non bootstrap weight proportion",y="proportion from alpha",color='Predator',shape='Predator')
  png(filename=file.path(output_dir,'bias_11.png'),width=700,height=700,pointsize=25)
  print(x);cleanup()

  x<- ggplot(tst, aes(x=prey_w, y=mu)) +
    geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+facet_wrap(~prey, scales='free')+
    labs(x = "non bootstrap weight proportion",y="proportion from alpha",color='Predator',shape='Predator')
  png(filename=file.path(output_dir,'bias_11b.png'),width=700,height=700,pointsize=25)
  print(x);cleanup()



  x<-ggplot(filter(tst,mean_w<0.2 & prey !='Other'), aes(x=prey_w, y=mu)) +
    geom_point(aes(shape=pred, color=pred))+
    geom_abline(col='red')+facet_wrap(~prey)
  png(filename=file.path(output_dir,'bias_12.png'),width=800,height=1000,pointsize=25)
  print(x);cleanup()



  by(tst,list(tst$pred),function(x) {
    predator<-as.character(as.data.frame(x[1,'pred'])$pred)
  x<-ggplot(filter(x,prey_w<0.2 & prey !='Other'), aes(x=prey_w, y=mu)) +
    geom_point()+ggtitle(predator)+
    geom_smooth(col='blue',se=FALSE)+
    geom_abline(col='red',lwd=1)+facet_wrap(~prey)+
    labs(x = "non bootstrap weight proportion",y="proportion from alpha")
    png(filename=file.path(output_dir,paste0('bias_15_0_2',predator,'.png')),width=800,height=1000,pointsize=25)
  print(x);
  cleanup()
  })


  by(tst,list(tst$pred),function(x) {
    predator<-as.character(as.data.frame(x[1,'pred'])$pred)
    x<-ggplot(filter(x,  prey !='Other'), aes(x=prey_w, y=mu)) +
      geom_point()+ggtitle(predator)+
      geom_smooth(col='blue',se=FALSE)+
      geom_abline(col='red',lwd=1)+facet_wrap(~prey)+
      xlim(0,0.05)+ylim(0,0.075)+
      labs(x = "non bootstrap weight proportion",y="proportion from alpha")

    png(filename=file.path(output_dir,paste0('bias_15_0_05',predator,'.png')),width=800,height=800)
    print(x);
    cleanup()
  })



  x<-ggplot(filter(tst,mean_w<0.2 & prey !='Other' & pred=='Cod'), aes(x=prey_w, y=mu)) +
    geom_point()+ggtitle('xxx')+
    geom_smooth(col='blue',se=FALSE)+
    geom_abline(col='red',lwd=1)+facet_wrap(~prey)
  png(filename=file.path(output_dir,'bias_15.png'))
  print(x);
  cleanup()


}


if (FALSE) {
  xtabs(~pred_size+stratum_time,data=diet2[['otherRound']][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=diet2[["horseMackerel"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=diet2[["mammals"]][[1]][['PRED']])
  xtabs(~pred_size+stratum_time,data=diet2[["Saithe"]][[1]][['PRED']])
}


##### mean and variance
if (FALSE) {
  #without size
  a<-do.call(rbind,diri) %>%  mutate(phi=NULL,   param=NULL, mu=NULL,    p_value=NULL,cv_w= sd_w/mean_w) %>%
    filter(!(pred_name %in% c("Grey seal","H. porpoise")))
  a %>% arrange(desc(cv_w))
  a<-do.call(rbind,diri) %>%  mutate(cv_w= sd_w/mean_w) %>%
    filter(!(pred_name %in% c("Grey seal","H. porpoise")))
  a %>% arrange(desc(cv_w))

  #with size
  a<-do.call(rbind,mean_size) %>%  mutate(phi=NULL,   param=NULL, mu=NULL,    p_value=NULL,cv_w= sd_w/mean_w) %>%
     filter(!(pred_name %in% c("Grey seal","H. porpoise")))
  a %>% arrange(desc(cv_w))
}

###  bootstrap plotting  ##
if (make_plots & do_diri) {

  # plotboots(b=diet2[[1]],show_plot=TRUE,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90)

  # plot (with no prey size) bootstrapped data
  bpl<-lapply(diet2,function(x){
    plotboots(b=x,show_plot=FALSE,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90)
  })


  bpl2<-lapply(bpl,function(x) purrr::discard(x,rlang::is_null))

  if (FALSE) {
    str(bpl2[['horseMackerel']],1)
    lapply(bpl2[["horseMackerel"]],function(x) head(x$data))
    bpl2[["horseMackerel"]][[2]]$data
    diet2[["horseMackerel"]][[2]]$data
    filter(diet2[["horseMackerel"]][[4]][['PRED']],key=="all_W_H_1991-Q1_0300-0350")
  }

  bppl<-lapply(bpl2,function(x){
    lapply(x,function(x1){
      dat<-x1$data[1,] %>% dplyr::mutate_if(is.factor,as.character)
      pred_name<-dat[1,'pred_name']
      strat<-dat[1,'stratum_time']
      cat(paste(pred_name,strat),'\n')
      png(filename=file.path(output_dir,paste0('boot_',pred_name,'_',strat,'.png')),width=800,height=1000,pointsize=25)
      print(x1)
      cleanup()
    })
  })
}

dd<-do.call(rbind,diri)

incl<-dd %>% select( year, quarter, pred_name, pred_size, phi, n_stom, n_sample,nboots) %>%  unique() %>% arrange( pred_name, pred_size, year, quarter)
incl
incl<-incl %>% filter(!is.na(phi) & !is.na(n_sample))

incl<-incl %>% mutate(stratum_time=paste0(year,'-Q',quarter))
pphi<-incl

if (FALSE) {
  junk<-by(pphi,pphi$pred_name,function(x) {
    b<-round(xtabs(phi~pred_size+stratum_time,data=x),1)
  })
  junk
  pphi %>% group_by(pred_name) %>% summarize(maxPhi=max(phi))
}

incl<- incl %>%select( pred_name, pred_size,stratum_time)
incl

# plot (with no prey size) bootstrapped data and statistics
if (make_plots  & do_diri) {

  bpl<-lapply(diet2,function(x){
  plotboots(b=x,show_plot=FALSE,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90,statistics=dd,inclData=incl)
 })

 bppl<-lapply(bpl,function(x){
    lapply(x,function(x1){
      if (!is.null(x1$data)) {
        dat<-x1$data[1,] %>% dplyr::mutate_if(is.factor,as.character)
        pred_name<-dat[1,'pred_name']
        strat<-dat[1,'stratum_time']
        cat(paste(pred_name,strat),'\n')
        png(filename=file.path(output_dir,paste0('boot_stat_',pred_name,'_',strat,'.png')),width=800,height=1000,pointsize=25)
        print(x1)
        cleanup()
      }
    })
  })

}

#diet2[[1]][[1]][['PRED']]

if (make_plots) {
  #select combinations
  myInc<-expand.grid(pred_name='Cod', pred_size=c("0350-0400","0400-0500","0500-0600"),stratum_time=c('1991-Q1','1991-Q2','1991-Q3','1991-Q4')) %>%distinct()
  myInc
  # plot (with no prey size) bootstrapped data and statistics
  bpl<-lapply(diet2,function(x){
    plotboots(b=x,show_plot=FALSE,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90,statistics=dd,inclData=myInc)
  })

  bppl<-lapply(bpl,function(x){
    lapply(x,function(x1){
      if (!is.null(x1$data)) {
        dat<-x1$data[1,] %>% dplyr::mutate_if(is.factor,as.character)
        pred_name<-dat[1,'pred_name']
        strat<-dat[1,'stratum_time']
        pred_size<-substr(dat[1,"pred_size"],1,4)
        fname<-file.path(output_dir,paste0('boot_stat_',pred_name,'_',strat,'_',pred_size,'.png'))
        cat(paste(pred_name,strat,fname),'\n')
        png(filename=fname,width=800,height=1000,pointsize=25)
        print(x1)
        cleanup()
      }
    })
  })

  # with prey size information
  myInc
  #plotboots.size(b=diet2,show_plot=TRUE,cut_pred_size=c(1,10),cut_prey_size=c(1,10),addTitle=TRUE,tAngle=90, Colours='red',maxbins=50,inclData=myInc)

  bpl<-lapply(diet2,function(x){
    plotboots.size(b=x,show_plot=FALSE,cut_pred_size=c(1,10),cut_prey_size=c(1,10),addTitle=TRUE,tAngle=90,inclData=myInc)
  })

  bppl<-lapply(bpl,function(x){
    lapply(x,function(x1){
      if (!is.null(x1$data)) {
        dat<-x1$data[1,] %>% dplyr::mutate_if(is.factor,as.character)
        pred_name<-dat[1,'pred_name']
        strat<-dat[1,'stratum_time']
        pred_size<-dat[1,"pred_size"]
        cat(paste(pred_name,strat),'\n')
        png(filename=file.path(output_dir,paste0('boot_stat_size_',pred_name,'_',strat,'_',pred_size,'.png')),width=800,height=1000,pointsize=25)
        print(x1)
        cleanup()
      }
    })
  })
}


################################################
# prepare for output for SMS file stomcon_list.dat
#

if (do_diri) {
  load(file=file.path(Rdata,paste0('diet2_',labRdata,'.Rdata')),,verbose=TRUE)

  # gse<- diet[['mammals']][[1]] %>% subset(pred_name=='Grey seal' & stratum_time=='2010-Q1');    gse; gse[['PREY']]
  # gse<- diet2[['mammals']][[1]] %>% subset(pred_name=='Grey seal' & stratum_time=='2010-Q1');    gse; gse[['PREY']]

  temp<-diet2[[1]][[1]]
  #sort(unique(temp[['PRED']]$pred_name))
  control<-attr(temp,'control')

  # extract the first object, which contains the non-bootstrapped data
  d<-lapply(diet2,function(x){x[[1]]})
  d<-do.call(c,d)
  d<-as.data.frame(d) %>%   dplyr::mutate_if(is.factor,as.character) %>% mutate(rep_id=NULL)

   #add phi at prey level (not prey size level)
  phi<-do.call(rbind,diri)
  phi$stratum_time <- with(phi, eval(control@strata_time_exp))
  phi<-phi %>% select(stratum_time,pred_name,pred_size,prey_name,phi,n_prey_sp) %>%
    arrange(stratum_time,pred_name,pred_size,prey_name)
  phi;d
  d<-left_join(d,phi,by = join_by(stratum_time, pred_size, pred_name, prey_name))

  # add prey proportions from mean of bootstrapped (mean_w), from (alfa/alpfa0=mu from dirichlet at prey size level)
  a<-do.call(rbind,mean_size_diri) %>% select(year,quarter,pred_name,pred_size,prey_name,prey_size,mu,mean_w)
  a$stratum_time <- with(a, eval(control@strata_time_exp))
  a<-a%>% mutate(year=NULL,quarter=NULL) %>% dplyr::mutate_if(is.factor,as.character) %>%arrange(stratum_time,pred_name,pred_size,prey_name,prey_size)

  d<-left_join(d,a,by = join_by(stratum_time, pred_size, pred_name, prey_name,prey_size))
  d<-d %>% mutate(obs_w=prey_w)


  pred_names<-c("stratum_area","stratum_time","pred_name","pred_size","pred_size_class","pred_l_mean","n_tot","n_sample","n_prey_sp","phi","key")
  prey_names<-c("key","prey_name", "prey_size","prey_size_class","prey_w","obs_w","mean_w","mu")
  all_names<-c(pred_names,prey_names[-1])
  aa<- d %>% select( tidyselect::all_of(all_names))
  dim(aa)
  attr(aa,'PRED')<-pred_names
  attr(aa,'PREY')<-prey_names

  aa<-aa %>% dplyr::mutate_if(is.character,as.factor)
  diet_boots<-as_STOMdiet(aa)
  #diet_boots[['PRED']]
  #diet_boots[['PREY']]
  diet_boots

  diet_boots<-diet_relative(diet_boots)

  if (FALSE) {
    diet_boots[['PRED']]
    a<-xtabs(n_tot~stratum_time+pred_name,data=diet_boots[['PRED']])
    a<-rbind(a,all=colSums(a))
    a<-cbind(a,all=rowSums(a))
    a
  }
  d2<-diet_boots
}
if (!do_diri) {
  load(file=file.path(Rdata,paste0('diet_',labRdata,'.Rdata')),verbose=TRUE)
  # subtract the first replicate (not bootstrapped)
  d2<-lapply(diet,function(x) x[[1]])
  diet<-do.call(c,d2) #combine STOMdiet objects
  diet<-diet_relative(diet)
  if (FALSE) {
    a<-xtabs(n_tot~stratum_time+pred_name,data=diet[['PRED']])
    a<-rbind(a,all=colSums(a))
    a<-cbind(a,all=rowSums(a))
    a
  }
  d2<-diet
}

control<-get_control(d2)
sz<<-get_option_string('size_class')
#rename to SMS codes
d2<-from_to_species_diet(d2,pred_from_to=c("short","code"),prey_from_to=c("short","code"),sp_info_file=file.path(config_dir,'species_info.csv'),refactor=TRUE)

if (FALSE) {
  a<-xtabs(n_tot~stratum_time+pred_name,data=d2[['PRED']])
  a<-rbind(a,all=colSums(a))
  a<-cbind(a,all=rowSums(a))
  a
  xtabs(~pred_name+pred_size,data=d2[['PRED']])
}

d2[['PRED']][d2[['PRED']]$pred_name %in% c("HBP"),'n_sample']<-100
d2[['PRED']][d2[['PRED']]$pred_name %in% c("HBP"),'n_tot']<-100

d2[['PRED']][d2[['PRED']]$pred_name %in% c("GSE"),'n_sample'] <-d2[['PRED']][d2[['PRED']]$pred_name %in% c("GSE"),'n_tot']


d2<-diet_relative(d2)


save(d2,sz,get_length_used_in_BootsMave,preds,birds,compile_options,file=file.path(Rdata,paste0('d2_',labRdata,'.Rdata')))
######################################################

# load(file=file.path(Rdata,paste0('d2_',labRdata,'.Rdata')),verbose=TRUE)
# gse<- d2 %>% subset(pred_name=="GSE" & stratum_time=='2010-Q1');  gse; gse[['PREY']]
# subset(d2,phi>200)[['PRED']]

# test   prey_prop<-prey_prop_all[1]
do_SMS<-function(d2,prey_prop) {
  labRdata2<-paste(labRdata,prey_prop,sep='_')

  if (do_diri) {
      if (prey_prop==prey_prop_all[1]) d2[['PREY']]<-d2[['PREY']] %>% mutate(prey_w=obs_w)  #redundant
      if (prey_prop==prey_prop_all[2]) d2[['PREY']]<-d2[['PREY']] %>% mutate(prey_w=mean_w)
      if (prey_prop==prey_prop_all[3]) d2[['PREY']]<-d2[['PREY']] %>% mutate(prey_w=mu)
      d2[['PREY']][is.na(d2[['PREY']]$prey_w),'prey_w'] <-d2[['PREY']][is.na(d2[['PREY']]$prey_w),'obs_w']
  }
  d2<-diet_relative(d2)

  SMS<-model_output(d2, length_classes_file=file.path(config_dir,paste0("length_classes_",sz,".csv")),sp_info_file=file.path(config_dir,'species_info.csv'),
                    intoSMSformat=TRUE)
  SMS[SMS$pred %in% c("GSE", "HBP"),"phi"]<-  -9

  sort(unique(SMS$prey))
  if (FALSE) {
    xtabs(~pred+pred.size,data=SMS)
    # number of stomachs
    a<-select(SMS,year,quarter,pred,pred.size,stom.no) %>% unique()
    a<-xtabs(stom.no~paste0(year,'_Q',quarter)+pred,data=a)
    a<-rbind(a,all=colSums(a))
    a<-cbind(a,all=rowSums(a))
    a
  }


  ##################################
  # include birds, gurnards(2013) and Hake diet from SAS
  sort(unique(paste(formatC(SMS$pred.no,width=2),SMS$pred,sep='-')))
  incl_sp<-unique(SMS$pred)
  k<-read.csv(file=file.path(finalExchangeDirOld,'stomcon_list_evac_Y.dat'),header=TRUE,sep=' ')
  k<-as_tibble(subset(k,pred %in% c(birds,'GUR','HKE'))) %>% filter(!(pred=='GUR' & year==1991))
  # unique(k$pred)
  # hke<-filter(k,pred=='HKE') ;summary(hke)
  #summary(subset(k,pred=='GUR')) #kun 2023 data

  #change length classes from data produced by BootsMave (SAS)
  # start with adding a size range (and check that the expected format actually have been used)
  l_old<-get_length_used_in_BootsMave()

  k<-left_join(k,l_old,by=join_by(year==year,quarter==quarter,pred.size==l1))

  # should be empty
  stopifnot(dim(filter(k,pred.size.class != no) %>% select(year, quarter, pred,  pred.no, pred.size, pred.size.class,no,group))[[1]]==0)

  k<-k  %>% mutate(pred.size=group,group=NULL,l2=NULL,no=NULL,Species=NULL)

  k_oth<-filter(k,prey=='OTH') %>% mutate(prey.size='9999-9999')
  kk<- filter(k,prey!='OTH')
  kk<-left_join(kk,l_old,by=join_by(year==year,quarter==quarter,prey.size==l1))

  # should be empty
  stopifnot(dim(filter(kk,prey.size.class != no) %>% select(year, quarter, pred,  pred.no, prey.size, prey.size.class,no,group))[[1]]==0)
  kk<-kk  %>% mutate(prey.size=group,l2=NULL,no=NULL,Species=NULL,group=NULL)


  k<-rbind(k_oth,kk)

  # change to the presently used length classes (pred.size.class)

  #length classes, as presently used
  ifile<-paste0("length_classes_config_",sz,".csv")
  len_classes_in<-make_length_classess(inp_dir=config_dir,inp_file=ifile,out_file=NULL,write_output=FALSE,max_l=control@max_l,minus_one= 0)
  s<-subset(len_classes_in,select=c( -Species,-l1,-l2))


  k<-left_join(k,s,by=join_by(year==year,quarter==quarter,pred.size==group)) %>%mutate(pred.size.class=no,no=NULL)
  k<-left_join(k,s,by=join_by(year==year,quarter==quarter,prey.size==group)) %>%
     mutate(prey.size.class=if_else(prey.no>0,no,prey.size.class),no=NULL)

  sort(unique(paste(formatC(k$pred.no,width=2),k$pred,sep='-')))
  head(SMS)
  head((k))


  k$SMS_area<-'all'
  k$phi<- -9  # no information

  filter(k,pred=='HKE')
  k<-k %>% mutate(stom.no=haul.no)  # shortcut
  stopifnot(setdiff(names(SMS),names(k))==character(0))

  SMS<-bind_rows(SMS,k)
  sort(unique(paste(formatC(SMS$pred.no,width=2),SMS$pred,sep='-')))
  xtabs(~pred.size+year,data=SMS)

  if (FALSE) {
    # number of stomachs
    a<-select(SMS,year,quarter,pred,pred.size,,pred.no,stom.no) %>% unique()
    a<-xtabs(stom.no~paste0(year,'_Q',quarter)+paste0(formatC(pred.no,flag='0',width=2),'-',pred),data=a)
    a<-rbind(a,all=colSums(a))
    a<-cbind(a,all=rowSums(a))
    a


    # number of hauls
    a<-select(SMS,year,quarter,pred,pred.size,,pred.no,haul.no) %>% unique()
    a<-xtabs(haul.no~paste0(year,'_Q',quarter)+paste0(formatC(pred.no,flag='0',width=2),'-',pred),data=a)
    a<-rbind(a,all=colSums(a))
    a<-cbind(a,all=rowSums(a))
    a

    # max n_sample
    a<-select(SMS,year,quarter,pred,pred.size,,pred.no,haul.no) %>% unique() %>% mutate(yq=paste0(year,'_Q',quarter),pred=paste0(formatC(pred.no,flag='0',width=2),'-',pred))
    a<-a %>% group_by(pred,yq) %>% summarize(max_haul_no=max(haul.no)) %>% ungroup()
    a<-xtabs(max_haul_no~yq+pred,data=a)
    a2<-apply(a,2,max)
    a<-rbind(a,all=a2)
    a1<-apply(a,1,max)
    a<-cbind(a,all=a1)
    a

  }

  #arbitrary change of number of mackerel samples (too high ?)
  if (TRUE) {
    crit<-SMS$year==1991 & SMS$pred=='MAC' & SMS$quarter %in% c(2,3,4);
    summary(SMS[crit,'haul.no'])
    summary(SMS[crit,'stom.no'])

    SMS[crit,'haul.no']<-round(sqrt(SMS[crit,'haul.no'])*2)
    SMS[crit,'stom.no']<-round(sqrt(SMS[crit,'stom.no'])*2)
  }
  ############
  ###### check that there exits an ALK for each combination of prey and prey size

  # sandeel ALK data are only available from Bootsmave (SAS)
  lak<-read.csv(file=file.path(finalExchangeDirOld,"LAK_from_SAS.csv")) %>% rename(quarter=Quarter) %>% as_tibble()
  sort(unique(lak$species))
  y1<-sort(unique(lak$year))
  y2<-compile_options$years
  misy<-setdiff(y2,y1)
  lakMis<-filter(lak,year==tail(y1,1))
  for (y in misy) {
    lak<-rbind(lak,mutate(lakMis,year=y))
  }

  y1<-sort(unique(lak$year))
  y2<-compile_options$years
   setdiff(y2,y1)

  tst<-filter(SMS,prey=='PLE')
  select(tst,year, pred,prey ) %>% unique()

  #change length classes to the presently used
  l_old<-get_length_used_in_BootsMave() %>% mutate(Species=NULL)
  lak<-left_join(lak,l_old,by=join_by(year==year,quarter==quarter,sizecl==l1))

  # should be empty
  stopifnot(dim(filter(lak,sms_l != no) %>% select(year, quarter, species,sizecl, sms_l,no,group))[[1]]==0)

  lak<-lak  %>% mutate(sizecl=group,group=NULL,l2=NULL,no=NULL)


  # change size classes to the presently used ones
  lak<-left_join(lak,s,by=join_by(year==year,quarter==quarter,sizecl==group)) %>%mutate(sms_l=no,no=NULL)
  #lsan<- filter(lak,species %in% c("NSA", "SSA" )) %>% as_tibble()


  source(file.path(R_prog,'ALK_plaice.R'));

  head(lak);head(ple_alk)
  sort(unique(lak$year))
  sort(unique(ple_alk$year))
  ple_alk<-ple_alk %>% ungroup() %>%mutate(l1=NULL,l2=NULL)
  lak<-rbind(lak,ple_alk)

  lak_bank<-lak

  #

  s<-SMS %>% filter(prey !='OTH') %>%  select(year,quarter,prey,prey.no,prey.size,prey.size.class)

  syear<-sort(unique(s$year))

  l<-lak %>%transmute(year,quarter,prey=species,prey.size=sizecl, prey.size.class=sms_l)%>% distinct()

  # anti_join(x,y) return all rows from x without a match in y.
  not_found<-anti_join(x=s,y=l,by = c("year", "quarter", "prey", "prey.size", "prey.size.class")) %>% distinct()
  not_found
  if (FALSE) {
    tst<-not_found
    tst$yqp<-paste(tst$year,tst$quarter,tst$prey,sep='_')
    xtabs(~yqp+prey.size,data=tst)
  }

  #delete those prey with unlikely size (no lak)
  # anti_join(x,y) return all rows from x without a match in y.
  SMS<-anti_join(SMS,not_found,by = c("year", "quarter", "prey", "prey.no", "prey.size","prey.size.class"))


  save(SMS,cleanup,birds,file=file.path(Rdata,paste0('preSMS_',labRdata2,'.Rdata')))
  # load(  file=file.path(Rdata,paste0('preSMS_',labRdata2,'.Rdata')),verbose=TRUE)

  #model for phi
  if (do_diri) {
    a<-SMS %>% select(SMS_area,  year, quarter, pred,  pred.no, pred.size, pred.size.class ,haul.no, stom.no,  phi ,prey ) %>% unique() %>%
       group_by(SMS_area,  year, quarter, pred,  pred.no, pred.size, pred.size.class ,haul.no, stom.no,  phi) %>% summarize(N_prey_sp=dplyr::n()) %>%
        mutate(Pred=paste(formatC(pred.no,width=2,flag='0'),pred,sep='_'),n_prey=as.character(min(N_prey_sp,6))) %>%
       mutate(n_prey=if_else(n_prey=='6','6+',n_prey),N_prey_sp=min(N_prey_sp,6)) %>% ungroup()
    if (boot_sample_id==boot_sample_id_all[1]) a$samp_effort<-a$stom.no else a$samp_effort<-a$haul.no
    a[a$pred %in% c("GSE", "HBP"),"phi"]<-  -9
    newa<-a%>%rename(obs_phi=phi)


    # tst<- newa  %>% select(all_of(c("SMS_area","year","quarter","pred","pred.size"))); tst[duplicated(tst),]

    round(do.call(rbind,tapply(a$phi,list(a$Pred),summary)),1)
    aa<-filter(a,phi>0 )
    round(do.call(rbind,tapply(aa$phi,list(aa$Pred),summary)),1)

    round(do.call(rbind,tapply(aa$samp_effort,list(aa$Pred),summary)),1)

    aa[aa$phi>200,'phi']<-200
    if (boot_sample_id==boot_sample_id_all[1]) aa[aa$samp_effort>1500,'samp_effort']<-1500
    if (boot_sample_id==boot_sample_id_all[2]) aa[aa$samp_effort>400,'samp_effort']<-400


    #  ggplot(aa, aes(x=pred.size, y=phi, group=n_prey)) +
    #  geom_point(aes(shape=n_prey, color=n_prey))

    #ggplot(aa, aes(x=Pred, y=phi, group=n_prey)) +
    #  geom_point(aes(shape=n_prey, color=n_prey))


    library(mgcv)
    if (FALSE) {
      a1<-gam(phi~n_prey+Pred,data=aa)
      summary(a1)
      xtabs(~n_prey,data=aa)
      xtabs(~N_prey_sp+Pred,data=aa)
      xtabs(~n_prey+Pred,data=aa)
    }
      # best fit with log (phi)
    #summary(gam(log(phi)~te(N_prey_sp)+te(haul.no), data=aa))
    #summary(gam(    phi ~te(N_prey_sp)+te(haul.no), data=aa))
    #summary(gam(    phi ~te(N_prey_sp)+te(stom.no), data=aa))

    #summary(gam(log(phi)~te(N_prey_sp)+te(haul.no), data=aa))
    #summary(gam(log(phi)~te(N_prey_sp)+te(stom.no), data=aa))
    #summary(gam(log(phi)~te(N_prey_sp)+te(samp_effort), data=aa))

    #
    a1<-gam(log(phi)~te(N_prey_sp)+te(samp_effort), data=aa)
    summary(a1)

    png(filename=file.path(output_dir,paste0('model1_',labRdata2,'.png')),width=700,height=500,pointsize=12)
    par(mfrow=c(1,2))
    plot(a1,ask=F)
    cleanup()


    png(filename=file.path(output_dir,paste0('model2_',labRdata2,'.png')),width=700,height=500,pointsize=12)
    par(mfrow=c(1,2))
    plot(a1,residuals=T)
    cleanup()

    png(filename=file.path(output_dir,paste0('model3_',labRdata2,'.png')),width=700,height=500,pointsize=12)
    par(mfrow=c(1,2))
    vis.gam(a1)
    vis.gam(a1,view=c('samp_effort','N_prey_sp'))
    cleanup()


    p1<-predict(a1,newdata=newa)
    p11<-cbind(newa,predict_phi=exp(p1))
    p11
    summary(p11$predict_phi)
    p11 %>% arrange(predict_phi)
    p11 %>% arrange(dplyr::desc(predict_phi))
    #plot(p11$obs_phi,p11$predict_phi)

   # effort and predator
    a2<-gam(log(phi)~s(N_prey_sp,k=3)+te(samp_effort) +pred, data=aa)
    summary(a2)

    png(filename=file.path(output_dir,paste0('model4_',labRdata2,'.png')),width=700,height=700,pointsize=12)
    par(mfrow=c(2,2))
    plot(a2,residuals=T)
    vis.gam(a2)
    vis.gam(a2,view=c('samp_effort','N_prey_sp'))
    cleanup()


    birdsm<-c(birds,"GSE", "HBP", "HKE") #birds and mammals hake
    non_birds<-filter(newa,!(pred %in% birdsm))
    sort(unique(non_birds$pred))
    p2<-predict(a2,newdata=non_birds)
    p22<-cbind(non_birds,predict_phi=exp(p2))
    #p22

    all_birds<-filter(newa,(pred %in% birdsm))
    #sort(unique(all_birds$pred))
    p23<-predict(a1,newdata=all_birds)
    p23<-cbind(all_birds,predict_phi=exp(p23))
    #p23
    b<-rbind(p22,p23) %>% ungroup()
    #round(tapply(b$predict_phi,list(b$Pred,b$pred.size),median),1)

    bb<- b  %>% select(all_of(c("SMS_area","year","quarter","pred","pred.size","predict_phi")))
    # tst<-bb %>% mutate(predict_phi=NULL); dim(bb);dim(unique(tst));tst[duplicated(tst),];dim(bb); dim(newa)

    SMS<-left_join(SMS,bb,by = join_by(SMS_area, year, quarter, pred, pred.size)) %>%
       mutate(phi=if_else(phi<0,predict_phi,phi),predict_phi=NULL)
    SMS[SMS$phi<2,'phi']<-2
    #print(filter(SMS,phi>150),n=50)
    #SMS[SMS$phi>150,'phi']<-150
  }

  SMS$SMS_area<-1

  summary(SMS); filter(SMS,phi>200)

    f<-paste0('stomcon_list_',labRdata2,'.dat')
  cat(file.path(SMS_dir,f),'\n')

  write.table(SMS,file=file.path(SMS_dir,f),col.names=TRUE,row.names=FALSE,sep=' ',quote=FALSE)
  write.table(SMS,file=file.path(finalExchangeDir,f),col.names=TRUE,row.names=FALSE,sep=' ',quote=TRUE)

  incl<-SMS %>% select( year, quarter, pred, pred.size, phi,stom.no, haul.no) %>%  unique() %>% arrange( pred, pred.size, year, quarter)
  incl
  incl<-incl %>% filter(!is.na(phi) )

  incl<-incl %>% mutate(stratum_time=paste0(year,'-Q',quarter))

  if (FALSE) {
    junk<-by(incl,incl$pred,function(x) {
      b<-round(xtabs(phi~pred.size+stratum_time,data=x),1)
    })

    junk
    incl %>% group_by(pred) %>% summarize(maxPhi=max(phi,na.rm=TRUE))
  }

  # ALK for stomach observations.


  if (FALSE) {
    l<-read.csv(file=file.path(finalExchangeDirOld,'ALK_stom_list_evac_Y.dat'),header=TRUE,sep=' ')
    head(l)
    head(lak)

    # anti_join() return all rows from x without a match in y.
    anti_join(l,lak,by=join_by(year,quarter,prey==species,prey.size.class==sms_l,prey.age==Age))
    left_join(l,lak,by=join_by(year,quarter,prey==species,prey.size.class==sms_l,prey.age==Age))
    anti_join(lak,l,by=join_by(year,quarter,species==prey,sms_l==prey.size.class,Age==prey.age))  #lak includes all species
  }

  ALK<- lak_bank %>%select(SMS_area,year,quarter,prey=species,prey.no=spno,prey.age=Age,prey.size=sizecl,  prey.size.class=sms_l,ALK.MeanL=length,ALK=new_alk)

  ALK
  x<-filter(ALK,prey=='COD' & year==1981)
  if (FALSE) by(ALK,list(ALK$prey,ALK$year),function(x){
    x<-droplevels(x)
    #ink('alk.txt')
    table(round(tapply(x$ALK,list(x$quarter,x$prey.age,x$prey.size),sum),2))
   #sink()
  })


  ALK<-ALK %>% select(SMS_area, year, quarter, prey, prey.no, prey.age, prey.size.class, prey.size, ALK.MeanL,ALK)

  #filter(SMS,prey=='SSA' & year==1987 &prey.size.class==7 & quarter==1)
  #filter(ALK,prey=='SSA' & year==1987 &prey.size.class==7 & quarter==1)
  #filter(ALK,prey=='SSA' & year==1987 & quarter==1)
  #ALK$SMS_area<-'all'

  f<-paste0('ALK_stom_list_',labRdata2,'.dat')
  cat(file.path(SMS_dir,f),'\n')

  write.table(ALK,file=file.path(finalExchangeDir,f),sep=' ',row.names=FALSE)
  write.table(ALK,file=file.path(SMS_dir,f),sep=' ',row.names=FALSE)

  if (make_plots) {
    my.year<-2010
    a<-ALK %>% filter(year==my.year) %>% mutate(quarter=paste0("Q:",quarter),prey.size=substr(prey.size,1,3))
    age<-sort(unique(a$prey.age))
    age<-data.frame(age,Age=paste('Age',age))
    a$age= factor(a$prey.age,levels=age$age,labels=age$Age)
    xx<-by(a,list(a$prey.no),function(x) {
      prey_name<-x[1,'prey']
      p<-ggplot(data=x, aes(x=prey.size, y=ALK)) +
         geom_bar(stat="identity", color='red',width=0.5)+
         facet_grid(rows=vars(age),cols=vars(quarter),scales = "free_y")+
         labs(x='Length class (cm)',y='Proportion',title=prey_name) +
         #theme_minimal() +
         theme( #panel.grid.major = element_line(linetype = "blank"),
               #panel.grid.minor = element_line(linetype = "blank"),
               axis.text.x = element_text(angle = 90, vjust = 0.5))


      png(filename=file.path(output_dir,paste0('ALK_',prey_name,'.png')),width=800,height=1000,pointsize=35)
      print(p)
      cleanup()
      return(NULL)
      })
  }



}

do_SMS(d2,prey_prop_all[1])
if (do_diri) do_SMS(d2,prey_prop_all[2])
if (do_diri) do_SMS(d2,prey_prop_all[3])



}

boot_sample_id_all
prey_prop_all


do_it(convert_data=FALSE,read_data=FALSE,my_n_boot=1, boot_sample_id=boot_sample_id_all[2], yfinal=2013,make_plots=FALSE,make_plots_simple=FALSE,gse_north_south_sandeel=TRUE)

do_it(convert_data=FALSE,read_data=FALSE, my_n_boot=500, boot_sample_id=boot_sample_id_all[2], yfinal=2013,make_plots=TRUE,gse_north_south_sandeel=TRUE)

if (FALSE) {
  #do_it(convert_data=FALSE,read_data=FALSE, my_n_boot=500, boot_sample_id=boot_sample_id_all[2], yfinal=2013,make_plots=TRUE,gse_north_south_sandeel=TRUE)
  #do_it(convert_data=FALSE,read_data=FALSE, my_n_boot=500, boot_sample_id=boot_sample_id_all[1], yfinal=2013,make_plots=TRUE,gse_north_south_sandeel=TRUE)

}



