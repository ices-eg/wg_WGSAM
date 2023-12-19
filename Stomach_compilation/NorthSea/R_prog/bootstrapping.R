
s<-subset(ns,pred_name=='COD' & quarter==1 & year==1981)
s
############### add variables for bootstrapping
s<-edit_control(s, bootstrapping = list(
  boots_id = expression(paste(ship,rectangle,year,quarter,station,haul,sep='_')),
  boots_strata = expression(paste(year,quarter,pred_name,sep='-'))
))

print(get_control(s),'bootstrapping')
s<-bootstrap_addVar(s)
names(s[['PRED']])


## ----ex06_02------------------------------------------------------------------

head(bootstrap_show(s,show=c("strata",'sample')[1],vari=c("stomach","sample")[1]))
head(bootstrap_show(s,show=c("strata",'sample')[1],vari=c("stomach","sample")[2]))


## ----ex06_03------------------------------------------------------------------


head(bootstrap_show(s,show=c("strata",'sample')[2],vari=c("stomach","sample")[1]))


# a very complex way of getting a list of prey names!
keep_prey <-(read_csv(file.path(config_dir,"species_info.csv"),col_types = cols())  %>%
               filter(prey_sp)  %>% select(SMS_species) %>% unique())$SMS_species
keep_prey #input is just a vector of prey names

s<-edit_control(s,sel_preys=keep_prey)

from_to <-make_from_to_species(inp_file=file.path(config_dir,'from_to_species.csv'))

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
                  weighting_factor_file=file.path(config_dir,'remains_weighting_total.csv')
                )
)



## ----ex06_05------------------------------------------------------------------

s
s[['PRED']]
sort(unique(s[['PREY']]$prey_name))
from_to

do_boots<-function(s) {

  #test  s<-bd[[1]]
   cat('*') # to show that something is happening

  # to keep information on bootstrap replicate
  rep_id<-unlist(s[['PRED']][1,'rep_id'])

  # by sample
  s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area=sample_id,
                                 dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,
                                 by_prey_size=FALSE,remains_to_other = FALSE)

  # by sub_area (=rectangel)
  s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area=stratum_sub_area,
                                 dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,
                                 by_prey_size=FALSE,remains_to_other = FALSE)

  # by area (=roundfish area)
  s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area=stratum_area,
                                 dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
                                 by_prey_size=FALSE,remains_to_other = FALSE)

  # all areas
  s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area='All',
                                 dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
                                 by_prey_size=FALSE,remains_to_other = FALSE)

  # all areas and half-year (dist_time is changed)
  s<-redist_unidentified_prey_sp(s,dist_time=paste(year,ifelse(quarter %in% c(1,2),'S1','S2')),
                                 dist_area='All',dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
                                 by_prey_size=FALSE,remains_to_other = FALSE)

  # the same as above, but unallocated remains to "other food".
  s<-redist_unidentified_prey_sp(s,dist_time=paste(year,ifelse(quarter %in% c(1,2),'S1','S2')),
                                 dist_area='All',dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
                                 by_prey_size=FALSE,remains_to_other = TRUE)

  # by statum_area (=roundfish area)
  s<-redist_unidentified_prey_lengths(s,dist_time=stratum_time,dist_area=stratum_area,
                                      dist_pred_size=pred_size,
                                      remains_to_other = FALSE, # do not add records with missing length allocation to "other food"
                                      others_to_other=TRUE      # do add other species that selected prey species to "other food"
  )

  # within all areas
  s<-redist_unidentified_prey_lengths(s,dist_time=stratum_time,dist_area='All',
                                      dist_pred_size=pred_size,remains_to_other = FALSE)

  #  within all areas, and year (ignoring quarter)
  s<-redist_unidentified_prey_lengths(s=s,dist_time=substr(stratum_time,1,4),dist_area='All',
                                      dist_pred_size=pred_size,remains_to_other = FALSE)

  # as above, but with conversion of unallocated into "other"
  s<-redist_unidentified_prey_lengths(s=s,dist_time=substr(stratum_time,1,4),dist_area='All',
                                      dist_pred_size=pred_size,remains_to_other = TRUE)

  d<-calc_population_stom(s)
  d[['PRED']]$rep_id<-rep_id
  return(d)

}


## ----ex06_06------------------------------------------------------------------

n_boot<-1:5  # make 3 bootstrap replicates
bd<-lapply(n_boot,function(x) bootstrap_data(s,seed=x,rep_id=x))

print(bd[[1]],show_attributes=FALSE)
print(bd[[2]],show_attributes=FALSE)


## ----ex06_07------------------------------------------------------------------

#convert observations into population diet
bt<-lapply(bd,do_boots)

bt[[1]] # STOMdiet object

bt[[2]] # STOMdiet object



## ----ex06_08------------------------------------------------------------------

aa<-read_csv(file.path(config_dir,"species_info.csv"))
aa
tail(aa)

x<-bt[[1]]
str(x)
x[['PRED']]
sort(unique(x[['PRED']]$pred_name))
sort(unique(x[['PREY']]$prey_name))

bt2<-lapply(bt,function(x){
  x<-from_to_species_diet(x,pred_from_to=c("species","short"),prey_from_to=c("SMS_species","short"),
                          sp_info_file=file.path(config_dir,"species_info.csv"))
  #Relative stomach contents
  x[['PREY']]<- x[['PREY']] %>% dplyr::group_by(key) %>%
    dplyr::mutate(prey_w=prey_w/sum(prey_w)*100) %>% dplyr::ungroup()
  return(x)

})

# change to English names in the source data for bootstrap, for a later merge
source<-from_to_species_diet(s,pred_from_to=c("species","short"),prey_from_to=c("species","short"),
                             sp_info_file=file.path(system.file( package = "FishStomachs"),"extdata",'species_info.csv'))



## ----ex06_09------------------------------------------------------------------

# comparison of two replicates
plotdif(d1=bt2[[1]],d2=bt2[[2]],show_plot=TRUE,cut_pred_size=c(1,4),cut_prey_size=c(2,4),addTitle=TRUE,
        tAngle=90, relative=FALSE,maxDif=3,
        byVar=c('year-quarter','year','quarter','none')[4])


## ----ex06_10------------------------------------------------------------------

bt3<-add_missing_boots(bt2, mis_value=0)

plotdif(d1=bt3[[1]],d2=bt3[[2]],show_plot=TRUE,cut_pred_size=c(1,4),cut_prey_size=c(2,4),addTitle=TRUE,
        tAngle=90, relative=FALSE, maxDif=3,
        byVar=c('year-quarter','year','quarter','none')[4])


## ----ex06_11------------------------------------------------------------------

# sub set for plot
bt4<-lapply(bt3,function(x){
  subset(x,pred_name=='Cod' &  pred_size=='0400-0499' & stratum_time=='1991-Q1')
})

plotboots.size(b=bt4,show_plot=TRUE,cut_pred_size=c(1,10),cut_prey_size=c(2,4),addTitle=TRUE,tAngle=90)


## ----ex06_12------------------------------------------------------------------
# sub set for plot
bt4<-lapply(bt3,function(x){
  subset(x,pred_name=='Cod' & stratum_time=='1991-Q1' & !(pred_size %in% c('0100-0149','0150-0199')))
})


plotboots(b=bt4,show_plot=TRUE,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90)


## ----ex06_13------------------------------------------------------------------


# load diet data (point estimate) from example 03
load(file=file.path(system.file( package = "FishStomachs"),"extdata","ex03d.Rdata"),verbose=TRUE)

# change from latin to English names (used later on)
pointEst<-from_to_species_diet(d,pred_from_to=c("species","short"),prey_from_to=c("species","short"),
                               sp_info_file=file.path(system.file( package = "FishStomachs"),"extdata",'species_info.csv'))


bb<-bootsMean(b=bt4,pointEst,by_prey_size=FALSE)
head(bb)

plotboots(b=bt4,show_plot=TRUE,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90)




## ----ex06_14------------------------------------------------------------------

load(file=file.path(system.file( package = "FishStomachs"),"extdata","bt4.Rdata"),verbose=TRUE)

bb<-bootsMean(b=bt4,pointEst,by_prey_size=FALSE,do_Diri=TRUE)
select(bb,prey_name, phi,param,mu,mean_w,sd_w,prey_w)

plotboots(b=bt4,cut_pred_size=c(1,4),addTitle=TRUE,tAngle=90,statistics=bb)


