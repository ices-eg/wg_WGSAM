
if (FALSE) {
  dirs<-  scenario.dirs<-c("NS_2020_s_01_MS_M","NS_2020_s_02_smooth_MS_M","NS_2020_s_03_avg_MS_M","NS_2020_s_04_M02")
  
  labels<-c('1. MS M',  '2. MS M smooth',         '3. M=avg. MS M',   '4. M=0.2' )
}


compare_runs_stock_rec(dirs,labels,first.year.on.plot=1975,last.year.on.plot=2020,incl.sp="all",include.CV=TRUE,include.CV2=TRUE,include.mean=TRUE,
                       nox=2, noy=2, run.ID='SBB_rec', include.year.labels=TRUE,incl_not_used=T,paper=FALSE) 
