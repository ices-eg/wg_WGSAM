bootsAverage<-function(b,n_rep=NA,do_Diri=FALSE,minPreyProportion=0.0,Diri_min=0.001,Diri_max=0.99,verbose=FALSE) {
  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-year<-NULL
  allNames<-prey_name<-prey_size<-stratum_time<-NULL

  control<-attr(b[[1]],'control')
  other<-control@other

  # to data frame
  x<-do.call(rbind,lapply(b,as.data.frame)) %>% filter(!is.na(prey_w)) %>%
    mutate(pred_l_mean=round(pred_l_mean)) %>% mutate(pred_l_mean=as.double(pred_l_mean))

  if (! is.na(n_rep))  {if (length(n_rep)==1) x<-subset(x,rep_id<=n_rep) else  x<-subset(x,rep_id %in% n_rep)}

    #rescale to 1.00
  x<-x %>% group_by(stratum_area,stratum_time,pred_name, pred_size,rep_id) %>%
    dplyr::mutate(prey_w=prey_w/sum(prey_w)) %>%ungroup()

  x

  x2<-x %>% dplyr::group_by(stratum_area,stratum_time,pred_name, pred_size, pred_size_class, pred_l_mean, key,prey_name,prey_size,prey_size_class ) %>%
                   summarise(prey_w2=mean(prey_w,na.rm=TRUE), n=n(),prey_sd=sd(prey_w,na.rm=TRUE),.groups = "keep") %>%
                        ungroup() %>% mutate(prey_w=prey_w2,prey_w2=NULL)
    meanw<-x %>% dplyr::group_by(year,quarter,pred_name, pred_size,prey_name) %>% dplyr::summarise(incl=mean(prey_w)>=minPreyProportion)
  x<-dplyr::left_join(x,meanw,by = c("year", "quarter", "pred_name", "pred_size", "prey_name")) %>% dplyr::ungroup() %>%
    mutate(prey_name=if_else(incl,prey_name,factor(other,levels=levels(x$prey_name)))) %>%
    dplyr::group_by(year,quarter,pred_name, pred_size,prey_name,rep_id) %>% dplyr::summarise(prey_w=sum(prey_w))

  #rescale to 1.00 again (should not be necessary ?)
  x<-x %>% group_by(year,quarter,pred_name, pred_size,rep_id) %>%
    dplyr::mutate(prey_w=prey_w/sum(prey_w))

  out<-by(x,list(x$pred_name,x$pred_size,x$year,x$quarter),function(x) {
    x<-droplevels(x)
    preys<-levels(x$prey_name)
    xm<- x %>% dplyr::group_by(year,quarter,pred_name, pred_size,prey_name) %>%
      dplyr::summarise(mean_w=mean(prey_w),sd_w=sd(prey_w),n=n(),.groups="keep") %>%  dplyr::ungroup()
    if (verbose) print(xm)
    maxxm<-max(xm$mean_w,na.rm=TRUE)
    a<-list(ok=TRUE,empirical=xm,preys=preys)
    if (do_Diri & maxxm<Diri_max) {
      xx<-tidyr::pivot_wider(x,values_from=prey_w,names_from=prey_name)  %>% dplyr::ungroup()
      xx<-dplyr::select(xx,all_of(preys))
      xx<-as.matrix(xx)
      if (any(xx==0.0)) {
        xx[xx==0]<-Diri_min
        xx<-xx/apply(xx,1,sum) #rescale to 1.0
      }
      aa<-try(Compositional::diri.est(xx,type = "prec"),silent=TRUE) #MLE of the parameters of a Dirichlet distribution.
      if (class(aa)=="try-error") a$ok<-FALSE else a<-append(a,aa)
      if (a$ok) aa<-try(Compositional::dirimean.test(xx,aa$param),silent=TRUE) #Log-likelihood ratio test for a Dirichlet mean vector.
      if (class(aa)=="try-error") a$ok<-FALSE else a<-append(a,list(p_value=as.numeric(aa$info['p-value'])))
    } else a$ok<-FALSE
    return(a)
  })

  #delete empty lists
  out<-out %>% purrr::discard(rlang::is_null)

  b<-lapply(out,function(x) {
    if (x$ok & do_Diri)  data.frame(nboots=x$empirical$n,
                                    year=x$empirical$year,quarter=x$empirical$quarter,
                                    pred_name=x$empirical$pred_name,pred_size=x$empirical$pred_size,
                                    prey=x$preys,n_prey_sp=length(x$preys),phi=x$phi,param=x$param,mu=x$mu,p_value=x$p_value,
                                    mean_w=x$empirical$mean_w,sd_w=x$empirical$sd_w) else
                                      data.frame(nboots=x$empirical$n,year=x$empirical$year,quarter=x$empirical$quarter,
                                                 pred_name=x$empirical$pred_name,pred_size=x$empirical$pred_size,
                                                 prey=x$preys,n_prey_sp=length(x$preys),phi=NA,param=NA,mu=NA, p_value=NA,
                                                 mean_w=x$empirical$mean_w,sd_w=x$empirical$sd_w)
  }
  )
  out<-do.call(rbind,b)
  if (missing(pointEst)) stop('A data set wth a point estimate of diet must be be provided')
  # samp<-left_join(
  #    bootstrap_show(source,show=c("strata",'sample')[1],vari=c("stomach","sample")[1]) %>%
  #      as.data.frame() %>% filter(Freq>0) %>% rename(n_stom=Freq),
  #   bootstrap_show(source,show=c("strata",'sample')[1],vari=c("stomach","sample")[2]) %>%
  #      as.data.frame() %>% filter(Freq>0) %>% rename(n_sample=Freq),
  #    by = c("boots_strata", "pred_size", "pred_name")
  #  )
  pc<-get_control(pointEst)
  p<-as.data.frame(pointEst)
  if (by_prey_size) p$prey<-paste(p$prey_name,p$prey_size,sep=':::') else p$prey<-p$prey_name

  p<-p %>% mutate(year=as.integer(eval(pc@strata_year_back)),
                  quarter=as.integer(eval(pc@strata_quarter_back))) %>%
    group_by(year,quarter,pred_name,pred_size,n_tot,n_sample,prey) %>% summarize(prey_w=sum(prey_w)) %>%
    group_by(year,quarter,pred_name,pred_size,n_tot,n_sample) %>% mutate(prey_w=prey_w/sum(prey_w)) %>%
    rename(n_stom=n_tot) %>% ungroup()


  out<-left_join(out,p, by = c("year", "quarter", "pred_name", "pred_size", "prey"))

  if (by_prey_size) {
    xx<-matrix(unlist(strsplit(out$prey,':::')),ncol=2,byrow=TRUE)
    out$prey_name<-xx[,1]
    out$prey_name<-factor(out$prey_name,levels=levels(pointEst[['PREY']]$prey_name))
    out$prey_size<-xx[,2]
    out$prey_size<-factor(out$prey_size,levels=levels(pointEst[['PREY']]$prey_size))
  } else  {
    out$prey_name<-factor(out$prey,levels=levels(pointEst[['PREY']]$prey_name))
    out$prey_size<-'All'
  }
  return(dplyr::as_tibble(out))
}
