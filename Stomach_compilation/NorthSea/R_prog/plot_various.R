if (make_plots) {
#tst<-diet[[1]][[1]];  plot(tst,show_plot=TRUE,addNstom=TRUE,cut_pred_size=c(1,4),addTitle = TRUE,Ncol=1)

pl<-lapply(diet,function(x){
  plot(x[[1]],show_plot=FALSE,addNstom=TRUE,cut_pred_size=c(1,4),addTitle = TRUE,Ncol=1)
})
pl[[1]]
pl[[3]][[1]]

out<-pl %>% purrr::discard(rlang::is_null)
out<-lapply(out,function(x) purrr::discard(x,rlang::is_null))

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

## plot with prey size
if (make_plots & FALSE) {
  pls<-lapply(diet,function(x){
    plotSize(x[[1]],show_plot=FALSE, cut_pred_size=c(1,3),addTitle=TRUE, byVar=c('year-quarter','year','quarter','none')[4])
  })

  x<-pls[[1]][[1]]
  x$data
  summary(x$data)
  unique(x$data$stratum_time)

  a<-lapply(pls,function(x){
    lapply(x,function(x1){
      dat<<-dplyr::mutate_if(x1$data,is.factor,as.character)
      pred_name<-dat[1,'pred_name']
      yq<-as.character(dat[1,'stratum_time'])
      year<-strsplit(yq,split="§")[[1]][1]
      quarter<-strsplit(yq,split="§")[[1]][2]

      cat(paste(pred_name,year),'\n')
      png(filename=file.path(output_dir,paste0(pred_name,'_d',year,'_',quarter,'.png')),width=800,height=1000,pointsize=25)
      print(x1)
      cleanup()
    })
  })
}

