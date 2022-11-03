make_plots<-FALSE  # make (and store for later use) plots and tables used for making HTML output.
make_html<-TRUE  # make markdown output
make_word_doc<-FALSE  # don't insert .tabset if TRUE
clean_html_dir<- TRUE  # delete (and re-create) directory for html output, and add necessary files,   

HTML_source<-file.path(root,"HTML_source_Baltic")  # directory for various files necessary for creating the HTML files

make_compositional_residuals<-TRUE


#directories and labels for comparisons

dirs.keyrun<-c("Baltic-2019-keyRun","Baltic-2022-keyRun")
labels.keyrun<-c("2019 Key run","2022 key run")


my.first.year.on.plot=1974
my.last.year.on.plot=2022
MyPalette=c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )

do_uncertanties_M2<-FALSE  #include M2 uncertainty plots

compare_with_single_species<-TRUE

dirs.single<-c("Baltic-2022-ICES", "Baltic-2022-singleS")  
labels.single<-c("ICES","SMS single sp.")

#################
capInBold<-TRUE

###### all the building blocks

tabset<-"{.tabset .tabset-fade .tabset-pills}"
if (make_word_doc) tabset<-''

d3<-"```"

noEcho<-paste0(d3,"{r, echo=FALSE,results='asis'}","\n")
options(encoding= "UTF-8")

wr<-function(item,out=rmd) {
  cat(item,file=rmd,append=TRUE)  
}

wr2<-function(item,out=rmd) {
  item<-paste0(item,'\n')
  cat(item,file=out,append=TRUE)  
}


end_level<-function(level) {
  wr2(paste(substr('########',1,level),"{-}"))
}


SIDESKIFT<-function() {
  wr('\\newpage\n\n')
}

insertPicture<-function(filename,caption,out=rmd) {
  if (capInBold) caption<-paste0('**',caption,'**')
  ow<- 'out.width = "100%"'
  pp<-paste0( "\n```{r, fig.align='left',",ow,", fig.cap='",caption,"'}\n",
              "knitr::include_graphics('",filename,"')\n```\n")
  wr(pp,out)
}


insertWidget<-function(filename,wf,caption,out=rmd) {
  if (capInBold) caption<-paste0('**',caption,'**')
  ow<- 'out.width = "100%"'
  pp<-paste0( "\n```{r, fig.align='left',",ow,", results='asis'}\n",
    "tmp <- URLencode(paste(readLines('",wf,"'), collapse='\\n'))\n",
    
    "cat('<iframe src=",  '"data:text/html;charset=utf-8,', "'", ",tmp,'", 
       ' " style=',  '"border: solid; seamless:seamless; width: 100%; height: 800px"',   "></iframe>')",
       "\n```\n")
  wr(pp,out)
}

#tmp <- URLencode(paste(readLines(ff), collapse="\n"))
#cat('<iframe src="data:text/html;charset=utf-8,', tmp ,'" style="border: solid; seamless:seamless; width: 100%; height: 800px"></iframe>')

do_fig<-function(vdir,cap='Mean weight at age in the sea for ',level=3,decreasing=TRUE,filter=NULL){
  files<-sort(list.files(path=vdir,pattern="*.png"))
  if (!is.null(filter)) files<-files[grep(filter,files)]
  ord<-lapply(sp.names,function(sp){
    list(pos=sort(grep(sp,files),decreasing = decreasing),sp.name=sp)
  })
  lapply(ord,function(a){
    if (length(a$pos) >0) {
      cap<-paste0(a$sp.name[1],'. ',cap)
      al<-substr('########',1,level)
      for (pic in (a$pos)) {
        wr2(paste(al,' ',a$sp.name[1],tabset))
        insertPicture(filename=file.path(vdir,files[pic]),caption=cap,out=rmd) 
      }  
    }
  })
  end_level(level)
} 


do_widget<-function(vdir,cap='biomass yyy ',level=3,decreasing=TRUE,filter=NULL){
  files<-sort(list.files(path=vdir,pattern="*.html"))
  if (!is.null(filter)) files<-files[grep(filter,files)]
  ord<-unlist(regmatches(files, gregexpr("[[:digit:]]+", files)))
  for (y in ord ) {
      al<-substr('########',1,level)
      w<-files[grep(y,files)]
         wr2(paste(al,' ',y,tabset))  
         insertWidget(wf= file.path(vdir,w),caption=cap,out=rmd)
  }
  end_level(level)
} 


# with sub select

do_fig_sub<-function(vdir,cap='Mean weight at age in the sea for ',level=3,decreasing=TRUE,filter=NULL,labels=paste0("Q:",1:4),lp=c(7,4)) {
  files<-sort(list.files(path=vdir,pattern="*.png"))
  if (!is.null(filter)) files<-files[grep(filter,files)]
  ord<-lapply(sp.names,function(sp){
    lab<-files[sort(grep(sp,files),decreasing = decreasing)]
    list(pos=sort(grep(sp,files),decreasing = decreasing), sp.name=sp,lab=substr(lab,nchar(lab)-lp[1],nchar(lab)-lp[1]+lp[2]-1))
  })
  lapply(ord,function(a){
    if (length(a$pos) >0) {
      cap<-paste0(a$sp.name[1],'. ',cap)
      al<-substr('########',1,level) 
      wr2(paste(al,' ',a$sp.name[1],tabset))
      al<-substr('########',1,level+1) 
      i<-0
      if (is.null(labels)) lab<-a$lab else lab<-labels
      for (pic in (a$pos)) {
        i<-i+1
        wr2(paste(al,lab[i],tabset))
        insertPicture(filename=file.path(vdir,files[pic]),caption=cap,out=rmd) 
      }  
    }
  })
  end_level(level)
} 


# several pictures
do_figs<-function(vdir,caps,level=3,files,labels,header) {
  al<-substr('########',1,level) 
  wr2(paste(al,header,tabset))
  
  for (i in (1:length(files))) {
    al<-substr('########',1,level+1) 
    wr2(paste(al,labels[i]))
    insertPicture(filename=file.path(vdir,files[i]),caption=caps[i],out=rmd) 
  }
  end_level(level)
} 

####




if (make_plots) source(file.path(prog.path,'tables_report.r'))  # makes tables and SMS.RData 

if (make_html) {

  html_dir<-file.path(data.path,'HTML')
  rmd<-file.path(html_dir,paste0(my.stock.dir,".Rmd")); rmd # name of R mark down file

  
  if (clean_html_dir) {
    if (file.exists( html_dir)) unlink( html_dir,recursive = T)
    dir.create(html_dir,showWarnings = FALSE)
    file.copy(file.path( HTML_source,'header.txt'),file.path(html_dir,'header.txt'),overwrite=TRUE)
    file.copy(file.path( HTML_source,'r_setup.txt'),file.path(html_dir,'r_setup.txt'),overwrite=TRUE)
    file.copy(file.path( HTML_source,'style.css'),file.path(html_dir,'style.css'),overwrite=TRUE)
    file.copy(file.path( HTML_source,'par_names.csv'),file.path(html_dir,'par_names.csv'),overwrite=TRUE)
    file.copy(file.path( HTML_source,'pred_format.dat'),file.path(data.path,'pred_format.dat'),overwrite=TRUE)
  
  }
  
 
  file.copy(file.path( html_dir,'header.txt'),rmd,overwrite=TRUE)
  wr2(readLines(file.path(html_dir,'r_setup.txt'),warn=TRUE))
  item<-paste0(d3,"{r ini, echo=FALSE }\n",
               "load(file='",file.path(data.path,'SMS.Rdata'),"');\n",
               d3,"\n")
  wr(item)
  
}




#### now we start


in.dir<-'Input'
out.dir<-'Output'
par.dir<-"Parameters"
diag.dir<-"Diagnostics"

inOut.dir<-'Input_Output'
outputDir<-file.path(data.path,inOut.dir)

if (make_plots) {    
  if (file.exists(outputDir)) unlink(outputDir,recursive = T)
  dir.create(outputDir,showWarnings = FALSE)
  dir.create(file.path(data.path,inOut.dir,out.dir),showWarnings = FALSE)
  dir.create(file.path(data.path,inOut.dir,in.dir),showWarnings = FALSE)
  dir.create(file.path(data.path,inOut.dir,diag.dir),showWarnings = FALSE)
  dir.create(file.path(data.path,inOut.dir,par.dir),showWarnings = FALSE)

}
 
if (make_plots) {  
  dirs=dirs.keyrun  # directory files to be compared
  labels=labels.keyrun  # output legends
 
  plotData<-function(vari) {
    my.dir<-file.path(data.path,inOut.dir,in.dir,vari)
    dir.create(my.dir,showWarnings = FALSE)
    my.dir<-file.path(inOut.dir,in.dir,vari)
    compare_runs_various(
      paper=TRUE,   # graphs on file (paper=TRUE) or on screen (paper=FALSE)
      first.year.on.plot=my.first.year.on.plot,
      last.year.on.plot=my.last.year.on.plot,
      compare.dir=my.dir,makeAllGraphs=TRUE,
      vari=vari,
      maxAge=5,     # age > max age are put in a separate plot 
      nonFish=c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill'),
      dirs=dirs.keyrun,
      labels=labels.keyrun
    ) 
  }
  
  plotData(vari='west') 
  plotData(vari='propmat') 
  plotData(vari='c.obs')
  plotData(vari='ration')
}

if (make_plots) {
  yield.dir<-file.path(data.path,inOut.dir,in.dir,'yield')
  dir.create(yield.dir,showWarnings = FALSE)
  plot_yield(out_dir=yield.dir)
  
  C_at_age.dir<-file.path(data.path,inOut.dir,in.dir,'catch_at_age')
  dir.create(C_at_age.dir,showWarnings = FALSE)
  plot_c_at_age_dist(out_dir= C_at_age.dir)
}



list.files(path=data.path,pattern="_tab_*.*")

table.dir<-file.path(data.path,inOut.dir,in.dir,'Tables')
dir.create( table.dir,showWarnings = FALSE)
tab_files<-list.files(path=data.path,pattern="_tab_*.*")

ofiles<-c('_tab_C1.html'
          ,'_tab_C4.html'
          ,'_tab_C4.txt'
          ,'_tab_Lsea.html'
          ,'_tab_Lsea.txt'
          ,'_tab_WECA.txt'
          ,'_tab_WECA.html'
          ,'_tab_WEST.txt'
          ,'_tab_WEST.html'
          ,'_tab_Yield4.html'
          ,'_tab_oth_N.html'
          ,'_tab_oth_N.html' )
 lapply(ofiles,function(f){
   if (any(grepl(pattern=f,tab_files)))  file.copy(file.path(data.path,f), table.dir,overwrite=TRUE)
 })          

 

 
 
 otherPred.dir<-file.path(data.path,inOut.dir,in.dir,'OtherPredators')
 if (make_plots) {  
   dir.create(otherPred.dir,showWarnings = FALSE)
   otherPred.dir<-file.path(inOut.dir,in.dir,'OtherPredators')
   dirs<-dirs.keyrun  # directory files to be compared
   labels<-labels.keyrun # output legends
   
   compare_runs_other_predators(
     dirs=dirs.keyrun,  # directory files to be compared
     labels=labels.keyrun,  # output legends
     paper=TRUE,   # graphs on file (paper=TRUE) or on screen (paper=FALSE)
     first.year.on.plot=my.first.year.on.plot,
     last.year.on.plot=my.last.year.on.plot,
     otherPred.dir=otherPred.dir,
     makeAllGraphs=TRUE,quarterN=1,
     nonFish=c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill') #biomass make no sense for non-fish
   )
 }  
 
 if (make_html) {
   wr2(paste('# Input'))

   wr2(paste('## Weight at age in the sea',tabset))
   vdir<-file.path(data.path,inOut.dir,in.dir,"west")
   do_fig_sub(vdir,cap='Mean weight (kg) at age in the sea.',level=3,labels=c("young","older") )
   
   wr2(paste('## Catch numbers',tabset))
   vdir<-file.path(data.path,inOut.dir,in.dir,'c.obs')
   do_fig_sub(vdir,cap='Catch number (thousands) at age.',level=3,labels=c("young","0lder") )
   
   wr2(paste('## Other predators abundance',tabset))
   vdir<-file.path(data.path,inOut.dir,in.dir,'OtherPredators')
   do_fig_sub(vdir,cap='Abundance or biomass.',level=3,labels=c("numbers","biomass") )
 }  
 
 
 resid.dir<-file.path(data.path,inOut.dir,diag.dir,'Residuals')
 if (make_plots)  dir.create(resid.dir,showWarnings = FALSE)
 
 
 stomach.dir<-file.path(data.path,inOut.dir,in.dir,'Stomachs')
 stomach.dir.old.new<-file.path(data.path,inOut.dir,in.dir,'Stomachs_old_new')
 resid.dir<-file.path(data.path,inOut.dir,diag.dir,'Residuals')
 stom.resid.dir<-file.path(data.path,inOut.dir,diag.dir,'Residuals','Stomachs')
 stom.resid.dir.obsEst<-file.path(stom.resid.dir,'obs_est')
 stom.resid.dir.stan_resid<-file.path(stom.resid.dir,'stan_resid')
 stom.resid.dir.raw_resid<-file.path(stom.resid.dir,'raw_resid')
 stom.resid.dir.comp_resid<-file.path(stom.resid.dir,'comp_resid')
 stom.resid.dir.pl_resid<-file.path(stom.resid.dir,'pl_resid')
 stom.resid.dir.like<-file.path(stom.resid.dir,'likelihood')
 stomach.size.dir<-file.path(data.path,inOut.dir,out.dir,'Stom_size_selection')
 
 if (make_plots) {  
   dir.create( stomach.dir,showWarnings = FALSE)
   dir.create( stomach.dir.old.new,showWarnings = FALSE)
   dir.create( resid.dir,showWarnings = FALSE)
   dir.create( stom.resid.dir,showWarnings = FALSE)
   dir.create( stom.resid.dir.obsEst,showWarnings = FALSE)
   dir.create( stom.resid.dir.stan_resid,showWarnings = FALSE)
   dir.create( stom.resid.dir.raw_resid,showWarnings = FALSE)   
   dir.create( stom.resid.dir.comp_resid,showWarnings = FALSE)
   dir.create( stom.resid.dir.pl_resid,showWarnings = FALSE)
   dir.create( stom.resid.dir.like,showWarnings = FALSE)
   
   source(file.path(prog.path,'plot_stomach_data_makeAllGraphs.r'))
 }

 if (make_html) {
   wr2(paste('## Stomach contents',tabset))
   vdir<-stomach.dir
   do_fig_sub(vdir,cap='Relative stomach contents weight by predator size class.',level=3,decreasing=TRUE,labels=NULL,lp=c(7,4)) 
     
 }
 
 if (make_html) {
   wr2(paste('## Stom. cont. old and new',tabset))
   vdir<-stomach.dir.old.new
   do_fig_sub(vdir,cap='Relative stomach contents weight by predator size class for the two key runs.',level=3,decreasing=TRUE,labels=NULL,lp=c(7,4)) 
   
 }
 ############# Parameters
 
 
 if (make_html) {
   wr2(paste('# Parameters'))
 }   
 
 
 par.table.dir<-'Tables'
 par.figures.dir<-'Figures'
 par.tables.dir<-file.path(data.path,inOut.dir,par.dir, par.table.dir)
 par.figures.dir<-file.path(data.path,inOut.dir,par.dir,  par.figures.dir)
 
 if (make_plots) {  
   dir.create(par.figures.dir,showWarnings = FALSE)
   dir.create(file.path(par.figures.dir,'Catchability'),showWarnings = FALSE)
   dir.create(file.path(par.figures.dir,'Vulnerability'),showWarnings = FALSE)
   dir.create(file.path(par.figures.dir,"Survey_observations"),showWarnings = FALSE)

   dir.create(par.tables.dir,showWarnings = FALSE)
   
   source(file.path(prog.path,'parameters_HTML.R'))
   do_parameters(tables=par.tables.dir,figures=par.figures.dir)
 }
 if (make_html) {
   wr2(paste('## F, Year effect'))
   insertPicture(filename=file.path(par.figures.dir,'uncertanty_F_year_effect.png'),caption='Year effect in separable model for F. Years without uncertainty values have the (input) value 1, or catches are missing from that year.')
   
   wr2(paste('## N first year'))
   insertPicture(filename=file.path(par.figures.dir,'uncertanty_N_first_year.png'),caption='Stock number (N) at age in the first year.')
 
    wr2(paste('## N first age'))
   insertPicture(filename=file.path(par.figures.dir,'uncertanty_recruit.png'),caption='Recruitment, N at the first age.')
   
   
   wr2(paste('## Survey catchability ',tabset))
   vdir<-file.path(par.figures.dir,'Catchability')
   do_fig(vdir,cap='Survey catchabiliy by fleet. Ages shown with no error bar have the same catchability and error bar as the previous age.')
   
   wr2(paste('## Survey obs. s2',tabset))
   vdir<-file.path(par.figures.dir,'Survey_observations')
   do_fig(vdir,cap='Survey observation variance by fleet. Ages shown with no error bar have the same variance and error bar as the previous age.')
   
   
 }
 

 
 ##############  output
 
 if (make_html) {
   wr2(paste('# Output'))
 }   

table.dir<-file.path(data.path,inOut.dir,out.dir,'Tables')
dir.create( table.dir,showWarnings = FALSE)
tab_files<-list.files(path=data.path,pattern="_tab_*.*")

ofiles<-c('_tab_F1.html'
          ,'_tab_F4.html'
          ,'_tab_F4.txt'
          ,'_tab_M1M2.html'
          ,'_tab_M1M2.txt'
          ,'_tab_M1M2_1.html'
          ,'_tab_M1M2_1.txt'
          ,'_tab_N.txt'
          ,'_tab_N.html'
          ,'_tab_N_annu.txt'
          ,'_tab_N_annu.html'
          ,'_tab_summary.html'
          )
lapply(ofiles,function(f){
  if (any(grepl(pattern=f,tab_files)))    file.copy(file.path(data.path,f), table.dir,overwrite=TRUE)
})          

 
StockSummary.dir<-file.path(data.path,inOut.dir,out.dir,'StockSummary')
if (make_plots) {  
  dir.create(StockSummary.dir,showWarnings = FALSE)
  source(file.path(prog.path,'plot_summary_ices_multi.r'))
  plot_summary_ices_multi(
    Portrait=T,                 # graphical output orientation
    include.terminal.year= FALSE,          # plot terminal year (last assessment year +1) as well?
    include.last.assess.year.recruit=FALSE,          # plot recruits terminal year as well?
    first.year= -1974,                #first year on plot, negative value means value defined by data
    last.year= 2300,             #last year on plot
    incl.M2.plot=TRUE,
    incl.reference.points=TRUE,
    output.dir=StockSummary.dir,
    my.dev='png') 
    
}


if (make_html) {
  wr2(paste('## Stock summary',tabset))
  vdir<-file.path(data.path,inOut.dir,out.dir,'StockSummary')
  do_fig(vdir,cap='Catch weight, Recruitment, F, SSB, Biomass removed due to fishery (F), predation by SMS species (M2) and residual natural mortality (M1). The predation mortality (M2) presented by the 0-group (black solid line) is for the second half of the year. The M2 for the rest of the ages are annual values.')
}

PartialM2.dir<-file.path(data.path,inOut.dir,out.dir,'PartialM2')
if (make_plots) {  
  dir.create(PartialM2.dir,showWarnings = FALSE)
  dir.create(file.path(PartialM2.dir,'Annually'),showWarnings = FALSE)
  dir.create(file.path(PartialM2.dir,'Quarterly'),showWarnings = FALSE)
  PartialM2.dir<-file.path(inOut.dir,out.dir,'PartialM2')
  source(file.path(prog.path,'part_m2.r'))
  partial_M2(
    nox=2,
    noy=3,
    pred_condense_file="pred_format.dat",
    MyPalette=MyPalette,
    makeAllGraphs=TRUE,
    PartialM2.dir= PartialM2.dir
  ) 
}

if (make_html) {
  wr2(paste('##  Predation mortality (M2)'))
  wr2(paste('###  Annual M2.',tabset))
  vdir<-file.path(data.path,inOut.dir,out.dir,'PartialM2','Annually')
  do_fig(vdir,cap="Annual predation mortality (sum of quarterly M2)",level=4 )
  wr2(paste('### Quarterly M2.',tabset))
  vdir<-file.path(data.path,inOut.dir,out.dir,'PartialM2','Quarterly')
  do_fig_sub(vdir,cap="Quarterly predation mortality (M2)",level=4,decreasing=FALSE,labels=paste0("Q:",1:4) )
}

if (make_html) {
  vdir<-stomach.size.dir
  wr2(paste('##  Size selection'))
  wr2(paste('###  by predator'))
  insertPicture(filename=file.path(vdir,"size_all-Cod.png" ),
                caption="Cod size selection, Observed and estimated.")
  wr2(paste('###  by predator and prey'))
  insertPicture(filename=file.path(vdir,"size_prey-Cod.png" ),
                caption="Cod size selection by prey, Observed and estimated.")
 
}



whoEatsWhom.dir<-file.path(data.path,inOut.dir,out.dir,'WhoEatsWhom')
if (make_plots) {  
  dir.create(whoEatsWhom.dir,showWarnings = FALSE)
  whoEatsWhom.dir<-file.path(inOut.dir,out.dir,'WhoEatsWhom')
  source(file.path(prog.path,'who_eats_whom.r'))
  who_eats_whom(
    first.year=my.first.year.on.plot,                #first year on plot, negative value means value defined by data
    last.year=my.last.year.on.plot,                  #last year on plot
    OperatingModel=FALSE,   # include data from forecast 
    op.dir=data.path,
    makeAllGraphs=TRUE,
    output.dir=data.path,
    my.colors=MyPalette
  )
    
  
  file.copy(file.path(data.path,'who_eats_whom_combined.csv'),whoEatsWhom.dir,overwrite=TRUE)
  file.copy(file.path(data.path,'who_eats_whom_level1.csv'),whoEatsWhom.dir,overwrite=TRUE)
  file.copy(file.path(data.path,'who_eats_whom_level2.csv'),whoEatsWhom.dir,overwrite=TRUE)
  file.copy(file.path(data.path,'who_eats_whom_level3.csv'),whoEatsWhom.dir,overwrite=TRUE)
  
}


M2_quarterly.dir<-file.path(data.path,inOut.dir,out.dir,'M2_quarter')
if (make_plots) { 
  dir.create(M2_quarterly.dir,showWarnings = FALSE)
  
  file.name<-'M2summary'
  nox<-3; noy<-3;
  noxy<-nox*noy
  
  #####################
  
  dat<-Read.summary.data(read.init.function=F)
  dat <-subset(dat, Z>0,select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,Z))
  
  
  dat<-subset(dat,Year<=my.last.year.on.plot )
  if (my.first.year.on.plot>0) dat<-subset(dat,Year>=my.first.year.on.plot )
  
  for (sp in (first.VPA:nsp)){
    sp.name<-sp.names[sp]
    
    newplot(dev='png',nox,noy,filename=paste(file.name,'_',sp.name,sep=''),Portrait=TRUE,w8=8,w11=10,dir=M2_quarterly.dir)
    par(mar=c(3,4,3,2))
    
    s<-subset(dat,Species.n==sp)
    b<-tapply(s$M2,list(s$Year,s$Age),sum)
    b<-apply(b,2,mean)
    M2<-tapply(s$M2,list(s$Quarter,s$Year,s$Age),sum)
    M2[is.na(M2)]<-0
    for (a in (1:length(b))) {
      if (b[a]>0.001 & a<=noxy) barplot(M2[,,a],space=0.5,xlab='',ylab='',main=paste(sp.name,", Age ",a-1,sep=''),col=c(1,2,3,4))
    }
    cleanup()
  }
}  



M2_smooth.dir<-file.path(data.path,inOut.dir,out.dir,'M2_smooth')
if (make_plots) { 
  dir.create(M2_smooth.dir,showWarnings = FALSE)
  
  dat$M1M2<-dat$M1+dat$M2
  library(mgcv)
  b<-aggregate(M1M2~Species.n+Species+Year+Age,sum,data=dat )
  
  by(b,list(b$Species),function(x) {
    png(filename=file.path(M2_smooth.dir,paste0('M2_smooth_',x[1,"Species"],'.png')),width=800,height=800 )
    #X11()
    print(ggplot(x,aes(Year,M1M2)) +
            theme_bw() +
            geom_point() +
            geom_smooth(method = "gam",formula = y ~ s(x, bs = "cs")) +
            #facet_wrap(~ paste0(Species,' Age:',Age), scale="free_y") +
            facet_wrap(~ paste0(Species,' Age:',formatC(Age,width=2))) +
            labs(x="Year", y="M=M1+M2",title=""))
    cleanup()
    
  })
}

if (make_html) {
   wr2(paste('###  M (M1+M2) smoothed',tabset))
  vdir<-M2_smooth.dir
  do_fig(vdir,cap="Annual natural mortalities (M) values smoothed (sum of quarterly M1+M2)",level=4 )
}



if (make_plots) { 
 
  bb<- by(b,list(b$Species,b$Age),function(x) {
    a<-gam(M1M2 ~s(Year,bs='cs'),data=x)
    return(data.frame(Species=x$Species,Species.n=x$Species.n,Year=x$Year,Age=x$Age,M=predict(a)))
  })
  
  M<-do.call(rbind,bb)

  tab_annual_sum_data<-function(data,vari='C.obs',outfile,outDir=data.path,title,decimals=0,tableNo=1,my.species=c(first.VPA:SMS.control@no.species)) {
    if (file.exists(file.path(outDir,outfile))) file.remove(file.path(outDir,outfile))
    for (sp in my.species) {
      a<-subset(data,Species.n==sp)
      if (dim(a)[1] >0) {
        if (vari=='C.obs') tab1<-tapply(a$C.obs,list(a$Year,a$Age),sum)
        if (vari=='F') tab1<-tapply(a$F,list(a$Year,a$Age),sum)
        if (vari=='M2') tab1<-tapply(a$M2,list(a$Year,a$Age),sum)
        if (vari=='M1M2') tab1<-tapply(a$M2+a$M1,list(a$Year,a$Age),sum)
        if (vari=='M') tab1<-tapply(a$M,list(a$Year,a$Age),sum)
        if (vari=='ration') tab1<-tapply(a$ration,list(a$Year,a$Age),sum)
        if (vari=='consum') tab1<-tapply(a$consum,list(a$Year,a$Age),sum)
        
        if (sum(tab1,na.rm=T)>0) {
          colnames(tab1)<-sort(unique(a$Age))
          xtab(tab1, caption=paste(sp.names[sp],title),
               cornername='Year/Age',
               file=file.path(outDir,'_tmp.html'), dec=rep(decimals,length(unique(a$Age))), width='"100%"')
          if (length(my.species)==1) {
            file.copy(file.path(outDir,'_tmp.html'), file.path(outDir,outfile), overwrite = T, recursive = FALSE)
          } else file.append(file.path(outDir,outfile), file.path(outDir,'_tmp.html'))
          
        }
        file.remove(file.path(outDir,'_tmp.html'))
      }
    }
  }

 tab_annual_sum_data(data=M,vari='M',outfile='Msmooth.html',outDir=table.dir, title='Smoothed M',decimals=3)
}
  
  
# who eats, sankey plot
sankey.dir<-file.path(data.path,inOut.dir,out.dir,'BiomassFlow')
if (make_plots) {  
  dir.create(sankey.dir,showWarnings = FALSE)
  
  source(file.path(prog.path,'sankey.r'))
  do_sankey(years=c(1974,2000,2021), out.dir= sankey.dir, excl_sp=c('Plaice','Sole'),incl_catch=TRUE,do_show=FALSE) 
}

if (make_html) {
  vdir<-file.path(data.path,inOut.dir,out.dir,'WhoEatsWhom')
  wr2(paste('##  Who eats whom'))
  wr2(paste('###  All preys eaten by predator'))
  insertPicture(filename=file.path(vdir,"WhoEats_All_Comb_Predators.png" ),
                caption="Biomass of all preys eaten by predator")
  wr2(paste('###  Preys eaten by all predator'))
  insertPicture(filename=file.path(vdir,"WhoEats_All_Comb_Preys.png" ),
                caption="Biomass eaten by prey ")
  
  wr2(paste('###  Preys eaten by predator', tabset))
  do_fig(vdir,cap="Who eats this prey.",filter="WhoEatsOne_",level=4)
  
 # wr2(paste('###  Biomass eaten flow', tabset))
#  vdir<-file.path(data.path,inOut.dir,out.dir,'BiomassFlow')
 # do_widget(vdir,cap="Flow, biomass eaten (weight in kt).",filter="sankey_",level=4)
}


# Exploitation pattern
exploiP.dir<-file.path(data.path,inOut.dir,out.dir,'ExploiPattern')
if (make_plots) {  
  dir.create(exploiP.dir,showWarnings = FALSE)
  plotExploitationPattern(indir=data.path,outdir=exploiP.dir)
}


if (make_html) {
  wr2(paste('## Exploitation pattern',tabset))
  vdir<-exploiP.dir
  do_fig(vdir,cap='Exploitation pattern, scaled to Fbar.')
}


#Uncertainties


uncertain.dir<-file.path(data.path,inOut.dir,out.dir,'Uncertainties')
if (make_plots) {  
  dir.create(uncertain.dir,showWarnings = FALSE)
  file.copy(file.path( html_dir,'par_names.csv'),file.path(data.path,'par_names.csv'),overwrite=TRUE)
  
  # only part of the script is used source(file.path(prog.path,'cv.r'))
  source(file.path(prog.path,'cv.r'))
  if (do_uncertanties_M2) {
    file.copy(file.path(data.path,'Uncertanties_M2_age0.png'),uncertain.dir,overwrite=TRUE)
    file.copy(file.path(data.path,'Uncertanties_M2_age1.png'),uncertain.dir,overwrite=TRUE)
    file.copy(file.path(data.path,'Uncertanties_M2_age2.png'),uncertain.dir,overwrite=TRUE)
  }
  file.copy(file.path(data.path,'CV_Recruit.png'),uncertain.dir,overwrite=TRUE)
  file.copy(file.path(data.path,'CV_SSB.png'),uncertain.dir,overwrite=TRUE)
  file.copy(file.path(data.path,'CV_avg_F.png'),uncertain.dir,overwrite=TRUE)
}

if (make_html) {
  vdir<-uncertain.dir
  files<-c("CV_SSB.png","CV_Recruit.png","CV_avg_F.png","Uncertanties_M2_age0.png","Uncertanties_M2_age1.png","Uncertanties_M2_age2.png")
  labels=c("SSB","Recruit","mean F","M2 age 0","M2 age 1","M2 age 2")
  caps=c("Uncertanties (1 sd / value) of Spawning stock biomass.","Uncertanties (1 sd / value) of Recruitment.",
         "Uncertanties (1 sd / value) of mean F.",
         "Age 0. Estimate of M2 and +- 2 times sd",
         "Age 1. Estimate of M2 and +- 2 times sd","Age 2. Estimate of M2 and +- 2 times sd")
  if (!do_uncertanties_M2) {
    files<-files[1:3]
    labels<-labels[1:3]
    caps<-caps[1:3]
  }
  
  do_figs(vdir=vdir,caps=caps,level=2,files=files,labels=labels,header='Uncertanties')
}

  

# Diagnostics #############################

#Retrospective
retro.dir<-file.path(data.path,inOut.dir,diag.dir,'Retrospective')
if (make_plots) {  
  dir.create(retro.dir,showWarnings = FALSE)
  retro.dir<-file.path(data.path,inOut.dir,diag.dir,'Retrospective','Summary')
  dir.create(retro.dir,showWarnings = FALSE)
  
  rfile<-file.path(data.path,'Retro_files.Rdata')
  if (!file.exists(rfile)) source(file.path(prog.path,'retrospectiv_multi_sp.r'))  # makes tables and SMS.RData 
  if (file.exists(rfile)) {
    load(file=rfile,verbose=TRUE)
   
     cp_retro<-function(rfiles){
      retro_files<- paste0(rfiles,'.png')
      for (from.file in retro_files) {
        to.file<-file.path(retro.dir,from.file)
        file.copy(from.file, to.file, overwrite = TRUE)
      }
    }
    cp_retro(retro_files)
    
    retro.dir<-file.path(data.path,inOut.dir,diag.dir,'Retrospective','M2')
    dir.create(retro.dir,showWarnings = FALSE)
    cp_retro(retro_files_M2)
  }
}


if (make_html) {
  wr2(paste('# Diagnostics'))
}
  

resid.dir<-file.path(data.path,inOut.dir,diag.dir,'Residuals','Catch')
if (make_plots) {
  dir.create(resid.dir,showWarnings = FALSE)
  plot.catch.residuals2(standardize=F,dev='png',nox=2,noy=2,Portrait=F,use.ref.dot=TRUE,add.title=T,over.all.max=0.5,oDir=resid.dir)
}

resid.dir<-file.path(data.path,inOut.dir,diag.dir,'Residuals','Survey')
if (make_plots) {  
  dir.create(resid.dir,showWarnings = FALSE)
  plot.survey.residuals2(standardize=F,reverse.colors=T,dev='png',oDir= resid.dir,pointsize=12,nox=2,noy=2,Portrait=F,start.year=1974,end.year=2023,over.all.max=0.5,my.species=NA)
}


if (make_html) {
  wr2(paste('## Residuals'))
  
  vdir<-file.path(data.path,inOut.dir,diag.dir,'Residuals','Catch')
  wr2(paste('### Catch residuals',tabset))
  do_fig(vdir,cap="Catch observations Residuals, log(Survey observed CPUE) - log(expected CPUE). Red is positive, White is negative. Q:9 means annual data.",level=4)
  
  vdir<-file.path(data.path,inOut.dir,diag.dir,'Residuals','Survey')
  wr2(paste('### Survey residuals',tabset))
  do_fig(vdir,cap="Survey observations Residuals, log(Survey observed CPUE) - log(expected CPUE). Red is positive, White is negative.",level=4)
  
  vdir<-stom.resid.dir.raw_resid
  wr2(paste('### Stomach residuals (Raw)',tabset))
  do_fig_sub(vdir,cap="Stomach observation residuals (raw = (observed proportion) - (expected proportion)). For residual plots, White is positive, green is negative.",level=4,decreasing=TRUE,labels=c("QQ plot","by year quarter","by quarter year","box plots"))
  
  vdir<-stom.resid.dir.stan_resid
  wr2(paste('### Stomach residuals (Standardised)',tabset))
  do_fig_sub(vdir,cap="Stomach observation residuals (Standardised = (observed-expected)/sd(expected)). For residual plots, White is positive, green is negative.",level=4,decreasing=TRUE,labels=c("QQ plot","by year quarter","by quarter year"))

  if (make_compositional_residuals) {
    vdir<-file.path(stom.resid.dir.comp_resid)
    wr2(paste('### Stomach resid. (compResidual)',tabset))
    do_fig_sub(vdir,cap="Stomach observation residuals (Residual calculation for compositional observations, see R Package compResidual). For residual plots, White is positive, green is negative.",level=4,decreasing=TRUE,labels=c("QQ plot","by year quarter","by quarter year"))
  }
}  


if (make_html) {
  wr2(paste('## Retrospective analysis'))
  
  wr2(paste('### Retro. summary',tabset))
  vdir<-file.path(data.path,inOut.dir,diag.dir,'Retrospective','Summary')
  do_fig(vdir,cap='Model retrospective analysis.',level=4)
  
  wr2(paste('### Retro. M2',tabset))
  vdir<-file.path(data.path,inOut.dir,diag.dir,'Retrospective','M2')
  do_fig(vdir,cap='Model retrospective analysis of M2.',level=4)
}


if (make_html) {
  wr2(paste('## Stomach contents',tabset))
  vdir<-stom.resid.dir.obsEst 
  do_fig_sub(vdir,cap='Observed and estimated stomach contents weight by predator size class.',filter="obs_est_st",level=3,decreasing=TRUE,labels=NULL,lp=c(7,4)) 
  
}



###### comparisons
compare.dir<-file.path(data.path,inOut.dir,out.dir,'Comparisons')
if (make_plots) { 
  dir.create(compare.dir,showWarnings = FALSE)
  compare.dir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','summary')
  dir.create(compare.dir,showWarnings = FALSE)
  compare.dir<-file.path(inOut.dir,out.dir,'Comparisons','summary')

root  <-root.copy 
  compare_runs(
    dirs=dirs.keyrun,
    labels=labels.keyrun,
    paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
    run.ID='summary',         # file id used for paper output
    first.year.on.plot= my.first.year.on.plot,
    last.year.on.plot=my.last.year.on.plot,
    makeAllGraphs=TRUE,
    compare.dir=compare.dir,
    single.species=TRUE                   # single species mode or multispecies mode
  )
  
  compare.dir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','M2')
  dir.create(compare.dir,showWarnings = FALSE)
  compare.dir<-file.path(inOut.dir,out.dir,'Comparisons','M2')
  compare_runs_M2(
    dirs=dirs.keyrun,
    labels=labels.keyrun,
    paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
    run.ID='summary',         # file id used for paper output
    makeAllGraphs=TRUE,
    first.year.on.plot= my.first.year.on.plot,
    last.year.on.plot=my.last.year.on.plot
  )
 
  compare.dir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','SSB_R')
  dir.create(compare.dir,showWarnings = FALSE)
  compare.dir<-file.path(inOut.dir,out.dir,'Comparisons','SSB_R')
  
  compare_runs_stock_rec(
    dirs=dirs.keyrun,
    labels=labels.keyrun,
    first.year.on.plot= my.first.year.on.plot,
    last.year.on.plot=my.last.year.on.plot,
    makeAllGraphs=TRUE,
    compare.dir=compare.dir,
    incl.sp="all",include.CV=TRUE,include.CV2=TRUE,include.mean=TRUE,
    nox=2, noy=1, w8=5,w11=11,include.year.labels=TRUE,incl_not_used=FALSE,run.ID='SBB_rec', paper=TRUE,facSSB=1000,facRec=1000000) 
  
  cleanup()
  
  compare.dir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','ICES')
  dir.create(compare.dir,showWarnings = FALSE)
  compare.dir<-file.path(inOut.dir,out.dir,'Comparisons','ICES')

  if (compare_with_single_species) compare_runs(
    dirs=dirs.single,
    labels=labels.single,
    paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
    run.ID='single_ICES',         # file id used for paper output
    first.year.on.plot= my.first.year.on.plot,
    last.year.on.plot=my.last.year.on.plot,
    makeAllGraphs=TRUE,
    compare.dir=compare.dir,
    single.species=TRUE
  )

 
    
  cleanup()
}




if (make_html) {
  wr2(paste('## Comparison, key-runs'))
  
  wr2(paste('### Key-runs, summary',tabset))
  vdir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','summary')
  do_fig(vdir,cap='Stock summary.',level=4)
  
  wr2(paste('### Key-runs M2',tabset))
  vdir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','M2')
  do_fig(vdir,cap='M2.',level=4)
  
  wr2(paste('### Key-runs SSB/R',tabset))
  vdir<-file.path(data.path,inOut.dir,out.dir,'Comparisons','SSB_R')
  do_fig(vdir,cap='Stock-recruitment. Values of SSB and recruits, and fitted stock recruitment relation (median, mean, +-1SD, +-2 SD). "Red" values are not used in the fit',level=4)
}

