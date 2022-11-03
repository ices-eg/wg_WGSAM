plot_summary_ices_multi(
  Portrait=T,                 # graphical output orientation
  include.terminal.year= FALSE,          # plot terminal year (last assessment year +1) as well?
  include.last.assess.year.recruit=FALSE,          # plot recruits terminal year as well?
   
  first.year= -1974,                #first year on plot, negative value means value defined by data
  last.year= 2050,             #last year on plot
  incl.M2.plot=TRUE,
  incl.reference.points=TRUE,
  incl.TSB=FALSE,
  splitLine=FALSE,
  OperatingModel=FALSE,
  redefine.scenario.manually=FALSE,
  output.dir=data.path,
  op.dir=data.path,
  my.dev=c('screen','wmf', 'png', 'pdf')[1]
)
 


