# user options

targetFs<-seq(0.0,1.0,0.05)
#targetFs<-seq(0.51,0.56,0.005)
#targetFs<-0.7

# area 1 and 2
recruit.noise.low<- -2
recruit.noise.high<-2

no.MCMC.iterations<-1

refYear<-seq(1999,2010,1) # years for calc of mean wsea and weca
refYearPropmat<-seq(1983,1984,1) # years for calc of mean proportion mature

legendPlace<-c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" , "center")[6]

nox<-1; noy<-2;
paper<-F   # if paper==T output on file, else screen
cleanup()

# initialise the function that does all the work
source(file.path(prog.path.func,"MSY_batch.r"))

# Change option in SMS.dat
control<-read.FLSMS.control()
control@read.HCR<-1
write.FLSMS.control(control,write.multi=F) 


# stochastich  run stochastic (TRUE) or determenistic analysis (FALSE)
# YieldPerRecruit do YPR (TRUE) or use SSB recruitment relations (FALSE)

do.HCR.batch(NewPlot=T,stochastich=F,YieldPerRecruit=T)
do.HCR.batch(NewPlot=F,stochastich=T,YieldPerRecruit=F)

if (paper) cleanup()  # to release graphics on file
control<-read.FLSMS.control()
control@read.HCR<-0
write.FLSMS.control(control,write.multi=F) 
