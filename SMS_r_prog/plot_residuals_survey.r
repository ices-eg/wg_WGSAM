
#Function to read and plot survey residuals
# parameter start.year: first year on X-axis, default=0 (defined from data)
# parameter end.year: end year on X-axis, default=0 (defined from data)
# 
# use over.all.max to set the maximum size of the reference buble. A value of 0 scales bubles individually  
# dev=screen or wmf with output on a wmf file in the default directory

cleanup()

# plot.survey.residuals2(standardize=F,reverse.colors=T,dev='screen',pointsize=12,nox=1,noy=2,Portrait=F,start.year=1974,end.year=2020,over.all.max=1,my.species=NA)
plot.survey.residuals2(standardize=F,reverse.colors=T,dev='png',pointsize=12,nox=2,noy=2,Portrait=F,start.year=1974,end.year=2023,over.all.max=0.5,my.species=NA)

#plot.survey.residuals2(reverse.colors=T,standardize=F,dev='screen',pointsize=12,nox=1,noy=2,Portrait=T,start.year=1990,end.year=2015,over.all.max=5,my.species=NA)

#plot.survey.residuals2(standardize=T,use.ref.dot=FALSE,dev='screen',nox=1,noy=3,Portrait=T,start.year=1990,end.year=2011,over.all.max=5,my.species=NA)
#plot.survey.residuals2(reverse.colors=T,standardize=T,use.ref.dot=FALSE,dev='screen',nox=1,noy=3,Portrait=T,start.year=1990,end.year=2011,over.all.max=5.5,my.species=NA)
