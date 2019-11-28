# script to make inclusion (=1) or exclusion (=0) of species year S/R data

ofile='recruitment_years.in'
yy<-SMS@first.year:SMS@last.year
cat("#",yy,'\n',file=ofile)
for (sp in (first.VPA:nsp)) cat(rep('   1',length(yy)),' # ',sp.names[sp],'\n',file=ofile,append=T)
