######################################################################################################
# Triple plot
#
# Version 1.00, 28/08/2007 10:59:53
#
# Author: Mark Payne
# DIFRES, Charlottenlund, DK
#
# Plots SSB, F.bar and Recruitment on a single stacked plot.
# Functions here are called from elsewhere
#
# Changes:
# V 1.00  - First release
####################################################################################################

plot.triple.panel<-function(x,label,xlimits,show.x.lbl=FALSE,legend.pos=NULL,legend.ncol=1) {
      yr  <-  as.numeric(dimnames(x)[[2]])
      n.runs  <-  dim(x)[3]
      plot(yr,x[sp,,1],ann=FALSE,type='b',ylim=c(0,max(x[sp,,],na.rm=T)),xlim=xlimits,
          lwd=3,pch=1,xaxt=if(show.x.lbl){"s"} else {"n"})
      mtext(label,las=0,side=2,padj=-6)
      grid()
      if (n.runs>1) for (yy in (2:n.runs)) {
        lines(yr,x[sp,,yy],type='b',pch=yy,col=1,lwd=1)
      }
      if(!is.null(legend.pos)) {
          run.names <-  dimnames(x)[[3]]
          legend(x=legend.pos,legend=as.character(run.names),lwd=c(2,rep(1,n.runs-1)),
              pch=seq(1,n.runs),col=1,bg="white",pt.bg="white",ncol=legend.ncol)
      }
    }

plot.triple<-function(legend.pos=NULL,legend.ncol=1) {
    layout(matrix(c(1,2,3),3,1,byrow=TRUE), height=c(1,1,1), width=1)
    oldpar  <-  par(oma=c(2.5,0.5,1.5,0.5),cex=1,las=1,mar=c(0,6,0,0))
    xlims   <-  range(pretty(as.numeric(c(dimnames(SSB)[[2]],dimnames(FI)[[2]],dimnames(REC)[[2]]))))
    for (sp in (1:nsp)) {
        #Not sure if this works in multispecies mode or not - needs to be checked. MRP 20070827
        if(nsp>1) {stop("Unsure whether this works in mulitspecies mode. Check it properly!! - MRP 20070827")}
        plot.triple.panel(SSB,"SSB (kt)",xlims,legend.pos=legend.pos,legend.ncol=legend.ncol)
        plot.triple.panel(FI,"F bar",xlims)
        plot.triple.panel(REC,"Recruits (10^6)",xlims,show.x.lbl=TRUE)
    }
    par(oldpar)
}

