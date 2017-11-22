
# counter for number of graphical files produces, included in file name
graph.counter<-1


newplot<-function(dev,nox,noy,Portrait=FALSE,filename='a',dir=data.path,w8=8,w11=11,pointsize=12)
  {
    noxy<<-nox*noy
    
    if (dev=="ps") {
       filename.out <- paste(filename,'.eps',sep="")   #name of graphical output file
       outfile <- file.path(dir,filename.out)
       postscript(file=outfile,horizontal=FALSE,onefile=FALSE,height=11,width=8,pointsize=pointsize)
    }
    else if (dev=="wmf") {
       filename.out <- paste(filename,'.wmf',sep="")   #name of graphical output file
       outfile <- file.path(dir,filename.out)
        if (Portrait==TRUE) win.metafile(filename=outfile,height=w11,width=w8,pointsize=pointsize) else  win.metafile(filename=outfile,height=w8,width=w11,pointsize=pointsize)
    }
    else if (dev=="print") {
       if (Portrait==TRUE) win.print(height=w11,width=w8,pointsize=pointsize) else win.print(height=w8,width=w11,pointsize=pointsize)
    }
    else if (dev=="testprint") {
       if (Portrait==TRUE) X11(height=w11,width=w8,pointsize=pointsize) else X11(height=w8,width=w11,pointsize=pointsize)
    }
    else if (dev=="screen") {
       if (Portrait==TRUE) X11(height=w11,width=w8,pointsize=pointsize) else X11(height=w8,width=w11,pointsize=pointsize)
    }
    else if (dev=='png'){
       filename.out <- paste(filename,'.png',sep="")   #name of graphical output file
       outfile <- file.path(dir,filename.out)
        if (Portrait==TRUE) png(filename =outfile,width =200*w8,height =200*w11,units = "px",pointsize = pointsize*3,bg = "white") else png(filename = outfile, width =200*w11, height =200*w8, units = "px", pointsize = pointsize*3, bg = "white")
    }
     else X11()

    par(mfcol=c(noy,nox))
  }
