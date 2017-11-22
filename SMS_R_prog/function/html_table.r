# function to write HTML output from a matix object
# code copied from Anders Nielsen, DTU Aqua.

xtab<-function(x,caption='Table X.', file=stdout(), width='"100%"', units=NULL,cornername='', dec=rep(1,ncol(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
         sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))

  if (length(units)>0) {
    unames<-units
    unames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',unames)), collapse='\t')
    lin<-c(lin,paste('<tr>',paste('<td align=left><b>',' ','</b></td>',sep=''),unames,'</tr>'))
    lin<-c(lin,hr)
  }

  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  #x<-sapply(1:ncol(x),function(i)sub('NA','  ',format(round(x[,i],dec[i]))))
  x<-sapply(1:ncol(x),function(i)sub('NA','  ',formatC(round(x[,i],dec[i]),digits=dec[i], format='f')))
  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}
# as xtab but use format by row
xtab3<-function(x,caption='Table X.', file=stdout(), width='"100%"', units=NULL,cornername='', dec=rep(1,nrow=nrow(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
         sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))

  if (length(units)>0) {
    unames<-units
    unames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',unames)), collapse='\t')
    lin<-c(lin,paste('<tr>',paste('<td align=left><b>',' ','</b></td>',sep=''),unames,'</tr>'))
    lin<-c(lin,hr)
  }

  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  #x<-sapply(1:ncol(x),function(i)sub('NA','  ',format(round(x[,i],dec[i]))))
  x<-sapply(1:nrow(x),function(i)sub('NA','  ',formatC(round(x[i,],dec[i]),digits=dec[i], format='f')))
  x<-t(x)
  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}

xtab2<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=matrix(1,ncol=ncol(x),nrow=nrow(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
         sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
  lin<-c(lin,hr)
  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  xx<-x
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      x[i,j]<-sub('NA','  ',formatC(round(xx[i,j],dec[i,j]),digits=dec[i,j], format='f'))
    }
  }

  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}