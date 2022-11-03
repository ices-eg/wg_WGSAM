read_op_average_val<-function(dir=data.path){
  a<-read.table(file.path(dir,"op_average_val.out"))
 # res<<mvalue<<" "<<myield<<" "<<mCW<<" "<<Fcombination(r,s)<<" "<<mFbar<<" "<<mSSB<<" "<<mTSB<<" "<<mrec<<" "<<belowBlim<<" "<<belowBpa<<" "<<s<<" "<<r+first_no_run-1<<" "<<iter<<" "<<Fcombination(r)<<endl;
  a<-a[,1:13]
  colnames(a)<-c('value','yield','catch','Fval_target','Fval','SSB','TSB','rec','prob_below_Blim','prob_below_Bpa','species.no','fcomb.no','iter')
  a$species<-sp.names[a$species.no]
  return(a)
}
  

b<-read_op_average_val()

  