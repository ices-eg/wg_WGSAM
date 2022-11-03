
    system('t.bat',show.output.on.console =T)


compDir<-file.path(root,'MASTER-SAN-area-1-2011')
 compDir<-file.path(root,'NS_tail_1_old')

compFiles<-c('objective_function.out','summary.out','Survey_residuals.out','summary_table.out',"Catch_survey_residuals.out",'sms.rep')
compFiles<-c('objective_function.out','summary.out','Survey_residuals.out','summary_table.out','sms.rep')


for (i in (1:length(compFiles))) {
  a<-readLines(file.path(data.path,compFiles[i]))
  b<-readLines(file.path(compDir,compFiles[i]))

  if (all(a==b)) cat('files ',compFiles[i],' are identical in two runs\n') else {
    cat('files ',compFiles[i],' is different\n')
    cat('differnt lines are:\n')
    #print(a[(a!=b)])
    for (j in (1:length(a))) if (a[j]!=b[j]) {
       cat('\n',a[j],'\n',b[j],'\n')
    }
  }
}
