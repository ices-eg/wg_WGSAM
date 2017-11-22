Read.catch.survey.residuals<-function(read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(data.path,'catch_survey_residuals.out')
  s<-read.table(file,header=TRUE)
  a<-data.frame(Species=sp.names[s$Species.n],col=ifelse((s$data=='catch'),1,2),s)
  subset(a,a$residual> -99.9)
}

