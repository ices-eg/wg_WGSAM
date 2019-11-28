toDir<-file.path("C:","mv","sms","FBA")

if (file.exists(toDir)) unlink(toDir,recursive = T)
dir.create(toDir,showWarnings = FALSE)
dir.create(file.path(toDir,"backup"),showWarnings = FALSE)
dir.create(file.path(toDir,"R-prog"),showWarnings = FALSE)

source(file.path(prog.path,"get_correlation3.r"))

OP.files<-c(
  "area_names.in",
  "just_one.in",
  "species_names.in",
  "op.exe", "OP.dat",
  "op.tpl",
  "OP_C.in",
  "OP_config.dat",
  "OP_trigger.dat",
  "OP_consum.in",
  "OP_Exploitation.in",
  "OP_F.in",
  "OP_M.in",
  "OP_M1.in",
  "OP_N.in",
  "OP_propmat.in",
  "OP_prop_landed.in",
  "OP_size.in",
  "OP_stock_distribution.in",
  "OP_WCATCH.in",
  "OP_WSEA.in",
  "OP_Nnext_F_covariance.in",
  "OP_Nnext_F_labels.in",
  "OP_Nnext_F_value.in",
  "OP_seed.in"
)

for (from.file in OP.files) {
  to.file<-file.path(toDir,"backup",from.file)
  cat(to.file,"\n")
  print(file.copy(from.file, to.file, overwrite = TRUE))
 }

dat<-Read.summary.data(extend=T,read.init.function=F)

a<-subset(dat,Year %in% c(2009,2010,2011) & ((Quarter==1 & Age>0) | (Quarter==3 & Age==0)), select=c(Species.n,Year,Age,N,Quarter))
ftable(tapply(a$N,list(a$Year,a$Species.n,a$Age),sum))
b<-tapply(a$N,list(a$Year,a$Species.n,a$Age),sum)

outfile<-file.path(toDir,"backup","init_pop_2009-2011.dat")
cat("# stock number by year (2009-2011), species and age \n",file=outfile)
for (i in (1:3)) write.table(b[i,,],file=outfile,col.names=F,row.names=F,append=T)


file.copy(file.path(root,"R_prog","FLSMS","FLOP.control.r"),file.path(toDir,"r-prog","FLOP.control.r"), overwrite = TRUE)
file.copy(file.path(root,"R_prog","OP_test.r"),file.path(toDir,"r-prog","OP_test.r"), overwrite = TRUE)
file.copy(file.path(root,"R_prog","get_correlated_noise_Francois.r"),file.path(toDir,"r-prog","get_correlated_noise_Francois.r"), overwrite = TRUE)
file.copy(file.path(root,"R_prog","function","OP_plot.r"),file.path(toDir,"r-prog","OP_plot.r"), overwrite = TRUE)
file.copy(file.path(root,"R_prog","get_correlation3.r"),file.path(toDir,"r-prog","get_correlation3.r"), overwrite = TRUE)

