
# Harddisk drive for SMS
dosDrive<-"C:"  

# root directory for the SMS package
rootLight<-file.path(dosDrive,"SMS-san")
dir.create(rootLight,showWarnings = FALSE)

SMSconf<-c("Baltic_sprat")

RprogFiles<-c(
  "compare_runs.r"                    
 ,"CV.R"                               ,"do_a_full_sms_run.R"                                                                             
 ,"init.R"                             ,"install_FLR.r"                     
 ,"Plot_catch_residuals_bubble.R"      ,"San_Tables_report.r"
 ,"Plot_obs_model_catch.R"             ,"SAN_effort.r"
 ,"Plot_SSB_rec.R"                     ,"Plot_stomach_data.R"               
 ,"Plot_summary_ICES_multi.R"          ,"Plot_survey_residuals_bubbles.R"   
 ,"Retrospectiv_single_sp.R"
 ,"Weighting_factor_single_sp.R"       ,"XSA2SMS.R")         


for (conf in SMSconf) {
  dir.create(file.path(rootLight,conf),showWarnings = FALSE)
  allFiles<-list.files(file.path(root,conf),full.names = F)
  
  for (fF in allFiles ) {
    from.file <-file.path(file.path(root,conf,fF))
    to.file<-file.path(file.path(rootLight,conf,fF))
    file.copy(from.file, to.file, overwrite = TRUE)
}}

myDir<-"program"
dir.create(file.path(rootLight,myDir),showWarnings = FALSE)
for (fF in c("sms.exe","sms.tpl")) {
  from.file <-file.path(file.path(root,myDir,fF))
  to.file<-file.path(file.path(rootLight,myDir,fF))
  file.copy(from.file, to.file, overwrite = TRUE)
}

myDir<-"R_Prog"
dir.create(file.path(rootLight,myDir),showWarnings = FALSE)
for (fF in RprogFiles) {
  from.file <-file.path(file.path(root,myDir,fF))
  to.file<-file.path(file.path(rootLight,myDir,fF))
  file.copy(from.file, to.file, overwrite = TRUE)
}

for (conf in c("Function","FLSMS")) {
  dir.create(file.path(rootLight,myDir,conf),showWarnings = FALSE)
  allFiles<-list.files(file.path(root,myDir,conf),full.names = F)
  
  for (fF in allFiles ) {
    from.file <-file.path(file.path(root,myDir,conf,fF))
    to.file<-file.path(file.path(rootLight,myDir,conf,fF))
    file.copy(from.file, to.file, overwrite = TRUE)
}}


