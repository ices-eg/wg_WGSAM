copySMSfiles<-function(control,scenario.dir,doSingle=TRUE,doMulti=TRUE,doArea=FALSE,verbose=FALSE) {
  
  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = TRUE)
  dir.create(scenario.dir,showWarnings = FALSE)
  
  SMS.files.single<-c("area_names.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "proportion_M_and_F_before_spawning.in","proportion_landed.in","recruitment_years.in",
                      "zero_catch_season_ages.in","zero_catch_year_season.in","F_q_ini.in","other_catch.in",
                      "Exploitation_pattern.in","covariance_N.in","HCR_options.dat","sms.dat",
                      "SMS.exe")
  
  if (doSingle) for (from.file in SMS.files.single) {
    to.file<-file.path(scenario.dir,from.file)
    out<-file.copy(from.file, to.file, overwrite = TRUE)
    if (verbose) print(paste(from.file,out))
  }
  
  SMS.files.multi<-c("alk_stom.in","consum.in","Length_weight_relations.in","lsea.in","N_haul_at_length.in",
                     "natmor1.in","other_food.in","season_overlap.in","stom_pred_length_at_sizecl.in","stom_struc_at_length.in",
                     "stomcon_at_length.in","stomlen_at_length.in","stomweight_at_length.in","stomtype_at_length.in",
                     "stomnumber_at_length.in","pred_prey_size_range_param.in","other_pred_N.in",
                     "incl_stom.in","temperature.in","n_proportion_m2.in","consum_ab.in","cons_multiplier_options.in","other_pred_n_noise.dat")
  
  if (doMulti) for (from.file in SMS.files.multi) {
    to.file<-file.path(scenario.dir,from.file)
    out<-file.copy(from.file, to.file, overwrite = TRUE)
    if (verbose) print(paste(from.file,out))
  }
  
  SMS.files.area<-c("stock_distribution.in","predator_area_presence.in")
  if (doArea & control@no.areas > 1)  for (from.file in SMS.files.area) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }  
  
}