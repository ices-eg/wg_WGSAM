copy.SMS.scenario.files<-function(directory) {

  dir.create(file.path(data.path,directory),showWarnings = FALSE)

  SMS.files.single<-c("natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "Exploitation_pattern.in","recruit_residuals.in")

  for (from.file in SMS.files.single) {
    to.file<-file.path(data.path,directory,from.file)
    file.copy(file.path(data.path,from.file), to.file, overwrite = TRUE)
  }
 }
