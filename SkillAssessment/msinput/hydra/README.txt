Simulated data from Norweigan-Barents Atlantis Model by Hansen et al. 

 Atlantis outputs stored in /Users/sgaichas/Documents/0_Data/ICES_WGSAM/wg_WGSAM/SkillAssessment/atlantisoutput/NOBA_sacc_38 

 Biomass output for surveys BTS_fall_allbox_effic1 

 Biomass output for surveys BTS_spring_allbox_effic1 

 Fishery catch output for fisheries census 

 Biomass and catch output units are annual tons.

 Survey specifications:
 ----------------------
 # Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Also now happens in "spring" and "fall"
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name="BTS_fall_allbox_effic1"

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May, 
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- 3 # fall survey

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

survtime <- survey_sample_full

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                     efficiency=rep(1.0,length(survspp)))

# survey selectivity (agecl based)
# this is by age class, need to change to use with ANNAGEBIO output
#survselex <- data.frame(species=rep(survspp, each=n_age_classes),
#                     agecl=rep(c(1:n_age_classes),length(survspp)),
#                     selex=rep(1.0,length(survspp)*n_age_classes))

# for annage output
survselex <- data.frame(species=rep(names(annages), n_annages), #  
                        agecl=unlist(sapply(n_annages,seq)),
                        selex=rep(1.0,sum(n_annages)))

survselex.agecl <- survselex 


# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(100000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10000000
unidprey <- 0
 
 ----------------------
 # Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Also now happens in "spring" and "fall"
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name="BTS_spring_allbox_effic1"

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May, 
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- 1 # spring survey

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

survtime <- survey_sample_full

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                     efficiency=rep(1.0,length(survspp)))

# survey selectivity (agecl based)
# this is by age class, need to change to use with ANNAGEBIO output
#survselex <- data.frame(species=rep(names(age_classes), each=n_age_classes),
#                     agecl=rep(c(1:n_age_classes),length(survspp)),
#                     selex=rep(1.0,length(survspp)*n_age_classes))

# for annage output uses names(annages) NOT alphabetical survspp
survselex <- data.frame(species=rep(names(annages), n_annages), #  
                        agecl=unlist(sapply(n_annages,seq)),
                        selex=rep(1.0,sum(n_annages)))

survselex.agecl <- survselex


# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(100000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10000000
unidprey <- 0


 
 

 Fishery specifications:
 ----------------------
 # Default fishery configuration here is a census
fishery.name="census"

# Inherits species from input omlist_ss
fishspp <- omlist_ss$species_ss 

#Number of years of data to pull
nyears <- 50

#Atlantis initialization period in years
burnin <- 30

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc


# same time dimensioning parameters as in surveycensus.R
#Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample)  #total_sample defined in sardinesurvey.R
fish_burnin <- burnin*fstepperyr+1
fish_nyears <- nyears*fstepperyr
fish_times <- fish_sample_full[fish_burnin:(fish_burnin+fish_nyears-1)]
fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times/fstepperyr)) #from Christine's new sardine_config.R

fishtime <- fish_times


# fishery sampling area
# should return all model areas, this assumes you see everything that it caught
fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1))

# effective sample size needed for sample_fish
# this effective N is divided by the number of annual timesteps below, so 200 per time
# use as input to the length samples, ages can be a subset
fisheffN <- data.frame(species=survspp, effN=rep(1000, length(survspp)))

#this adjusts for subannual fishery output so original effN is for whole year
fisheffN$effN <- fisheffN$effN/fstepperyr 

# fishery catch cv can be used in sample_survey_biomass
# perfect observation
fish_cv <- data.frame(species=survspp, cv=rep(0.01,length(survspp)))

 
