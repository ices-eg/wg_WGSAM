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
