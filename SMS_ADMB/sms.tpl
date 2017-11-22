// Units:
//  fish numbers (e.g. N, C and Recruits) in 1000 fish (more correctly, 
//      all numbers get the same unit as catch numbers)
//  Mean weights (e.g. West, Weca) in Kg
//  Biomass, SSB in ton  (more correctly the product of fish numbers and mean weights)
//  Lengths (predator and prey length) in mm   
//  Consumption per fish in Kg 
//  Biomass of other food in ton (more correctly, the same unit as the 
//      product of fish numbers and consumption)

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 
GLOBALS_SECTION
 #include <admodel.h>
 #include <time.h>

 // test output
 #define TRACE(obj) cout<<"line "<<__LINE__<<", "<<#obj" =\n "<<obj<<endl<<endl;
 #define HEJ(no) cout<<"HEJ "<<no<<endl;

 #define checkSum(x,fileN) if ( x!=-999.0) {cout<<endl<<"Something is wrong with the check sum value: "<<x<<" expected -999.0"<<endl<<"input file: "<<fileN<< endl<<"program stopped"<<endl; exit(9); }
 
 int do_prediction_mean=0;
  
 long int seed=10;    // for random number generation
 int parNo=0;
 
 adstring_array species_names; // vector to species names
 adstring_array fleet_names;   // vector to fleet names
 adstring_array area_names;   // vector to area names
 
 // date and time
 char dateStr [40];
 // char f_timeStr [9];
 time_t start_time,end_time;

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
                             
DATA_SECTION

 !! cout<<"SMS version October 2017 using ADMB version 11.4"<<endl;

 

  // Print today's date and time
  !! time_t mytime = time(NULL);
  !! strftime (dateStr, 40, "%B %d, %Y %H:%M:%S", localtime(&mytime));
  !! cout<<dateStr<<endl;


 !! time(&start_time);

 !! int par_file=0;
 int mceval
 !! mceval=0;
 int mcmc
 !! mcmc=0;

 int y;      // Year 
 int q;      // quarter
 int s;      // species
 int a;      // age
 int d;      // division (SMS area)


 int yqd;    // counter. Year quarter area
 int yq;     // counter. year quarter
 int q_d;    // lq times no_areas 
 int i;
 int j;
 int k;
 int l;
 int f; 
 int pred;    // predator
 int prey;    // prey
 int sag;     // species age group
 int syg;     // species year group
 int sq;      // species quarter
 int sp;
 int spl;
 int splp;
 int sy;      // species year
 int iyqp;
 int iyqpl;
 int iyqplp;
 int iprey_l;
 
 number xx;
 
 !! for (i=1;i<argc;i++) {
 !!   if (strstr(argv[i],"-ainp")   !=NULL)  par_file=i+1;
 !!   if (strstr(argv[i],"-mceval") !=NULL)  mceval=1; 
 !!   if (strstr(argv[i],"-mcmc")   !=NULL) { mcmc=1; }
 !! } 
 

 init_int test_output       // Produce test output (0=no 1..9.. increasing output)
 !! if (test_output==1) cout<<"test.output: "<<test_output<<endl;

 init_int OP_output       // # Produce output for SMS-OP program. 0=no, 1=yes
 !! if (test_output==1) cout<<"op.output: "<<OP_output<<endl;
 !! if (mceval==1) OP_output=0;

 init_int multi             // multispecies mode (multi>=1) or single species(multi=0)
 !! if (test_output==1) cout <<"ms.mode: "<<multi<<endl;

 // append "1" (used as dummy multispecies parameters) to the parameter file
 !! if (par_file>0 && multi==1) {
 !!   // cout <<"par_file: "<<par_file<<"  "<<argv[par_file]<<endl;
 !!   ofstream res(argv[par_file],ios::app);
 !!   res<<"# inserted dummy parameter values "<<endl;
 !!   for (i=1;i<400;i++) res<<1<<endl;
 !!   res.close();
 !! }

 init_int no_areas;
 !! if (test_output==1) cout<<"no.areas: "<<no_areas<<endl;
 
 init_int fyData                // first year
 !! if (test_output==1) cout<<"first.year: "<<fyData<<endl;

 init_int fyModel                // first year
 !! if (test_output==1) cout<<"first.year.model: "<<fyModel<<endl;

 init_int lyData            // last year
 !! if (test_output==1) cout<<"last.year: "<<lyData<<endl;

 init_int lyModel           // last model year 
 !! if (test_output==1) cout<<"last.year.model: "<<lyModel<<endl;

 int ny_model
 !! ny_model=lyModel-fyModel+1;
 
 int fq                     // first season (quarter)
 !! fq=1;

 init_int lq                // last season (quarter)
 !! if (test_output==1) cout<<"last.quarter: "<<lq<<endl;

 !! q_d=lq*no_areas; 

 init_int lqly              // last season in the last year (last-quarter-last-year)
 !! if (test_output==1) cout<<"last.quarter.last.year: "<<lqly<<endl;

 int lqLocal                // last quarter variable used locally in many places 
 
 init_int nsp               // number of species
 !! if (test_output==1) cout<<"no.species: "<<nsp<<endl;

 init_int fa                // first age all species
 !! if (test_output==1) cout<<"first.age: "<<fa<<endl;

 init_int recq              // season for recruitment
 !! if (test_output==1) cout<<"rec.season: "<<recq<<endl;

 ivector faq(fq,lq)         // first age by quarter
 !! faq=fa;
 !! for (q=fq;q<recq;q++) faq(q)=fa+1;
 
 init_int max_a             // overall maximum age, all species
 !! if (test_output==1) cout<<"max.age: "<<max_a<<endl;
 
 init_matrix tmp(1,nsp,1,11)// various species information on "input" format
                            // 1. last age by species
                            // 2. first age where catch data are used (else F=0 assumed) 
                            // 3. last age with age dependent fishing selection 
                            // 4. Esimate F year effect from effort data. 0=no, 1=yes
                            // 5. last age included in the catch at age likelihood
                            // 6. plus group, 0=no plus group, 1=plus group
                            // 7. predator species, 0=no, 1=yes VPA, 2=yes "other predator"
                            // 8. prey species, 0=no, 1=yes
                            // 9. Stock Recruit relation, 1=Ricker, 2=Beverton & Holt, 3=log-normal, 4=hockey stick model
                            // 10-11. additional information for SR
 
 !! if (test_output==1) cout<<"species.info:"<<endl<<tmp<<endl;

 // species information, re-organised
 ivector la(1,nsp)          // last age by species 
 ivector nplus(1,nsp)       // plus group, 0=no plus group, 1=plus group
 ivector is_pred(1,nsp)     // is the species a predator? 0=no, 1=yes, 2=yes "other predator"
 ivector is_prey(1,nsp)     // is the species a prey? 0=no, 1=yes

 // Stock-recruitment model (1=Ricker, 2=Beverton&Holt, 3=Geometric mean,  
 //                          4=hockey stick model 
 //                         >100 Hockey stick model with known breakpoint
 
 
 ivector do_effort(1,nsp)    // Calculate F on the basis of estimated year factor (normal SMS) do_effort=0 or
                             // Calculate F on the basis of observed total effort, do effort=1
 int any_do_effort           // is the any species that use do_effort=1? (0=no, 1=yes)
 int all_do_effort           // is it all species that use do_effort=1? (0=no, 1=yes)
   
 !! any_do_effort=0;
 !! all_do_effort=0;
 
 int nOthPred              // no of "other predators"
 !! nOthPred=0;
 int npr                   // number of predators=VPA preadtors + Other predators
 !! npr=0;

 int first_VPA              // species number for the first species where the stock numbers are estimated 
 int min_first_VPA         // first VPA species nummer; used to select a subset of speices (works only in single species mode)
 int max_last_VPA         // last VPA species nummer; used to select a subset of speices (works only in single species mode)

 int max_nsp
 int first_VPA_prey         // species number for the first species where the stock numbers are estimated and which is a prey
 int nprey; 
 // Reorganise a bit
 !! for (s=1;s<=nsp;s++) {
 !!  la(s)=int(tmp(s,1));
 !!  if (s<=npr) la_pred(s)=la(s);
 !!  do_effort(s)=int(tmp(s,4));
 !!  if(do_effort(s)==1) any_do_effort=1;
 !!  nplus(s)=int(tmp(s,6));
 !!  is_pred(s)=int(tmp(s,7));
 !!  if (is_pred(s)==2) nOthPred=nOthPred+1;
 !!  if (is_pred(s)==1 || is_pred(s)==2) npr=npr+1;
 !!  is_prey(s)=int(tmp(s,8));
 !! }
 
 !! first_VPA=nOthPred+1;
 ivector F_y_species_index(first_VPA,nsp)

 matrix Rec_add_inf(first_VPA,nsp,1,2)

 !! for (s=first_VPA;s<=nsp;s++) {
 !!  Rec_add_inf(s,1)=tmp(s,10);
 !!  Rec_add_inf(s,2)=tmp(s,11);
 !! }
  
 // !! cout<<"first_VPA:"<<first_VPA<<endl; 
 !! if (sum(do_effort)==(nsp-first_VPA+1)) all_do_effort=1;


 !! cout<<"no. of species: "<<nsp<<"  no. of predators: "<<npr<<"  no. of other predators:"<<nOthPred<<endl;
 !! if (npr==0) npr=1;
 
 !!  if (multi<1) is_prey=0;
 !!  nprey=sum(is_prey);

 ivector SSB_Rec_model(first_VPA,nsp) 
 ivector use_beta_SSB_Rec(first_VPA,nsp)
 vector SSB_Rec_hockey_breakpoint(first_VPA,nsp)
 int read_temperature
 !! read_temperature=0;

 // STN int n_use_opt61_Rec;    // special for option 61 (Stefan's temperature dependent recruitment model   STN
 // STN ivector use_opt61_Rec(first_VPA,nsp)
 // STN !! n_use_opt61_Rec=0;
 // STN int phase_alfa_61
 // STN int phase_beta_61
 
 // STN int n_use_opt71_Rec;    // special for option 71 (Stefan's Oxygen dependent recruitment model   STN
 // STN ivector use_opt71_Rec(first_VPA,nsp)
 // STN !! n_use_opt71_Rec=0;
 // STN int phase_alfa_71
 // STN int phase_beta_71

  
 !! for (s=first_VPA;s<=nsp;s++) {
 !!    SSB_Rec_model(s)=int(tmp(s,9));
 // STN !!    use_opt61_Rec(s)=0; use_opt71_Rec(s)=0;
 !!    if (SSB_Rec_model(s)==51 || SSB_Rec_model(s)==52) read_temperature=1;
 // STN !!    if (SSB_Rec_model(s)==61) { use_opt61_Rec(s)=1; n_use_opt61_Rec=n_use_opt61_Rec+1; }
 // STN !!    if (SSB_Rec_model(s)==71) { use_opt71_Rec(s)=1; n_use_opt71_Rec=n_use_opt71_Rec+1; }
 !!    if (SSB_Rec_model(s)>100) {
 !!      SSB_Rec_hockey_breakpoint(s)=tmp(s,9);
 !!      SSB_Rec_model(s)=100;
 !!    }
 !!    if (SSB_Rec_model(s)==3 || SSB_Rec_model(s)==100) use_beta_SSB_Rec(s)=0; else use_beta_SSB_Rec(s)=1;
 !! }



 
 ivector la_pred(1,npr)          // last age by predator species
 !! for (s=1;s<=npr;s++) la_pred(s)=la(s);
 ivector la_VPA(first_VPA,nsp)          // last ages for VPA species
 ivector las(first_VPA,nsp)             // last age with age dependent fishing selection 
 ivector cfa(first_VPA,nsp)         // first age where catch data are used (else F=0 assumed) 
 ivector la_like(first_VPA,nsp)     // last age included in catch likelihood

 !! first_VPA_prey=0;
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   la_VPA(s)=la(s);
 !!   la_like(s)=int(tmp(s,5));
 !!   las(s)=int(tmp(s,3));
 !!   cfa(s)=int(tmp(s,2));
 !!   if (first_VPA_prey==0 && is_prey(s)==1) first_VPA_prey=s;
 !! }
 !!  cout<<"first VPA species no.:"<<first_VPA<<"  first VPA species as prey:"<<first_VPA_prey<<endl;
 
 init_int use_known_rec_option     //  use input recruitment estimate (option use.known.rec)
                                   // 0=estimate all recruitments, 1=yes use input recruitment from file known_recruitment.in
  !! if (test_output==1) cout<<"known.recruitment: "<<use_known_rec_option<<endl;
 
  
 init_vector SSB_R_beta_cor(first_VPA,nsp) // Internal factor on the beta parameter in the stock
                                   // recruitment relation. used to get parameter estimates close to one 
 !! if (test_output==1) cout<<"SSB.R.beta.cor:"<<endl<<SSB_R_beta_cor<<endl;

 init_ivector SSB_year_first(first_VPA,nsp) // First year to include in SSB-R relation
 init_ivector SSB_year_last(first_VPA,nsp) // Last year to include in SSB-R relation
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   if (SSB_year_first(s)<=fyModel)  SSB_year_first(s)=fyModel;
 !!   if (SSB_year_last(s)==-1)  SSB_year_last(s)=lyModel;
 !!   if (SSB_year_last(s)>lyModel)  SSB_year_last(s)=lyModel;
 !! }
 !! if (test_output==1) 
 !!  cout<<"First year recruitment:"<<SSB_year_first<<endl<<"Last year recruitment: "<<SSB_year_last<<endl;

 // weighting factor for objective function contribution                                     
 init_matrix obj_weight(1,nsp,1,5)       // 1=catch observations, 
                                         // 2=CPUE observations,
                                         // 3=SSB/R relations
                                         // 4=stomach observations
                                         // 5= stomach number dist
 !! if (test_output==1) cout<<"objective.function.weight:"<<endl<<obj_weight<<endl;
 
 // 0= no technical creeping, 1=year specific creep, 2=age specific creep
 // ivector use_creep(first_VPA,nsp);
 // ivector creep_option(first_VPA,nsp)
 // !!  for (s=first_VPA;s<=nsp;s++) {
 // !!   use_creep(s)= -obj_weight(s,4);
 // !!   obj_weight(s,4)=0;
 // !! }
  
 // phases for single species parameter estimation
 int phase_single_species_fixed;
 !! phase_single_species_fixed=0;
 
 init_int phase_log_rec;
 !! if (test_output==1) cout<<"phase.log.rec: "<<phase_log_rec<<endl;
 
 init_int phase_log_rec_older;
 !! if (test_output==1) cout<<"phase.log.rec_older: "<<phase_log_rec_older<<endl;
 
 init_int phase_F_y_ini;
 !! if (test_output==1) cout<<"phase.F.y: "<<phase_F_y_ini<<endl;
 
 init_int phase_F_y_spline
 !! if (test_output==1) cout<<"phase.F.y.spline: "<<phase_F_y_spline<<endl;

 init_int phase_F_q_ini;
 !! if (test_output==1) cout<<"phase.F.q: "<<phase_F_q_ini<<endl;
 !! if  ( phase_F_q_ini>0 && lq==1 ) {
 !!   phase_F_q_ini=-1; 
 !!   if (test_output==1) cout<<"phase.F.q changed to: "<<phase_F_q_ini<<" as annual data are used"<<endl;
 !! }
 
 
 init_int phase_log_F_a_ini;
 !! if (test_output==1) cout<<"phase.F.a: "<<phase_log_F_a_ini<<endl;;

 init_int phase_qq_ini;
 !! if (test_output==1) cout<<"phase.catchability: "<<phase_qq_ini<<endl;
 int phase_qq_power_ini;
 !! phase_qq_power_ini=phase_qq_ini;
 
 //int phase_year_effect_ini;
 //!! phase_year_effect_ini=phase_qq_ini;


 init_int phase_SSB_R_alfa;
 !! if (test_output==1) cout<<"phase.SSB.R.alfa: "<<phase_SSB_R_alfa<<endl;

 init_int phase_SSB_R_beta;
 !! if (test_output==1) cout<<"phase.SSB.R.beta: "<<phase_SSB_R_beta<<endl;
 
 init_number min_catch_CV    // minimum accepted CV of catch at age from commercial catches
 !! if (test_output==1) cout <<"min.catch.CV: "<<min_catch_CV<<endl;
 
 init_number min_SR_CV    // minimum accepted CV of catch at age form commercial catches
 !! if (test_output==1) cout <<"min.SR.CV: "<<min_SR_CV<<endl;
 number  min_SR_s2
 !! min_SR_s2=min_SR_CV*min_SR_CV;
 
 init_ivector calc_discard_sp(first_VPA,nsp)        // yield is calculated from proportion_landed.in
 !! if (test_output==1) cout <<"calc.discard: "<<calc_discard_sp<<endl;
 int calc_discard;
 !! calc_discard=0;
 !! for (s=first_VPA;s<=nsp;s++) if (calc_discard_sp(s)==1) calc_discard=1;
 
 int seasonal_annual_catches_any        // use seasonal or annual catches in objective function
 init_ivector seasonal_annual_catches(first_VPA,nsp)
 !! if (sum(seasonal_annual_catches)==0) seasonal_annual_catches_any=0; else seasonal_annual_catches_any=1;
 !! if (test_output==1) cout <<"combined.catches: "<<seasonal_annual_catches<<endl;
 //!! if (test_output==1) cout <<"seasonal_annual_catches_any: "<<seasonal_annual_catches_any<<endl;
 
 init_ivector seasonal_combined_catch_s2(first_VPA,nsp)        //  use seasonal or common combined variances for catch observation 
 !! if (test_output==1) cout <<"seasonal.combined.catch.s2: "<<seasonal_combined_catch_s2<<endl;
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   if (seasonal_combined_catch_s2(s)==0) seasonal_combined_catch_s2(s)=lq-fq+1; 
 !!   else seasonal_combined_catch_s2(s)=1;
 !! }
 
 init_ivector n_catch_s2_group(first_VPA,nsp)                    // no of separate catch variance groups by species
 !! if (test_output==1) cout<<"n.catch.s2.group: "<<n_catch_s2_group<<endl;

 init_imatrix catch_s2_group(first_VPA,nsp,1,n_catch_s2_group)   // first age in catch variance group by species 
 !! if (test_output==1) cout<<"catch.s2.group: "<<endl<<catch_s2_group<<endl;
 
 init_ivector n_catch_season_age_group(first_VPA,nsp)                    // no of groups (of ages) with individual seasonal pattern by species
 !! if (test_output==1) cout <<"n.catch.season.age.group: "<<endl<<n_catch_season_age_group<<endl;

 init_imatrix catch_season_age(first_VPA,nsp,1,n_catch_season_age_group) // first age in each seasonal component group by species 
 !! if (test_output==1) cout<<"catch.season.age: "<<endl<<catch_season_age<<endl;

 init_imatrix avg_F_ages(first_VPA,nsp,1,2)      // First and last ages in calculation of average F by species
 !! if (test_output==1) cout<<"avg.F.ages: "<<endl<<avg_F_ages<<endl;
  
 init_vector min_catch(first_VPA,nsp)            // minimum "observed" catch, used to substitute zero catches
 !! if (test_output==1) cout<<"min.catch: "<<endl<<min_catch<<endl;
 
 init_ivector n_catch_sep_year_group(first_VPA,nsp) //number of year groups with different age separable parameters
 !! if (test_output==1) cout<<"n.catch.sep.year.group: "<<endl<<n_catch_sep_year_group<<endl;
  
 init_imatrix catch_sep_year(first_VPA,nsp,1,n_catch_sep_year_group)  //first year in a group with different age separable parameters
 !! if (test_output==1) cout <<"catch.sep.year: "<<endl<<catch_sep_year<<endl;
 !! for (s=first_VPA;s<=nsp;s++) if (catch_sep_year(s,1) != fyModel) {
 !!   cout<<"First year in separabel year group ("<<catch_sep_year(s,1)<<") is changed to first model year for species no:"<<s<<endl;
 !!   catch_sep_year(s,1) = fyModel;
 !! }
 !! for (s=first_VPA;s<=nsp;s++) for (i=1;i<=n_catch_sep_year_group(s);i++) { 
 !!  if (catch_sep_year(s,i)<fyModel) {
 !!    cout<<"ERROR in years for separable model"<<endl;
 !!    cout<<"  Years selceted is smaller than first year used in the model"<<endl;
 !!    cout<<"  species:"<<s<<" separable years:"<<catch_sep_year(s)<<endl;
 !!    cout<<"  program stopped"<<endl;
 !!    exit(9);
 !!  }}
  
 ivector no_F_y_groups(first_VPA,nsp)              // number of year groups
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   no_F_y_groups(s)=0;
 !!   if (n_catch_sep_year_group(s)>1) {
 !!     no_F_y_groups(s)=n_catch_sep_year_group(s)-1;
 !!   }
 !! }

 !! for (s=first_VPA;s<=nsp;s++) catch_sep_year(s,1)=fyModel; //First year are always the overall assessment first year

  int no_sp_sag_syg       //number of species, separable season and separabel age combinations
 !! no_sp_sag_syg=0;       
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   for (sag=1;sag<=n_catch_season_age_group(s);sag++){
 !!     for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
 !!       no_sp_sag_syg++;
 !!     }
 !!   }
 !! }


 // splines for year effect in Fishing mortality
 init_ivector Nknots(first_VPA,nsp)
 !! if (test_output==1) cout<<"Nknots: "<<endl<<Nknots<<endl;

 init_matrix knotsX(first_VPA,nsp,1,Nknots)
 !! if (test_output==1) cout<<"Years for nodes: "<<endl<<knotsX<<endl;


 // seasons with with zero catch (F=0) (option zero.catch.year.season)
 // 0=no, all seasons have catchs, 1=yes there is seasons with no catch. Read from file zero_catch_year_season.in
  init_int zero_catch_year_season
  !! if (test_output==1) cout<<"zero.catch.year.season: "<<zero_catch_year_season<<endl;
  !! if  (zero_catch_year_season!=1 && zero_catch_year_season!=0) {
  !!    cout<<"ERROR: option zero.catch.year.season must be 0 or 1"<<endl;
  !!    exit(9);
  !! }

  // seasons with with zero catch (F=0) (option no.zero.catch.season.ages)
 // 0=no, all seasons have catchs, 1=yes there is seasons with no catch. Read from file zero_catch_season_ages.in
  init_int zero_catch_season_age
  !! if (test_output==1) cout<<"zero.catch.season.ages: "<<zero_catch_season_age<<endl;
  !! if  (zero_catch_season_age!=1 && zero_catch_season_age!=0) {
  !!    cout<<"ERROR: option zero.catch.season.ages must be 0 or 1"<<endl;
  !!    exit(9);
  !! }


 // factor for fixing the last season in the seasonal effect of the F-model (default=1)
 //init_vector fix_F_factor(first_VPA,nsp)
 init_vector fix_F_factor(first_VPA,nsp)
 !! if (test_output==1) cout<<"fix.F.factor:"<<endl<< fix_F_factor<<endl;


 // ## Uncertanties for catch, CPUE and SSB-R observations
 //#  values: 0=Calculate sigma (to reduce the number of  parameters, but gives too narrow confidence limits), 
 //#         1=estimate sigma as a parameter (the right way of doing it)

 int est_catch_sigma
 init_ivector est_calc_sigma(1,3)
 !! if (test_output==1) cout<<"est.calc.sigma:"<<endl<< est_calc_sigma<<endl;
 !! for (i=1;i<=3;i++) if  (est_calc_sigma(i)!=1 &&  est_calc_sigma(i)!=0 &&  est_calc_sigma(i)!=2) {
 !!   cout<<"ERROR: est.calc.sigma must be 0, 1 or 2."<<endl; exit(9);
 !!}
 !! if (est_calc_sigma(1)==0) est_catch_sigma=1; else est_catch_sigma=0;

  init_int read_HCR_file;
 !! if (test_output==1) cout<<"read.HCR.File: " <<read_HCR_file<<endl;
 !! if (mceval==1 && (read_HCR_file==0)) {
 !!    cout<<"ERROR: you have to set option read.HCR.File=0 and want to make prediction!?"<<endl;
 !!    exit(9);
 !! } 

 !! if (read_HCR_file<0 || read_HCR_file>3) {
 !!    cout<<"ERROR: you have to set option read.HCR.File to 0 "<<read_HCR_file<<" whic is outside the range!?"<<endl;
 !!    exit(9);
 !! } 

 !! if (mceval==1 && no_areas>1) {
 !!    cout<<"ERROR: SMS cannot make forecast with more than one area!"<<endl;
 !!    exit(9);
 !! }

 int do_short_term_forecast;
 
 !! if (read_HCR_file==2) do_short_term_forecast=1; else if (read_HCR_file==3) do_short_term_forecast=2; else do_short_term_forecast=0;
 !! if (test_output==1 && read_HCR_file==2 || read_HCR_file==3) cout<<"Short term forecast has been selected, option: "<<do_short_term_forecast<<endl;

  
 !! if (multi==0) ad_comm::change_datafile_name("just_one.in");

 init_int incl_stom_all; 
 !! if (test_output==1 && multi>0) cout<<"Include selected stomachs option (option incl.stom.all):"<<endl<<incl_stom_all<<endl;
   
 init_int use_Nbar          // Stock numbers used in fitting suitability paramters.
                            // (0: Use N; 1 Use mean N) 
 !! if (test_output==1 && multi>0) cout <<"use.Nbar: "<< use_Nbar<<endl;

 init_int max_M2_iteration  // Max M2 iterations
 !! if (test_output==1 && multi>0) cout <<"max.M2.iteration: "<<max_M2_iteration<<endl;
 

 init_number max_M2_sum2    // max M2 sum of square for two iterations
 !! if (test_output==1 && multi>0) cout<<"max.M2.sum2: "<<max_M2_sum2<<endl;

 init_int stom_likelihood  // # likelihood model for stomach content observations (option stom.likelihood)
 //#  1 =likelihood from prey weight proportions only (see option below)
 // #  2 =likelihood from prey weight proportions and from prey numbers to estimate size selection
 //#  3 =Gamma distribution for prey absolute weight and size selection from prey numbers
 !! if (test_output==1 && multi>0) cout <<"stom.likelihood: "<<stom_likelihood<<endl;
 
 init_int stomach_variance  // stomach variance model, 1=log normal distributed; 2=normal distributed; 3= Dirichlet, 
 !! if (test_output==1 && multi>0) cout <<"stomach.variance: "<<stomach_variance<<endl;
 
 
  init_int simple_ALK   // use age-size-key for calculation of M2 values
    //  0=Use only one sizegroup per age (file lsea.in or west.in)
    //  1=Use size distribution per age (file ALK_all.in)
   !! if (test_output==1 && multi>0) cout <<"simple.ALK: "<<simple_ALK<<endl;

  init_int consum_op   // Use food-rations from input values or from size and regression parameters (option consum)
     //  0=Use input values by age (consum.in)
     // 1=use weight at size and regression parameters (consum_ab.in)
   !! if (test_output==1 && multi>0) cout <<"consum: "<<consum_op<<endl;


 init_int size_select_model // Use length or weight proportion in size selection model; 
 !! if (test_output==1 && multi>0) cout <<"size.select.model: "<<size_select_model<<endl;
 !! if (multi>0 && !(size_select_model>=1 && size_select_model<=6)) {
 !!  cout<<"ERROR in size.selct.model [1;6]:"<<size_select_model<<" program stopped"<<endl;
 !!  exit(9);
 !! }
  
 init_vector L50_mesh(first_VPA,nsp)  //Adjust Length at Age distribution by a mesh selection function
 // # L50 (mm) is optional given as input. Selection Range is estimated by the model
 // # L50= -1 do not adjust
 // # L50=0, estimate L50
 // # L50>0, inpul L50 (mm) 
 !! if (test_output==1 && multi>0) cout <<"L50_mesh: "<<L50_mesh<<endl;
 int mesh_size_active;  // is the estimation of mesh size parameters 0=FALSE, 1=TRUE
 
 init_ivector size_selection(1,npr) // 0: no size selection, 
                                    // 1: log-normal distribution, 
                                    // 2: non symmetric log-normal 
                                    // 3: Gamma distributed size selection, 
                                    // 4:no size selection, but within observed range defined from input quantile regression parameters (intercept and slope) ,
                                    // 5:beta distribution 
                                    // 6:beta distribution, unimodal
                                    
 !! if (test_output==1 && multi>0) cout <<"size.selection: "<<endl<<size_selection<<endl;
 !! int read_size_a_b=0;
 !! for (s=1;s<=npr;s++) if (size_selection(s)==4) read_size_a_b=1;

 ivector do_number_like(1,npr);   //  do likelihood for number by species  PL
 !! do_number_like=0;

 int do_number_like_any;  // do likelihood for number for any species     PL
 !! do_number_like_any=0;
 !! if (stom_likelihood==2 || stom_likelihood==3) {
 !!    for (s=1;s<=npr;s++) {
 !!     if  (size_selection(s)==1) {
 !!       do_number_like_any=1;
 !!       do_number_like(s)=1;
 !!     }
 !!   }
 !!   if (do_number_like_any==0) {
 !!    cout<<"ERROR:"<<endl;
 !!    cout<<"  You have chosen number model for food likelihood (option stom.likelihood),"<<endl;
 !!    cout<<"   but there is no species with prey size selection!"<<endl;
 !!    cout<<"  program stopped"<<endl;
 !!    exit(9);
 !!  }}

 

 init_ivector sumStomLike(1,npr); // sum stomach contents over prey size for use in likelihood (option sum.stom.like)
                                  //  0=no, use observation as they are, 1=yes, sum observed and predicted stomach contents before used in likelihood
 !! if (test_output==1 && multi>0) cout <<"sum.stom.like: "<<endl<<setprecision(0)<<sumStomLike<<endl;
 //!! if (stom_likelihood==2 || stomach_likelihood==3) for (s=1;s<=npr;s++) {
 //!!    if  (sumStomLike(s)==1 && size_selection(s)==1) {sumStomLike(s)==0; cout<<"Option sum.stom.like changed to 1 for species:"<<s<<" as likelihood including numbers has been chosen (option sttomach.variance)\n"<<endl; } 
 //!! }
 
 init_ivector StomObsVar(1,npr); //Use estimated scaling factor to link number of observation to variance for stomach observation likelihood (option stom.obs.var)
                                  //  0=no, do not estiamte factor (assumed=1);  1=yes, estimate the factor , 2= no factor and obs on sampling level.
 !! if (test_output==1 && multi>0) cout <<"stom.obs.var: "<<endl<<setprecision(0)<<StomObsVar<<endl;
                                
 init_vector stomMaxSumP(1,npr); //  Upper limit for scaling factor used to link number of observation to variance for stomach observation likelihood
 !! if (test_output==1 && multi>0) cout <<"stom.max.sumP: "<<endl<<setprecision(0)<<stomMaxSumP<<endl;
                                

 init_vector Stom_var_fac(1,npr)      //scaling factor to adjust relation between sampling intensity and variance of stomach observations
 
 !! for (s=1;s<=npr;s++) if (Stom_var_fac(s)==0) {  // Overwrite with default values
 !!   if (sumStomLike(s)==1) Stom_var_fac(s)=1.0; else Stom_var_fac(s)=0.1;
 !! }
 !! if (test_output==1 && multi>0) cout <<"var.scale.stom: "<<endl<<setprecision(3)<<Stom_var_fac<<endl;


 init_ivector size_other_food_suit(1,npr) //  other food suitability dependency on predator size,  0=no size dependency, 1=yes
 !! if (test_output==1 && multi>0) cout <<"size.other.food.suit: "<<endl<<size_other_food_suit<<endl;
 int no_size_other_food_suit
 !! no_size_other_food_suit=sum(size_other_food_suit);
 !! if (no_size_other_food_suit==0 || multi==0) no_size_other_food_suit=0;
 
 init_vector min_stom_cont(1,npr)  // Minimum observer relative stomach contents weight for inclusion in ML estimation
 !! if (test_output==1 && multi>0) cout <<"min.stom.cont:  "<<min_stom_cont<<endl;

  init_vector max_stom_sampl(1,npr)  //Maximum number of samples used for calculation of stomach observation variance (option max.stom.sampl)
 !! if (test_output==1 && multi>0) cout <<"max.stom.sampl:  "<<max_stom_sampl<<endl;

 init_vector prey_pred_size_fac(1,npr)    // Max prey size/pred size ratio for inclusion in M2 calc
 !! if (test_output==1 && multi>0) cout<<"max.prey.pred.size.fac: "<<endl<<prey_pred_size_fac<<endl;

 init_vector  stom_type_input(1,npr)
 !! if (test_output==1 && multi>0) cout<<"stom.type.input: "<<endl<<stom_type_input<<endl;



 init_int use_overlap    // Use overlap index from file overlap.in
 !! if (test_output==1 && multi>0) cout<<"use.overlap: "<<use_overlap<<endl;

 !! if (multi==1) phase_single_species_fixed=1;
 !! if (phase_single_species_fixed==1) { 
 !!   phase_log_rec=phase_log_rec_older=phase_F_y_ini=phase_F_q_ini=phase_log_F_a_ini=-1;
 !!   phase_qq_ini=phase_qq_power_ini=phase_SSB_R_alfa=phase_SSB_R_beta=-1;
 !!   phase_F_y_spline=-1;
 !! }

 int stom_phase;
 // !! if (test_output==1 && multi>0) cout<<"stom.phase: "<<stom_phase<<endl;
 !! stom_phase=2;

 int def_pred_par;
 //!! if (test_output==1 && multi>0) cout<<"def.pred.par: "<<def_pred_par;
 !! if (multi==1) def_pred_par=1;  else def_pred_par=0;

 init_int phase_vulnera; 
 !! if (test_output==1 && multi>0) cout<<"phase.vulnera: "<<phase_vulnera<<endl; 

 init_int phase_stl_other_suit_slope;
 !! if (test_output==1 && multi>0) cout<<"phase.other.suit.slope: "<<phase_stl_other_suit_slope<<endl;

 init_int phase_pref_size_ratio;
 !! if (test_output==1 && multi>0) cout<<"phase.pref.size.ratio: "<<phase_pref_size_ratio<<endl;
 int use_size_selection;
 !! use_size_selection=0;
 !! for (s=1;s<=npr;s++) if (size_selection(s)!=4 || size_selection(s)!=0) use_size_selection=1;
 !! if (use_size_selection==0) phase_pref_size_ratio=-1;

 init_int phase_pref_size_ratio_correction;
 !! if (test_output==1 && multi>0) cout<<"phase.pref.size.ratio.correction: "<<phase_pref_size_ratio_correction<<endl;
 !! if (use_size_selection==0) phase_pref_size_ratio_correction=-1;

 init_int phase_prey_size_adjustment;
 !! if (test_output==1 && multi>0) cout<<"phase_prey_size_adjustment: "<<phase_prey_size_adjustment<<endl;
 !! if (use_size_selection==0) phase_prey_size_adjustment=-1;

 init_int phase_var_size_ratio;
 !! if (test_output==1 && multi>0) cout<<"phase.var.size.ratio: "<<phase_var_size_ratio<<endl;
 !! if (use_size_selection==0) phase_var_size_ratio=-1;
 
 init_int phase_season_overlap;
 !! if (test_output==1 && multi>0) cout<<"phase.season.overlap: "<<phase_season_overlap<<endl;

 init_int phase_Stom_var;
 !! if (test_output==1 && multi>0) cout<<"phase.Stom.var: "<<phase_Stom_var<<endl;
 
 init_int  phase_mesh_adjust ;
 !! if (test_output==1 && multi>0) cout<<"phase.mesh.adjust: "<<phase_mesh_adjust<<endl;

 // the sms.dat file has now been read.
 
 
 // Read data for short term forecast, if requasted     
 !! if (do_short_term_forecast>0) ad_comm::change_datafile_name("short-term-configuration.dat");
 !! else ad_comm::change_datafile_name("just_one.in");
 init_int no_F_multipliers
 init_matrix F_multipliers(first_VPA,nsp,1,no_F_multipliers)
 init_vector rec_TAC_year(first_VPA,nsp)



    // season with no fishery and no F
 !! if (zero_catch_year_season==1) ad_comm::change_datafile_name("zero_catch_year_season.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 init_3darray zero_catch_y_season(first_VPA,nsp,fyData,lyData,fq,lq)
 !! if (zero_catch_year_season==1 &&  test_output==2) cout<<"zero_catch_y_season:\n"<<zero_catch_y_season<<endl;
 init_number check1;
 !! checkSum(check1,"zero_catch_year_season.in"); 


  // start to process seasons and age combinations with zero catch
 !! if (zero_catch_season_age==1) ad_comm::change_datafile_name("zero_catch_season_ages.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 init_3darray incl_catch_season_age(first_VPA,nsp,fq,lq,fa,max_a);
 !! if (test_output==2 && zero_catch_season_age==1) cout<<"input incl_catch_season_age(s,q,a):"<<endl<<incl_catch_season_age<<endl;
 init_number check2;
 !! checkSum(check2,"zero_catch_season_ages.in"); 

 !! for (s=first_VPA;s<=nsp;s++)  for (q=fq;q<=lq;q++) {
 !!   for (a=fa;a<cfa(s);a++) incl_catch_season_age(s,q,a)=0;
 !!   for (a=la_like(s)+1;a<=max_a;a++) incl_catch_season_age(s,q,a)=0;
 !! }
 !! for (s=first_VPA;s<=nsp;s++)  for (q=fq;q<recq;q++) incl_catch_season_age(s,q,fa)=0;
 !! if (test_output==2 && zero_catch_season_age==1) cout<<"incl_catch_season_age(s,q,a):"<<endl<<incl_catch_season_age<<endl;

 imatrix fq_F(first_VPA,nsp,cfa,la_like);  // first season with catch
 imatrix lq_F(first_VPA,nsp,cfa,la_like);  // first season with catch

 !! for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=la_like(s);a++) for (q=lq;q>=fq;q--)
 !!   if (incl_catch_season_age(s,q,a)==1) fq_F(s,a)=q;
  //!! cout <<"fq_F:"<<endl<<fq_F<<endl;

 !! for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=la_like(s);a++) for (q=fq;q<=lq;q++)
 !!   if (incl_catch_season_age(s,q,a)==1) lq_F(s,a)=q;
 //!! cout <<"lq_F:"<<endl<<lq_F<<endl;


  ivector lcq(1,no_sp_sag_syg)  // last season   catch is included (minus one where possibel (lq>fq))
  ivector fcq(1,no_sp_sag_syg)  // first season where catch is included

  int max_syg                   // maximum number of year groups with different separable parameters, all species combined
  int max_sag                   // maximum number of separable season-age  groups, all species combined
  int no_sp_sag                 // number of species and separability age group combinations

 !! int sp_sag_syg=0;
 !! no_sp_sag=0;
 !! max_syg=1; max_sag=1;
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   if (n_catch_sep_year_group(s)>max_syg) max_syg=n_catch_sep_year_group(s);
 !!   if (n_catch_season_age_group(s)>max_sag) max_sag=n_catch_season_age_group(s);
 !!    for (sag=1;sag<=n_catch_season_age_group(s);sag++){
 !!     no_sp_sag++;
 !!     for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
 !!       sp_sag_syg++;
 !!
 //!!       if ((catch_season_age(s,sag)==fa) && (n_catch_season_age_group(s)>1)) {
 //!!         if (catch_season_age(s,sag+1)-catch_season_age(s,sag)==1)fcq(sp_sag_syg)=recq;
 //!!         else fcq(sp_sag_syg)=fq;
 //!!       }
 //!!       else fcq(sp_sag_syg)=fq;
 //!!
 //!!       if ((catch_season_age(s,sag)==fa) && (n_catch_season_age_group(s)>1)) lcq(sp_sag_syg)=max(fcq(sp_sag_syg),lq-1);
 //!!       else lcq(sp_sag_syg)=max(lq-1,1);
 !!       a=catch_season_age(s,sag);
 !!       for (q=fq;q<=lq;q++) {if (incl_catch_season_age(s,q,a)==1)  lcq(sp_sag_syg)=max(q-1,1);}
 !!       for (    q=lq;q>=fq;q--) {if (incl_catch_season_age(s,q,a)==1) fcq(sp_sag_syg)=q;}
 !!     }
 !!   }
 !! }

 // read quaterly distribution of F
 !! if (seasonal_annual_catches_any==1) ad_comm::change_datafile_name("f_q_ini.in"); 
 !! else ad_comm::change_datafile_name("just_one.in");
 
 init_matrix F_q_ini_read(1,no_sp_sag_syg,fcq,lcq)
 !! if (test_output==2 && seasonal_annual_catches_any==1) cout<<"input f_q_ini.in:"<<endl<<F_q_ini_read<<endl;
 
 init_number check3;
 !!  if (seasonal_annual_catches_any==1) checkSum(check3,"f_q_ini.in"); 

 !! if (seasonal_annual_catches_any==1 && test_output>=1){
 !!    cout<<"seasonal_annual_catches_any: "<<seasonal_annual_catches_any<<endl;
  //!!    cout<<"F_q_ini_read:"<<endl<<setprecision(4)<<F_q_ini_read<<endl;
 !!    cout<<"F_q_ini_read:"<<endl<<setprecision(4)<<endl;
 !!    i=0;       
 !!    for (s=first_VPA;s<=nsp;s++) {
 !!      cout<<"Species: "<<s<<endl;
 !!      for (sag=1;sag<=n_catch_season_age_group(s);sag++){
 !!        for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
 !!          i++;
 !!         cout<<F_q_ini_read(i)<<endl;
 !!        }
 !!      }
 !!    }
 !!  }
 
 

 
 !! ad_comm::change_datafile_name("recruitment_years.in"); 
 init_imatrix recruitment_years(first_VPA,nsp,fyData,lyData)
 init_number check4;
 !! checkSum(check4,"recruitment_years.in"); 

 !! if (test_output>=1) cout<<"recruitment years:"<< endl<<recruitment_years<<endl;
 
 int save_SSB_Rec_residuals;
 !! save_SSB_Rec_residuals=0;
 ivector SSB_Rec_nobs(first_VPA,nsp)  // number of observations used in the S/R fit
 matrix SSB_Rec_residuals(first_VPA,nsp,1,ny_model)


 // STN
 // STN !! if (n_use_opt61_Rec>0) ad_comm::change_datafile_name("Sprat_rec_61.in");
 // STN !! else ad_comm::change_datafile_name("just_one.in");
 // STN init_vector R61Parms(1,3); 
 // STN init_vector T_61(fyData,lyData)
 // STN !! if (n_use_opt61_Rec>0) cout<<"R61Parms"<<endl<<R61Parms<<endl<<"T_61"<<endl<<T_61<<endl;
 
 // STN
 // STN !! if (n_use_opt71_Rec>0) ad_comm::change_datafile_name("COd_rec_71.in");
 // STN !! else ad_comm::change_datafile_name("just_one.in");
 // STN init_vector R71Parms(1,3); 
 // STN init_vector O_71(fyData,lyData)
 // STN  !! if (n_use_opt71_Rec>0) cout<<"R71Parms"<<endl<<R71Parms<<endl<<"O_71"<<endl<<O_71<<endl;
 
 
  
 // ##################### Start to read OP.dat if requested
 int pnsp            // prediction, number of species, normally nsp
 int pnpr            //  number of predators
 int pnopr            //  number of other predators
 
 !! if (OP_output==1) {pnsp=nsp; pnpr=npr; pnopr=nOthPred; ad_comm::change_datafile_name("op.dat");}
 !! else {pnsp=-1; pnpr=-1; pnopr=-1; ad_comm::change_datafile_name("just_one.in");}
 
 init_ivector OP_dummy(1,6)
 
 LOCAL_CALCS
  #define check_yr(X) {if (OP_output==1) {for (s=first_VPA;s<=pnsp;s++) if (X(2,s)<X(1,s)) {cout<<"wrong year range in "<<#X<<endl<<X<<endl; exit(9);}}}
  #define check_yrpred(X) {if (OP_output==1) {for (s=1;s<=pnpr;s++) if (X(2,s)<X(1,s)) {cout<<"wrong year range in "<<#X<<endl<<X<<endl; exit(9);}}}
  #define check_yrOtherpred(X) {if (OP_output==1) {for (s=1;s<=pnopr;s++) if (X(2,s)<X(1,s)) {cout<<"wrong year range in "<<#X<<endl<<X<<endl; exit(9);}}}
 END_CALCS
 
 init_imatrix OP_wsea(1,2,1,pnsp)    // first year and last year for calc of OP mean weight in the sea
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.wsea: "<<OP_wsea <<endl;
 !! check_yr(OP_wsea)
 
 init_imatrix OP_M(1,2,first_VPA,pnsp)    // first year and last year for calc of OP M and OP M1
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.M: "<<OP_M <<endl;
  !! check_yr(OP_M)

 init_imatrix OP_propmat(1,2,first_VPA,pnsp)    // first year and last year for calc of OP proportion mature
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.propmat: "<<OP_propmat <<endl;
 !! check_yr(OP_propmat)

 init_imatrix OP_F(1,2,first_VPA,pnsp)    // first year and last year for calc of OP F and OP exploitation pattern
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.F: "<<OP_F <<endl;
 !! check_yr(OP_F)

 init_imatrix OP_weca(1,2,first_VPA,pnsp)    // first year and last year for calc of OP mean weight in the catch
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.weca: "<<OP_weca <<endl;
 !! check_yr(OP_weca)

 init_imatrix OP_prop_landed(1,2,first_VPA,pnsp)    // first year and last year for calc of OP proportion of the catch landed
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.prop.landed: "<<OP_prop_landed <<endl;
 !! check_yr(OP_prop_landed)

 init_imatrix OP_stock_dist(1,2,first_VPA,pnsp)    // first year and last year for calc of OP stock distribution
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.stock.dist: "<<OP_stock_dist <<endl;
 !! check_yr(OP_stock_dist)

 init_imatrix OP_consum(1,2,1,pnpr)    // first year and last year for calc of OP food consumption
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.consum: "<<OP_consum <<endl;
 !! check_yrpred(OP_consum)

 init_matrix OP_growth_model(1,5,first_VPA,pnsp)    //  Growth model 0=no growth; food; 2=density dependent
 !! if (read_HCR_file==1) if (test_output==51) cout<<"OP.growth.model: "<<endl<<OP_growth_model <<endl;
 int OP_do_growth_all
 int OP_do_growth_type_1
 ivector OP_do_growth_sp(first_VPA,pnsp)
 !! OP_do_growth_all=0; OP_do_growth_type_1=0;
 !! for (s=first_VPA;s<=pnsp;s++) if (OP_growth_model(1,s)>0) {OP_do_growth_sp(s)=1; OP_do_growth_all+=1; if (OP_growth_model(1,s)==1) OP_do_growth_type_1=1; } else OP_do_growth_sp(s)=0;
 !! if (OP_do_growth_all>1) OP_do_growth_all=1;

 // dummy values to get to Other predators
 init_matrix OP_dummy_parms(1,6,first_VPA,pnsp)    
 init_int OP_dummy1
 init_int OP_dummy2
 
 init_imatrix OP_other_N(1,2,1,pnopr)    // first year and last year for calc of OP other stock N
 !! if (OP_output==1) if (test_output>=1) cout<<"OP.other_N: "<<OP_other_N <<endl;
 !! check_yrOtherpred(OP_other_N)
 
 
 !! if (OP_output==1) {ad_comm::change_datafile_name("reference_points.in");}
 !! else {ad_comm::change_datafile_name("just_one.in");}
  init_matrix reference_points(1,nsp,1,4)   //(Flim, Fpa, Blim, Bpa

  // ################ starts to process HCR.dat file
 !! if (read_HCR_file!=1) ad_comm::change_datafile_name("just_one.in");
 !! else if (read_HCR_file==1) ad_comm::change_datafile_name("hcr_options.dat");
  
 !! if (read_HCR_file==1 ) pnsp=nsp; else pnsp=1; 
   
  init_int lpy        // last year in prediction
 !! if (read_HCR_file!=1) lpy=lyModel+1; 
 !!  if (read_HCR_file==1) if (test_output==51) cout <<"Last.prediction.year: "<< lpy<<endl;
 
  init_int no_MCMC_iterations       // no of iterations within each MCMC prediction
 !! if (read_HCR_file==1) if (test_output==51) cout<<"no.MCMC.iterations: "<<no_MCMC_iterations <<endl;
 
 init_ivector first_year_wsea(first_VPA,pnsp)    // first year for calc of prediction mean weight in the sea
 !! if (read_HCR_file==1) if (test_output==51) cout<<"first.year.wsea: "<<first_year_wsea <<endl;
 
 init_ivector last_year_wsea(first_VPA,pnsp)    //  last year for calc of prediction mean weight in the sea
 !! if (read_HCR_file==1) if (test_output==51) cout<<"last.year.wsea:  "<<last_year_wsea <<endl;
 
 init_ivector first_year_weca(first_VPA,pnsp)    // first year for calc of prediction mean weight in the catch
 !! if (read_HCR_file==1) if (test_output==51) cout<<"first.year.weca:  "<<first_year_weca <<endl;
 
 init_ivector last_year_weca(first_VPA,pnsp)    //  last year for calc of prediction mean weight in the catch
 !! if (read_HCR_file==1) if (test_output==51) cout<<"last.year.weca: "<<last_year_weca <<endl;

  init_ivector first_year_prop_landed(first_VPA,pnsp)    // first year for calc of proportion of the catch landed
 !! if (read_HCR_file==1) if (test_output==51) cout<<"first.year.prop_landed:  "<<first_year_prop_landed <<endl;

 init_ivector last_year_prop_landed(first_VPA,pnsp)    //  last year for calc of proportion of the catch landed
 !! if (read_HCR_file==1) if (test_output==51) cout<<"last.year.prop.landed: "<<last_year_prop_landed <<endl;

 init_ivector first_year_propmat(first_VPA,pnsp)    // first year for calc of prediction proportion mature
 !! if (read_HCR_file==1) if (test_output==51) cout<<"first.year.propmat:  "<<first_year_propmat <<endl;
 
 init_ivector last_year_propmat(first_VPA,pnsp)    //  last year for calc of prediction proportion mature
 !! if (read_HCR_file==1) if (test_output==51) cout<<"last.year.propmat: "<<last_year_propmat <<endl;

 init_matrix growth_model(1,5,first_VPA,pnsp)    //  Growth model 0=no growth; food; 2=density dependent
 !! if (read_HCR_file==1) if (test_output==51) cout<<"growth.model: "<<endl<<growth_model <<endl;
 int do_growth_all
 int do_growth_type_1
 ivector do_growth_sp(first_VPA,pnsp)
 !! do_growth_all=0; do_growth_type_1=0;
 !! for (s=first_VPA;s<=pnsp;s++) if (growth_model(1,s)>0 && read_HCR_file==1 ) {do_growth_sp(s)=1; do_growth_all+=1; if (growth_model(1,s)==1) do_growth_type_1=1; } else do_growth_sp(s)=0;
 !! if (do_growth_all>1) do_growth_all=1;
 
 init_ivector last_year_season_F_adjustment(first_VPA,pnsp)   //
 !! if (read_HCR_file==1) if (test_output==51) cout <<"last.Seasons.adjustment.factor:"<< last_year_season_F_adjustment <<endl;


 init_vector stochastic_recruitment(first_VPA,pnsp)
  !! if (read_HCR_file==1) if (test_output==51) cout <<"stochastic.recruitment:"<<endl<<stochastic_recruitment<<endl;

 init_vector rec_noise_trunc_a(first_VPA,pnsp)
 init_vector rec_noise_trunc_b(first_VPA,pnsp)

    // Reorganise a bit
   matrix rec_noise_trunc(first_VPA,pnsp,1,2);
 !! for (s=first_VPA;s<=pnsp;s++) {
 !!  rec_noise_trunc(s,1)=rec_noise_trunc_a(s); rec_noise_trunc(s,2)=rec_noise_trunc_b(s);
 !!  if (stochastic_recruitment(s)==0) {rec_noise_trunc(s,1)=0; rec_noise_trunc(s,2)=0;}
 !! }
 !! if (read_HCR_file==1) if (test_output==51) cout <<"rec.noise.trunc:"<<endl<<rec_noise_trunc<<endl;  

 init_vector rec_noise_input(first_VPA,pnsp)
  !! if (read_HCR_file==1) if (test_output==51) cout <<"rec.noise.input:"<< rec_noise_input <<endl;

  init_matrix tmp2(1,47,first_VPA,pnsp)// various HCR information by species on "input" format
 !! if (read_HCR_file==1) if (test_output==51) cout<<"hcr.input:"<<endl<<tmp2<<endl;
 
 ivector HCR(first_VPA,pnsp)  
 ivector HCR_copy(first_VPA,pnsp)       
 vector T1(first_VPA,pnsp)         
 vector T2(first_VPA,pnsp)         
 matrix maxFT1(first_VPA,pnsp,1,2) 
 matrix maxFT1T2(first_VPA,pnsp,1,2) 
 matrix maxFT2(first_VPA,pnsp,1,2)
 ivector HCR_F_TAC(first_VPA,pnsp); 
 vector constantF(first_VPA,pnsp)
 vector constantTAC(first_VPA,pnsp)
 vector target_SSB(first_VPA,pnsp)
 vector real_time_F(first_VPA,pnsp);
 vector F_cap(first_VPA,pnsp);
 vector TAC_cap(first_VPA,pnsp)
 vector TAC_min(first_VPA,pnsp)
 matrix F_constraint(first_VPA,pnsp,1,2)
 matrix TAC_constraint(first_VPA,pnsp,1,2)
 matrix TAC_constraint_copy(first_VPA,pnsp,1,2)
 matrix SSB_constraint(first_VPA,pnsp,1,2)
 matrix obs_noise_trunc(first_VPA,pnsp,1,2);
 matrix real_time_uncertanty(first_VPA,pnsp,1,4);
 matrix real_time_uncertanty_copy(first_VPA,pnsp,1,4);
 matrix survey_uncertanty(first_VPA,pnsp,1,4);
 matrix assess_uncertanty(first_VPA,pnsp,1,4);
 matrix implemt_uncertanty(first_VPA,pnsp,1,4);
 vector TAC_first(first_VPA,pnsp);
 vector TAC_second(first_VPA,pnsp);
 vector F_first(first_VPA,pnsp);
 vector F_second(first_VPA,pnsp);
 ivector interYear(first_VPA,pnsp);
 vector inter_F_TAC(first_VPA,pnsp);
 int make_sim_data; 
  // Reorganise
 !! for (s=first_VPA;s<=pnsp;s++) {
 !!     HCR(s)                  =int(tmp2(1,s));
 !!     T1(s)                   =tmp2(2,s);
 !!     T2(s)                   =tmp2(3,s);
 !!     maxFT1(s,1)             =tmp2(4,s);
 !!     maxFT1(s,2)             =tmp2(5,s);
 !!     maxFT1T2(s,1)           =tmp2(6,s);
 !!     maxFT1T2(s,2)           =tmp2(7,s);
 !!     maxFT2(s,1)             =tmp2(8,s);
 !!     maxFT2(s,2)             =tmp2(9,s);
 !!     HCR_F_TAC(s)            =int(tmp2(10,s));
 !!     constantF(s)            =tmp2(11,s);
 !!     constantTAC(s)          =tmp2(12,s);
 !!     target_SSB(s)           =tmp2(13,s);
 !!     real_time_F(s)          =tmp2(14,s);
 !!     TAC_cap(s)              =tmp2(15,s);
 !!     TAC_min(s)              =tmp2(16,s);
 !!     F_cap(s)                =tmp2(17,s);
 !!     F_constraint(s,1)       =tmp2(18,s);
 !!     F_constraint(s,2)       =tmp2(19,s);
 !!     TAC_constraint(s,1)     =tmp2(20,s);
 !!     TAC_constraint(s,2)     =tmp2(21,s);
 !!     SSB_constraint(s,1)      =tmp2(22,s);
 !!     SSB_constraint(s,2)      =tmp2(23,s); 
 !!     obs_noise_trunc(s,1)     =tmp2(24,s);
 !!     obs_noise_trunc(s,2)     =tmp2(25,s);
 
 !!     real_time_uncertanty(s,1)=tmp2(26,s);
 !!     real_time_uncertanty(s,2)=tmp2(27,s);
 !!     real_time_uncertanty(s,3)=tmp2(28,s);
 !!     real_time_uncertanty(s,4)=tmp2(29,s);

 !!     survey_uncertanty(s,1)   =tmp2(30,s);
 !!     survey_uncertanty(s,2)   =tmp2(31,s);
 !!     survey_uncertanty(s,3)   =tmp2(32,s);
 !!     survey_uncertanty(s,4)   =tmp2(33,s);

 !!     assess_uncertanty(s,1)   =tmp2(34,s);
 !!     assess_uncertanty(s,2)   =tmp2(35,s);
 !!     assess_uncertanty(s,3)   =tmp2(36,s);
 !!     assess_uncertanty(s,4)   =tmp2(37,s);
  
 !!     implemt_uncertanty(s,1)  =tmp2(38,s);
 !!     implemt_uncertanty(s,2)  =tmp2(39,s);
 !!     implemt_uncertanty(s,3)  =tmp2(40,s);
 !!     implemt_uncertanty(s,4)  =tmp2(41,s);

 !!     TAC_first(s)             =tmp2(42,s);
 !!     TAC_second(s)            =tmp2(43,s);
 !!     F_first(s)               =tmp2(44,s);
 !!     F_second(s)              =tmp2(45,s);
 !!     interYear(s)             =int(tmp2(46,s));
 !!     inter_F_TAC(s)           =tmp2(47,s);
 !! }
 !! make_sim_data=0;
 !! if (read_HCR_file==1) if (HCR(first_VPA)==0) { 
 !!   make_sim_data=1; 
 !!   //cout<<"make_sim_data:"<<make_sim_data<<endl;
 !! }      

 
 !! TAC_constraint_copy=TAC_constraint;
 !! real_time_uncertanty_copy=real_time_uncertanty;
 !! HCR_copy=HCR;
 !!
 !! if (read_HCR_file==1) if (test_output==51) { 
 !!   cout<<"HCR: "; if (pnsp>1) cout<<endl; cout<<HCR<<endl;
 !!   cout<<"T1: "; if (pnsp>1) cout<<endl; cout<<T1<<endl;
 !!   cout<<"T2: "; if (pnsp>1) cout<<endl; cout<<T2<<endl;
 !!   cout<<"FT1a: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<maxFT1(s,1)<<" "; cout<<endl;
 !!   cout<<"FT1b: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<maxFT1(s,2)<<" "; cout<<endl;
 !!   cout<<"FT12a: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<maxFT1T2(s,1)<<" "; cout<<endl;
 !!   cout<<"FT12b: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<maxFT1T2(s,2)<<" "; cout<<endl;
 !!   cout<<"FT2a: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<maxFT2(s,1)<<" "; cout<<endl;
 !!   cout<<"FT2b: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<maxFT2(s,2)<<" "; cout<<endl;
 !!   cout<<"HCR.F.TAC: "; if (pnsp>1) cout<<endl; cout<<HCR_F_TAC<<endl;
 !!   cout<<"constant.F: "; if (pnsp>1) cout<<endl; cout<<constantF<<endl;
 !!   cout<<"constant.TAC: "; if (pnsp>1) cout<<endl; cout<<constantTAC<<endl;
 !!   cout<<"target.SSB: "; if (pnsp>1) cout<<endl; cout<<target_SSB<<endl;
 !!   cout<<"real.time.F: "; if (pnsp>1) cout<<endl; cout<<real_time_F<<endl;
 !!   cout<<"cap.TAC: "; if (pnsp>1) cout<<endl; cout<<TAC_cap<<endl;
 !!   cout<<"min.TAC: "; if (pnsp>1) cout<<endl; cout<<TAC_min<<endl;
 !!   cout<<"cap.F: "; if (pnsp>1) cout<<endl; cout<<F_cap<<endl;
 !!   cout<<"min.F.change: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<F_constraint(s,1)<<" "; cout<<endl;
 !!   cout<<"max.F.change: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<F_constraint(s,2)<<" "; cout<<endl;
 !!   cout<<"min.TAC.change: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<TAC_constraint(s,1)<<" "; cout<<endl;
 !!   cout<<"max.TAC.change: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<TAC_constraint(s,2)<<" "; cout<<endl;
 !!   cout<<"min.SSB.change: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<SSB_constraint(s,1)<<" "; cout<<endl;
 !!   cout<<"max.SSB.change: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<SSB_constraint(s,2)<<" "; cout<<endl;
 !!   cout<<"obs.noise.lower: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<obs_noise_trunc(s,1)<<" "; cout<<endl;
 !!   cout<<"obs.noise.upper: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<obs_noise_trunc(s,2)<<" "; cout<<endl;
 !!   cout<<"real.T.dist: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<real_time_uncertanty(s,1)<<" "; cout<<endl;
 !!   cout<<"real.T.bias: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<real_time_uncertanty(s,2)<<" "; cout<<endl;
 !!   cout<<"real.T.std: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<real_time_uncertanty(s,3)<<" "; cout<<endl;
 !!   cout<<"real.T.cor: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<real_time_uncertanty(s,4)<<" "; cout<<endl;
 !!   cout<<"survey.dist: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<survey_uncertanty(s,1)<<" "; cout<<endl;
 !!   cout<<"survey.bias: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<survey_uncertanty(s,2)<<" "; cout<<endl;
 !!   cout<<"survey.std: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<survey_uncertanty(s,3)<<" "; cout<<endl;
 !!   cout<<"survey.cor: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<survey_uncertanty(s,4)<<" "; cout<<endl;

 !!   cout<<"assess.dist: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<assess_uncertanty(s,1)<<" "; cout<<endl;
 !!   cout<<"assess.bias: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<assess_uncertanty(s,2)<<" "; cout<<endl;
 !!   cout<<"assess.std: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<assess_uncertanty(s,3)<<" "; cout<<endl;
 !!   cout<<"assess.cor: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<assess_uncertanty(s,4)<<" "; cout<<endl;
 !!   cout<<"implemt.dist: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<implemt_uncertanty(s,1)<<" "; cout<<endl;
 !!   cout<<"implemt.bias: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<implemt_uncertanty(s,2)<<" "; cout<<endl;
 !!   cout<<"implemt.std: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<implemt_uncertanty(s,3)<<" "; cout<<endl;
 !!   cout<<"implemt.cor: "; if (pnsp>1) cout<<endl<<" ";  for (s=first_VPA;s<=pnsp;s++)cout<<implemt_uncertanty(s,4)<<" "; cout<<endl;
 !!   cout<<"TAC.first: "; if (pnsp>1) cout<<endl; cout<<TAC_first<<endl;
 !!   cout<<"TAC.second: "; if (pnsp>1) cout<<endl; cout<<TAC_second<<endl;
 !!   cout<<"F.first: "; if (pnsp>1) cout<<endl; cout<<F_first<<endl;
 !!   cout<<"F.second: "; if (pnsp>1) cout<<endl; cout<<F_second<<endl;
 !!   cout<<"inter.Year: "; if (pnsp>1) cout<<endl; cout<<interYear<<endl;
 !!   cout<<"inter.F.TAC: "; if (pnsp>1) cout<<endl; cout<<inter_F_TAC<<endl;
 !! }

 !! if (read_HCR_file==1) for (s=first_VPA;s<=pnsp;s++) { 
 !!   if ((TAC_first(s)>0 && F_first(s)>0) || (TAC_second(s)>0 && F_second(s)>0)) {
 !!     cout <<endl<<"You cannot have both TAC and F for the first prediction years for species: "<<s<<endl;
 !!     exit(9); 
 !!    }
 !!    if ((HCR(s)==15 || HCR(s)==20 || HCR(s)==21) && interYear(s)>1) {
 !!      cout <<"ERROR: species:"<<s<<". Intermediate year must 1 used with HCR=15, 20 or 21"<<endl;
 !!      exit(9);
 !!    }
 !! }

 !! for (s=first_VPA;s<=pnsp;s++) {
 !!  if (obs_noise_trunc(s,1)>= obs_noise_trunc(s,2) && (read_HCR_file==1)) {
 !!   cout<<"ERROR: Observation noise truncation. First value must be smaller than second value\nProgram stopped"<<endl; 
 !!   exit(9);
 !! }}

 ivector HCR_state(first_VPA,nsp);
 !!  HCR_state=1;
 
 // auto co-relation in (noise for) recruitment
 init_ivector input_no_recruit_autocor(first_VPA,nsp);          // input number of years used for autocorrelation 
 !! if (read_HCR_file==1) if (test_output==51) cout<<"input_no_recruit_autocor:"<<input_no_recruit_autocor<<endl;
 ivector no_recruit_autocor(first_VPA,nsp)                      // number of years used for autocorrelation  
 !! no_recruit_autocor=input_no_recruit_autocor;
 !! for (s=first_VPA;s<=nsp;s++) if (input_no_recruit_autocor(s)<1) no_recruit_autocor(s)=1;
 init_matrix recruit_autocor(first_VPA,nsp,1,no_recruit_autocor) 
 !! for (s=first_VPA;s<=nsp;s++) if (input_no_recruit_autocor(s)<1) recruit_autocor(s)=0;
 !! no_recruit_autocor=input_no_recruit_autocor;
 !! if (read_HCR_file==1) if (test_output==51) cout<<"recruit_autocor:"<<recruit_autocor<<endl;

 init_vector recruit_adjust(first_VPA,nsp);          // factor for adjustment of future recruits
 !! if (read_HCR_file==1) if (test_output==51) cout<<"recruit_adjust:"<<recruit_adjust<<endl;

  init_ivector recruit_adjust_CV(first_VPA,nsp);          // factor for adjustment of future recruits by exp(-0.5*CV^2)
 !! if (read_HCR_file==1) if (test_output==51) cout<<"recruit_adjust_CV:"<<recruit_adjust_CV<<endl;

 init_int read_SSB_R;
 !! if (test_output==51 && read_HCR_file==1) cout<<"read SSB R parameters :" <<read_SSB_R<<endl;
 !! if (read_HCR_file!=1) read_SSB_R=0; 
 

 init_ivector at_age_output(1,8) // flag for at age output (F M2 Z N C FLR, constraints, closure)
  !! if (test_output==51 && read_HCR_file==1) cout<<"at.age.output: "<<at_age_output<<endl;

 init_int read_predict_N_first_year;
 init_int read_predict_N_last_year;
 !! if (test_output==51 && read_HCR_file==1) cout<<"First and last year, stock numbers :" 
 !!               <<read_predict_N_first_year<<" "<<read_predict_N_last_year<<endl;
 int read_predict_N;
 !! if (read_predict_N_first_year>lyModel) read_predict_N=1; else read_predict_N=0; 

 init_int n_init_pop;
 !! if (test_output==51 && read_HCR_file==1) cout<<"n.init.pop: "<<n_init_pop<<endl;

 init_int read_expl_pat_first_year;
 init_int read_expl_pat_last_year;
 !! if (test_output==51 && read_HCR_file==1) cout<<"First and last year, exploitation pattern :" 
 !!               <<read_expl_pat_first_year<<" "<<read_expl_pat_last_year<<endl;
 int read_expl_pat;
 !! if (read_HCR_file==0) read_expl_pat=0; 
 !! else {if (read_expl_pat_first_year>lyModel) read_expl_pat=1; else read_expl_pat=0;}


 init_int use_read_expl_pat;
 !! if (test_output==51 && read_HCR_file==1) cout<<"use.read.expl.pat: "<<use_read_expl_pat<<endl;


  
 // other predator change. for use in prediction
 int loc_nOthPred;
 !!   loc_nOthPred=nOthPred;
 !! if (nOthPred==0) {
 !!   ad_comm::change_datafile_name("just_one.in");
 !! if (nOthPred==0) loc_nOthPred=1;
 !! }
 init_vector other_pred_N_change(1,loc_nOthPred)        //factor for change in other predator stock number
 !! for (s=1;s<=loc_nOthPred;s++) if (other_pred_N_change(s)<0) {
 !!   cout<<"ERROR: option other_pred_N_change must be >= 0"<<endl;
 !!    exit(9);
 !! }
 init_ivector other_pred_N_first_year(1,loc_nOthPred)    // first year for change
 init_ivector other_pred_N_last_year(1,loc_nOthPred)     //last year for change
 !! if (test_output==51) {
 !!   cout<<"other_pred_N_change:"<<endl<<other_pred_N_change<<endl;
 !!   cout<<"other_pred_N_first_year:"<<endl<<other_pred_N_first_year<<endl;
 !!   cout<<"other_pred_N_last_year:"<<endl<<other_pred_N_last_year<<endl;
 !! }

 // co-variance matrix of log stock numbers
 !! int read_cov=0; int tmpA=0;
 !! if (read_HCR_file==1) for (s=first_VPA;s<=nsp;s++) if (assess_uncertanty(s,1)==2 || assess_uncertanty(s,1)==3) read_cov=1;
 !! if (read_cov==0) { ad_comm::change_datafile_name("just_one.in"); tmpA=-100;}
 !! else { ad_comm::change_datafile_name("covariance_n.in"); tmpA=fa;}
  init_3darray coVariance(first_VPA,nsp,fa,max_a,tmpA,la_VPA);
 !! if (test_output==51 && read_HCR_file==1) for (s=first_VPA;s<=pnsp;s++) if (assess_uncertanty(s,1)==2) cout <<"co-Variance:"<<endl<< coVariance(s)<<endl;
 
 // CV of stock numbers
 !! int read_var_age=0; tmpA=0;
 !! if (read_HCR_file==1) for (s=first_VPA;s<=nsp;s++) if (assess_uncertanty(s,1)==4) read_var_age=1;
 !! if (read_var_age==0) { ad_comm::change_datafile_name("just_one.in"); tmpA=-100;}
 !! else { ad_comm::change_datafile_name("assessment_cv_age.in"); tmpA=fa;}
  init_matrix assessment_CV_age(first_VPA,nsp,tmpA,la_VPA);
 !! if (test_output==51 && read_HCR_file==1) for (s=first_VPA;s<=pnsp;s++) if (assess_uncertanty(s,1)==4) cout <<"co-Variance:"<<endl<< assessment_CV_age(s)<<endl;
 

 // read in recruitment residuals
 int local_lpy
 int use_rec_noise_input
 !! use_rec_noise_input=0;
 !!  for (s=first_VPA;s<=pnsp;s++) if(rec_noise_input(s)==1) use_rec_noise_input=1;
 !! local_lpy=lpy+2;
 !! if (use_rec_noise_input==0 || mceval==0 || do_short_term_forecast>0) {
 !!   local_lpy=lyModel-1;
 !!    ad_comm::change_datafile_name("just_one.in");
 !! }
 !! else ad_comm::change_datafile_name("recruit_residuals.in");

 init_matrix rec_residuals(first_VPA,nsp,lyModel,local_lpy)
 
 //Species.n model alfa  beta std
 !! if (read_SSB_R==0) ad_comm::change_datafile_name("just_one.in");
 !! else if (read_SSB_R==1) ad_comm::change_datafile_name("ssb_r.in");
 init_matrix SSB_R_in(first_VPA,nsp,1,4)
 !! if (read_SSB_R==1 && test_output==51) cout<<"Init SSB_R_in:"<<endl<<SSB_R_in<<endl;

 // predicted recrutiment (and stock numbers
 !! if (read_predict_N==0) ad_comm::change_datafile_name("just_one.in");
 !! else if (read_predict_N==1) ad_comm::change_datafile_name("predict_stock_n.in");
 init_4darray init_predict_N(first_VPA,nsp,read_predict_N_first_year,read_predict_N_last_year,1,n_init_pop,fa,max_a)

 !! if (read_predict_N==1 && test_output>=51) cout<<"Init prediction N:"<<endl<<init_predict_N<<endl;
 
 !! if (read_expl_pat==0) ad_comm::change_datafile_name("just_one.in");
 !! else if (read_expl_pat==1) ad_comm::change_datafile_name("exploitation_pattern.in");
 init_4darray  init_exploitation_pattern(first_VPA,nsp,read_expl_pat_first_year,read_expl_pat_last_year,fq,lq,fa,max_a); //  exploitation pattern in forecast
 !! if (read_expl_pat==1 && test_output>=51) { 
 !!  cout<<"input explotation pattern from file exploitation_pattern.in"<<endl<<setprecision(3)<<endl<<init_exploitation_pattern<<endl;
 !! }

 int lnsp;
 !! if (do_growth_type_1==0 && OP_do_growth_type_1==0 ) {lnsp=first_VPA; ad_comm::change_datafile_name("just_one.in");}
 !! else if (do_growth_type_1==1 || OP_do_growth_type_1==1) {lnsp=nsp; ad_comm::change_datafile_name("growth_type1.in"); }
 // !! cout<<"first_VPA:"<<first_VPA<<" lnsp:"<<lnsp<<" fa:"<<fa<<" max_a:"<<max_a<<endl;
 init_3darray  growth_type1_regr(first_VPA,lnsp,fa,max_a,1,3); //  Growth in forecast
 !! if (do_growth_type_1==1 && test_output>=51) {
 !!  cout<<"growth_type1.in, Regression parameters:"<<endl<<setprecision(3)<<endl<<growth_type1_regr<<endl;
 !! }
 init_3darray  growth_type1_in_year(first_VPA,lnsp,fa,max_a,fq,lq); //  Growth in forecast
 !! if (do_growth_type_1==1 && test_output>=51) {
 !!  cout<<"growth_type1.in, in year increment ratio:"<<endl<<setprecision(3)<<endl<<growth_type1_in_year<<endl;
 !! }
 init_3darray  growth_ratio_weca_west(first_VPA,lnsp,fq,lq,fa,max_a); //  ratio between west and weca
 !! if (do_growth_type_1==1 && test_output>=51) {
 !!  cout<<"growth_type1.in, ratio between weca and west"<<endl<<setprecision(2)<<endl<<growth_ratio_weca_west<<endl;
 !! }
 init_matrix  growth_min_w(first_VPA,lnsp,fa,max_a); //  minimum west
 !! if (do_growth_type_1==1 && test_output>=51) {
 !!  cout<<"Growth, minimum weight at age Q1"<<endl<<setprecision(2)<<endl<<growth_min_w<<endl;
 !! }
 init_matrix  growth_max_w(first_VPA,lnsp,fa,max_a); //  minimum west
 !! if (do_growth_type_1==1 && test_output>=51) {
 !!  cout<<"Growth, maximum weight at age Q1"<<endl<<setprecision(2)<<endl<<growth_max_w<<endl;
 !! }



 LOCAL_CALCS
    char * string_ptr;
    ifstream ifs("species_names.in");
    for (s=1;s<=nsp;s++)
    {
      //line_adstring sAD; // defines a line of input, including any special characters
      char sAD[11];
      ifs >> sAD; // reads the line from input stream  
        while((string_ptr=strpbrk(sAD,"_"))!=NULL) *string_ptr=' ';   // remove '_'
        species_names+=sAD; // appends the line to array, thus extending the vector by one addressable element
     }
     ifs.close();
     
     if (no_areas>1) {
      ifstream ifs("area_names.in");
      for (d=1;d<=no_areas;d++)
      {
        //line_adstring sAD; // defines a line of input, including any special characters
         char sAD[11];
         ifs >> sAD; // reads the line from input stream
          while((string_ptr=strpbrk(sAD,"_"))!=NULL) *string_ptr=' ';   // remove '_'
          area_names+=sAD; // appends the line to array, thus extending the vector by one addressable element
       }
       ifs.close();
     }
 END_CALCS
 
  
 //******************************************************************************************** 
 // Data for stock number estimates from survey CPUE data
 !! ad_comm::change_datafile_name("fleet_info.dat");
 !! if (test_output==1) cout<<"Start to read file: fleet_info.dat"<<endl;
 init_number CPUE_minStd          // minimum CV of CPUE observations
 number CPUE_min_s2
 number CPUE_min_s2_bound
 !! CPUE_min_s2=CPUE_minStd*CPUE_minStd;
 !! CPUE_min_s2_bound=CPUE_min_s2;

 !! if (test_output==1) cout<<"fleet.CPUE.CV:"<<endl<<CPUE_minStd<<endl;

 init_ivector n_fleet(first_VPA,nsp)    // number of fleets by species
 !! if (test_output==1) cout<<"n.fleet:"<<endl<<n_fleet<<endl; 

 init_3darray fleet_info(first_VPA,nsp,1,n_fleet,1,10)  //1-2 First and last year
                                                //3-4 alfa and beta, fraction of the season for survey
                                                //5-6 First and last age, 
                                                //7. last age with age dependent catchability,
                                                //8. last age with stock size dependent catchability (power model) 
                                                //  not used 9.   survey year effect (e.g. acoustic surveys): 0=no year effect, 1=year effect is estimated
                                                //9. season for observation
                                                //10. Max number of CPUE s2 groups
                                                //   all by fleet and species
 !! if (test_output==1) cout<<"Fleet_info:"<<endl<<setfixed()<<setprecision(0)<<fleet_info<<endl; 

 //matrix CPUE_minStd_fl(first_VPA,nsp,1,n_fleet)
 //!! CPUE_minStd_fl(1,1)=CPUE_minStd/2;
 //!! CPUE_minStd_fl(1,2)=CPUE_minStd/2;
 //!! CPUE_minStd_fl(1,3)=CPUE_minStd;

  
 // re-organize information
 imatrix first_fleet_year(first_VPA,nsp,1,n_fleet)
 imatrix last_fleet_year(first_VPA,nsp,1,n_fleet)
 matrix fleet_alfa(first_VPA,nsp,1,n_fleet)
 matrix fleet_beta(first_VPA,nsp,1,n_fleet)
 
 //imatrix fleet_year_effect(first_VPA,nsp,1,n_fleet)  // use "year effect" for tuning fleets
 //!! fleet_year_effect=0;

 int no_sp_fl       //number of species & fleet combinations
 int dummy_no_sp_fl
 !! no_sp_fl=0;
 !! for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){
 !!   no_sp_fl++;
 !!   first_fleet_year(s,f)=int(fleet_info(s,f,1)); 
 !!   last_fleet_year(s,f)=int(fleet_info(s,f,2));
 !!   fleet_alfa(s,f)=fleet_info(s,f,3);
 !!   fleet_beta(s,f)=fleet_info(s,f,4);
 !!   //fleet_year_effect(s,f)=fleet_info(s,f,9);
 !!   if ((last_fleet_year(s,f) >lyModel && fq==lq) || (last_fleet_year(s,f)==lyModel && fleet_info(s,f,9)>lqly))  {
 !!     //if (fleet_info(s,f,3)!=0 && fleet_info(s,f,4)!= 0) cout<<"WARNING: sp:"<<s<<" fleet:"<<f<<"  Last survey period cannot be used with alfa & beta !=0"<<endl;  
 !!   }
 !! }


 imatrix first_fleet_age(first_VPA,nsp,1,n_fleet)      // first age group by species and fleet in CPUE data
 imatrix last_fleet_age(first_VPA,nsp,1,n_fleet)       // last age group by species and fleet in CPUE data
 imatrix last_fleet_age_q(first_VPA,nsp,1,n_fleet)     // last age group with age dependen catchability by species and fleet in CPUE data
 imatrix last_fleet_age_power(first_VPA,nsp,1,n_fleet) // last age group with stock size dependent  catchability by species and fleet in CPUE data

 imatrix n_CPUE_s2_group(first_VPA,nsp,1,n_fleet)      // no of separate CPUE s2 groups by species and fleet
 imatrix fleet_season(first_VPA,nsp,1,n_fleet)         // Season for CPUE data by species and fleet

 ivector v_first_fleet_age(1,no_sp_fl)
 ivector v_last_fleet_age_q(1,no_sp_fl)
 ivector v_last_fleet_age(1,no_sp_fl)
 ivector v_last_fleet_age_plus_one(1,no_sp_fl)
 ivector v_last_fleet_age_power(1,no_sp_fl)
 
 ivector v_n_CPUE_s2_group(1,no_sp_fl)
 ivector dummy_v_n_CPUE_s2_group(1,no_sp_fl)
 ivector v_first_fleet_year(1,no_sp_fl)
 ivector v_last_fleet_year(1,no_sp_fl)

 
 !! int sp_fl=0;  
 !! for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){
 !!   sp_fl++;
 !!   first_fleet_age(s,f)=int(fleet_info(s,f,5)); 
 !!   if (first_fleet_age(s,f)<fa) {
 !!     cout<<"ERROR in CPUE data input. First CPUE age lower than lowest species age."<<
 !!        endl<<"Species:"<<s<<" fleet:"<<f<<endl<<"program stopped"<<endl;
 !!     exit(9);
 !!   }
 !!   if (fleet_info(s,f,7)>fleet_info(s,f,6)) {
 !!     cout<<"ERROR in CPUE data input. Last age with age dependent catchability higher than oldest age."<<
 !!        endl<<"Species:"<<s<<" fleet:"<<f<<endl<<"program stopped"<<endl;
 !!     exit(9);
 !!   }

 !!   v_first_fleet_age(sp_fl)=int(fleet_info(s,f,5)); 

 !!   last_fleet_age(s,f)=int(fleet_info(s,f,6));
 !!   v_last_fleet_age(sp_fl)=int(fleet_info(s,f,6)); 
 !!   v_last_fleet_age_plus_one(sp_fl)=v_last_fleet_age(sp_fl)+1;

 !!   last_fleet_age_q(s,f)=int(fleet_info(s,f,7));
 !!   v_last_fleet_age_q(sp_fl)=int(fleet_info(s,f,7));

 !!   last_fleet_age_power(s,f)=int(fleet_info(s,f,8));
 !!   v_last_fleet_age_power(sp_fl)=int(fleet_info(s,f,8)); 

 !!   fleet_season(s,f)=int(fleet_info(s,f,9));

 !!   n_CPUE_s2_group(s,f)=int(fleet_info(s,f,10));
 !!   v_n_CPUE_s2_group(sp_fl)=int(fleet_info(s,f,10));

 !!   v_first_fleet_year(sp_fl)=first_fleet_year(s,f);
 !!   v_last_fleet_year(sp_fl)=last_fleet_year(s,f);
 
 !! }
 
 3darray qq_power_index(first_VPA,nsp,1,n_fleet,first_fleet_age,last_fleet_age) //pointer to qq_power_ini
 !! for (s=first_VPA;s<=nsp;s++) qq_power_index(s)=0;

 // count number of power functions in use
 int no_of_power_exponents
 !! no_of_power_exponents=0;
 !! for (s=first_VPA;s<=nsp;s++) qq_power_index(s)=0;
 !! sp_fl=0;  
 !! for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){
 !!   sp_fl++;
 !!   if (last_fleet_age_power(s,f)<0) v_last_fleet_age_power(sp_fl)=v_first_fleet_age(sp_fl);
 !!   else {
 !!     for (a=v_first_fleet_age(sp_fl);a<=v_last_fleet_age_power(sp_fl);a++) {
 !!       no_of_power_exponents++;
 !!       qq_power_index(s,f,a)=no_of_power_exponents;
 !!     }
 !!   } 
 !! }
 
 //!!   cout<<"qq_power_index:"<<endl<<qq_power_index<<endl;


 // first age in  CPUE group by species and fleet
 init_3darray CPUE_s2_group(first_VPA,nsp,1,n_fleet,1,n_CPUE_s2_group)     
 !! if (test_output==1) cout<<"CPUE_s2_group:"<<endl<<CPUE_s2_group<<endl; 
 !! for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){
 !!   if (CPUE_s2_group(s,f,1)>first_fleet_age(s,f)) {
 !!      cout <<"ERROR ERROR ERROR first age in fleet variance group is younger than first age for the fleet"<<endl;
 !!      cout <<"Species:"<<s<<" fleet:"<<f<<endl;
 !!      exit(9);
 !!   }
 !!   if (n_CPUE_s2_group(s,f)>1) {
 !!     if (CPUE_s2_group(s,f,n_CPUE_s2_group(s,f))>last_fleet_age(s,f)) {
 !!      cout <<"ERROR ERROR ERROR last age in fleet variance group is older than last age for the fleet"<<endl;
 !!      cout <<"Species:"<<s<<" fleet:"<<f<<endl;
 !!      exit(9);
 !!     }
 !!   }
 !! }
 
 init_number check1b; 
 !! checkSum(check1b,"fleet_info.dat");
 
 LOCAL_CALCS
    ifstream ifss("fleet_names.in");
    for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++) {   
      //line_adstring sAD; // defines a line of input, including any special characters
      char sAD[26];
      ifss >> sAD; // reads the line from input stream 
      while((string_ptr=strpbrk(sAD,"_"))!=NULL) *string_ptr=' ';   // remove '_'
      fleet_names+=sAD; // appends the line to array, thus extending the vector by one addressable element
     }
     ifss.close();
 END_CALCS

 
 !! if (test_output==2) cout<<"starts reading file: fleet_catch.in"<<endl;
 !! //cout<<no_sp_fl<<endl<<v_first_fleet_year<<endl<<v_last_fleet_year<<endl<<v_first_fleet_age<<endl<<v_last_fleet_age_plus_one<<endl;
 !! //cout<<fleet_names<<endl;
 !! //cout<<n_fleet<<endl;
 
 !! ad_comm::change_datafile_name("fleet_catch.in");
 init_3darray fleet_catch_tmp(1,no_sp_fl,v_first_fleet_year,v_last_fleet_year,v_first_fleet_age,v_last_fleet_age_plus_one)
 init_number check5;

 3darray fleet_catch(1,no_sp_fl,v_first_fleet_year,v_last_fleet_year,v_first_fleet_age,v_last_fleet_age)
 !! if (test_output==2) {
 !!   sp_fl=0;
 !!   cout<<"Fleet_catches_input:"<<endl;
 !!   for (s=first_VPA;s<=nsp;s++)  for (f=1;f<=n_fleet(s);f++){
 !!     sp_fl++;
 !!     cout<<"fleet: "<<fleet_names[sp_fl]<<endl<<fleet_catch_tmp(sp_fl)<<endl;
 !!   }
 !!  }
 
 !! checkSum(check5,"fleet_catch.in"); 
 
 int sf
 matrix fleet_effort(1,no_sp_fl,v_first_fleet_year,v_last_fleet_year)     //effort by species and fleet
 !! for (sf=1;sf<=no_sp_fl;sf++) for (y=v_first_fleet_year(sf);y<=v_last_fleet_year(sf);y++){
 !!   fleet_effort(sf,y)=fleet_catch_tmp(sf,y,v_first_fleet_age(sf));
 !!   for (a=v_first_fleet_age(sf);a<=v_last_fleet_age(sf);a++) {
 !!    fleet_catch(sf,y,a)=fleet_catch_tmp(sf,y,a+1);
 !!   }
 !! } 
 
  // variance at age for catch at age observations
 !!  if (make_sim_data==1) {
 !!    i=no_MCMC_iterations;
 !!    ad_comm::change_datafile_name("catch_noise.dat");
 !!  } else {
 !!    i=-1;
 !!    j=-1;
 !!  }
 !! j=0;
 !! if (make_sim_data==1) for (s=first_VPA;s<=nsp;s++) j+= seasonal_combined_catch_s2(s)*n_catch_s2_group(s);
 init_matrix catch_s2_input(1,i,1,j);
 int g;
 3darray catch_s2_index(first_VPA,nsp,1,seasonal_combined_catch_s2,1,n_catch_s2_group)
 !! i=0; 
 !! if (make_sim_data==1) {
 !!   for (s=first_VPA;s<=nsp;s++) for (q=fq;q<=seasonal_combined_catch_s2(s);q++) for (g=1;g<=n_catch_s2_group(s);g++) {i+=1;  catch_s2_index(s,q,g)=i;}
 !! }
 
 // variance at age for CPUE at age observations
 !!  if (make_sim_data==1) {
 !!    i=no_MCMC_iterations;
 !!    no_sp_fl=0; j=0;
 !!    for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){
 !!      no_sp_fl++;
 !!      for (k=1;k<=v_n_CPUE_s2_group(no_sp_fl);k++) {
 !!         j+=1; //cout<<" survey s:"<<s<<" f:"<<f<<" no_sp_fl:"<<no_sp_fl<<" j:"<<j<<endl; 
 !!      }
 !!     }
 !!    ad_comm::change_datafile_name("survey_noise.dat");
 !!  } else {
 !!    i=-1; j=1;
 !!  }
  //!! cout<<" survey i:"<<i<<" j:"<<j<<endl;
 init_matrix qq_s2_input(1,i,1,j);
 
 //3darray log_year_effect_index(first_VPA,nsp,1,n_fleet,first_fleet_year,last_fleet_year);  //pointer to log_year_effect_ini
  // count number of year effects in use
 //int no_of_fleet_year_effect
 //!! no_of_fleet_year_effect=0;
 //!! for (s=first_VPA;s<=nsp;s++)  log_year_effect_index(s)=0;
 //!! sp_fl=0;
 //!! for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){
 //!!   sp_fl++;
 //!!   for (y=first_fleet_year(s,f)+1;y<=last_fleet_year(s,f);y++) if (fleet_year_effect(s,f)==1 && fleet_effort(sp_fl,y)>0 ) {
 //!!     no_of_fleet_year_effect++;
 //!!     log_year_effect_index(s,f,y)=no_of_fleet_year_effect;
 //!!   }
 //!! }
 //!!   cout<<"log_year_effect_index:"<<endl<<log_year_effect_index<<endl;


 //********************************************************************************************* 
 // Observed effort total effort, only used in SMS-effort version
 
  
 int tmpE;
 !! if (any_do_effort==0) {     // normal SMS
 !!  ad_comm::change_datafile_name("just_one.in");
 !!  tmpE=first_VPA;
 !! } else {  // SMS-effort version for at least one species
 !!   tmpE=nsp;
 !!   ad_comm::change_datafile_name("effort.in");
 !!    if (test_output==2) cout<<"starts reading file: effort.in"<<endl;
 !! }
 
 init_3darray effort(first_VPA,tmpE,fq,lq,fyData,lyData) 
 !! if (any_do_effort!=0 && test_output==2) cout<<"Observed total effort:"<<endl<<effort<<endl;
 // Normalise to mean effort=1   SKAL 鷹DRES TIL NORMALISERES INDENFOR 霹R鬢KE MED SAMME EXPLOITATION PATTERN (M露KE??)
 number sum_effort
 !! if (any_do_effort==1) {
 !!   for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==1) {
 !!     sum_effort=0;
 !!     for (q=fq;q<=lq;q++)  sum_effort+=sum(effort(s,q));
 !!     sum_effort=sum_effort/(lyData-fyData+1);
 !!     for (q=fq;q<=lq;q++) for (y=fyData;y<=lyData;y++) effort(s,q,y)=effort(s,q,y)/sum_effort;
 !! }}
  //!! cout<<"Normalised total effort:"<<endl<<effort<<endl;
 
 int ly_group
 !!  if (any_do_effort==1) {
 !!    for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==1) {
 !!     for(syg=1;syg<=n_catch_sep_year_group(s);syg++) {
 !!      if (syg==n_catch_sep_year_group(s)) ly_group=lyModel;
 !!       else ly_group=catch_sep_year(s,syg+1)-1;
 !!       sum_effort=0;
 !!       i=0;
 !!       for (y=catch_sep_year(s,syg);y<=ly_group;y++) {
 !!         for (q=fq;q<=lq;q++) if (effort(s,q,y)>0) {
 !!            sum_effort+=effort(s,q,y);
 !!            i=i+1;
 !!         }
 !!       }
 !!       sum_effort=sum_effort/i;
 !!       for (y=catch_sep_year(s,syg);y<=ly_group;y++) for (q=fq;q<=lq;q++) effort(s,q,y)=effort(s,q,y)/sum_effort;
 !!     }
 !!   }
 !!  }
 //!! cout<<"Normalised total effort:"<<endl<<effort<<endl;


 //********************************************************************************************* 
 //
 

 LOCAL_CALCS
  #define DIM2(X) \
  { cout<<endl<<" Dimensions:"<<#X<<endl;\
    cout<<"rowmin: "<<X.rowmin()<<"  rowmax:"<<X.rowmax<<endl; \
    cout<<"colmin: "<<X.colmin()<<"  rowmax:"<<X.colmax<<endl; \
   }

  #define DIM3(X) \
   { cout<<endl<<" Dimensions:"<<#X<<endl;\
    cout<<"rowmin: "<<X.rowmin()<<"  rowmax:"<<X.rowmax<<endl; \
    cout<<"colmin: "<<X.colmin()<<"  rowmax:"<<X.colmax<<endl; \
    cout<<"slicemin: "<<X.slicemin()<<"  rowmax:"<<X.slicemax<<endl; \
   }
  // macro for index calculation
  #define CALC_yq yq=(y-fyData)*lq+q;
  #define CALC_yqd yqd=(y-fyData)*q_d+(q-fq)*no_areas+d;

  // macroes to transform data from input format (by species) to format by timestep
  // without area
  #define MOVE(in,out,first,last,lyear) \
   { for(y=fyData;y<=lyear;y++) for(q=fq;q<=lq;q++)  {  \
     CALC_yq \
     for (s=first;s<=last;s++) for(a=faq(q);a<=la(s);a++)  out(yq,s,a)=in(s,y,q,a);   \
   }}
   
   
  // with area  
  #define MOVEd(in,out,first,last) \
   { for(y=fyData;y<=lyData;y++) for(q=fq;q<=lq;q++) for(d=1;d<=no_areas;d++) {  \
     CALC_yqd   \
     for (s=first;s<=last;s++) for(a=faq(q);a<=la(s);a++) out(yqd,s,a)=in(d,s,y,q,a); \
   }} 
   
   // macro to change input file
   #define change_input(in) ad_comm::change_datafile_name(in); if (test_output==2) cout<<"Begin reading file:"<<in<<endl;
   
   #define change_input_multi(in) if (multi>=1) ad_comm::change_datafile_name(in); else ad_comm::change_datafile_name("just_one.in"); if (test_output==2 && multi>=1) cout<<"Begin reading file:"<<in<<endl;

 END_CALCS

 //********************************************************************************************* 
 //
 // Make common structures for basic data structures (ragged array) 
 
 !! y=lyData; q=lq; d=no_areas;
 !! CALC_yq
 int def_yq
 !! def_yqd=yq;
 !! CALC_yqd
 int def_yqd
 !! def_yqd=yqd;
 
 !! // with area
 
 ivector yqdAll(1,def_yqd)           // for used in ragged array for all species
 !! for (i=1;i<=def_yqd;i++) yqdAll(i)=nsp;
         
 imatrix yqdsAll_fa(1,def_yqd,1,nsp)       // for used in ragged array for all species
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  {
 !!  CALC_yqd 
 !!  yqdsAll_fa(yqd)=faq(q); 
 !! }
 imatrix yqdsAll_la(1,def_yqd,1,nsp)       // for used in ragged array for all species
 !! for (i=1;i<=def_yqd;i++) for (s=1;s<=nsp;s++) yqdsAll_la(i,s)=la(s);
 
 ivector yqdVPA(1,def_yqd)           // for used in ragged array for VPA species
 !! for (i=1;i<=def_yqd;i++) yqdVPA(i)=nsp;
 
 imatrix yqdsVPA_fa(1,def_yqd,first_VPA,nsp)       // for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  {
 !!  CALC_yqd 
 !!  yqdsVPA_fa(yqd)=faq(q); 
 !! }
 imatrix yqdsVPA_la(1,def_yqd,first_VPA,nsp)       // for used in ragged array for VPA species
 !! for (i=1;i<=def_yqd;i++) for (s=first_VPA;s<=nsp;s++) yqdsVPA_la(i,s)=la(s);

 !! k=max(1,npr); 
 ivector yqdPred(1,def_yqd)           // for used in ragged array for all predators
 !! for (i=1;i<=def_yqd;i++) yqdPred(i)=k;
 
 imatrix yqdsPred_fa(1,def_yqd,1,k)       // for used in ragged array for all predators
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  {
 !!  CALC_yqd 
 !!  yqdsPred_fa(yqd)=faq(q); 
 !! }     
 imatrix yqdsPred_la(1,def_yqd,1,k)       // for used in ragged array for all predators
 !! for (i=1;i<=def_yqd;i++) for (s=1;s<=k;s++) yqdsPred_la(i,s)=la(s);

 !! k=max(1,nOthPred);
 ivector yqdOPred(1,def_yqd)           // for used in ragged array for all other predators
 !! for (i=1;i<=def_yqd;i++) yqdOPred(i)=k;
         
 imatrix yqdsOPred(1,def_yqd,1,k)       // for used in ragged array for all other predators
 !! for (i=1;i<=def_yqd;i++) for (s=1;s<=k;s++) yqdsOPred(i,s)=la(s);

 !!// ########
 !! // without area
 // all species
 ivector v_last_sp(1,yq)           // last species no, for used in ragged array for all species
 !! v_last_sp=nsp;
 imatrix vAll_fa(1,yq,1,nsp)       // first age for all species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;  
 !!  vAll_fa(i)=faq(q);
 !! }
 imatrix vAll_la(1,yq,1,nsp)       // last age for all species, for used in ragged array for VPA species
 !! for (i=1;i<=yq;i++) for (s=1;s<=nsp;s++) vAll_la(i,s)=la(s);
 
 // all species and one year extra
 int yq1;
 !! yq1=(lyData-fyData+1)*lq+lq;
 ivector v_last_sp1(1,yq1)           // last species no, for used in ragged array for all species
 !! v_last_sp1=nsp;
 imatrix vAll_fa1(1,yq1,1,nsp)       // first age VPA species, for used in ragged array for VPA species
!! for (y=fyData;y<=lyData+1;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vAll_fa1(i)=faq(q);
 !! }
 imatrix vAll_la1(1,yq1,1,nsp)       // last age VPA species, for used in ragged array for VPA species
 !! for (i=1;i<=yq1;i++) for (s=1;s<=nsp;s++) vAll_la1(i,s)=la(s);
 
 
 // all species and two years extra
 int yq2;
 !! yq2=(lyData-fyData+2)*lq+lq;

 ivector v_last_sp2(1,yq2)           // last species no, for used in ragged array for all species
 !! v_last_sp2=nsp;
 imatrix vAll_fa2(1,yq2,1,nsp)       // first age VPA species, for used in ragged array for VPA species
!! for (y=fyData;y<=lyData+2;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vAll_fa2(i)=faq(q);
 !! }
 imatrix vAll_la2(1,yq2,1,nsp)       // last age VPA species, for used in ragged array for VPA species
 !! for (i=1;i<=yq2;i++) for (s=1;s<=nsp;s++) vAll_la2(i,s)=la(s);
 
 
 // VPA species
 !!v_last_sp=nsp;
 imatrix vVPA_fa(1,yq,first_VPA,nsp)       // first age VPA species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vVPA_fa(i)=faq(q);
 !! }
 imatrix vVPA_la(1,yq,first_VPA,nsp)       // last age VPA species, for used in ragged array for VPA species
 !! for (i=1;i<=yq;i++) for (s=first_VPA;s<=nsp;s++) vVPA_la(i,s)=la(s);
 
 // VPA species, and 1 year extra
 !! yq1=(lyData-fyData+1)*lq+lq;
 imatrix vVPA_fa1(1,yq1,first_VPA,nsp)       // first age VPA species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData+1;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vVPA_fa1(i)=faq(q);
 !! }
 imatrix vVPA_la1(1,yq1,first_VPA,nsp)       // last age VPA species, for used in ragged array for VPA species
 !! for (i=1;i<=yq1;i++) for (s=first_VPA;s<=nsp;s++) vVPA_la1(i,s)=la(s);
  
   // VPA species, 2 years extra
 !! yq2=(lyData-fyData+2)*lq+lq;
 imatrix vVPA_fa2(1,yq2,first_VPA,nsp)       // first age VPA species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData+2;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vVPA_fa2(i)=faq(q);
 !! }
 
 
 // all species, 2 years extra
 !! yq2=(lyData-fyData+2)*lq+lq;
 imatrix vAll2_fa2(1,yq2,1,nsp)       // first age all species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData+2;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vAll2_fa2(i)=faq(q);
 !! }


 imatrix vVPA_la2(1,yq2,first_VPA,nsp)       // last age VPA species, for used in ragged array for VPA species
 !! for (i=1;i<=yq2;i++) for (s=first_VPA;s<=nsp;s++) vVPA_la2(i,s)=la(s);
 //ivector v_last_sp2(1,yq2)
 //!! v_last_sp2=nsp;
  
  
 imatrix vAll2_la2(1,yq2,1,nsp)       // last age VPA species, for used in ragged array for VPA species
 !! for (i=1;i<=yq2;i++) for (s=1;s<=nsp;s++) vAll2_la2(i,s)=la(s);
 //ivector v_last_sp2(1,yq2)
 //!! v_last_sp2=nsp;

  //  predator species
 ivector v_last_Psp(1,yq)           // last species no, for used in ragged array for all species
 !!v_last_Psp=npr;
 imatrix vpr_fa(1,yq,1,npr)       // first age predator species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vpr_fa(i)=faq(q);
 !! }
 imatrix vpr_la(1,yq,1,npr)       // last age other predator species, for used in ragged array for VPA species
 !! for (i=1;i<=yq;i++) for (s=1;s<=npr;s++) vpr_la(i,s)=la(s);

  // Other predator species
 ivector v_last_Osp(1,yq)           // last species no, for used in ragged array for all species
 !!v_last_Osp=nOthPred;
 imatrix vOpr_fa(1,yq,1,nOthPred)       // first age other predator species, for used in ragged array for VPA species
 !! for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) {
 !!  i=(y-fyData)*lq+q;
 !!  vOpr_fa(i)=faq(q);
 !! }
 imatrix vOpr_la(1,yq,1,nOthPred)       // last age other predator species, for used in ragged array for VPA species
 !! for (i=1;i<=yq;i++) for (s=1;s<=nOthPred;s++) vOpr_la(i,s)=la(s);

 // stucture for use in ragged "year,species, age" structure
 ivector vys1(fyData,lyData)   // year-species
 ivector vys2(fyData,lyData)   // year-species
 !! vys1=first_VPA;
 !! vys2=nsp;
 imatrix vVPA_lay(fyData,lyData,first_VPA,nsp)       // last age VPA species, for used in ragged array for VPA species
 imatrix vVPA_fay(fyData,lyData,first_VPA,nsp)
 !! for (i=fyData;i<=lyData;i++) for (s=first_VPA;s<=nsp;s++) {vVPA_fay(i,s)=fa; vVPA_lay(i,s)=la(s);}

 //  ragged  quarter,species,age  structure
  ivector p_last_sp(fq,lq)           // last species no, for used in ragged array for all species
 !! p_last_sp=nsp;
 imatrix p_VPA_fa(fq,lq,first_VPA,nsp)       // first age for all species, for used in ragged array for VPA species
 !! for (q=fq;q<=lq;q++) {
 !!  p_VPA_fa(q)=faq(q);
 !! }
 imatrix p_VPA_la(fq,lq,first_VPA,nsp)       // last age for all species, for used in ragged array for VPA species
 !! for (q=fq;q<=lq;q++) for (s=first_VPA;s<=nsp;s++) p_VPA_la(q,s)=la(s);

 imatrix p_All_fa(fq,lq,1,nsp)       // first age for all species, for used in ragged array for VPA species
 !! for (q=fq;q<=lq;q++) {
 !!  p_All_fa(q)=faq(q);
 !! }
 imatrix p_All_la(fq,lq,1,nsp)       // last age for all species, for used in ragged array for VPA species
 !! for (q=fq;q<=lq;q++) for (s=1;s<=nsp;s++) p_All_la(q,s)=la(s);

  //********************************************************************************************* 
  // Mean weight in the catch
 !! change_input("weca.in");
  int lastyear;
 !! if (do_short_term_forecast>0) lastyear=lyData+1; else lastyear=lyData;
 init_4darray weca_input(first_VPA,nsp,fyData,lastyear,fq,lq,fa,max_a) 
 init_number check6;
 !! checkSum(check6,"weca.in"); 

 !! if (test_output==2) cout<<"Weight in the catch from file weca.in:"<<endl<<weca_input<<endl;

 4darray CW(first_VPA,nsp,fyData,lyData,recq,recq,fa,max_a)  // used to calculate weighted mean weight for annual catch  
  
 !!  change_input("canum.in"); 
 init_4darray obs_C_input(first_VPA,nsp,fyData,lyData,fq,lq,fa,max_a) 
 !! if (test_output==2) cout<<"Observed catch numbers from file canum.in:"<<endl<<obs_C_input<<endl;
 init_number check7;
 !! checkSum(check7,"canum.in"); 

 !! if (seasonal_annual_catches_any==1 && lq>1 && recq>1) { //add all catches into recruitment quarter, such that nothing is lost in the MOVE, where information from nonrecruited ages are deleted 
 !!   for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) for (y=fyData;y<=lyData;y++)  for (a=fa;a<=la(s);a++)  {
 !!     CW(s,y,recq,a)=obs_C_input(s,y,recq,a)*weca_input(s,y,recq,a);
 !!   }
 !!   for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) for (y=fyData;y<=lyData;y++) for (q=fq;q<recq;q++) for (a=fa;a<=la(s);a++)  {
 !!     if (obs_C_input(s,y,q,a)>0) {
 !!      obs_C_input(s,y,recq,a)+=obs_C_input(s,y,q,a);
 !!      CW(s,y,recq,a)+=obs_C_input(s,y,q,a)*weca_input(s,y,q,a);
 !!     }
 !!     obs_C_input(s,y,q,a)=0;
 !!   }
 !!   for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) for (y=fyData;y<=lyData;y++) for (q=recq+1;q<=lq;q++) for (a=fa;a<=la(s);a++) {
 !!     if (obs_C_input(s,y,q,a)>0) {
 !!        obs_C_input(s,y,recq,a)+=obs_C_input(s,y,q,a);
 !!        CW(s,y,recq,a)+=obs_C_input(s,y,q,a)*weca_input(s,y,q,a);
 !!     }
 !!     obs_C_input(s,y,q,a)=0;
 !!   }
 !!   for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) for (y=fyData;y<=lyData;y++)  for (a=fa;a<=la(s);a++)  {
 !!      if (obs_C_input(s,y,recq,a)>0 )weca_input(s,y,recq,a)=CW(s,y,recq,a)/obs_C_input(s,y,recq,a);
 !!   }
 !!   for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) for (y=fyData;y<=lyData;y++) for (q=fq;q<=lq;q++) for (a=fa;a<=la(s);a++)  {
 !!      weca_input(s,y,q,a)=weca_input(s,y,recq,a);
 !!   }
 !! }
 !! 
 // !!  cout<<"Weight in the catch from file weca.in and after annaul catch adjustment:"<<endl<<setprecision(3)<<weca_input<<endl;
 // !!  for (s=first_VPA;s<=nsp;s++) {cout<<"CANUM: s: "<<s<<endl; for (y=fyData;y<=lyData;y++) cout<<y<<endl<<setprecision(0)<<obs_C_input(s,y)<<endl;  }
 
 3darray obs_C(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la);
 !! MOVE(obs_C_input,obs_C,first_VPA,nsp,lyData);
 
 3darray weca(1,yq1,first_VPA,v_last_sp1,vVPA_fa1,vVPA_la1)
 !! MOVE(weca_input,weca,first_VPA,nsp,lastyear);
 
 
  //********************************************************************************************* 
 // M, single species Natural mortalities
 !! change_input("natmor.in");
 !! if (do_short_term_forecast>0) lastyear=lyData+1; else lastyear=lyData;
 init_4darray M_input(first_VPA,nsp,fyData,lastyear,fq,lq,fa,max_a) 
 !! if (test_output==2) cout<<"Natural mortalities from file natmor.in:"<<endl<<M_input<<endl;
 init_number check8;
 !! checkSum(check8,"natmor.in"); 


 3darray M(1,yq1,first_VPA,v_last_sp1,vVPA_fa1,vVPA_la1)
 3darray M1M2(1,yq1,first_VPA,v_last_sp1,vVPA_fa1,vVPA_la1)
 !! MOVE(M_input,M,first_VPA,nsp,lastyear);

 //********************************************************************************************* 
 // Mean weight in the sea
 !! change_input("west.in");
 !! if (do_short_term_forecast>0 || any_do_effort==1) lastyear=lyData+2; else lastyear=lyData;
 init_4darray west_input(1,nsp,fyData,lastyear,fq,lq,fa,max_a) 
 !! if (test_output==2) cout<<"Weight in the stock from file west.in:"<<endl<<west_input<<endl;
 init_number check9;
 !! checkSum(check9,"west.in"); 


 3darray west(1,yq2,1,v_last_sp2,vAll_fa2,vAll_la2)
 !! MOVE(west_input,west,1,nsp,lastyear);

        
 //********************************************************************************************* 
 
  // Proportion landed of the catch
 !! if (calc_discard==1) change_input("proportion_landed.in");
 !! if (calc_discard==0) change_input("just_one.in");
 init_4darray prop_landed_input(first_VPA,nsp,fyData,lyData,fq,lq,fa,max_a)
 !! if (calc_discard==0) {
 !!    for (s=first_VPA;s<=nsp;s++) for(y=fyData;y<=lyData;y++) prop_landed_input(s,y)=1;
 !! }
 !! if (test_output==2) cout<<"Proportion landed from file proportion_landed.in:"<<setfixed()<<setprecision(3)<<endl<<prop_landed_input<<endl;
 !! y=lyData; q=lq; CALC_yq
 3darray prop_landed(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la)
 
 !! MOVE(prop_landed_input,prop_landed,first_VPA,nsp,lyData);
 //********************************************************************************************* 
 
  // Proportion of N used for calculation of M2
 !! if (multi>0) ad_comm::change_datafile_name("n_proportion_m2.in");   
 !! else ad_comm::change_datafile_name("just_one.in");

 init_4darray N_prop_M2_input(first_VPA,nsp,fyData,lyData,fq,lq,fa,max_a)
 !! if (test_output==2) cout<<"Proportion of N used for calculation of M2 from  file n_proportion_m2.in:"<<setfixed()<<setprecision(3)<<endl<<N_prop_M2_input<<endl;
 init_number check10a;
 !!  if (multi>0) checkSum(check10a,"n_proportion_m2.in");
 
 !! y=lyData; q=lq; CALC_yq
 3darray N_prop_M2(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la)
 
 !! MOVE(N_prop_M2_input,N_prop_M2,first_VPA,nsp,lyData);

 //********************************************************************************************* 
 // Proportion mature
 !! change_input("propmat.in");

 !! if (do_short_term_forecast>0 || any_do_effort==1) lastyear=lyData+2; else lastyear=lyData;
 init_4darray propmat_input(first_VPA,nsp,fyData,lastyear,fq,lq,fa,max_a) 
 init_number check10;
 !! checkSum(check10,"propmat.in"); 

 !! if (test_output==2) cout<<"proportion mature from file propmat.in:"<<endl<<propmat_input<<endl;
 3darray propmat(1,yq2,first_VPA,v_last_sp2,vVPA_fa2,vVPA_la2)    // two extra years, used to make structure also useable for stochastic (single species) forecast
 
  // !! cout<<"PROPMAT: lyData:"<<lyData<<" lastyear:"<<lastyear<<"  do_short_term_forecast:"<<do_short_term_forecast<<endl;
 !! MOVE(propmat_input,propmat,first_VPA,nsp,lastyear);

 //*********************************************************************************************
 // Proportion of M and F before spawning
 !! change_input("proportion_m_and_f_before_spawning.in");

 init_vector prop_M(first_VPA,nsp);
 init_vector prop_F(first_VPA,nsp);
 !! if (test_output==2) cout<<"proportion mature befor spawning from file proportion_m_and_f_before_spawning.in:"<<endl<<prop_M<<endl<<prop_F<<endl;

 //********************************************************************************************* 


 // Temperature
 !! if (read_temperature==0) ad_comm::change_datafile_name("just_one.in");
 !! else ad_comm::change_datafile_name("temperature.in");
 init_vector temperature(fyData,lyData);
 !!  if (test_output==2 && read_temperature==1) cout<<"temperature:"<<temperature<<endl;

 // option for "known" input recrutiment
 !! if (use_known_rec_option==0) ad_comm::change_datafile_name("just_one.in");
 !! else change_input("known_recruitment.in");
 init_matrix known_recruitment(first_VPA,nsp,fyData,lyData);
 
  init_number check11;
 !! if (use_known_rec_option==1)  checkSum(check11,"known_recruitment.in"); 
 !! if (test_output==2) cout<< "known_recruitment:"<<endl<<known_recruitment<<endl;
  
  ivector use_known_rec_option_by_sp(first_VPA,nsp);
 !! use_known_rec_option_by_sp=0;
 !! if (use_known_rec_option==1) {for (s=first_VPA;s<=nsp;s++) for(y=fyData;y<=lyData;y++) if (known_recruitment(s,y)>0) use_known_rec_option_by_sp(s)=1;}                     

 //*********************************************************************************************

 !! if (test_output<3 && test_output>0) cout << endl << " Input of single species input data completed" << endl;
  3darray log_obs_C_annual(fyData,lyData,first_VPA,vys2,vVPA_fay,vVPA_lay)   // log of observed catches (annual catches)
  !! y=lyData; q=lq; CALC_yq; 
  3darray log_obs_C(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la);
  3darray sum_C(first_VPA,nsp,fq,lq,fa,la_VPA)                 // mean catch number per age and season
  matrix sum_C_annual(first_VPA,nsp,fa,la_VPA)
 // for single species read multispecies variables from a dummy files including a lot of 1
 !! if (multi==0) ad_comm::change_datafile_name("just_one.in");

   //*********************************************************************************************
 // minimize arrays not used in multi speies mode 
 int Mnsp
 int Mnpr
 int Mnopr;
 int Mly 
 int Mlq 
 int Mmax_a
 int Mindex
 int Myq;
 !! if (multi==0) {
 !!   Mnsp=first_VPA; Mnpr=1; Mnopr=1; Mly=fyData; Mlq=fq; Mmax_a=fa; Myq=1;
 !!   //Mnsp=0; Mnpr=0; Mnopr=0; Mly=0; Mlq=0; Mmax_a=0;    // to avoid allocation of non used variables
 !! }
 !! else {
 !!  Mnsp=nsp; Mnpr=npr; Mnopr=nOthPred; if (Mnopr<1) Mnopr=1; Mly=lyData; Mlq=lq; Mmax_a=max_a; Myq=yq;
 // !!  cout<<"Mnopr: "<<Mnopr<<endl;
 !! }

 //********************************************************************************************* 
 // M1, Residual natural mortalities
 !! if (multi>=1) ad_comm::change_datafile_name("natmor1.in");
 !! if (multi==0) Mindex=-1; else Mindex=max_a;
 init_4darray M1_input(first_VPA,Mnsp,fyData,Mly,fq,Mlq,fa,Mindex)
 init_number check12;
 !! if (multi>=1) checkSum(check12,"natmor1.in"); 

 !! if (test_output==3 && multi>0) cout<<"M1 from file natmor1.in:"<<endl;
 3darray M1(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la)
 !! if (multi>0) MOVE(M1_input,M1,first_VPA,nsp,lyData);
 
  //*********************************************************************************************

 !! if (test_output==3 && multi>0) cout<<"starts reading file: pred_prey_size_range_param.in"<<endl;
 !! if (multi>=1 && read_size_a_b==1) ad_comm::change_datafile_name("pred_prey_size_range_param.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 
 !! if (multi==0) Mindex=-1; else Mindex=nsp;
 init_3darray size_range_a_b(1,4,1,Mnpr,first_VPA,Mindex)
 !! if (multi>=1 && read_size_a_b==1 && test_output==3) {
 !!  cout<<"predator prey size range quantiles from pred_prey_size_range_param.in"<<setprecision(4)<<setw(11)<<setfixed()<<endl;
 !!  cout<<size_range_a_b(1)<<endl<<endl;
 !!  cout<<size_range_a_b(2)<<endl<<endl;
 !!  cout<<size_range_a_b(3)<<endl<<endl;
 !!  cout<<size_range_a_b(4)<<endl<<endl;
 !!  cout<<setprecision(3);
 !! }
 init_number check12b;
 !! if (multi>=1  && read_size_a_b==1 ) checkSum(check12b,"pred_prey_size_range_param.in"); 

 //*********************************************************************************************
 // overlap, Predator prey overlap by year and quarter
 !! if (test_output==3 && multi>0 && use_overlap==1) cout<<"starts reading file: overlap.in"<<endl;
 !! if (multi>=1 && use_overlap==1) ad_comm::change_datafile_name("overlap.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 !! if (multi==0) Mindex=-1; else Mindex=nsp;
 init_5darray overlap(1,no_areas,1,Mnpr,fyData,Mly,fq,Mlq,0,Mindex)
 !! if (test_output==3 && multi>0 && use_overlap==1) cout<<"Overlap from file overlap.in:"<<endl<<overlap<<endl;
 // fill in 1.0 into overlap array if overlap should not be used 
 !!  if (use_overlap==0 && multi>0) for(d=1;d<=no_areas;d++) for(s=1;s<=Mnpr;s++) for(y=fyData;y<=Mly;y++) overlap(d,s,y)=1.0;

 !! if (test_output==3 && multi>0 && use_overlap==2) cout<<"starts reading file: overlap_forecast.in"<<endl;
 !! if (multi>=1 && use_overlap==2) ad_comm::change_datafile_name("overlap_forcast.in");
 !! if (multi==0) Mindex=-1; else Mindex=nsp;
 init_5darray overlap_forecast(1,no_areas,1,Mnpr,lyModel+1,lpy,fq,Mlq,0,Mindex)
 !! if (test_output==3 && multi>0 && use_overlap==2) cout<<"Overlap from file overlap_forcast.in:"<<endl<<overlap<<endl;
 // fill in 1.0 into overlap array if overlap should not be used 
 !!  if (use_overlap !=2 && multi>0) for(d=1;d<=no_areas;d++) for(s=1;s<=Mnpr;s++) for(y=lyModel+1;y<=lpy;y++) overlap_forecast(d,s,y)=1.0;

 //********************************************************************************************* 
 // season_overlap_input, Predator prey overlap by quarter
 !! if (test_output==3 && multi>0) cout<<"starts reading file: season_overlap.in"<<endl;
 !! if (multi>=1) ad_comm::change_datafile_name("season_overlap.in");
 !! if (multi==0) Mindex=-1; else Mindex=nsp;
 init_4darray season_overlap_input(1,no_areas,1,Mnpr,fq,Mlq,0,Mindex)
 init_number check13;
 !! if (test_output==3 && multi>0) cout<<"Overlap from file season_overlap.in:"<<endl<<season_overlap_input<<endl;
 !! if (multi>0) checkSum(check13,"season_overlap.in");

 int no_season_overlap_to_estimate       //number of overlap factors to be estimated
 !! if (multi==0) Mindex=-1; else Mindex=nsp;
 4darray season_overlap_index(1,no_areas,1,Mnpr,fq,Mlq,0,Mindex) ;
 int p
 !! no_season_overlap_to_estimate=0;
 !! if (multi>=1) for (d=1;d<=no_areas;d++) for (p=1;p<=Mnpr;p++) for (q=fq;q<=Mlq; q++)  for (s=0;s<=Mnsp;s++){
 !!   if (season_overlap_input(d,p,q,s)<0) {
 !!     if (phase_season_overlap<=0) {
 !!       cout <<endl<<"You cannot have negative season overlap if overlap is not estimated ( phase.season.overlap<1 ) "<<endl;
 !!       exit(9); 
 !!      }
 !!     if (abs(int(season_overlap_input(d,p,q,s)))>no_season_overlap_to_estimate) no_season_overlap_to_estimate=int(-season_overlap_input(d,p,q,s));
 !!     season_overlap_index(d,p,q,s)=-season_overlap_input(d,p,q,s);
 !!   }
 !!   else { season_overlap_index(d,p,q,s)=0;}      
 !! }
 //!! if (no_season_overlap_to_estimate==0) no_season_overlap_to_estimate=1;        // for construction of data structure later on
 !! else
 !! if (mceval==0 && multi>=1) {
 !!   cout <<"no_season_overlap_to_estimate:  "<<no_season_overlap_to_estimate<<endl;
 !!   cout <<"season_overlap_input:"<<endl<<season_overlap_input<<endl;
 !!   cout <<"season_overlap_index:"<<endl<<season_overlap_index<<endl; 
 !!  }


//*********************************************************************************************
 // presence of a predator in a given area
 !! if (multi>=1 && no_areas>1) ad_comm::change_datafile_name("predator_area_presence.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 init_imatrix pred_area_present(1,npr,1,no_areas)
 !! if (multi>=1 && no_areas>1 &&  test_output==2) cout<<"predator_area_presence:\n"<<pred_area_present<<endl;
 init_number check14;
 !! if (multi>=1 && no_areas>1) checkSum(check14,"predator_area_presence.in"); 

 //*********************************************************************************************

 !! if (multi>=1 && consum_op>=1) { ad_comm::change_datafile_name("consum_ab.in"); Mindex=lq;}
 !! else {ad_comm::change_datafile_name("just_one.in"); Mindex=-1; }
 init_4darray consum_ab(1,no_areas,1,Mnpr,fq,Mindex,1,2)
 init_number check15;

 !! if (multi>=1 && consum_op>=1) checkSum(check15,"consum_ab.in"); 
 !! if (test_output==3 && multi>0 && consum_op>0) cout<<"Consumption parameters from file consum_ab.in:"<<endl<<setprecision(3)<<consum_ab<<endl;


 //********************************************************************************************* 
 // Consumption weight (Ration) per individual,
  !! if (multi>=1 && consum_op==0) { ad_comm::change_datafile_name("consum.in"); Mindex=max_a;}
 !! else {ad_comm::change_datafile_name("just_one.in"); Mindex=-1; }
 init_5darray consum_input(1,no_areas,1,Mnpr,fyData,Mly,fq,Mlq,fa,Mindex)
 init_number check16;
 !! if (multi>=1 && consum_op==0) checkSum(check16,"consum.in"); 
 !! if (test_output==3 && multi>0 && consum_op==0) cout<<"Consum (ration) from file consum.in:"<<endl<<consum_input<<endl;
 3darray consum(1,def_yqd,1,yqdPred,yqdsPred_fa,yqdsPred_la)
 !! if (multi>0 && consum_op==0) MOVEd(consum_input,consum,1,npr);
 
 //********************************************************************************************* 
  // Proportion of the stock number by area,
 
 !! if (no_areas>1) ad_comm::change_datafile_name("stock_distribution.in");
 !! if (no_areas==1 || multi<=1) ad_comm::change_datafile_name("just_one.in");
 !! if (no_areas==1) Mindex=-1; else Mindex=max_a;
 init_5darray N_dist_input(1,no_areas,1,Mnsp,fyData,lyData,fq,Mlq,fa,Mindex)
 !! if (test_output==3 && no_areas>1) cout<<"Stock distribution from file Stock_distribution.in:"<<endl<<setfixed()<<setprecision(3)<<N_dist_input<<endl;
 init_number check17;
 !! if (multi>=1 && no_areas>1) checkSum(check17,"stock_distribution.in"); 

 number sumP
 LOCAL_CALCS
   // make proportion to sum up to 1.0
   if (multi>=1  && no_areas>1) {
      for (s=first_VPA;s<=nsp;s++) for (y=fyData;y<=lyData;y++) for (q=1;q<=lq;q++)for (a=faq(q);a<=la(s);a++)  {
       sumP=0.0;
       for (d=1;d<=no_areas;d++) sumP=sumP+N_dist_input(d,s,y,q,a);
       for (d=1;d<=no_areas;d++) N_dist_input(d,s,y,q,a)=N_dist_input(d,s,y,q,a)/sumP;
     }
     if (test_output==3 && multi>0 && no_areas>1) cout<<"Stock distribution after normalization:"<<endl<<setfixed()<<setprecision(3)<<N_dist_input<<endl;
   }

 END_CALCS
 3darray N_dist(1,def_yqd,1,yqdAll,yqdsAll_fa,yqdsAll_la)
 !! if (no_areas>1 && multi>=1) MOVEd(N_dist_input,N_dist,1,nsp);

 //********************************************************************************************* 
 // Mean length of a species (predator) at age in the sea, all years
 !! change_input_multi("lsea.in");
 !! if (multi==0) Mindex=-1; else Mindex=max_a;
 init_5darray lsea_input(1,no_areas,1,Mnsp,fyData,lyData,fq,Mlq,fa,Mindex)
 !! if (test_output==3 && multi>0) cout<<"Mean length in the sea from file lsea.in:"<<endl<<lsea_input<<endl;
 init_number check18;
 !! if (multi>=1 && no_areas>1) checkSum(check18,"Mean length in the sea from file lsea.in"); 

 3darray lsea(1,def_yqd,1,yqdAll,yqdsAll_fa,yqdsAll_la)
 !! if (multi>0) MOVEd(lsea_input,lsea,1,nsp);
 
 !! if (multi==0) Mindex=-1; else Mindex=max_a;

 3darray        size_sea(1,def_yqd,1,yqdAll,yqdsAll_fa,yqdsAll_la)  //length or weight of species at age in the sea
 3darray size_sea_prey_w(1,def_yqd,first_VPA,yqdVPA,yqdsVPA_fa,yqdsVPA_la) //weight of species at age in the sea
  //*********************************************************************************************
 // Data for splitting numbers per ages on length groups. Used in stomach contents calc. Length at Age Key
 !! if (test_output==3 && multi>0) cout<<"starts reading file: alk_stom.in"<<endl;

  !!  if (multi>=1) ad_comm::change_datafile_name("alk_stom.in");
  init_int n_ALK_y                           // no. of years
  !! if (test_output==3 && multi>0) cout<<"alk file a):"<<endl<<n_ALK_y<<endl;

  init_int n_ALK_yq                          // no. of year-quarter combinations
  !! if (test_output==3 && multi>0) cout<<"alk file b):"<<endl<<n_ALK_yq<<endl;

  init_int n_ALK_yqd                         // no. of year-quarter-area combinations
  !! if (test_output==3 && multi>0) cout<<"alk file c):"<<endl<<n_ALK_yqd<<endl;

  init_int n_ALK_yqds                         // no. of year-quarter-area-species combinations
  !! if (test_output==3 && multi>0) cout<<"alk file d):"<<endl<<n_ALK_yqds<<endl;

  init_int n_ALK_yqdsa                        // no. of year-quarter-area-species-age combinations
  !! if (test_output==3 && multi>0) cout<<"alk file e):"<<endl<<n_ALK_yqdsa<<endl;

  !! if (multi==0) Mindex=-1; else Mindex=3;
  init_imatrix ALK_y(1,n_ALK_y,1,Mindex);         // year name, and first and last year-quarter index
  !! if (test_output==3 && multi>0) cout<<"alk file f):"<<endl<<ALK_y<<endl;

  init_imatrix ALK_yq(1,n_ALK_yq,1,Mindex);       // quarter name, and first and last year-quarter-area index
  !! if (test_output==3 && multi>0) cout<<"alk file g):"<<endl<<ALK_yq<<endl;

  init_imatrix ALK_yqd(1,n_ALK_yqd,1,Mindex);       // area name, and first and last year-quarter-area-species index
  !! if (test_output==3 && multi>0) cout<<"alk file h):"<<endl<<ALK_yq<<endl;

  init_imatrix ALK_yqds(1,n_ALK_yqds,1,Mindex);     // species name, and first and last year-quarter-area-species-age index
  !! if (test_output==3 && multi>0) cout<<"alk file i):"<<endl<<ALK_yqds<<endl;

  init_imatrix ALK_yqdsa(1,n_ALK_yqdsa,1,Mindex);   // age name in year-quarter-species-age-length combination and

  

  !! if (test_output==3 && multi>0) cout<<"alk file j):"<<endl<<ALK_yqdsa<<endl;

  ivector f_ALK_yqdsal(1,n_ALK_yqdsa);         // first length group  
  ivector l_ALK_yqdsal(1,n_ALK_yqdsa);         // last length group  

  !! int il;
  !! if (multi>=1) for (il=1;il<=n_ALK_yqdsa;il++) {
  !!  f_ALK_yqdsal(il)=ALK_yqdsa(il,2);
  !!  l_ALK_yqdsal(il)=ALK_yqdsa(il,3);
  !! } else {f_ALK_yqdsal(1)=1; l_ALK_yqdsal(1)=1; }

 init_matrix ALK(1,n_ALK_yqdsa,f_ALK_yqdsal,l_ALK_yqdsal); //Length Age Key (proportion by length goup for each age group)
  !! if (test_output==3 && multi>0) {
  !!   cout<<"alk file in):"<<endl;
  !!   for (il=1;il<=n_ALK_yqdsa;il++) {
  !!     cout<<ALK(il)<<" SUM:"<<sum(ALK(il))<<endl;
  !!  }}
  
 init_matrix ALK_length(1,n_ALK_yqdsa,f_ALK_yqdsal,l_ALK_yqdsal); // Length at each length group
  !! if (test_output==3 && multi>0) cout<<"lak length file):"<<endl<<ALK_length<<endl;

 init_number check19;
 !! if (multi>=1 && no_areas>1) checkSum(check19,"alk_stom.in"); 

 //********************************************************************************************* 
 // Data for splitting numbers per ages on length groups. Used for calc of M2 Length at Age Key
 !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"####################\nstarts reading file: alk_all.in"<<endl;

  !!  if (multi>=1 && simple_ALK==1) ad_comm::change_datafile_name("alk_all.in");
  !! else  ad_comm::change_datafile_name("just_one.in");
  
  init_int n_ALKS_y                           // no. of years
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file a):"<<endl<<n_ALKS_y<<endl;

   init_int n_ALKS_yq                          // no. of year-quarter combinations
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file b):"<<endl<<n_ALKS_yq<<endl;

   init_int n_ALKS_yqd                          // no. of year-quarter-area combinations(same as year-quarter-division. d for division)
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file c):"<<endl<<n_ALKS_yqd<<endl;
  
  init_int n_ALKS_yqds                         // no. of year-quarter-division-species combinations
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file d):"<<endl<<n_ALKS_yqds<<endl;

  init_int n_ALKS_yqdsa                        // no. of year-quarter-division-species-age combinations
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file e):"<<endl<<n_ALKS_yqdsa<<endl;

  !! if (multi==0) Mindex=-1; else Mindex=3;
  init_imatrix ALKS_y(1,n_ALKS_y,1,Mindex);         // year name, and first and last year-quarter index
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file f):"<<endl<<ALKS_y<<endl;

  init_imatrix ALKS_yq(1,n_ALKS_yq,1,Mindex);       // quarter name, and first and last year-quarter-division index
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file g):"<<endl<<ALKS_yq<<endl;

  init_imatrix ALKS_yqd(1,n_ALKS_yqd,1,Mindex);     // species name, and first and last year-quarter-division-species-age index
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file h):"<<endl<<ALKS_yqd<<endl;

  init_imatrix ALKS_yqds(1,n_ALKS_yqds,1,Mindex);     // species name, and first and last year-quarter-division-species-age index
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file i):"<<endl<<ALKS_yqds<<endl;

  init_imatrix ALKS_yqdsa(1,n_ALKS_yqdsa,1,Mindex);   // age name in year-quarter-species-age-length combination and


  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file j):"<<endl<<ALKS_yqdsa<<endl;

  ivector f_ALKS_yqdsal(1,n_ALKS_yqdsa);         // first length group  
  ivector l_ALKS_yqdsal(1,n_ALKS_yqdsa);         // last length group  
 

  !! if (multi>=1) for (il=1;il<=n_ALKS_yqdsa;il++) {
  !!  f_ALKS_yqdsal(il)=ALKS_yqdsa(il,2);
  !!  l_ALKS_yqdsal(il)=ALKS_yqdsa(il,3);
  !! } else {f_ALKS_yqdsal(1)=1; l_ALKS_yqdsal(1)=1; }

 init_matrix ALKS_input(1,n_ALKS_yqdsa,f_ALKS_yqdsal,l_ALKS_yqdsal); //Length Age Key (proportion by length goup for each age group)
  !! if (test_output==3 && multi>0 && simple_ALK==1) cout<<"alk all file in):"<<endl<<ALKS_input<<endl;
 
 init_matrix ALKS_length(1,n_ALKS_yqdsa,f_ALKS_yqdsal,l_ALKS_yqdsal); // mean length at each length group
  !! if (test_output==3 && multi>0 && simple_ALK==1) {
  !!   cout<<"alk all length file):"<<endl;
  !!   for (il=1;il<=n_ALKS_yqdsa;il++) {
  !!     cout<<ALKS_input(il)<<" SUM:"<<sum(ALKS_input(il))<<endl;
  !!     if (sum(ALKS_input(il))< 0.9 || sum(ALKS_input(il)) >1.1) cout<<"Warning: The sum of proportion of length groups is differnt from 1:"<< endl;
  !!  }}
 
 init_number check20;
 !! if (multi>=1 && simple_ALK==1) checkSum(check20,"alk_all.in"); 

 //*****************************************************************  
 // find min and max length index for each combination of species and age

 // structures used for N_l_bar,M2_l, Z_l and avail_food_l 
 imatrix minlAll(1,nsp,fa,la)
 imatrix maxlAll(1,nsp,fa,la)
 imatrix minlVPA(first_VPA,nsp,fa,la_VPA)
 imatrix maxlVPA(first_VPA,nsp,fa,la_VPA)
 imatrix minlPred(1,npr,fa,la_pred)
 imatrix maxlPred(1,npr,fa,la_pred)

 ivector minmaxAll_fa(1,nsp)       // first age on matrix format
 ivector minmaxAll_la(1,nsp)       // last age on matrix format
 ivector minmaxVPA_fa(first_VPA,nsp)       // first age on matrix format
 ivector minmaxVPA_la(first_VPA,nsp)       // last age on matrix format
 ivector minmaxPred_fa(1,npr)       // first age on matrix format
 ivector minmaxPred_la(1,npr)       // last age on matrix format

 LOCAL_CALCS 
  minmaxAll_fa=fa;
  minmaxVPA_fa=fa;
  minmaxPred_fa=fa;
  for (s=1;s<=nsp;s++) minmaxAll_la(s)=la(s);
  for (s=first_VPA;s<=nsp;s++) minmaxVPA_la(s)=la(s);
  for (s=1;s<=npr;s++) minmaxPred_la(s)=la(s);

  if (multi>=1 && simple_ALK==1) {
    minlAll=100;
    maxlAll=0;
    minlVPA=100;
    maxlVPA=0;
    minlPred=100;
    maxlPred=0;
  } else {
    minlAll=0;
    maxlAll=0;
    minlVPA=0;
    maxlVPA=0;
    minlPred=0;
    maxlPred=0;
  }   
    
  int Ly, Lq, Ld, Ls, La, Ll, low, high;
  
  if (multi>=1 && simple_ALK==1) {
    for (Ly=1;Ly<=n_ALKS_y;Ly++) { 
     //y=ALKS_y(Ly,1); cout<<"y:"<<y<<endl;
     for (Lq=ALKS_y(Ly,2);Lq<=ALKS_y(Ly,3);Lq++) { 
       //q=ALKS_yq(Lq,1); cout<<"q:"<<q<<endl;
       for (Ld=ALKS_yq(Lq,2);Ld<=ALKS_yq(Lq,3);Ld++) { 
         //d=ALKS_yqd(Ld,1); cout<<"d:"<<d<<endl; 
         for (Ls=ALKS_yqd(Ld,2);Ls<=ALKS_yqd(Ld,3);Ls++) { 
           s=ALKS_yqds(Ls,1);
           //cout<<"s:"<<s<<endl; 
           for (La=ALKS_yqds(Ls,2);La<=ALKS_yqds(Ls,3);La++) { 
             a=ALKS_yqdsa(La,1);
             // cout<<"a:"<<a<<" Length: "; 
             low=ALKS_yqdsa(La,2);
             high=ALKS_yqdsa(La,3); 
             if (low<minlAll(s,a)) minlAll(s,a)=low;
             if (high>maxlAll(s,a)) maxlAll(s,a)=high;
             if (s>=first_VPA) {
               if (low<minlVPA(s,a)) minlVPA(s,a)=low;
               if (high>maxlVPA(s,a)) maxlVPA(s,a)=high;
             }
             if (s<=npr) {
               if (low<minlPred(s,a)) minlPred(s,a)=low;
               if (high>maxlPred(s,a)) maxlPred(s,a)=high;
             }
           }    
             //for (Ll=ALKS_yqdsa(La,2);Ll<=ALKS_yqdsa(La,3);Ll++) {
             //  cout<<ALKS_length(La,Ll)<<" ";
             //}
             //cout<<endl;  
    }}}}
  
    
    // set not used entries to 0
    for (s=1;s<=nsp;s++) {
      for (a=fa;a<=la(s);a++) {
        if (minlAll(s,a)==100) minlAll(s,a)=0;
        if (s>=first_VPA) if (minlVPA(s,a)==100) minlVPA(s,a)=0;
        if (s<=npr) if (minlPred(s,a)==100) minlPred(s,a)=0;
      }
     }   
    //cout <<"minlAll:"<<endl<<minlAll<<endl<<"maxlAll:"<<endl<<maxlAll<<endl;
    //cout <<"minlVPA:"<<endl<<minlVPA<<endl<<"maxlVPA:"<<endl<<maxlVPA<<endl;
    //cout <<"minlPred:"<<endl<<minlPred<<endl<<"maxlPred:"<<endl<<maxlPred<<endl;
  }  // end simple_ALK==1
 END_CALCS

 imatrix index_Lq(fyData,lyData,fq,lq)      // pointer to the first species for a given year and quarter

 !! if (multi==0) Mindex=-1; else Mindex=max_a;
 
 3darray index_minl(1,def_yqd,1,yqdAll,yqdsAll_fa,yqdsAll_la)  // first size class in use
 3darray index_maxl(1,def_yqd,1,yqdAll,yqdsAll_fa,yqdsAll_la) // last size class in use

  int minl
  int maxl
  
  !! minl=min(f_ALKS_yqdsal);   // minimum and maximum size class   RETTES: kan laves til vector (species) for at spare plads i size_l_sea variable
  !! maxl=max(l_ALKS_yqdsal);   
  !! if (multi==0) maxl=-1;

  4darray size_l_sea(1,def_yqd,1,nsp,fa,max_a,minl,maxl)    //length or weight of species at age in the sea BURDE V鼇E "RAGGED"
  4darray consum_l(1,def_yqd,1,npr,fa,max_a,minl,maxl)  //Consumption (food ration) per length class
 
 //********************************************************************************************* 
 // Data for relative stomach contents weight by length groups
 !! if (test_output==3 && multi>0) cout<<"####################\nstarts reading file: stom_struc_at_length.in"<<endl;
 !! if (multi>=1) ad_comm::change_datafile_name("stom_struc_at_length.in");
 init_int n_stl_y                           // no. of years
 !! if (test_output==3 && multi>0) cout<<"stomach structure file a):"<<endl<<n_stl_y<<endl;
 init_int n_stl_yq                          // no. of year-quarter combinations
 !! if (test_output==3 && multi>0) cout<<"stomach structure file b):"<<endl<<n_stl_yq<<endl;
 init_int n_stl_yqd                         // no. of year-quarter-area combinations
 !! if (n_stl_yq != n_stl_yqd && no_areas==1) {
 !!    cout<<"ERROR: You have chosen 1 area, but have stomach data by area!"<<endl;
 !!    exit(9);
 !! }
 !! if (test_output==3 && multi>0) cout<<"stomach structure file c):"<<endl<<n_stl_yqd<<endl;
 init_int n_stl_yqdp                         // no. of year-quarter-area-predator combinations
 !! if (test_output==3 && multi>0) cout<<"stomach structure file d):"<<endl<<n_stl_yqdp<<endl;
 init_int n_stl_yqdpl                        // no. of year-quarter-area-predator-predator length combinations
 !! if (test_output==3 && multi>0) cout<<"stomach structure file e):"<<endl<<n_stl_yqdpl<<endl;
 init_int n_stl_yqdplp                       // no. of year-quarter-area-predator-predator length-prey combinations
 !! if (test_output==3 && multi>0) cout<<"stomach structure file f):"<<endl<<n_stl_yqdplp<<endl;

 !! if (multi==0 && n_stl_yqdplp!=1) {
 !!   cout<<"some thing is wrong! Try to add more 1 values to the just_one.dat file"<<endl;
 !!   exit(9);
 !! }

 init_imatrix stl_y(1,n_stl_y,1,3);         // year name, and first and last year-quarter index
 !! if (test_output==3 && multi>0) cout<<"stomach structure file g):"<<endl<<stl_y<<endl;
 init_imatrix stl_yq(1,n_stl_yq,1,3);       // quarter name, and first and last year-quarter-area index
 !! if (test_output==3 && multi>0) cout<<"stomach structure file h):"<<endl<<stl_yq<<endl;
 init_imatrix stl_yqd(1,n_stl_yqd,1,3);     // area name, and first and last year-quarter-area-predator index
 !! if (test_output==3 && multi>0) cout<<"stomach structure file i):"<<endl<<stl_yqd<<endl;
  init_imatrix stl_yqdp(1,n_stl_yqdp,1,3);     // predator name, and first and last year-quarter-area-predator-length index
 !! if (test_output==3 && multi>0) cout<<"stomach structure file i):"<<endl<<stl_yqdp<<endl;
  init_imatrix stl_yqdpl(1,n_stl_yqdpl,1,4);   // predator length name and first and last year-quarter-area-predator-length-prey index and predator length class length
 !! if (test_output==3 && multi>0) cout<<"stomach structure file j):"<<endl<<stl_yqdpl<<endl;
  init_imatrix stl_yqdplp(1,n_stl_yqdplp,1,3); // prey name and first and last year-quarter-predator-area-length-prey length
 !! if (test_output==3 && multi>0) cout<<"stomach structure file k):"<<endl<<stl_yqdplp<<endl;

 
 init_number check21;
 !! if (multi>=1) checkSum(check21,"stom_struc_at_length.in"); 


 number init_min_pred_prey_size_ratio
 !! init_min_pred_prey_size_ratio=5000;
 number init_max_pred_prey_size_ratio
 !! init_max_pred_prey_size_ratio=-5000;

 int f_stom_year;
 int l_stom_year;
 !! if (multi>=0) {
 !!   f_stom_year=stl_y(1,1);
 !!   l_stom_year=stl_y(n_stl_y,1);
 !! } else {f_stom_year=0; l_stom_year=0; }

 ivector l_stl_yqdplpl(1,n_stl_yqdpl)         // No of prey-length combinations for a given predator length 
 int no_splpl
 int ip
 !! for (il=1;il<=n_stl_yqdpl;il++) {
 !!   no_splpl=0;
 !!   for (ip=stl_yqdpl(il,2);ip<=stl_yqdpl(il,3);ip++) {
 !!      no_splpl+=stl_yqdplp(ip,3)-stl_yqdplp(ip,2)+1;
 !!   }
 !!   l_stl_yqdplpl(il)=no_splpl;
 !! }
 
 //*********************************************************************************************
 !! if (test_output==3 && multi>0) cout<<"starts reading file: stomcon_at_length.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("stomcon_at_length.in");
 init_matrix stl_stom_input(1,n_stl_yqdpl,1,l_stl_yqdplpl);  // input relative stomach contents weight per lenght group
 !! if (test_output==3 && multi>0) cout<<"Relaltive stomach contents from file stomcon_at_length.in:"<<endl<<stl_stom_input<<endl;
 matrix     stl_stom(1,n_stl_yqdpl,1,l_stl_yqdplpl);  // relative stomach contents weight per lenght group
 matrix log_stl_stom(1,n_stl_yqdpl,1,l_stl_yqdplpl);  // log of relative stomach contents weight per lenght group

 !! if (test_output==3 && multi>0) cout<<"starts reading file: stomtype_at_length.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("stomtype_at_length.in");
 init_imatrix stl_stom_type(1,n_stl_yqdpl,1,l_stl_yqdplpl);  // type of input per lenght group
 !! if (test_output==3 && multi>0) cout<<"Type of stomach content input from file stomtype_at_length.in:"<<endl<<stl_stom_type<<endl;

 imatrix  stl_stom_use_like(1,n_stl_yqdpl,1,l_stl_yqdplpl);   // make use of the particular stomach observation in the likelihood?
 imatrix stl_stom_use_avail(1,n_stl_yqdpl,1,l_stl_yqdplpl);  // make use of the particular stomach observation for calc of available food?

 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: Stomlen_at_length.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("stomlen_at_length.in");
 init_matrix stl_lstom(1,n_stl_yqdpl,1,l_stl_yqdplpl); //mean length per lenght group
 !! if (test_output==3 && multi>0) cout<<"Mean prey length from file Stomlen_at_length.in:"<<endl<<stl_lstom<<endl;

 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: Stomnumber_at_length.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("stomnumber_at_length.in");
 init_matrix stl_nopreystom(1,n_stl_yqdpl,1,l_stl_yqdplpl); // number of preys per lenght group
 !! if (test_output==3 && multi>0) cout<<"Number of preys from file Stomnumber_at_length.in:"<<endl<<stl_nopreystom<<endl;
 !! if (test_output==3 && multi>0) cout<<"Number of prey from file Stomnumber_at_length.in:"<<endl<<stl_nopreystom<<endl;
 
 init_number check22;
 !! if (multi>0) checkSum(check22,"stomcon_at_length.in");


 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: stom_pred_length_at_sizecl.in"<<endl; 
 !!  if (multi>=1) ad_comm::change_datafile_name("stom_pred_length_at_sizecl.in");
 init_vector pred_length(1,n_stl_yqdpl); //mean length per length group     
 !! if (test_output==3 && multi>0) cout<<"Predator mean length from file stom_pred_length_at_sizecl.in:"<<endl<<pred_length<<endl;

 vector pred_size(1,n_stl_yqdpl);   //Predator size (weight or length) for stomach contents data 

 matrix  Prey_number_fac_term(1,n_stl_yqdpl,1,l_stl_yqdplpl);   // fixed term for multinomial likelihood of prey numbers  (the structure does really not fit, I use only the first ll index by species)        // PL
 matrix  Prey_number_like(1,n_stl_yqdpl,1,l_stl_yqdplpl);   // likelihood contribution for multinomial likelihood of prey numbers  (the structure does really not fit, I use only the first ll index by species)        // PL
 !! Prey_number_like=0;
 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: length_weight_relations.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("length_weight_relations.in");
 !! if (multi==0) Mindex=-1; else Mindex=2;
 init_matrix L_W_ab(1,nsp,1,2); //W=a*L**b relations, a and b by species
 !! if (test_output==3 && multi>0) cout<<"Length-weight relations parameters from file length_weight_relations.in:"<<endl<<L_W_ab<<endl;
 init_number check23;
 !! if (multi>0) checkSum(check23,"length_weight_relations.in"); 

 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: stomweight_at_length.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("stomweight_at_length.in");
 init_matrix stl_wstom(1,n_stl_yqdpl,1,l_stl_yqdplpl); //mean weight of prey per lenght group
 !! if (test_output==3 && multi>0) cout<<"Prey mean weight from file stomweight_at_length.in:"<<endl<<stl_wstom<<endl;
 init_number check24;
 !! if (multi>0) checkSum(check24,"stomweight_at_length.in"); 
 matrix prey_size(1,n_stl_yqdpl,1,l_stl_yqdplpl);     //size (length or weight) of prey per lenght class group
 
  //********************************************************************************************* 
  
 !! int mil=100;
 !! int mal=0;
 !! for (i=1;i<=n_stl_yqdpl;i++) { 
 !!   if (stl_yqdpl(i,1) < mil) mil=stl_yqdpl(i,1);  
 !!   if (stl_yqdpl(i,1) > mal) mal=stl_yqdpl(i,1);
 !! }
 !! if (incl_stom_all==1) { 
 !!   if (test_output==3 && multi>0) cout<<"starts reading file: incl_stom.in"<<endl;
 !!   if (multi>=1) ad_comm::change_datafile_name("incl_stom.in");
 !! }
 !! if (incl_stom_all==0){
 !!  ad_comm::change_datafile_name("just_one.in");
 !! }
                                  
 init_5darray incl_stom(1,no_areas,1,Mnpr,fq,lq,mil,mal,1,n_stl_y);  //Include stomach sample observations in likelihood 0=no inlusion, >=1 include data
 
 5darray incl_stom_out(1,no_areas,1,Mnpr,fq,lq,mil,mal,1,n_stl_y);  // copy of above, for output purposes
 !! if (test_output==3 && multi>0 && incl_stom_all==1) cout<<"Include stomach data in likelihood from file Number of hauls from file incl_stom.in:"<<endl<<incl_stom<<endl;
   init_number check25;
 !! if (multi>0) checkSum(check25,"incl_stom.in");                   // you can comment this line out if you want to produce incl_stom.out

 !! if (incl_stom_all==0){
 !!  for (d=1;d<=no_areas;d++) for (p=1;p<=Mnpr;p++) for (q=fq;q<=lq;q++) { incl_stom(d,p,q)=1; incl_stom_out(d,p,q)=0; }
 !! }
  !! if (incl_stom_all==1){
 !!  for (d=1;d<=no_areas;d++) for (p=1;p<=Mnpr;p++) for (q=fq;q<=lq;q++) { incl_stom_out(d,p,q)=incl_stom(d,p,q); }
 !! }
                                   
      

  //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: n_haul_at_length.in"<<endl;
 !!  if (multi>=1) ad_comm::change_datafile_name("n_haul_at_length.in");
 init_matrix stl_N_haul(1,n_stl_yqdpl,1,l_stl_yqdplpl); //Number of hauls per predator length, including copies to fit structure
 !! if (test_output==3 && multi>0) cout<<"Number of hauls from file N_haul_at_length.in:"<<endl<<stl_N_haul<<endl;
 
 //********************************************************************************************* 
 matrix stl_no_samples(1,n_stl_yqdpl,1,l_stl_yqdplpl); //Number no of hauls
 !! if (multi>=1) {
 !!   for (spl=1;spl<=n_stl_yqdpl;spl++) {
 !!     for (splp=1;splp<=l_stl_yqdplpl(spl);splp++) {
 !!       stl_no_samples(spl,splp)=stl_N_haul(spl,splp);           //Number of hauls per predator length, including copies to fit structure
 !!     }
 !!   }
 !!
 
 vector min_no_samples(1,Mnpr);     // minimum number of stomach samples per predator
 vector max_no_samples(1,Mnpr);     // maximum number of stomach samples per predator
 vector max_sumP(1,Mnpr);     // maximum sumpP (alfa0) used in Dirichlet
 int sd;
 LOCAL_CALCS
  for (s=1;s<=Mnpr;s++)  {min_no_samples(s)=10000; max_no_samples(s)=0;}
   
    for (sy=1;sy<=n_stl_y;sy++) {
      //cout<<"y: "<<stl_y(sy,1)<<endl; 
      for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
        //cout<<"q: "<< stl_yq(sy,1)<<endl;
        for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
           d=stl_yqd(sd,1);
          //cout<<"area: "<< stl_yqd(sd,1)<<endl;
          for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
             s=stl_yqdp(sp,1);
             //cout<<"predator: "<<s<<"  ";
             for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
                //cout<<"predator length:"<<stl_yqdpl(spl,1)<<endl;
                int ll=0; 
                //cout<<incl_stom_out(d,stl_yqdp(sp,1),stl_yq(sq,1),stl_yqdpl(spl,1),sy)<<" "<< stl_no_samples(spl,1)<<"  "<<incl_stom_out(d,stl_yqdp(sp,1),stl_yq(sq,1),stl_yqdpl(spl,1),sy)-stl_no_samples(spl,1) <<endl;
                if ((incl_stom_out(d,stl_yqdp(sp,1),stl_yq(sq,1),stl_yqdpl(spl,1),sy)==0) && (stl_no_samples(spl,1)!=0) && (incl_stom_all==1)) {
                   cout<<"WARNING, something might be wrong: inclusion of stomachs (file incl_stom.in) has a zero value where there exist stomachs)"<<endl<<
                         "    predator:"<<s<<" year:"<<stl_y(sy,1)<<" q:"<<stl_yq(sy,1)<<"  size:"<<stl_yqdpl(spl,1)<<"  no of stomachs:"<< stl_no_samples(spl,1)<<endl;
                }
                if (stl_lstom(spl,1) != 9999) {
                   cout<<endl<<"WARNING: something might be wrong with the data structure of stomach data. Mean length of other food is different from 9999"<<endl;
                   cout<<"    Area:"<<d<<" predator:"<<s<<" year:"<<stl_y(sy,1)<<" q:"<<stl_yq(sy,1)<<"  size:"<<stl_yqdpl(spl,1)<<endl;
                }
                incl_stom_out(d,stl_yqdp(sp,1),stl_yq(sq,1),stl_yqdpl(spl,1),sy)= stl_no_samples(spl,1);
                for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
                  //cout<<" prey:"<< stl_yqdplp(splp,1)<<" length: " ;
                  for (l=stl_yqdplp(splp,2);l<=stl_yqdplp(splp,3);l++) {
                    //cout<< l<<" ";
                    ll++;
                    
                    if (stomach_variance==3) {
                      if (incl_stom(d,stl_yqdp(sp,1),stl_yq(sq,1),stl_yqdpl(spl,1),sy) >0) {
                         if (stl_no_samples(spl,ll)<min_no_samples(s)) min_no_samples(s)=stl_no_samples(spl,ll);
                         if (stl_no_samples(spl,ll)>max_no_samples(s)) max_no_samples(s)=stl_no_samples(spl,ll);
                         if (max_stom_sampl(s)< stl_no_samples(spl,ll))  stl_no_samples(spl,ll)=max_stom_sampl(s);
                       }
                    }
                    //cout<<" "<<stl_no_samples(spl,ll);
                  }
                  //cout<<endl;
                }
             }
   }}}}
   //cout<<"min number of stomach samples: "<<min_no_samples<<endl;
   //cout<<"max number of stomach samples: "<<max_no_samples<<endl;
  }
 END_CALCS
 

  !! if (mceval==0 && multi>=1) {
  !! ofstream res("incl_stom.out",ios::out);
  !!  res<<"# file incl_stom.out "<<endl<<"# A value >=1 indicate that the stomach sample is used in SMS. Default number is no. of hauls (a number > 0)"<<endl;
  !! for(d=1;d<=no_areas;d++) {
  !!  if (no_areas>1)  res <<"##############"<<endl<<"# area: "<<d<<"  "<<area_names[d]<<endl;
  !!  else res <<"##############"<<endl<<endl;
  !!  for (p=1;p<=Mnpr;p++) {
  !!   res <<"##############"<<endl<<"# Predator: "<<p<<"  "<<species_names[p]<<endl;
  !!   for (q=fq;q<=lq;q++) {
  !!     res<<"# Quarter: "<< q<<endl;
  !!     res <<"# Year: "<<endl<<"# ";
  !!     for (sy=1;sy<=n_stl_y;sy++) res<<setw(6)<<stl_y(sy,1); 
  !!     res <<endl;  
  !!     for (i=mil;i<=mal;i++) {
  !!      for (j=1;j<=n_stl_y;j++) {
  !!        if (incl_stom(d,p,q,i,j)<0) res<< setw(6)<<-incl_stom_out(d,p,q,i,j);
  !!        else res<< setw(6)<<incl_stom_out(d,p,q,i,j);
  !!      }
  !!      res<<" # Size: "<<i<<endl; 
  !!   }}}}
  !! res.close();
  !!
  !! }

   init_number check26;
 !! if (multi>0) checkSum(check26,"n_haul_at_length.in");

  
 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: other_pred_n.in"<<endl;
 !! if (multi>=1 && nOthPred>0) ad_comm::change_datafile_name("other_pred_n.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 !! if (multi==0) Mindex=-1; else Mindex=max_a;
 init_4darray other_pred_N_input(1,Mnopr,fyData,Mly,fq,Mlq,fa,Mmax_a) 
 !! if (test_output==3 && multi>0 && nOthPred>0) cout<<"Other predator stock size from file other_pred_n.in:"<<endl<<other_pred_N_input<<endl;
 3darray other_pred_N(1,yq,1,v_last_Osp,vOpr_fa,vOpr_la)
 !! if (multi>0) MOVE(other_pred_N_input,other_pred_N,1,nOthPred,lyData);

 //********************************************************************************************* 
 !! if (test_output==3 && multi>0) cout<<"starts reading file: other_food.in"<<endl;
 !! if (multi>=1) ad_comm::change_datafile_name("other_food.in");
 !! if (multi==0) Mindex=0; else Mindex=Mnpr;
 init_matrix AV_other_food(1,no_areas,1,Mnpr)          // available food
 !! if (test_output==3 && multi>0) cout<<"Other food biomass from file other_food.in:"<<endl<<AV_other_food<<endl;
  init_number check1a;
 !! if (multi>=1) checkSum(check1a,"other_food.in"); 

 vector AV_other_food_size(1,npr)          // length (or weight) of predator for constant input available food
 
 vector min_pred_length(1,npr)             // min predator_length
 vector max_pred_length(1,npr)             // max predator_length
 
 matrix min_pred_prey_size_ratio(1,npr,first_VPA,nsp)             // min predator prey size ratio
 matrix max_pred_prey_size_ratio(1,npr,first_VPA,nsp)             // max predator prey size ratio

 vector all_min_pred_prey_size_ratio(1,npr)         // min predator prey size ratio, all preys combined
 vector all_max_pred_prey_size_ratio(1,npr)         // max predator prey size ratio, all preys combined
 vector all_range_pred_prey_size_ratio(1,npr)       // max predator prey size ratio, all preys combined



 !! if (test_output>=1) cout << endl << "Input of multispecies data completed" << endl;

 //********************************************************************************************* 
 3darray pred_prey_comb(1,no_areas,1,npr,first_VPA,nsp)       // overview of predator prey combinations.0=no predation >=1 predation (index in vulerability vector)
 // find actual combinations of predator and preys
 // and the overall maximum prey length
 imatrix max_prey_length(1,no_areas,first_VPA,nsp)             // overall maximum prey length
 imatrix min_prey_length(1,no_areas,first_VPA,nsp)  
 LOCAL_CALCS
  for (d=1;d<=no_areas;d++) pred_prey_comb(d)=0;
  min_prey_length=1000;
  max_prey_length=0;
  if (multi>=1) { 
   for (sy=1;sy<=n_stl_y;sy++) {
    for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
      for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
        d=stl_yqd(sd,1);
        for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
          pred=stl_yqdp(sp,1);
          for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
             for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
               prey=stl_yqdplp(splp,1); 
               //cout<<"y:"<<stl_y(sy,1)<<"q:"<<stl_yq(sy,1)<<" d:"<<d<<" pred:"<<pred<<" pred length:"<<stl_yqdpl(spl,1)<<" prey:"<<prey<<endl;
               if (prey>0) {
                 //cout<<"max_prey_length:"<<max_prey_length(d,prey)<<endl;
                 if (max_prey_length(d,prey)<stl_yqdplp(splp,3)) max_prey_length(d,prey)=stl_yqdplp(splp,3);
                 if (min_prey_length(d,prey)>stl_yqdplp(splp,2)) min_prey_length(d,prey)=stl_yqdplp(splp,2);
                 pred_prey_comb(d,pred,prey)=1;
               }
            }
          }
        }
      }
    }
   }
   //cout << "Min prey length: "<< min_prey_length<< endl;
   //cout << "Max prey length: "<< max_prey_length<< endl;
   for (d=1;d<=no_areas;d++) for (s=first_VPA;s<=nsp;s++) if (min_prey_length(d,s)==1000) min_prey_length(d,s)=0;
   //for (d=1;d<=no_areas;d++) cout<<"d:"<<d<<" pred_prey_comb:"<<endl<<pred_prey_comb(d)<<endl;
  }
 END_CALCS
 
 // ######
 // make structure for N_l_bar_like
 !! y=f_stom_year;
 !! q=fq;
 !! d=1;
 !! CALC_yqd
 int yqd_f_stom;
 !! yqd_f_stom=yqd;
 
 !! y=l_stom_year;
 !! q=lq;
 !! d=no_areas;
 !! CALC_yqd
 int yqd_l_stom
 !! yqd_l_stom=yqd;

 ivector stom_VPA(yqd_f_stom,yqd_l_stom)           // 
 !! stom_VPA=nsp;
 imatrix stom_minl(yqd_f_stom,yqd_l_stom,first_VPA,nsp) 
 imatrix stom_maxl(yqd_f_stom,yqd_l_stom,first_VPA,nsp)       // for used in ragged array for VPA species
 !! for (y=f_stom_year;y<=l_stom_year;y++) for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  {
 !!   CALC_yqd 
 !!   for (s=first_VPA;s<=nsp;s++) stom_minl(yqd,s)=min_prey_length(d,s);  
 !!   for (s=first_VPA;s<=nsp;s++) stom_maxl(yqd,s)=max_prey_length(d,s);
 !! } 
  // ###
 

 // count number of combinations and fill in pred_prey_comb
 int no_of_pred_prey_comb
 !! no_of_pred_prey_comb=0;
 !! for (d=1;d<=no_areas;d++) {
 !!   for (pred=1;pred<=npr;pred++) {
 !!     for (prey=first_VPA;prey<=nsp;prey++) {
 !!       if (pred_prey_comb(d,pred,prey)==1) {
 !!         no_of_pred_prey_comb++;
 !!         pred_prey_comb(d,pred,prey)=no_of_pred_prey_comb;
 !!       }
 !!       else pred_prey_comb(d,pred,prey)=0;
 !!     }
 !!   }
 !! }

 // !! cout <<"no. of Pred and prey species comb: "<< no_of_pred_prey_comb<<endl;
 //!! cout <<"pred_prey_comb(pred,prey):"<<endl<<pred_prey_comb<<endl;

 //********************************************************************************************* 
 !! if (multi==0) {
 !!  phase_vulnera=phase_stl_other_suit_slope=phase_pref_size_ratio=phase_pref_size_ratio_correction=phase_prey_size_adjustment=phase_var_size_ratio=phase_season_overlap=phase_Stom_var=phase_mesh_adjust=-1;
 !!  stom_phase=100;
 !! }
 

 int no_short_term_forecast;
 !! no_short_term_forecast=1;
  
 int MCMC_prediction;        // no of MCMC prediction 
 int MCMC_iteration;         // no of iteration within the same MCMC prediction
 int init_pop;               // counter for initial population in use
 
 !! init_pop=1;
 
 int ndim;


 LOCAL_CALCS
  ndim=0;
  if (mceval==1) {
    uistream uis("sms.psv");
    if (!uis){  
      cerr << " Error trying to open file sms.psv"  << endl;
      exit(1);
    }
    uis >> ndim;
  dvector rec(1,ndim);
  int i=0;
  do {
      uis >> rec;
      if  (uis.eof()) break;
      if (!uis) {
        cerr << " Error trying to read file sms.psv " << i <<endl;
        exit(1);
      }
      i++;
   }
   while (1);
    cout << "There are " << i << " sets of parameters in file sms.psv" << endl;
    ndim=i;
  }
 END_CALCS

 // variables used for predictions 
 3darray    pred_west(1,nsp,fq,lq,fa,max_a);
 3darray    pred_west_old(1,nsp,fq,lq,fa,max_a);
 3darray    pred_weca(first_VPA,nsp,fq,lq,fa,max_a);
 3darray    pred_prop_landed(first_VPA,nsp,fq,lq,fa,max_a);
 3darray    pred_propmat(first_VPA,nsp,fq,lq,fa,max_a);
 3darray    pred_size_sea(1,nsp,fq,lq,fa,max_a);

 4darray              tmp_SSB(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);
 4darray    tmp_SSB_percieved(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,lyModel+1,lpy);
 4darray              tmp_TSB(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);
 4darray           tmp_mean_F(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);
 4darray tmp_mean_F_percieved(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);
 4darray            tmp_yield(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);
 4darray         tmp_eaten_M2(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);
 4darray          tmp_recruit(1,ndim,1,no_MCMC_iterations,first_VPA,nsp,fyModel,lpy);

 int sdReportYear;
 !! sdReportYear=min(30,lyModel-fyModel);        // number of years in SD report for mean F and SSB
  // !! cout <<"sdReportYear"<<sdReportYear<<endl;

  
 // STN, environment
 
 // !! ad_comm::change_datafile_name("environment.in");
 matrix environment(lyModel+1,lyModel+93,1,2);
 // !! cout<<"environment:"<<endl<<setfixed()<<setprecision(3)<<setw(11)<<environment<<endl;


 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

INITIALIZATION_SECTION

  F_y_ini 1.0
  F_q_ini 1.0           //overwritten in PRELIMINARY_CALCS_SECTION
  log_F_a_ini 0.0       //overwritten in PRELIMINARY_CALCS_SECTION
  F_y_spline 1.0
  log_rec 0.0
  log_rec_older 0.0     //overwritten in PRELIMINARY_CALCS_SECTION
  SSB_R_alfa 1.0        //overwritten in BETWEEN_PHASES_SECTION
  SSB_R_beta_ini 0.5       //overwritten in BETWEEN_PHASES_SECTION

  qq_ini 1.0            //overwritten in BETWEEN_PHASES_SECTION
  //qq_efficiency 0.0
  qq_power_ini 1.0
  init_s1 10.0
  // creep 1.0             // technical creep
  //init_L50 100
 
// 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中


PARAMETER_SECTION
  

 // file for attributes like year, species and age to estimated paramerter. 
 // Dessigned to be used together with the sms.std and sms.cor files
 !! ofstream parexp("par_exp.out",ios::out);
 !! parexp<<" par parNo species year quarter area age predator prey fleet"<<endl;

 // Initial value guessed for recruitment all years
 vector log_rec_scale(first_VPA,nsp)  

 //  Recruits first age
 init_bounded_matrix log_rec(first_VPA,nsp,fyModel,lyModel,-15.,10.,phase_log_rec)
 !! if  (phase_log_rec>0) {
 !!   for (s=first_VPA;s<=nsp;s++) for (y=fyModel;y<=lyModel;y++)
 !!    {parNo++; parexp<<"log_rec "<<parNo<<" "<<s<<" "<<y<<" -1 -1 -1 -1 -1 -1"<<endl;}
 !! }

 // "Recruits" first year
  init_bounded_matrix log_rec_older(first_VPA,nsp,fa+1,la_like,-20.,10.,phase_log_rec_older)
 !! if  (phase_log_rec_older>0) {
 !!   for (s=first_VPA;s<=nsp;s++) for (a=fa+1;a<=la_like(s);a++) {
 !!   parNo++; parexp<<"log_rec_older "<<parNo<<" "<<s<<" "<<fyModel<<" "<<fq<<" -1 "<<a<<" -1 -1 -1"<<endl;} 
 !! }

  3darray F_a(first_VPA,nsp,fyModel,lyModel,fa,la_VPA)             //age selection in separable model
  matrix F_y(first_VPA,nsp,fyModel,lyModel)                          //year selection in separable model
  4darray F_q(first_VPA,nsp,1,max_sag,1,max_syg,fq,lq)          //season selection in separable model
  3darray F_q_last_year(first_VPA,nsp,fq,lq,fa,la_VPA)          //season selection in separable model, last year                                                               //used for prediction 
 
 //!! int lly=lyModel;
 !! ivector lly(first_VPA,nsp);
 !! for (s=first_VPA;s<=nsp;s++) if (Nknots(s)==1) {lly(s)=lyModel; Nknots(s)=0;} else lly(s)=fyModel;
 !! int nsp_Fy=first_VPA-1;
 !! for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==0) nsp_Fy++;
 !! i=first_VPA;
 !! for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==0) {F_y_species_index(s)=i; i++;}

 
 // Year effect in Fishing mortality. F_y(s,fyModel)=1 is constant
 // init_bounded_vector_vector F_y_ini(first_VPA,nsp_Fy,fyModel+1,lly,0.01,10.0,phase_F_y_ini)
 init_bounded_matrix F_y_ini(first_VPA,nsp_Fy,fyModel+1,lly,0.01,10.0,phase_F_y_ini)
 !! if (phase_F_y_ini>0 ) {           
 !!   for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==0) for (y=fyModel+1;y<=lly(s);y++) {
 !!    parNo++;
 !!    parexp<<"F_y_ini "<<parNo<<" "<<s<<" "<<y<<" -1 -1 -1 -1 -1 -1"<<endl;
 !!   }
 !! }


 // splines
 init_bounded_matrix F_y_spline(first_VPA,nsp,1,Nknots,0.01,3,phase_F_y_spline)
 
 !! if (phase_F_y_spline>0 ) {
 !!   for (s=first_VPA;s<=nsp;s++) if (Nknots(s)>1) for (i=1;i<=Nknots(s);i++) {
 !!    parNo++;
 !!    parexp<<"F_y_spline "<<parNo<<" "<<s<<" "<<knotsX(s,i)<<" -1 -1 -1 -1 -1 -1"<<endl;
 !!   }
 !! }




 !! ivector seasonal_annual_catches_phase(1,no_sp_sag_syg);      // estimate seasonal effect
 !! seasonal_annual_catches_phase=-1;
  //!! for (s=first_VPA;s<=nsp;s++) {if (seasonal_annual_catches(s)==0) seasonal_annual_catches_phase(s)=1; else seasonal_annual_catches_phase(s)=-1; }
  
 !! i=0;      
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   for (sag=1;sag<=n_catch_season_age_group(s);sag++){
 !!     for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
 !!       i++;
 !!       if (seasonal_annual_catches(s)==0 ) seasonal_annual_catches_phase(i)=phase_F_q_ini;
 !!     }
 !!   }
 !! }
 //!! cout<<"seasonal_annual_catches_phase"<<endl<<seasonal_annual_catches_phase<<endl;
 
 init_bounded_vector_vector F_q_ini(1,no_sp_sag_syg,fcq,lcq,0.0001,3E3,seasonal_annual_catches_phase) 
 !! if (1==2) {
 !!  cout<<"TEST 1:"<<endl;
 !!  for (i=1;i<=no_sp_sag_syg;i++) {
 !!   cout<<"i:"<<i<<" ";
 !!   for (j=fcq(i);j<=lcq(i);j++) { 
 !!    cout<<F_q_ini(i,j)<<" ";
 !!   }
 !!   cout<<endl;
 !!  }
 !! }
 
  
  
  
 !! if (phase_F_q_ini>0) {
 !!  int no_sp_sag_syg2=0;       
 !!  for (s=first_VPA;s<=nsp;s++) {
 !!    for (sag=1;sag<=n_catch_season_age_group(s);sag++){
 !!      for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
 !!       no_sp_sag_syg2++;
 !!       for (q=fcq(no_sp_sag_syg2);q<=lcq(no_sp_sag_syg2);q++) { 
 !!          parNo++;
 !!          parexp<<"F_q_ini "<<parNo<<" "<<s<<" "<<catch_sep_year(s,syg)<<" "<<q<<" -1 "<<catch_season_age(s,sag)<<" -1 -1 -1"<<endl;
 !!       } 
 !!      }
 !!    }
 !!  }
 !! }
  
  // age separability by group  (log values to get values >0, for a non bound 3darray)
  //!! cout<<"first_VPA: "<<first_VPA<<endl<<"nsp:"<<nsp<<endl<<"cfa: "<<cfa<<endl<<"las: "<<las<<endl<<"no_F_y_groups:"<<no_F_y_groups<<endl;
 init_3darray log_F_a_ini(first_VPA,nsp,cfa,las,0,no_F_y_groups,phase_log_F_a_ini)               
 !! if  (phase_log_F_a_ini>0) {
 !!   for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=las(s);a++) for (i=1;i<= n_catch_sep_year_group(s);i++) {
 !!     parNo++;
 !!     parexp<<"log_F_a_ini "<<parNo<<" "<<s<<" "<<catch_sep_year(s,i)<<" -1 -1 "<<a<<" -1 -1 -1"<<endl; 
 !!   }
 !! }
  
 // technical creep factor by age (or common for all ages)  and separability group 

 // !! int phase_creep;
 // !! phase_creep=2;          
 // !! for (s=first_VPA;s<=nsp;s++) {
 // !!   if (use_creep(s)==2) creep_option(s)= las(s);
 // !!   else if (use_creep(s)==1) creep_option(s)=cfa(s);
 // !!   else {creep_option(s)=-1;  phase_creep=-1; }
 // !! }
 //
 // init_3darray creep(first_VPA,nsp,cfa,creep_option,0,no_F_y_groups,phase_creep)               
 // !! if  (phase_log_F_a_ini>0) {
 // !!   for (s=first_VPA;s<=nsp;s++) if (use_creep(s)>0) for (a=cfa(s);a<=creep_option(s);a++) for (yy=1;yy<= n_catch_sep_year_group(s);yy++) {
 // !!     parNo++;
 // !!     parexp<<"creep "<<parNo<<" "<<s<<" "<<catch_sep_year(s,yy)<<" -1 -1 "<<a<<" -1 -1 -1"<<endl; 
 // !!   }
 // !! }                                                        
 
    
 // variance at age for catch at age observations
 3darray catch_s2(first_VPA,nsp,1,seasonal_combined_catch_s2,1,n_catch_s2_group)


 // objective function contributions by survey fleet
 vector fleet_contribution(1,no_sp_fl)
 vector fleet_contribution_mean(1,no_sp_fl)

 // other obj function book-keeping vars
 
 matrix no_obj_obs(1,nsp+1,1,5)  // Number of observations included in objective function contribution (Catch, CPUE, SSB/R, stomach) by species
 matrix obj_func(1,nsp,1,6)    // objective function contribution (Catch, CPUE, SSB/R, stomach, ,stomach length dist penalty) by species

 //catchability by species, fleet and age
 init_bounded_matrix qq_ini(1,no_sp_fl,v_first_fleet_age,v_last_fleet_age_q,1E-12,1E4,phase_qq_ini)
 !! if (phase_qq_ini>0) {
 !!   int no_sp_fl=0;       
 !!   for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){ 
 !!     no_sp_fl++;
 !!     for (a=v_first_fleet_age(no_sp_fl);a<= v_last_fleet_age_q(no_sp_fl);a++) {
 !!        parNo++;
 !!        parexp<<"qq_ini "<<parNo<<" "<<s<<" -1 -1 -1 " <<a<<" -1 -1 "<<f<<endl;
 !!     }
 !!   }
 !! }




  //init_bounded_matrix qq_efficiency(first_VPA,nsp,1,n_fleet,1.0,1.1,phase_qq_power_ini)
  
  
 // catchability, stock size dependent (power model)
 !! if (no_of_power_exponents==0) phase_qq_power_ini=-1;
 init_bounded_vector qq_power_ini(1,no_of_power_exponents,0.1,3.0,phase_qq_power_ini)

 !! if (phase_qq_power_ini>0){
 !!   for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++) for (a=first_fleet_age(s,f);a<=last_fleet_age(s,f);a++) {
 !!     if (qq_power_index(s,f,a)>0) {parNo++; parexp<<"qq_power_ini "<<parNo<<" "<<s<<" -1 -1 -1 " <<a<<" -1 -1 "<<f<<endl;}
 !!   }
 !! } 

 //!! if (no_of_fleet_year_effect==0) phase_year_effect_ini=-1;
 //init_bounded_vector log_year_effect_ini(1,no_of_fleet_year_effect,0.2,3,phase_year_effect_ini)

 //!! if (phase_year_effect_ini>0){
 //!!   for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++) for (y=first_fleet_year(s,f)+1;y<=last_fleet_year(s,f);y++) {
 //!!     if (log_year_effect_index(s,f,y)>0) {parNo++; parexp<<"log_year_effect_ini "<<parNo<<" "<<s<<" "<<y<<" -1 -1 -1 -1 -1 "<<f<<endl;}
 //!!   }
 //!! }

 // catchability variance by species, fleet and age (or more correctly: variance at survey obs)
 matrix qq_s2(1,no_sp_fl,1,v_n_CPUE_s2_group)


 !! if (est_calc_sigma(2)==0) {
 !!   dummy_v_n_CPUE_s2_group=v_n_CPUE_s2_group;
 !!   dummy_no_sp_fl=no_sp_fl;
 !! }
 !! else {
 !!   dummy_v_n_CPUE_s2_group=-1;
 !!   dummy_no_sp_fl=-1;
 !! }

 init_bounded_matrix qq_s2_ini(1,dummy_no_sp_fl,1,dummy_v_n_CPUE_s2_group,CPUE_min_s2_bound,2.0,phase_qq_ini)
 !! if (phase_qq_ini>0 && est_calc_sigma(2)==0) {
 !!   for (s=first_VPA;s<=nsp;s++) for (f=1;f<=n_fleet(s);f++){ 
 !!     for (i=1;i<= n_CPUE_s2_group(s,f);i++) {
 !!        parNo++;
 !!        parexp<<"qq_s2_ini "<<parNo<<" "<<s<<" -1 -1 -1 " <<CPUE_s2_group(s,f,i)<<" -1 -1 "<<f<<endl;
 !!     }
 !!   }
 !! }

 
 //catchability by species, fleet and age
 3darray qq(first_VPA,nsp,1,n_fleet,first_fleet_age,last_fleet_age)                                            

 //catchability,power model, by species, fleet and age
 3darray qq_power(first_VPA,nsp,1,n_fleet,first_fleet_age,last_fleet_age)                                            

 // catchability, yer effect by species, fleet and year
 //3darray log_year_effect(first_VPA,nsp,1,n_fleet,first_fleet_year,last_fleet_year);
 
 //CPUE by species, fleet, year and age
 3darray log_CPUE(1,no_sp_fl,v_first_fleet_year,v_last_fleet_year,v_first_fleet_age,v_last_fleet_age)  
 3darray CPUE_residuals(1,no_sp_fl,v_first_fleet_year,v_last_fleet_year,v_first_fleet_age,v_last_fleet_age)  

 // alfa in  SSB-recruit relation, or geometric mean factor
 init_bounded_vector SSB_R_alfa(first_VPA,nsp,0,1.0E4,phase_SSB_R_alfa)
 !! if (phase_SSB_R_alfa>0)  for (s=first_VPA;s<=nsp;s++)  {parNo++; parexp<<"SSB_R_alfa "<<parNo<<" "<<s<<" -1 -1 -1 -1 -1 -1 -1"<<endl; }
 
 // beta in  SSB-recruit relation
 !! i=0;
 !! for (s=first_VPA;s<=nsp;s++) if (use_beta_SSB_Rec(s)==1) i=i+1;
 init_bounded_vector SSB_R_beta_ini(1,i,1.0E-5,1.0E5,phase_SSB_R_beta) 
 vector SSB_R_beta(first_VPA,nsp)
 !! if (phase_SSB_R_beta>0)  for (s=first_VPA;s<=nsp;s++) if (use_beta_SSB_Rec(s)==1) {parNo++;  parexp<<"SSB_R_beta "<<parNo<<" "<<s<<" -1 -1 -1 -1 -1 -1 -1"<<endl; }
 

 // Temperature dependent factor in Ricker
 !! i=0;
 !! for (s=first_VPA;s<=nsp;s++) if (SSB_Rec_model(s)==51) i=i+1;
 init_vector RecTempVar_ini(1,i,phase_SSB_R_beta)
 vector RecTempVar(first_VPA,nsp)
 !! if (phase_SSB_R_beta>0)  for (s=first_VPA;s<=nsp;s++) if (SSB_Rec_model(s)==51) {parNo++; parexp<<"RecTempVar "<<parNo<<" "<<s<<" -1 -1 -1 -1 -1 -1 -1"<<endl;}

 // STN sprat model,opion 61
 // STN !! if (n_use_opt61_Rec>0) {
 // STN !!   if (alfa_61 ==0) phase_alfa_61=phase_SSB_R_beta; else phase_alfa_61=-1;
 // STN !!   if (beta_61 ==0) phase_beta_61=phase_SSB_R_beta; else phase_beta_61=-1;
 // STN !! } 

 // STN init_bounded_number alfa_61(0,2,phase_alfa_61)
 // STN init_bounded_number beta_61(0,1,phase_beta_61)

  // STN cod model,opion 71
 // STN !! if (n_use_opt71_Rec>0) {
 // STN !!   if (alfa_71 ==0) phase_alfa_71=phase_SSB_R_beta; else phase_alfa_71=-1;
 // STN !!   if (beta_71 ==0) phase_beta_71=phase_SSB_R_beta; else phase_beta_71=-1;
 // STN !! } 
 
 // STN init_bounded_number alfa_71(-5e-5,-0.5e-5,phase_alfa_71)
 // STN init_bounded_number beta_71(-3,0,phase_beta_71)

 

 // variance at age for catch at age observations
 !! if (est_catch_sigma==1) i=nsp; else i=-1; 
 !! j=phase_log_F_a_ini;    // I cant use "phase_log_F_a_ini" directly in the init_bounded .. below ???
 !! xx=min_catch_CV*min_catch_CV;        // same as above???
 init_bounded_matrix_vector catch_s2_ini(first_VPA,i,1,seasonal_combined_catch_s2,1,n_catch_s2_group,xx,2.0,j)
 !! if (phase_log_F_a_ini>0 && i>0)  for (s=first_VPA;s<=nsp;s++)  for (j=1;j<=seasonal_combined_catch_s2(s);j++) for (k=1;k<=n_catch_s2_group(s);k++) 
 !!             {parNo++; parexp<<"catch_s2_ini "<<parNo<<" "<<s<<" -1 "<< j <<" -1 "<< catch_s2_group(s,k)<<" -1 -1 -1"<<endl; }
   
 // variance in  SSB-recruit relation 
 vector SSB_R_s2(first_VPA,nsp)
 !! if (est_calc_sigma(3)==0) i=nsp; else i=-1; 

 init_bounded_vector SSB_R_s2_ini(first_VPA,i,min_SR_s2,2,phase_SSB_R_alfa) 
 !! if (phase_SSB_R_alfa>0 && est_calc_sigma(3)==0)  for (s=first_VPA;s<=nsp;s++) {parNo++; parexp<<"SSB_R_s2_ini "<<parNo<<" "<<s<<" -1 -1 -1 -1 -1 -1 -1"<<endl; }
 
 3darray   C_hat_annual(fyData,lyData,first_VPA,vys2,vVPA_fay,vVPA_lay)   // Expected catch annual
  
 3darray         M2(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la)
 3darray     old_M2(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la)
 3darray          F(1,yq1,1,v_last_sp1,vAll_fa1,vAll_la1)
 3darray          Z(1,yq1,1,v_last_sp1,vAll_fa1,vAll_la1)
 //3darray          N(1,yq2,first_VPA,v_last_sp2,vVPA_fa2,vVPA_la2)    // stock number at the start of the season
 3darray          N(1,yq2,1,v_last_sp2,vAll2_fa2,vAll2_la2)    // stock number at the start of the season
 3darray      N_bar(1,yq1,1,v_last_sp1,vAll_fa1,vAll_la1)   // average N within a season
 3darray N_bar_stom(1,yq1,1,v_last_sp1,vAll_fa1,vAll_la1)   // average N within a season or N in the beginning of a season, depending on input option use_Nbar 
 3darray      C_hat(1,yq,first_VPA,v_last_sp,vVPA_fa,vVPA_la)    // Expected catch seasonal

 //3darray   avail_food(1,yq,1,v_last_Psp,vpr_fa,vpr_la)
 3darray avail_food(1,def_yqd,1,yqdPred,yqdsPred_fa,yqdsPred_la)  // available food
 
 //!! int maxl=max(max_prey_length);    // overall maximum length class
 
 3darray N_l_bar_like(yqd_f_stom,yqd_l_stom,first_VPA,stom_VPA,stom_minl,stom_maxl)  //average N at length within a season, used for calc of likelihood for stomach observations
 
 matrix ALK_adjusted(1,n_ALK_yqdsa,f_ALK_yqdsal,l_ALK_yqdsal)          // selction adjusted ALK for stomach likelihood
 !! ALK_adjusted=ALK;

 matrix ALKS_adjusted(1,n_ALKS_yqdsa,f_ALKS_yqdsal,l_ALKS_yqdsal);    // selction adjusted ALK for stomach M2 calculation
 !! ALKS_adjusted=ALKS_input;

 4darray    part_M2(1,npr,fa,max_a,first_VPA,nsp,fa,max_a)    // partial M2
  
 3darray   other_bio(1,Mnopr,fyModel,lpy,fq,lq)      // Biomass of other predators
 
 matrix    TSB(first_VPA,nsp,fyModel,lpy)           // Total Stock Biomass, first season
 matrix    SSB(first_VPA,nsp,fyModel,lpy)           // Spawning Stock Biomass, first season
 matrix    SSB_percieved(first_VPA,nsp,lyModel+1,lpy)           // Percieved Spawning Stock Biomass, first season
 matrix    CWsum(first_VPA,nsp,fyModel,lpy)         // total catch weight
 matrix    CWsum_hat(first_VPA,nsp,fyModel,lpy)     // expected total catch weight
 matrix    yield(first_VPA,nsp,fyModel,lpy)         // Yield
 matrix    yield_hat(first_VPA,nsp,fyModel,lpy)     // expected Yield

 matrix    eaten_M2(first_VPA,nsp,fyModel,lpy)      // eaten biomass
 matrix    Mean_F(first_VPA,nsp,fyModel,lpy)        // Mean  F (real)
 matrix    Mean_F_percieved(first_VPA,nsp,fyModel,lpy)    // Mean  F (percieved)
 matrix    closure(first_VPA,nsp,lyModel+1,lpy)// open or closure of a fishery, predictions only 
 matrix    constraints(first_VPA,nsp,lyModel+1,lpy)     // Constraints in force on F, yield and SSB
                                               // for each variable the values 0=not applied, 1=lower constarints applied, 2=upper applied
                                               //  code in a 3based system
 // variables for stomachs and predation
 matrix          stl_E_stom(1,n_stl_yqdpl,1,l_stl_yqdplpl);    //Expected relative stomach contents weight per lenght group
 matrix      stl_prey_avail(1,n_stl_yqdpl,1,l_stl_yqdplpl);   //Availeble food by prey
 matrix stl_prey_avail_part(1,n_stl_yqdpl,1,l_stl_yqdplpl);   //Availeble food by prey
 !! stl_prey_avail=0;
 !! stl_prey_avail_part=0;                

 matrix          stl_E_Nstom(1,n_stl_yqdpl,1,l_stl_yqdplpl);    //Expected relative stomach contents number (transformed into probability) per lenght group
 matrix      stl_Nprey_avail(1,n_stl_yqdpl,1,l_stl_yqdplpl);   //Availeble Number of food items by fish prey         // PL
 matrix      stl_NSumprey_avail(1,n_stl_yqdpl,1,l_stl_yqdplpl);   // sum of Availeble Number of food items by fish prey  (the structure does really not fit, I use only the first ll index by species)        // PL

 
 vector     sum_p_Dirichlet(1,n_stl_yqdpl);                 // used for Dirichlet ditribution
 vector      like_Dirichlet(1,n_stl_yqdpl);                 // used for Dirichlet ditribution

 !! if(multi==0) no_of_pred_prey_comb=0;
 init_bounded_vector vulnera(1,no_of_pred_prey_comb,1E-3,1E3,phase_vulnera)   // predation vulnerability
 !! if (phase_vulnera>0) {
 !!   for (d=1;d<=no_areas;d++) {
 !!     for (pred=1;pred<=npr;pred++) {
 !!       for (prey=first_VPA;prey<=nsp;prey++) {
 !!         if (pred_prey_comb(d,pred,prey)>0) {parNo++; parexp<<"vulnera "<<parNo<<" -1 -1 -1 "<<d<<" -1 "<<pred<<" "<<prey<<" -1"<<endl; }
 !!       }
 !!     }
 !!   }
 !! }
 

 //init_bounded_vector vulnera_size(1,no_of_pred_prey_comb,-1,1,phase_vulnera)   // Size dependent vulnerability
 //!! if (phase_vulnera>0) {
 //!!   for (pred=1;pred<=npr;pred++) {
 //!!     for (prey=first_VPA;prey<=nsp;prey++) {
 //!!       if (pred_prey_comb(pred,prey)>0) {parNo++; parexp<<"vulnera_size "<<parNo<<" -1 -1 -1 -1 -1 "<<pred<<" "<<prey<<" -1"<<endl; }
 //!!     }
 //!!   }
 //!! }

 init_bounded_vector   init_stl_other_suit_slope(1,no_size_other_food_suit,-10,10,phase_stl_other_suit_slope)     // Suitability other food, intercept and slope
 vector  stl_other_suit_slope(1,Mnpr)
  
 !! if (phase_stl_other_suit_slope>0) {                      
 !!   for (pred=1;pred<=npr;pred++) if (size_other_food_suit(pred)>0) 
 !!     {parNo++; parexp<<"init_stl_other_suit_slope "<<parNo<<" -1 -1 -1 -1 -1 "<<pred<<" -1 -1"<<endl; }
 !! }
 
 
 // prefered predator:prey size ratio
 !! int n=0;
 !! for (s=1;s<=npr;s++) {if (size_selection(s)>=1 && size_selection(s)!=4) n++;}    //count number of parameters needed
 !! n=max(n,1);  // use at least one parameter to avoid warnings

//!! double spl,spu; //lower and upper value for size preference parameter
 !! dvector spl(1,n);
 !! dvector spu(1,n);
 !! ivector phase_pref_size_ratio2(1,n);
 !! phase_pref_size_ratio2=phase_pref_size_ratio;
 
 !! if (size_select_model==1)      {spl=0.7; spu=5.0; }  //length
 !! else {spl=2.0; spu=8.0; }   //weight

 // special initialization for various size selections
 !! int nn;
 !! nn=0;
 !! for (s=1;s<=Mnpr;s++) {  
 !!   if (size_selection(s)>=1 && size_selection(s)!=4) nn++;
 !!   if (size_selection(s)==3) {  // Gamma
 !!     spl(nn)=1.1; spu(nn)=10.0;
 !!   }
 !!   else if (size_selection(s)==5 ) {  //beta
 !!     spl(nn)=0.1; spu(nn)=6.0;
 !!   }
 !!   else if (size_selection(s)==6) {  //beta, unimodal. Both parameters >=1.0
 !!     spl(nn)=1.0; spu(nn)=6.0;
 !!   }

 !! }
  
  
 !! if(multi==0) n=0;
 init_bounded_number_vector  init_pref_size_ratio(1,n,spl,spu,phase_pref_size_ratio2)
 vector pref_size_ratio(1,npr)
 !!    if (multi>0) for (s=1;s<=npr;s++) if (phase_pref_size_ratio2(n)>0 && size_selection(s)>=1 && size_selection(s)!=4) {  
 !!     parNo++; parexp<<"init_pref_size_ratio "<<parNo<<" -1 -1 -1 -1 -1 "<<s<<" -1 -1"<<endl; 
 !! }

 !! if (multi==0 || phase_prey_size_adjustment<=0 ) nn=first_VPA_prey-1;  // single species mode or option not in use
 !! else nn=nsp-1;
 init_bounded_vector   init_prey_size_adjustment(first_VPA_prey,nn,0.5,4,phase_prey_size_adjustment)
 vector prey_size_adjustment(first_VPA,nsp)
 !!  if (phase_prey_size_adjustment>0) {
 !!   for (prey=first_VPA_prey;prey<=nn;prey++)  
 !!     {parNo++; parexp<<"init_prey_size_adjustment "<<parNo<<" -1 -1 -1 -1 -1 -1 "<<prey<<" -1"<<endl; }
 !! }

  
 // prefered predator:prey size ratio size correction factor
 !!  n=0;  // single species mode
 !! if (multi>0 && phase_pref_size_ratio_correction>-1)  n=npr;

 init_bounded_vector   init_pref_size_ratio_correction(1,n,-5,5,phase_pref_size_ratio_correction)      
 vector pref_size_ratio_correction(1,npr)
 
 !! if (phase_pref_size_ratio_correction>0) {
 !!   for (pred=1;pred<=n;pred++)  
 !!     {parNo++; parexp<<"init_pref_size_ratio_correction "<<parNo<<" -1 -1 -1 -1 -1 "<<pred<<" -1 -1"<<endl; }
 !! }

 
 // variance of prefered predator:prey size ratio
 !! n=0;
 !! for (s=1;s<=Mnpr;s++) { if (size_selection(s)==2) n=n+2; else if (size_selection(s)!=0 && size_selection(s)!=4 ) n++;}   //count number of parameters needed
 !! n=max(n,1);  // use at least one parameter to avoid warnings

 //lower and upper value for size preference parameter variance
 !! dvector vpl(1,n);
 !! dvector vpu(1,n);
 !! ivector phase_var_size_ratio2(1,n);
 !! phase_var_size_ratio2=phase_var_size_ratio;

 !! if (size_select_model==1)      { vpl=0.5; vpu=2.0; }  //length
 !! else {vpl=0.05; vpu=12.0; }   //weight
 
 !! n=0;          // special initialization for beta distribution
 !! for (s=1;s<=Mnpr;s++) { 
 !!   if (size_selection(s)==2) n=n+2; 
 !!   else if (size_selection(s)==1) n++;
 !!   else if (size_selection(s)==3) {   // gamma
 !!     n++;
 !!     vpl(n)=0.1; vpu(n)=10.0;
 !!   }
 !!   else if (size_selection(s)==5 ) {   // beta
 !!     n++;
 !!     vpl(n)=0.1; vpu(n)=6.0;
 !!   }
 !!   else if (size_selection(s)==6 ) {   // beta, unimodal
 !!     n++;
 !!     vpl(n)=1.0; vpu(n)=6.0;
 !!   }

 !! }   
 
 !! if(multi==0) n=0;
 init_bounded_number_vector  var_size_ratio_ini(1,n,vpl,vpu,phase_var_size_ratio2)   
 !! if (phase_var_size_ratio>0) {
 !!   if (multi>=1) for (p=1;p<=Mnpr;p++) if (size_selection(p)>0) {
 !!      parNo++; parexp<<"var_size_ratio_ini "<<parNo<<" -1 -1 -1 -1 -1 "<<p<<" -1 -1"<<endl; 
 !!       if (size_selection(p)==2) {  parNo++; parexp<<"var_size_ratio_ini "<<parNo<<" -1 -1 -1 -1 -1 -1 "<<p<<" -1 -1"<<endl; }
 !!  }
 !!  }
  
 
 vector  var_size_ratio(1,npr)
 4darray     season_overlap(1,no_areas,1,npr,fq,lq,0,nsp)     //predator prey overlap
 init_bounded_vector init_season_overlap(1,no_season_overlap_to_estimate,0.05,20.0,phase_season_overlap);

 !! if (phase_season_overlap>0) {
 !!   if (multi>=1) for (i=1;i<=no_season_overlap_to_estimate;i++)  {
 !!      parNo++; parexp<<"init_season_overlap "<<parNo<<" -1 -1 -1 -1 -1 -1 -1 -1"<<endl; 
 !!    }
 !!  }

 !! if (multi==0) n=1; else n=Mnpr;
//lower and upper value for stomach variance parameter 
 !! dvector Stom_var_l(1,n);
 !! dvector Stom_var_u(1,n);
 vector Stom_var_l_save(1,n);
 vector Stom_var_u_save(1,n);
 !! ivector phase_Stom_var2(1,n);
 !! phase_Stom_var2=phase_Stom_var;  
 //!! cout<<"phase_Stom_var2:"<<phase_Stom_var2<<endl;
 // !! cout<<"n:"<<n<<endl;    
 !! for (s=1;s<=n;s++) { 
 !!  if (stomach_variance==1 || stomach_variance==2) {
 !!   Stom_var_l(s)= 0.0001;
 !!   Stom_var_u(s)= 1000.0;
 !!  }      
 !!  else if (stomach_variance==3) {  // Dirichlet
 !!   Stom_var_l(s)=1.00000001/min_no_samples(s)/Stom_var_fac(s);  // to ensure p >0
 //!!   Stom_var_u(s)= stomMaxSumP(s)*Stom_var_l(s);
 !!   Stom_var_u(s)=(stomMaxSumP(s)+1)/ max_no_samples(s)/Stom_var_fac(s);
 !!   if (Stom_var_u(s)<(1.1*Stom_var_l(s))) {
 !!     Stom_var_u(s)=1.1*Stom_var_l(s)  ;
 !!     cout<<"option Stom_var_Max changed to:"<<Stom_var_u(s)<<" for species="<<s<<", "<<species_names[s]<<endl;
 !!   }
 !!  }
 !!  
 !!  if (StomObsVar(s)==0) {  // no species dependent variance factor to link sampling level and variance
 !!    Stom_var_l(s)=0.9; Stom_var_u(s)=1.1; phase_Stom_var2=-1;
 !!  }
 !! }
 !! // cout<<"Stom_var_l:"<<Stom_var_l<<endl;
 !! // cout<<"Stom_var_u:"<<Stom_var_u<<endl;
 !! for (s=1;s<=n;s++) { 
 !!   Stom_var_l_save(s)= Stom_var_l(s);
 !!   Stom_var_u_save(s)= Stom_var_u(s);
 !! }
 
 !! if (multi==0) n=0;
 init_bounded_number_vector Stom_var(1,n,Stom_var_l,Stom_var_u,phase_Stom_var2);
 !! if (multi>0) for (p=1;p<=n;p++) if (phase_Stom_var2(n)>0){
 !!      parNo++; parexp<<"Stom_var "<<parNo<<" -1 -1 -1 -1 -1 "<<p<<" -1  -1"<<endl; 
 !!  }
 
 matrix stl_N_bar(1,n_stl_yqdpl,1,l_stl_yqdplpl)         //N-bar at length for prey species
 vector stl_avail_food(1,n_stl_yqdpl)                   // available food weightfor a predator-length

 // mesh selection adjustment for stomach ALK
 // Selection = 1 /(1 + exp(s1 - s2*length))
 // s2 = s1/L50
 // L50 = s1/s2
 // L50 is input or a parameter
 // s1 is always a parameter
 //
 vector L50(first_VPA,nsp)  //Adjust Length at Age distribution by a mesh selection function
 !! n=0;
 !! for (s=first_VPA;s<=nsp;s++) if (L50_mesh(s)==0) n++;
 init_bounded_vector init_L50(1,n,40,500,phase_mesh_adjust) 
 
 !! if (multi>0) for (p=1;p<=n;p++) if (phase_mesh_adjust>0){
 !!   parNo++; parexp<<"init_L50 "<<parNo<<" -1 -1 -1 "<<p<<" -1 -1  -1  -1"<<endl; 
 !! }

 vector s1(first_VPA,nsp)   // mesh selection parameter
 !! n=0;
 !! for (s=first_VPA;s<=nsp;s++) if ((L50_mesh(s)>=0) && (multi>=1 )) n++;
  init_bounded_vector init_s1(1,n,1,30,phase_mesh_adjust) 
 !! if (n==0  || multi==0) mesh_size_active=0; else mesh_size_active=1;
 !! if (multi>0) for (p=1;p<=n;p++) if (phase_mesh_adjust>0){
 !!   parNo++; parexp<<"init_s1 "<<parNo<<" -1 -1 -1 "<<p<<" -1 -1  -1  -1"<<endl; 
 !! }

 /////////////////////////////////////////////////////////////////////////////////////////// 
 // optional sdreport  variables ///////////////////////////////////////////////////////////

 
  sdreport_matrix avg_F(first_VPA,nsp,lyModel-sdReportYear,lyModel)
  !! for (s=first_VPA;s<=nsp;s++) for (y=lyModel-sdReportYear;y<=lyModel;y++) {parNo++;  parexp<<"avg_F "<<parNo<<" "<<s<<" "<<y<<" -1 -1 -1 -1 -1 -1"<<endl; }
  //matrix avg_F(first_VPA,nsp,lyModel-sdReportYear,lyModel)
  
  
 sdreport_matrix hist_SSB(first_VPA,nsp,lyModel-sdReportYear,lyModel) 
 !! for (s=first_VPA;s<=nsp;s++) for (y=lyModel-sdReportYear;y<=lyModel;y++) {parNo++;  parexp<<"hist_SSB "<<parNo<<" "<<s<<" "<<y<<" -1 -1 -1 -1 -1 -1"<<endl; }
  //matrix hist_SSB(first_VPA,nsp,lyModel-sdReportYear,lyModel)

 !! int na_sp=0;
 !! for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la_VPA(s);a++) if (!(lq>1 && a==fa && fq!=recq)) na_sp++;

 // terminal population 1. Jan last assessment year 
 vector term_N(1,na_sp)
 //sdreport_vector term_N(1,na_sp)
 //!! for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la_VPA(s);a++) if (!(lq>1 && a==fa)) {parNo++; parexp<<"term_N "<<parNo<<" "<<s<<" "<<lyModel<<" "<<fq<<" -1 "<<a<<"  -1 -1 -1"<<endl; }



 !! na_sp=0;
 !! for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la_VPA(s);a++) na_sp++;

 // terminal population in the beginning og period following the last model year and last model season
 //sdreport_vector term_N_next(1,na_sp)
 // !! for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la_VPA(s);a++) {parNo++;  parexp<<"term_N_next "<<parNo<<" "<<s<<" "<<lyModel+1<<" "<<fq<<" -1 "<<a<<"  -1 -1 -1"<<endl; }
 vector term_N_next(1,na_sp)
   
 // log terminal population in the beginning og period following the last model year and last model season 
 //sdreport_vector term_logN_next(1,na_sp)
 //!! for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la_VPA(s);a++) {parNo++;  parexp<<"term_logN_next "<<parNo<<" "<<s<<" "<<lyModel+1<<" "<<fq<<" -1 "<<a<<"  -1 -1 -1"<<endl; }
 vector term_logN_next(1,na_sp)
 
 // Fishing mortality at age, last assessment year 
 !! int na_spF=0;
 !! for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=la(s);a++) na_spF++;

 //sdreport_vector term_F(1,na_spF)
 //!!  for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=la(s);a++) {parNo++;  parexp<<"term_F "<<parNo<<" "<<s<<" "<<lyModel<<" -1 -1 "<<a<<"  -1 -1 -1"<<endl; }
 vector term_F(1,na_spF)

 //sdreport_matrix exploi_pattern(fq,lq,1,na_spF)
 //!! for (q=fq;q<=lq;q++) for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=la(s);a++) {parNo++;  parexp<<"exploi_pattern "<<parNo<<" "<<s<<" "<<lyModel<<" "<<q<<"  -1 "<<a<<"  -1 -1 -1"<<endl; }
 matrix exploi_pattern(fq,lq,1,na_spF)

 //sdreport_vector log_term_F(1,na_spF)
 // !! for (s=first_VPA;s<=nsp;s++) for (a=cfa(s);a<=la(s);a++) {parNo++;  parexp<<"log_term_F "<<parNo<<" "<<s<<" "<<lyModel<<" -1 -1 "<<a<<"  -1 -1 -1"<<endl; }
 vector log_term_F(1,na_spF)

 // SSB in the first year after the last model year
 //matrix next_SSB(first_VPA,nsp,lyModel+1,lyModel+1);
 sdreport_matrix next_SSB(first_VPA,nsp,lyModel+1,lyModel+1);
 !! for (s=first_VPA;s<=nsp;s++)  for (y=lyModel+1;y<=lyModel+1;y++) {parNo++;  parexp<<"next_SSB "<<parNo<<" "<<s<<" "<<y<<" "<<fq<<" -1 -1 -1 -1 -1"<<endl; }

 matrix short_term_SSB(first_VPA,nsp,1,no_F_multipliers);
 //sdreport_matrix short_term_SSB(first_VPA,nsp,1,no_F_multipliers);
 //!! for (s=first_VPA;s<=nsp;s++)  for (int i=1;i<=no_F_multipliers;i++) {parNo++;  parexp<<"short_term_SSB "<<parNo<<" "<<s<<" "<<y<<" "<<fq<<" "<<i<<" -1 -1 -1 -1"<<endl; }
 !! short_term_SSB=0;
 
 matrix log_short_term_SSB(first_VPA,nsp,1,no_F_multipliers);
 //sdreport_matrix log_short_term_SSB(first_VPA,nsp,1,no_F_multipliers);
 //!! for (s=first_VPA;s<=nsp;s++)  for (int i=1;i<=no_F_multipliers;i++) {parNo++;  parexp<<"log_short_term_SSB "<<parNo<<" "<<s<<" "<<y<<" "<<fq<<" "<<i<<" -1 -1 -1 -1"<<endl; }
 !! log_short_term_SSB=0;
 
 likeprof_number SSB_likeprof;
 !! parNo++; parexp<<"SSB_likeprof "<<parNo<<" "<<first_VPA<<" "<<lyModel+2<<" "<<fq<<" "<<-1<<" -1 -1 -1 -1"<<endl;
 !! SSB_likeprof=0;
 
 //likeprof_number stock_N_likeprof;
 //!! stock_N_likeprof=0;
 //!! parNo++; parexp<<"stock_N_likeprof "<<parNo<<" "<<first_VPA<<" "<<lyModel+1<<" "<<fq<<" "<<-1<<" -1 -1 -1 -1"<<endl;


 matrix short_term_yield(first_VPA,nsp,1,no_F_multipliers);
 matrix short_term_F(first_VPA,nsp,1,no_F_multipliers);
 matrix short_term_SSB0(first_VPA,nsp,1,no_F_multipliers)


 // TSB in the first year after the last model year
 //matrix next_TSB(first_VPA,nsp,lyModel+1,lyModel+1);
 //sdreport_matrix next_TSB(first_VPA,nsp,lyModel+1,lyModel+1);
 //!! for (s=first_VPA;s<=nsp;s++)  for (y=lyModel+1;y<=lyModel+1;y++) {parNo++;  parexp<<"next_TSB "<<parNo<<" "<<s<<" "<<y<<" "<<fq<<" -1 -1 -1 -1 -1"<<endl; }

 //matrix M2_sd0(1,nprey,lyModel-sdReportYear,lyModel)
 sdreport_matrix M2_sd0(1,nprey,lyModel-sdReportYear,lyModel)
 !! for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) for (y=lyModel-sdReportYear;y<=lyModel;y++) {parNo++;  parexp<<"M2_sd0 "<<parNo<<" "<<s<<" "<<y<<" -1 -1 0 -1 -1 -1"<<endl; }

 //matrix M2_sd1(1,nprey,fyModel,lyModel)
 sdreport_matrix M2_sd1(1,nprey,lyModel-sdReportYear,lyModel)
 !! for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) for (y=lyModel-sdReportYear;y<=lyModel;y++) {parNo++;  parexp<<"M2_sd1 "<<parNo<<" "<<s<<" "<<y<<" -1 -1 1 -1 -1 -1"<<endl; }

 //matrix M2_sd2(1,nprey,lyModel-sdReportYear,lyModel)
 sdreport_matrix M2_sd2(1,nprey,lyModel-sdReportYear,lyModel)
 !! for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) for (y=lyModel-sdReportYear;y<=lyModel;y++) {parNo++;  parexp<<"M2_sd2 "<<parNo<<" "<<s<<" "<<y<<" -1 -1 2 -1 -1 -1"<<endl; }

 // recruitment
 sdreport_matrix rec_sd(first_VPA,nsp,lyModel-sdReportYear,lyModel);
 !!for (s=first_VPA;s<=nsp;s++) for (y=lyModel-sdReportYear;y<=lyModel;y++) {parNo++;  parexp<<"rec_sd "<<parNo<<" "<<s<<" "<<y<<" "<<recq<<" -1 "<<fa<<" -1 -1 -1"<<endl; }
 // matrix rec_sd(first_VPA,nsp,lyModel-sdReportYear,lyModel);
  
 objective_function_value obf

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

PRELIMINARY_CALCS_SECTION
 int s,y,q,a,sp_fl,sag,syg,pred_l,pred, prey,ri;
 int sy,sq,sp,spl,splp,ll,l;  //stomach index counters
 int nobs;
 double tmp; 

 // The functions set_stepnumber() and set_stepsize() can be used to modify the number
 // of points used to approximate the profile likelihood, or to change the stepsize between the points.
 
 //SSB_likeprof.set_stepnumber(16); // default value is 8
 //SSB_likeprof.set_stepsize(0.2); // default value is 0.5
 //stock_N_likeprof.set_stepnumber(16); // default value is 8
 //stock_N_likeprof.set_stepsize(0.2); // default value is 0.5
 
 
 cout << "PRELIMINARY_CALCS_SECTION begins"<<endl;

  ri=0;
  for (s=first_VPA;s<=nsp;s++) if (use_beta_SSB_Rec(s)==1) {
     ri=ri+1;
    SSB_R_beta(s)=SSB_R_beta_ini(ri);
  }
  // Separable model, set first year (and all the remaning years) , year effect to 1 
  F_y=1.0;

    
   // set F_q to 1/(no of seasons) as initial guess if F_q_ini is used, else copy read-in values for F_q_ini into F_q
  int sp_sag_syg=0;
  for (s=first_VPA;s<=nsp;s++) {
    for (sag=1;sag<=n_catch_season_age_group(s);sag++) {    
      for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {            
         sp_sag_syg++;
         a=catch_season_age(s,sag);
         if (seasonal_annual_catches(s)==0) {
            if (multi==0) for(q=fq_F(s,a);q<lq_F(s,a);q++) {  F_q_ini(sp_sag_syg,q)=fix_F_factor(s)/(lq_F(s,a)-fq_F(s,a)+1);}
         }
         else if (seasonal_annual_catches(s)==1){ for(q=fq_F(s,a);q<lq_F(s,a);q++) F_q(s,sag,syg,q)=F_q_ini_read(sp_sag_syg,q);}
        }
     }
  }

  // set last season, season effect to 1 (or 1/no of seasons) for a unique solution
  for (s=first_VPA;s<=nsp;s++){
    for (sag=1;sag<=n_catch_season_age_group(s);sag++){
      a=catch_season_age(s,sag);
      for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
        F_q(s,sag,syg,lq_F(s,a))=fix_F_factor(s)/(lq_F(s,a)-fq_F(s,a)+1);
      }
    }
  }
  
  if (sum(log_F_a_ini)>0) {    // do not overwrite estimates from parameter file
    for (s=first_VPA;s<=nsp;s++) log_F_a_ini(s)=0.0;
  }
  
  // initialise number of observations used
  no_obj_obs=0;
   
   
  // Calc yield, count number of observations and calc sum of catch numbers per age
  for (y=fyModel;y<=lyModel;y++) log_obs_C_annual(y)=0;
  for (s=first_VPA;s<=nsp;s++){
    nobs=0;
    sum_C(s)=0.0;
    sum_C_annual(s)=0.0;
    yield(s)=0.0;
    CWsum(s)=0.0;

    for (y=fyModel;y<=lyModel;y++){
      lqLocal=(y==lyModel)?lqly:lq;
       for (q=fq;q<=lqLocal;q++) {
         CALC_yq
         for (a=cfa(s);a<=la(s);a++) if (!(a==fa && q<recq)) {
          log_obs_C_annual(y,s,a)+=obs_C(yq,s,a);
          CWsum(s,y)+=weca(yq,s,a)*obs_C(yq,s,a);
          yield(s,y)+=weca(yq,s,a)*obs_C(yq,s,a)*prop_landed(yq,s,a);
          sum_C(s,q,a)+=obs_C(yq,s,a);
          sum_C_annual(s,a)+=obs_C(yq,s,a);
          if (a<=la_like(s)) {
            if (seasonal_annual_catches(s)==1 && q==fq) nobs++;
            else if (seasonal_annual_catches(s)==0) nobs++;
          }
        }
      }
    }  
    no_obj_obs(s,1)=nobs;
  }
  
  // number of obs for stock recruitment
  for (s=first_VPA;s<=nsp;s++) {
   //no_obj_obs(s,3)=0;
   for (y=fyModel;y<=lyModel;y++) if (recruitment_years(s,y)==1) no_obj_obs(s,3)+=1;   
  }    
  // substitute low catches with minimum and  log catches which are used later on several times

  random_number_generator rnd(123);  // noise on invented catches from mean catch
  
  for (s=first_VPA;s<=nsp;s++){
    if (seasonal_annual_catches(s)==1) {
     for (y=fyModel;y<=lyModel;y++){
      for (a=fa;a<=la(s);a++) {
        if (log_obs_C_annual(y,s,a)<=0) {
           tmp=-sum_C_annual(s,a)/(lyModel-fyModel+1)*min_catch(s)/100.0;   // minimum catch ;
           log_obs_C_annual(y,s,a)=log(tmp);
        }
        else log_obs_C_annual(y,s,a)=log(log_obs_C_annual(y,s,a));
      }
     }
   }
   
    else {
      for (y=fyModel;y<=lyModel;y++){
      lqLocal=(y==lyModel)?lqly:lq;
      for (q=fq;q<=lqLocal;q++) {
      CALC_yq
         for (a=cfa(s);a<=la(s);a++) if (!(a==fa && q<recq)) {
          if (min_catch(s)==0  ) {  // annual(no quarters) and seasonal catches
            if (obs_C(yq,s,a)==0.0 ) log_obs_C(yq,s,a)=1E-99;
          } 
          if (min_catch(s)>0) {  
            if (obs_C(yq,s,a)==0.0 && zero_catch_y_season(s,y,q)==1 ) {
               if (mceval==0 && test_output==1) cout<<"catch "<<obs_C(yq,s,a)<<" substituted by "<<min_catch(s)<<" s:"<<species_names[s]<<" y:"<<y<<" q:"<<q<<" a:"<<a<<endl;
              log_obs_C(yq,s,a)=0.0+log(min_catch(s));
            }
            else log_obs_C(yq,s,a)=0.0+log(obs_C(yq,s,a));
          }  
          else if (min_catch(s)>-100) {                    // substitute by a percentage of the average catch
            if (sum_C(s,q,a)>0) tmp=-sum_C(s,q,a)/(lyModel-fyModel+1)*min_catch(s)/100.0;
            else tmp=1.0;
            tmp=tmp*exp(0.2*randn(rnd));
            if ((obs_C(yq,s,a)==0) && (!((a==fa) && (q <recq))) && zero_catch_y_season(s,y,q)==1) {
              if (mceval==0 && test_output==1) cout<<"catch "<<obs_C(yq,s,a)<<" substituted by "<<tmp<<" s:"<<species_names[s]<<" y:"<<y<<" q:"<<q<<" a:"<<a<<endl;
              log_obs_C(yq,s,a)=log(tmp);
            }
            else log_obs_C(yq,s,a)=0.0+log(obs_C(yq,s,a));
          }
          else if (min_catch(s)<-100) {                    // substitute by a percentage of the average catch
            if (sum_C(s,q,a)>0) tmp=-sum_C(s,q,a)/(lyModel-fyModel+1)*min_catch(s)/10000.0;
            else tmp=1.0;
            tmp=tmp*exp(0.2*randn(rnd));
            if ((obs_C(yq,s,a)<tmp) && (!((a==fa) && (q <recq))) && zero_catch_y_season(s,y,q)==1) {
              if (mceval==0 && test_output==1) cout<<"catch "<<obs_C(yq,s,a)<<" substituted by "<<tmp<<" s:"<<species_names[s]<<" y:"<<y<<" q:"<<q<<" a:"<<a<<endl;
              log_obs_C(yq,s,a)=log(tmp);
            }
            else if (!((a==fa) && (q <recq))) log_obs_C(yq,s,a)=0.0+log(obs_C(yq,s,a));
          }
          if (zero_catch_y_season(s,y,q)==0)  log_obs_C(yq,s,a)=1E-99;
        }
      }
    }
   }
  }
  //initial guess on population recruits all years
  // use average annual catch * 2.7 as a guess

  for (s=first_VPA;s<=nsp;s++){
    tmp=0.0;
    for (y=fyModel;y<=lyModel;y++) {
       for (q=fq;q<=lq;q++) {
         CALC_yq
         tmp+=sum(obs_C(yq,s));      
       }
    }
    tmp=tmp/(lyModel-fyModel+1);
    log_rec_scale(s)=1.0+log(tmp);
  }

   //initial guess on population "recruits" older ages, first year
  if (sum(log_rec_older)>0.0) {      // do not overwrite estimates from parameter file
  for (s=first_VPA;s<=nsp;s++){
    for (a=fa+1;a<=la(s);a++) {
       log_rec_older(s,a)=-(a-fa)*0.5;
    }
  }
  }

  // set 0 catches to minimum observed for the parcicular age in the particular survey
 sp_fl=0;
  for (s=first_VPA;s<=nsp;s++) {
    for(f=1;f<=n_fleet(s);f++) {
      tmp=1E9;  // initial value for catch > 0
      sp_fl++;
      for(a=first_fleet_age(s,f);a<=last_fleet_age(s,f);a++){
        for(y=first_fleet_year(s,f);y<=last_fleet_year(s,f);y++) { 
          if (fleet_catch(sp_fl,y,a)>0.0 && fleet_catch(sp_fl,y,a)<tmp) tmp=fleet_catch(sp_fl,y,a);    
        } 
        for(y=first_fleet_year(s,f);y<=last_fleet_year(s,f);y++) { 
          if (fleet_catch(sp_fl,y,a)==0.0) fleet_catch(sp_fl,y,a)=tmp;    
        }

      }
     // cout<<"tmp:"<<tmp<<endl;
     // cout<<"Fleet catch:"<<endl<<fleet_catch(sp_fl)<<endl;
    }
  } 
  
  // initialize variance 
  SSB_R_s2=0.2;
  if (est_calc_sigma(3)==0) SSB_R_s2_ini=0.3;
    
  // calculate survey CPUE

  sp_fl=0;
  for (s=first_VPA;s<=nsp;s++) {
  nobs=0;
    for(f=1;f<=n_fleet(s);f++) {
      sp_fl++;
      log_CPUE(sp_fl)=999.0;
      CPUE_residuals(sp_fl)=-99.9;
      for (a=v_first_fleet_age(sp_fl);a<=v_last_fleet_age(sp_fl);a++) {
        for(y=first_fleet_year(s,f);y<=last_fleet_year(s,f);y++) {
          if ((fleet_catch(sp_fl,y,a)>0) && (fleet_effort(sp_fl,y)>0)) { 
            nobs++;  
            log_CPUE(sp_fl,y,a)=log(fleet_catch(sp_fl,y,a)/fleet_effort(sp_fl,y));
          }  
        }
      }
    }
    no_obj_obs(s,2)=nobs;
  } 
  
 // init qq_power, CPUE catchability exponent 
 for (s=first_VPA;s<=nsp;s++) qq_power(s)=1.0;

 // estimate ration from length or weight if required
 if (multi>=1 && (consum_op==1 || consum_op==2)) for (s=1;s<=npr;s++){
  for (d=1;d<=no_areas;d++) 
   for (y=fyModel;y<=lyModel;y++) {
     lqLocal=(y==lyModel)?lqly:lq;
     for (q=fq;q<=lqLocal;q++) {
      CALC_yq
      CALC_yqd
      for (a=faq(q);a<=la(s);a++) {
         if (consum_op==1) {
           if (west(yq,s,a)>0) consum(yqd,s,a)=consum_ab(d,s,q,1)* pow(west(yq,s,a),consum_ab(d,s,q,2));
           else consum(yqd,s,a)=0;
         }  
         else {
            if (lsea(yqd,s,a)>0) consum(yqd,s,a)=consum_ab(d,s,q,1)* pow( L_W_ab(s,1)* pow(lsea(yqd,s,a),L_W_ab(s,2)),consum_ab(d,s,q,2));
           else consum(yqd,s,a)=0;
        }
      }
     }
   }
 }
 
 // calc biomass of other predators and check that a stock number has a consumption
 if (multi>=1 && nOthPred>0 ) {
 ofstream othp("other_predators.out",ios::out);
 othp<<"Species.n Year Quarter Age N west consum"<<endl; 
 for (s=1;s<=nOthPred;s++){
   for (y=fyModel;y<=lyModel;y++) {
     lqLocal=(y==lyModel)?lqly:lq;
     for (q=fq;q<=lqLocal;q++) {
      CALC_yq
      d=1; // skal 熡dres
      CALC_yqd
      other_bio(s,y,q)=0.0;
      for (a=faq(q);a<=la(s);a++) {
        other_bio(s,y,q)=other_bio(s,y,q)+other_pred_N(yq,s,a)*west(yq,s,a);
        if (mceval==0 ) {
         if (( (consum_op=0 && other_pred_N(yq,s,a)>0 && consum(yqd,s,a)==0) )&& test_output>=1 )
            cout<<"Warning. Stock number>0, but consumption are =0: pred:"<<species_names[s]<<" age:"<<a<<" y:"<<y<<" q:"<<q<<" N:"<<other_pred_N(yq,s,a)<<" consum:"<<consum(yqd,s,a)<<endl;
         othp<<s<<" "<<y<<" "<<q<<" " <<a<<" " <<other_pred_N(yq,s,a)<<" "<<west(yq,s,a)<<" "<<consum(yqd,s,a)<<endl;
         }
      }
     }
   }
  }
  othp.close();
 }

 //  find min and max predator length, 
 if (multi>=1) {
  max_pred_length=0;
  min_pred_length=9999;
  for (sy=1;sy<=n_stl_y;sy++) { 
    for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
       for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
         for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
           s=stl_yqdp(sp,1);
           for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
             pred_l=int(pred_length(spl));
             if (pred_l>max_pred_length(s)) max_pred_length(s)=pred_l; 
             if (pred_l<min_pred_length(s)) min_pred_length(s)=pred_l; 
           }
        }
      }
    }
  }
  
  
   //  Use length or weight for size predation function
  // calc weight from length and a season independent length weight relation
 for (sy=1;sy<=n_stl_y;sy++) { 
    for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
      for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
        for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
          pred=stl_yqdp(sp,1);
          for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
            if (size_select_model==1) pred_size(spl)=pred_length(spl);
            else { pred_size(spl)=L_W_ab(pred,1)*pow(pred_length(spl),L_W_ab(pred,2));}
          }  
        }
      }
    }
  }
  int iprey_l;
  if (size_select_model==1) prey_size=stl_lstom;        // Use length of pred and prey as size selection  for likelihood
  else if (size_select_model==2 || size_select_model==5) prey_size=stl_wstom;   // Use weight of pred and prey as size selection
  else if (size_select_model==3 || size_select_model==4 || size_select_model==6) {
    prey_size=stl_lstom;  // calculate prey (size) mean weight from mean length and length weight relation
    for (sy=1;sy<=n_stl_y;sy++) { 
      for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
        for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
          for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
            for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
              ll=0; 
              for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
                prey=stl_yqdplp(splp,1);
                for (iprey_l=stl_yqdplp(splp,2);iprey_l<=stl_yqdplp(splp,3);iprey_l++) {
                  ll++;      
                  if (prey>0) prey_size(spl,ll)=L_W_ab(prey,1)*pow(stl_lstom(spl,ll),L_W_ab(prey,2));
  }}}}}}}}
  
 //make an index over the first entry in ALKS for a given year and quarter
 // and calc mean weight at size class from mean length and length-weight relation
 // and calc ration at size class from mean weight
 
 for (yqd=1;yqd<=def_yqd;yqd++){
   index_minl(yqd)=0;
   index_maxl(yqd)=0;
 }

 int Ly, Lq, Ld, Ls,La, Ll;
 if (simple_ALK ==1) {
  for (y=fyModel;y<=lyModel;y++) {
   Ly=1;
   while (Ly<n_ALKS_y) { if (y<=ALKS_y(Ly,1)) break; Ly++;}  // find the right year index
   if (y<ALKS_y(Ly,1)) Ly--;
   for (q=fq;q<=lq;q++) {
     for (Lq=ALKS_y(Ly,2);Lq<=ALKS_y(Ly,3);Lq++)  if (q==ALKS_yq(Lq,1)) break;   // find the right quarter index
     index_Lq(y,q)=Lq;
     for (Ld=ALKS_yq(Lq,2);Ld<=ALKS_yq(Lq,3);Ld++) {
       d=ALKS_yqd(Ld,1); 
       CALC_yqd 
       //cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" yqd:"<<yqd<<endl; 
       for (Ls=ALKS_yqd(Lq,2);Ls<=ALKS_yqd(Ld,3);Ls++) {
         s=ALKS_yqds(Ls,1);
         size_l_sea(yqd,s)=0;
         if (s<=npr) consum_l(yqd,s)=0;
         for (La=ALKS_yqds(Ls,2);La<=ALKS_yqds(Ls,3);La++) {                                                                   
           a=ALKS_yqdsa(La,1);          
           //cout<<endl<<"y:"<<y<<" q:"<<q<<" area:"<<d<<" s:"<<s<<" a:"<<a<<" Ly:"<<Ly<<" Lq:"<<Lq<<" Ld:"<<Ld<<" Ls:"<<Ls<<" La:"<<La<<endl;
           index_minl(yqd,s,a)=ALKS_yqdsa(La,2);
           index_maxl(yqd,s,a) =ALKS_yqdsa(La,3);
           //cout<<" ALKS_input:";
           for (Ll=ALKS_yqdsa(La,2);Ll<=ALKS_yqdsa(La,3);Ll++) {
             //cout<<" L1:"<<Ll<<" ALKS:"<<ALKS_input(La,Ll)<<" Length:"<<ALKS_length(La,Ll);
             size_l_sea(yqd,s,a,Ll)=L_W_ab(s,1)*pow(ALKS_length(La,Ll),L_W_ab(s,2));
             //cout<<" size_l_sea:"<<size_l_sea(yqd,s,a,Ll)<<endl; 
             if (s<=npr) {
               if (consum_op==0) consum_l(yqd,s,a,Ll)=consum(yqd,s,a);
               else if (consum_op==1 ||consum_op==2) consum_l(yqd,s,a,Ll)= consum_ab(d,s,q,1)* pow(size_l_sea(yqd,s,a,Ll),consum_ab(d,s,q,2));
             }
           }
         } 
 }}}}}  
 
 if (1==9) {
 y=fyModel; d=1;
 for (q=fq;q<=lq;q++) {
  CALC_yqd;
  cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" yqd:"<<yqd<<endl;
  cout<<"index_Lq:"<<endl<<index_Lq(y,q)<<endl; 
  for (s=1;s<=nsp;s++) {
    cout<<"s:"<<s<<endl; 
    cout<<"size_l_sea:"<<endl<<size_l_sea(yqd,s)<<endl;
    cout<<"consum_l:"<<endl<<consum_l(yqd,s)<<endl;
    cout<<"index_minl:"<<endl<<index_minl(yqd,s)<<endl<<"index_maxl:"<<index_maxl(yqd,s)<<endl;
  }  
 }
 }
  
 double pred_prey_size;
 int  noPrey, nTot, n, first_ll_prey;

 stl_stom=stl_stom_input;  // copy input data into used (and adjusted) variable
 stl_stom_use_like=1;      // use as default all observations in likelihood
 stl_stom_use_avail=1;      // use as default all observations for calculation of availeble food
   
 // classifyModel each stomach observation: used or not used? and calculate fixed term for multinomial likelihood of prey numbers 
 if (mceval==0 || make_sim_data>0) for (sy=1;sy<=n_stl_y;sy++) { 
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
    for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
     for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
       pred=stl_yqdp(sp,1);
        for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
         ll=0;
         for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
           prey=stl_yqdplp(splp,1);
           //cout<<"prey:"<<prey<<" nobs: ";
           nTot=0;
           first_ll_prey=ll+1;
           Prey_number_fac_term(spl,first_ll_prey)=0;
           noPrey=0;  
           for(iprey_l=stl_yqdplp(splp,2);iprey_l<=stl_yqdplp(splp,3);iprey_l++) {
              ll++;
              noPrey++;   // ny  
              if (prey>0) {
                if (stl_stom(spl,ll)<min_stom_cont(pred)) {stl_stom_use_like(spl,ll)=0;  stl_stom_use_avail(spl,ll)=0; }       // exclude too small stomach contents (weight proportion model only)
                else  if  (prey_size(spl,ll)/pred_size(spl)>prey_pred_size_fac(pred)) {stl_stom_use_like(spl,ll)=0;  stl_stom_use_avail(spl,ll)=0; }      // exclude too large preys  
               
                if (stl_stom_type(spl,ll)>stom_type_input(pred)) {stl_stom_use_like(spl,ll)=0;  stl_stom_use_avail(spl,ll)=0; }   // do not include that type of observation in the likelihood (weight proportion model only)
                
                if (stl_nopreystom(spl,ll)> 0 && do_number_like(pred)==1) {   // numbers for multinomial model
                    nTot+= stl_nopreystom(spl,ll);
                    //cout<<stl_nopreystom(spl,ll)<< " ";
                    if (stl_nopreystom(spl,ll) <100) for (n=1; n<=stl_nopreystom(spl,ll);n++) Prey_number_fac_term(spl,first_ll_prey)-=log(n); //  exact factorial function
                    else Prey_number_fac_term(spl,first_ll_prey)-=stl_nopreystom(spl,ll)*log(stl_nopreystom(spl,ll))-stl_nopreystom(spl,ll);  // Stirlings approximation  
                } 
              }
            }
            

             if (nTot>0) {
              if (nTot <100) for (n=1; n<=nTot;n++) Prey_number_fac_term(spl,first_ll_prey)+=log(n); //  exact factorial function
              else Prey_number_fac_term(spl,first_ll_prey)+=nTot*log(nTot)-nTot;  // Stirlings approximation  
              Prey_number_fac_term(spl,first_ll_prey)=-Prey_number_fac_term(spl,first_ll_prey); // negative log likelihood
              // cout<<"total:"<< nTot<<" neg likelihood:"" "<< Prey_number_fac_term(spl,first_ll_prey)<<endl;
             }
           }  // end prey species
         } 
       }
     }
   }
  }
 // cout<< "stl_stom_use_like"<<endl<<stl_stom_use_like<<endl;
 // find minimum and maximum pred prey size ratio
  
  min_pred_prey_size_ratio= init_min_pred_prey_size_ratio;
  max_pred_prey_size_ratio= init_max_pred_prey_size_ratio;

 for (sy=1;sy<=n_stl_y;sy++) { 
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
     for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
       for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
         pred=stl_yqdp(sp,1);
          for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
           ll=0; 
           for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
             prey=stl_yqdplp(splp,1);
             //cout<<"prey:"<<prey;
             for(iprey_l=stl_yqdplp(splp,2);iprey_l<=stl_yqdplp(splp,3);iprey_l++) {
                ll++;  
                //cout <<" "<<stl_stom(spl,ll)<<" "<<min_stom_cont(pred)<<endl;    
                if (prey>0 && stl_stom(spl,ll)>=min_stom_cont(pred) && stl_stom_type(spl,ll)<=stom_type_input(pred) ){
                   pred_prey_size=pred_size(spl)/prey_size(spl,ll);
                  if (pred_prey_size<min_pred_prey_size_ratio(pred,prey)) min_pred_prey_size_ratio(pred,prey)=pred_prey_size;
                  if (pred_prey_size>max_pred_prey_size_ratio(pred,prey)) max_pred_prey_size_ratio(pred,prey)=pred_prey_size;
 }}}}}}}}
 
  // print min and max pred/prey ratio 
  
  if (mceval==0) {
   cout<<endl<<"observed min predator prey size ratio: "<<endl;
    cout<<"                  ";
    for (s=first_VPA;s<=nsp;s++) cout <<species_names[s];
    for (pred=1;pred<=npr;pred++) {
      cout <<endl<<species_names[pred];
      for (prey=first_VPA;prey<=nsp;prey++) {     
        if (min_pred_prey_size_ratio(pred,prey)==init_min_pred_prey_size_ratio) cout<<"          .";
        else cout<<setfixed()<<setprecision(1)<<setw(11)<<min_pred_prey_size_ratio(pred,prey); 
      }
    }
   }
   
  if (mceval==0) {
   cout<<endl<<endl<<"observed max predator prey size ratio: "<<endl;
    cout<<"                  ";
    for (s=first_VPA;s<=nsp;s++) cout <<species_names[s];
    for (pred=1;pred<=npr;pred++) {
      cout <<endl<<species_names[pred];
      for (prey=first_VPA;prey<=nsp;prey++) {     
        if (max_pred_prey_size_ratio(pred,prey)<0) cout<<"          .";
        else cout<<setfixed()<<setprecision(0)<<setw(11)<<max_pred_prey_size_ratio(pred,prey); 
      }
    }
    cout <<endl<<endl;
  }
   
   if (mceval==0) {
    cout<<"                  ";
    for (s=first_VPA;s<=nsp;s++) cout <<species_names[s];
    for (pred=1;pred<=npr;pred++) {
      cout <<endl<<species_names[pred];
      for (prey=first_VPA;prey<=nsp;prey++) {     
        if ((max_pred_prey_size_ratio(pred,prey)>0 && min_pred_prey_size_ratio(pred,prey)==init_min_pred_prey_size_ratio) ||
            (max_pred_prey_size_ratio(pred,prey)<0 && min_pred_prey_size_ratio(pred,prey)!=init_min_pred_prey_size_ratio))
         cout<<endl<<"Something is wrong with size range for "<<species_names(pred)<<" eating "<<species_names(prey)<<endl;
      }
    }
  } 
 // Select size for size preference for use in calculation of M2
  for (d=1;d<=no_areas;d++) for (s=1;s<=nsp;s++){
   for (y=fyModel;y<=lyModel;y++) {
     lqLocal=(y==lyModel)?lqly:lq;
     for (q=fq;q<=lqLocal;q++) {
      CALC_yq
      CALC_yqd
      for (a=faq(q);a<=la(s);a++){
        if (size_select_model==1) {
            size_sea(yqd,s,a)=lsea(yqd,s,a);           //length of species at age in the sea
            if (s>=first_VPA) size_sea_prey_w(yqd,s,a)=west(yq,s,a);
        } 
        else if (size_select_model==2 || size_select_model==3){
            size_sea(yqd,s,a)=west(yq,s,a);    //weight of species at age in the sea
            if (s>=first_VPA) size_sea_prey_w(yqd,s,a)=west(yq,s,a);
        }            
        else if (size_select_model==4 ) {
           if (lsea(yqd,s,a)>0) size_sea(yqd,s,a)=L_W_ab(s,1)*pow(lsea(yqd,s,a),L_W_ab(s,2)); //weight of species at age in the sea from lenght an  length-weight relation
           if (s>=first_VPA) size_sea_prey_w(yqd,s,a)=size_sea(yqd,s,a);
        } 
        //else { //  size_select_model==5 and 6, not used  
        //   size_sea(d,s,y,q,a)=0;
        //   if (s>=first_VPA) size_sea_prey_w(d,s,y,q,a)=0;
        //  }                  
      }
      //if (y==lyModel && d==1) {cout<<setfixed()<<setprecision(3)<<setw(6)<<endl; TRACE(size_sea(yqd,1)); }

     }
   }
  }
 
 // make reference size for constant other food to length or weight according to the size selection model   
 if (size_select_model==1) AV_other_food_size=min_pred_length;
 else {   // use weight, not length
   for (s=1;s<=npr;s++){
    AV_other_food_size(s)=L_W_ab(s,1)*pow(min_pred_length(s),L_W_ab(s,2));
   }
 }
 
 // Check existente of ALK for stomach species (pred or prey) and age
 int found, sqd ,prey_l ,ALK_yi,ALK_yqi,ALK_yqdi,ALK_yqdsi,ALK_yqdsli,ALK_yqdsi_prey,ll;
 
 if (mceval==0) {
   for (sy=1;sy<=n_stl_y;sy++) {       
     y=stl_y(sy,1);
     //cout <<"y:"<<y<<endl;
     found=0;
     for (ALK_yi=1;ALK_yi<=n_ALK_y;ALK_yi++) {
       if (y==ALK_y(ALK_yi,1)) {found=1; break;} 
     } 
     if (found==0) { cerr<<"ERROR:  Stomach year:"<<y<<" not found in ALK"<<endl; exit(9); }
     
     for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
      q=stl_yq(sq,1); 
      //cout <<"q:"<<q<<endl;
      found=0;
      for (ALK_yqi=ALK_y(ALK_yi,2);ALK_yqi<=ALK_y(ALK_yi,3);ALK_yqi++) {
        if (q==ALK_yq(ALK_yqi,1)) {found=1; break;}
      } 
      if (found==0) { cerr<<"ERROR: Stomach quarter:"<<q<<" in year:"<<y<<" not found in ALK"<<endl; exit(9); }
       
      for (sqd=stl_yq(sq,2);sqd<=stl_yq(sq,3);sqd++) {
        //cout<<"sqd:"<<sqd<<endl;
        d=stl_yqd(sqd,1);
        //cout <<"d:"<<d<<endl;
        found=0;
        for (ALK_yqdi=ALK_yq(ALK_yqi,2);ALK_yqdi<=ALK_yq(ALK_yqi,3);ALK_yqdi++) {
          if (d==ALK_yqd(ALK_yqdi,1)) {found=1; break;}
        } 
        if (found==0) { cerr<<"ERROR: Stomach Area:"<<d<<" in quarter:"<<q<<" in year:"<<y<<" not found in ALK"<<endl; exit(9); }
       
        for (sp=stl_yqd(sqd,2);sp<=stl_yqd(sqd,3);sp++) {
          pred=stl_yqdp(sp,1);
          //cout<<"pred: "<<pred<<endl;
          found=0;
          for (ALK_yqdsi=ALK_yqd(ALK_yqdi,2);ALK_yqdsi<=ALK_yqd(ALK_yqdi,3);ALK_yqdsi++) {
            if (pred==ALK_yqds(ALK_yqdsi,1)) {found=1; break;}
          } 
          // if (found==0) { cerr<<"ERROR: Predator:"<<pred<<" in quarter:"<<q<<" in year:"<<y<<" in area:"<<d<<" not found in ALK"<<endl; exit(9); }
          for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
            //pred_l=stl_yqdpl(spl,1);     
            ll=0;
            for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
              prey=stl_yqdplp(splp,1);
              //cout<<"prey: "<<prey<<endl;
             if (prey>0) {
               found=0;
               for (ALK_yqdsi_prey=ALK_yqd(ALK_yqdi,2);ALK_yqdsi_prey<=ALK_yqd(ALK_yqdi,3);ALK_yqdsi_prey++) {
                 if (prey==ALK_yqds(ALK_yqdsi_prey,1)) {found=1; break; }
               } 
               if (found==0) { cerr<<"ERROR: Prey:"<<prey<<" in stomach quarter:"<<q<<" in year:"<<y<<" in area:"<<d<<" not found in ALK"<<endl; exit(9); }
             
               for(prey_l=stl_yqdplp(splp,2);prey_l<=stl_yqdplp(splp,3);prey_l++) {
                 ll++;
                 //cout<<"prey_l:"<<prey_l<<" ll:"<<ll<<endl;
                 found=0;
                 for (ALK_yqdsli=ALK_yqds(ALK_yqdsi_prey,2);ALK_yqdsli<=ALK_yqds(ALK_yqdsi_prey,3);ALK_yqdsli++) {
                   if (prey_l>=ALK_yqdsa(ALK_yqdsli,2) && prey_l<=ALK_yqdsa(ALK_yqdsli,3)) {found=1; break; }
                 } 
                 if (found==0 && stl_stom(spl,ll)==0.0)  {cerr<<"WARNING: Length class:"<<prey_l<<" of prey: "<<prey<<" in stomach quarter:"<<q<<" in year:"<<y<<" in area:"<<d<<" not found in ALK"<<endl; }                      
                 else if (found==0 && stl_stom(spl,ll)>0.0) { cerr<<"ERROR: Length class:"<<prey_l<<" of prey: "<<prey<<" in stomach quarter:"<<q<<" in year:"<<y<<" in area:"<<d<<" not found in ALK"<<endl;exit(9); }                      

               } 
             } else ll++;     // prey==0
           }
         }
       }
      }
    }
   } 
   cout<<" ALK exist for all prey length classes"<<endl;
  }
  // print min and max pred/prey ratio
  // and estimate pred-prey ratio irrespective of prey
  all_min_pred_prey_size_ratio=init_min_pred_prey_size_ratio;
  all_max_pred_prey_size_ratio=init_max_pred_prey_size_ratio;
  if (mceval==0) {
   cout<<endl<<"Adjusted min predator prey size ratio (according to option max.prey.pred.size.fac), to be used for calculation of M2: "<<endl;
    cout<<"                  ";
    for (s=first_VPA;s<=nsp;s++) cout <<species_names[s];
    for (pred=1;pred<=npr;pred++) {
      cout <<endl<<species_names[pred];
      for (prey=first_VPA;prey<=nsp;prey++) {
        if (min_pred_prey_size_ratio(pred,prey)==init_min_pred_prey_size_ratio) cout<<"          .";
        else {
        if (min_pred_prey_size_ratio(pred,prey)< (1/prey_pred_size_fac(pred)))  min_pred_prey_size_ratio(pred,prey)= 1/prey_pred_size_fac(pred);
         cout<<setfixed()<<setprecision(1)<<setw(11)<<min_pred_prey_size_ratio(pred,prey);
        } 
        if (all_min_pred_prey_size_ratio(pred)>min_pred_prey_size_ratio(pred,prey)) all_min_pred_prey_size_ratio(pred)=min_pred_prey_size_ratio(pred,prey);
        if (all_max_pred_prey_size_ratio(pred)<max_pred_prey_size_ratio(pred,prey)) all_max_pred_prey_size_ratio(pred)=max_pred_prey_size_ratio(pred,prey);
      }
      cout<<setfixed()<<setprecision(1)<<setw(11)<<all_min_pred_prey_size_ratio(pred);
      if (size_selection(pred)==5 || size_selection(pred)==6) {
         all_min_pred_prey_size_ratio(pred)=log(all_min_pred_prey_size_ratio(pred));
         all_max_pred_prey_size_ratio(pred)=log(all_max_pred_prey_size_ratio(pred));
      }  
      // adjust to avoid outer bounds in beta distribution [0;1] 
      all_range_pred_prey_size_ratio(pred)= 1.001*(all_max_pred_prey_size_ratio(pred)-all_min_pred_prey_size_ratio(pred));
      all_min_pred_prey_size_ratio(pred)= 0.999*all_min_pred_prey_size_ratio(pred); 
     }
    cout<<endl;
   }

  //cout<<"all_min_pred_prey_size_ratio(pred):"<<endl<<all_min_pred_prey_size_ratio<<endl;
  //cout<<"all_max_pred_prey_size_ratio(pred):"<<endl<<all_max_pred_prey_size_ratio<<endl;
  //cout<<"all_range_pred_prey_size_ratio(pred):"<<endl<<all_range_pred_prey_size_ratio<<endl;


  // check positive weight for preys and count number of stomachs observations used for estimation
  // and count number of pred-prey observations
  // and log stomach contents in a new variable
  // and set stomach contents to zero for values outside size range
  
  int used_stom;
  dvar_matrix stoms(1,2,1,npr);
  dvar_matrix Nstom(first_VPA,nsp,1,npr);

  stoms=0.0;
  Nstom=0.0;
  cout<<endl;
  
  for (sy=1;sy<=n_stl_y;sy++) { 
    for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
      for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
      d=stl_yqd(sd,1);
      for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
        pred=stl_yqdp(sp,1);
        for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
          ll=0; 
          used_stom=0;
         if (incl_stom(d,pred,stl_yq(sq,1),stl_yqdpl(spl,1),sy)>=1) { //Include stomach observations in likelihood 0=no inlusion, >=1 include data

          for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
            prey=stl_yqdplp(splp,1);
            for(l=stl_yqdplp(splp,2);l<=stl_yqdplp(splp,3);l++) {
               ll++;
                if (stl_stom(spl,ll)>=min_stom_cont(pred)) {
                 used_stom++; 
                 log_stl_stom(spl,ll)=log(stl_stom(spl,ll)); 
                 if (prey>0) {
                   if (stl_stom_use_like(spl,ll)==1) Nstom(prey,pred)=Nstom(prey,pred)+1;
                   else stl_stom(spl,ll)=0.0;
                 }                                                                                                     
               }
               
               if ((stl_wstom(spl,ll)<0) && (stl_yqdplp(splp,1)!=0)) {
                  cout <<"stom error, Weight <0" <<endl; 
                  cout<<"Year:"<<stl_y(sy,1)<<" Quarter:"<<stl_yq(sq,1)<<" Predator:"<<pred<<" Length:"<<stl_yqdpl(spl,4)<<" Prey:"<<prey<<endl; 
                  exit(1); 
               }
            }
          }
          stoms(1,pred)+=ll;
          stoms(2,pred)+=used_stom;
          }
          else if (mceval!=1) { cout<<"Area:"<<d<<" pred:"<<pred<<", "<<species_names[pred]<<" year:"<<stl_y(sy,1)<<" Q:"<<stl_yq(sq,1)<<" pred length class:"<<stl_yqdpl(spl,1)<<
             " pred length group:"<<stl_yqdpl(spl,4)<<" not included, as exclusion specified in input file incl_stom.in"<<endl;  }
        }
       }
     }
    }
  }

  if (mceval==0) {
    cout <<endl<<"Min observed stomach contents observations to be used: "<<setprecision(3)<<setscientific()<<min_stom_cont<<endl;
    cout <<endl<<"Species, number of stomachs observations totally, and used"<<endl;
    for (pred=1;pred<=npr;pred++) {
       cout <<species_names[pred]<<"            "<<setfixed()<<setprecision(0)<<setw(15)<<stoms(1,pred)<<"    "<<stoms(2,pred)<<endl;
    }
  }
  // print number of stomach observation per predator
  if (mceval==0) {
    cout<<endl<<"Number of predator-prey interaction observation used"<<endl;
    cout<<"                  ";
    for (s=first_VPA;s<=nsp;s++) cout <<species_names[s];
    for (pred=1;pred<=npr;pred++) {
      cout <<endl<<species_names[pred];
      for (prey=first_VPA;prey<=nsp;prey++) {     
        cout<<setfixed()<<setprecision(0)<<setw(11)<<Nstom(prey,pred); 
      }
    }
    cout <<endl<<endl;
  }
 
 // init 
 stl_other_suit_slope=0.0;
 sum_p_Dirichlet=0;
 like_Dirichlet=0;

 // copy season_overlap_input values to season_overlap (used)
 // values to be estimated are entered later on
  for(d=1;d<=no_areas;d++) for (pred=1;pred<=npr;pred++) {
    season_overlap(d,pred)=season_overlap_input(d,pred);
  }

  
 // for predators wih no size selection:
 // add observed stomach contents within each prey species,
 //  use the first prey size of stl_stom for this sum
 //  and use max no of samples as samples for the group
 double max_samples;
 int first_ll;
 
 for (pred=1;pred<=npr;pred++) if (sumStomLike(pred)==1)  stoms(2,pred)=0;
 if (mceval==0 || make_sim_data>0) for (sy=1;sy<=n_stl_y;sy++) {
   //y=stl_y(sy,1); cout<<"Y:"<<y<<endl;
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
     //q=stl_yq(sq,1); cout<<"q:"<<q<<endl;
     for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
      for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
        pred=stl_yqdp(sp,1);
        //cout<<"pred:"<<pred<<endl;
       
        if (sumStomLike(pred)==1) {  // sum of obs per length
           used_stom=0;
           for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
           ll=0;
           //pred_l=int(pred_length(spl));
           for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
             prey=stl_yqdplp(splp,1); 
             //cout<<"prey:"<<prey"<<endl;
             first_ll=-1;
             max_samples=0.0;
             for(iprey_l=stl_yqdplp(splp,2);iprey_l<=stl_yqdplp(splp,3);iprey_l++) {
               ll++;
               if (prey==0) used_stom++;
               else if (stl_stom_use_like(spl,ll)==1){
                 if (first_ll==-1) {
                   first_ll=ll;
                   max_samples=stl_no_samples(spl,ll);
                   if (incl_stom(stl_yqd(sd,1),pred,stl_yq(sq,1),stl_yqdpl(spl,1),sy)>=1) used_stom++;
                 }
                 else if (first_ll > -1) { // second or higher observation for a prey species
                   stl_stom(spl,first_ll)=stl_stom(spl,first_ll)+stl_stom(spl,ll);
                   stl_stom(spl,ll)=0.0;
                   stl_stom_use_like(spl,ll)=0;  
                   // MV  stl_stom_use_avail(spl,ll)=1;
                   if (stl_no_samples(spl,ll)>max_samples) max_samples=stl_no_samples(spl,ll);
                 }
               } 
             }
             if (first_ll> -1){
               log_stl_stom(spl,first_ll)=log(stl_stom(spl,first_ll));
               stl_no_samples(spl,first_ll)=max_samples;
             }
            }
          }
          stoms(2,pred)+=used_stom; // reset number of usede stomachs 
        }  
      }
    } 
   }
  }
 
  for (pred=1;pred<=npr;pred++) no_obj_obs(pred,4)=stoms(2,pred);

   //cout<<setprecision(5)<<"after: stl_stom"<<endl<<stl_stom<<endl;
   //cout<<setprecision(5)<<"after: prey_size"<<endl<<prey_size<<endl;
  // standardize stomach contents to 1
  if (mceval==0) {
    for (sy=1;sy<=n_stl_y;sy++) {
      for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
        for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
          for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
            for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
              ll=0;
              tmp=0.0;
              for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
                for(l=stl_yqdplp(splp,2);l<=stl_yqdplp(splp,3);l++) {
                   ll++;
                   if (stl_stom_use_like(spl,ll)==1) tmp+=stl_stom(spl,ll);
                }
              }
              ll=0;
              for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
                for(l=stl_yqdplp(splp,2);l<=stl_yqdplp(splp,3);l++) {
                  ll++;
                  if (tmp>0 && (stl_stom_use_like(spl,ll)==1)) stl_stom(spl,ll)=stl_stom(spl,ll)/tmp;
                }
              }
            }
          }
        }
      }
    }
  }

 //cout<<setprecision(5)<<"after standard: stl_stom"<<endl<<stl_stom<<endl;

 
 if (mceval==0) {
   ofstream res("summary_stom_start.out",ios::out);
   res<<"Year Quarter.no Area Predator.no Size.model Predator.length.class Predator.length ";
   res<<"Predator.length.mean  Predator.size Prey.no ";
   res<<"Prey.length.class Prey.length.mean Prey.weight Prey.size size.ratio N.haul stomcon stomcon.input use_like use_avail stom.type  ";
   res<<"Y YQ YQP YQPL YQPLP YQPLPL"<<endl;
  
   for (sy=1;sy<=n_stl_y;sy++) {
     y=stl_y(sy,1);
     for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
       q=stl_yq(sq,1);
       for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
        d=stl_yqd(sd,1);
        for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
         pred=stl_yqdp(sp,1);
         for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
           ll=0;
           for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
             prey=stl_yqdplp(splp,1);
             for(prey_l=stl_yqdplp(splp,2);prey_l<=stl_yqdplp(splp,3);prey_l++) {
               ll++;
               res<<y<<" "<<q<<" "<<d<<" "<<pred<<" "<<size_selection(pred)<<" "<<stl_yqdpl(spl,1)<<" "<<stl_yqdpl(spl,4)<<" "<<pred_length(spl)<<" ";
               res<<pred_size(spl)<<" "<<prey<<" "<<prey_l<<" "<<stl_lstom(spl,ll)<<" ";
               res<<stl_wstom(spl,ll)<<" "<<prey_size(spl,ll)<<" "<<prey_size(spl,ll)/pred_size(spl)<<" "<<stl_N_haul(spl,ll)<<" ";
               res <<stl_stom(spl,ll)<<" "<<stl_stom_input(spl,ll)<<" "<<stl_stom_use_like(spl,ll)<<" "<<stl_stom_use_avail(spl,ll)<<" "<<stl_stom_type(spl,ll)<<" ";
               res<<sy<<" "<<sq<<" "<<sp<<" "<<spl<<" "<<splp<<" "<< prey_l<<endl;
              }
            }
          }
         }
        }
      }
    }
    res.close();
  }
  for (y=fyModel;y<=lyModel;y++) for (q=fq;q<=lq;q++) { 
    CALC_yq
      M2(yq)=1.5*(M(yq)-M1(yq))+0.01;  //initial guess on M2
   }  
  
  //cout<<"%%%%%%%%%%% initial guess on M2:"<<endl<<setprecision(4)<<M2<<endl<<setprecision(0);
  
  
   if (first_VPA>1) for (s=1;s<first_VPA;s++) for (y=fyModel;y<=lyModel;y++) for (q=fq;q<=lq;q++) {
          CALC_yq
          N(yq,s)=other_pred_N(yq,s);  //move other predator stock size to N
          N_bar(yq,s)=other_pred_N(yq,s);
          N_bar_stom(yq,s)=other_pred_N(yq,s);
  }
  // use default values for predation parmeters when option def_pred_par=1
  if (def_pred_par==1) { 
    if (no_size_other_food_suit>0) init_stl_other_suit_slope=0.0; 
    //cout<<"init_stl_other_suit_slope:"<<init_stl_other_suit_slope<<endl;
   
    if (stomach_variance==1 || stomach_variance==2) for (s=1;s<=npr;s++) if (StomObsVar(s)==1) Stom_var(s)=1.05; 
    else if (stomach_variance==3 || stomach_variance==4) for (s=1;s<=npr;s++)  if (StomObsVar(s)==1) Stom_var(s)=(Stom_var_l_save(s)+Stom_var_u_save(s))/2;
    int n;
    n=0;        
    for (s=1;s<=npr;s++) { 
       if (size_selection(s)!=0 && size_selection(s)!=4) {
        n++;
        if (size_selection(s)==1 ||  size_selection(s)==2 || size_selection(s)==3 ) {
          if (size_select_model==1)  init_pref_size_ratio(n)=2.0; else init_pref_size_ratio(n)=4.0; 
        } 
        else if (size_selection(s)==5) {
          init_pref_size_ratio(n)=1.2;    // first parameter in beta distribution
        }
        else if (size_selection(s)==6) {
          init_pref_size_ratio(n)=4.0;    // first parameter in beta distribution
        }
     }   
    }
    //cout<<"init_pref_size_ratio:"<<init_pref_size_ratio<<endl;
    if (phase_prey_size_adjustment>0) init_prey_size_adjustment=1.0;  
    prey_size_adjustment=1.0;   
    if (multi>0 && phase_pref_size_ratio_correction>-1) init_pref_size_ratio_correction=0.0;
   
    n=0;        
    for (s=1;s<=npr;s++) { 
      if (size_selection(s)!=0 && size_selection(s)!=4) {
        n++;

        if (size_selection(s)==1 || size_selection(s)==2 || size_selection(s)==3 ) {
          if (size_select_model==1)  var_size_ratio_ini(n)=1.5; else var_size_ratio_ini(n)=3.0; 
        } 
        if (size_selection(s)==5 || size_selection(s)==6) {
           var_size_ratio_ini(n)=4.0;    // second parameter in beta distribution
        }
        var_size_ratio(s)=var_size_ratio_ini(n);
        if (size_selection(s)==2) {
          n++;
          if (size_select_model==1)  var_size_ratio_ini(n)=1.5; else var_size_ratio_ini(n)=2.0; 
        }   
      }   
    }
                          
    if ( no_season_overlap_to_estimate>0 && phase_season_overlap>-1) init_season_overlap=1.0; 

    vulnera=1.0; 
    //vulnera_size=0.0;    
    if (mesh_size_active==1) init_L50=100;

   } // end (def_pred_par==1)
 
  
   prey_size_adjustment=1.0;
   
    //  mesh selction parameters
   if (mesh_size_active==1) L50=L50_mesh;
   
  }  // end if (multi >=1)

  // Prepare prediction variables
  MCMC_prediction=0;
 
  if (mceval==1) { 
    //reset temporaty ASCII files
 
    if (at_age_output(1)) {   
      ofstream mcout_F("mcout_f.out",ios::out);
      mcout_F<< "Species.n Age Year Quarter Repetion Iteration F"<<endl;
    }
    if (at_age_output(2) && multi==2)  {   
      ofstream mcout_M2("mcout_m2.out",ios::out);
      mcout_M2<< "Species.n Age Year Quarter Repetion Iteration M2"<<endl;
    }  
    if (at_age_output(3)) {   
      ofstream mcout_Z("mcout_z.out",ios::out);
      mcout_Z<< "Species.n Age Year Quarter Repetion Iteration Z"<<endl;
    }   
    if (at_age_output(4)) {   
      ofstream mcout_F("mcout_n.out",ios::out);
      mcout_F<< "Species.n Age Year Quarter Repetion Iteration N"<<endl;
    }
    if (at_age_output(5)) {   
      ofstream mcout_C("mcout_c.out",ios::out);
      mcout_C<< "Species.n Age Year Quarter Repetion Iteration C"<<endl;
    }   
    if (at_age_output(7)) {   
      ofstream mcout_constraints("mcout_constraints.out",ios::out);
      mcout_constraints<< "Species.n Year Repetion Iteration constraints"<<endl;
    }
     if (at_age_output(8)) {   
      ofstream mcout_closure("mcout_closure.out",ios::out);
      mcout_closure<< "Species.n Year Repetion Iteration closure"<<endl;
    }
    
    ofstream obj_func_file("mcout_obj_func.out",ios::out);
    obj_func_file<<"Repetion Species.n obj.catch obj.survey obj.SR obj.stom obj.stom.no obj.penalty obj.all"<<endl;
    
  }
  if (do_short_term_forecast==1) {
     ofstream res("short_term.out",ios::out);
     res<<"mcmc species n.Fmulti Fmulti FF SSB0 SSB log_SSB Yield"<<endl;
     res.close();
  } else if (do_short_term_forecast==2) {
     ofstream res("short_term.out",ios::out);
     res<<"mcmc species n.TAC TAC FF SSB0 SSB Yield"<<endl;
     res.close();
  }

  
  // reset file for detailed output
  ofstream details("details.out",ios::out);
  ofstream det("details_ices.out",ios::out);

  // print parameter settings
  if (test_output==4) print_input_par();
  
  cout << "PRELIMINARY_CALCS_SECTION done"<<endl;


  min_first_VPA=first_VPA; // used for testing; selects af sub-set of species
  max_last_VPA=nsp;   // used for testing; selects af sub-set of species
  
   // testing
  //min_first_VPA=18; // used for testing; selects af sub-set of species
  //max_last_VPA=18;   // used for testing; selects af sub-set of species

  

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

  
PROCEDURE_SECTION 

 int iter,s,y,q,a;
 int yqMaster;
 dvariable tmp;
 
 //check_print_par();
 Rec_parm_adm();
 predation_parm_adm();          // reorganise parameters for biological interaction
 CPUE_parm_adm();
 get_initial_N_at_age();
 calc_F(0);
   
 //  testing !!nsp=first_VPA+4;
 for (y=fyModel;y<=lyModel;y++) {
   lqLocal=(y==lyModel)?lqly:lq; 
   for (q=fq;q<=lqLocal;q++) {
     CALC_yq
     yqMaster=yq;    // master counter on year quarter index
     if (multi==0) {                        // Single species mode
       calc_Z(y,q);
       get_N_bar_at_age(y,q);              //Calc N within the period
     } 
     else {                                 // Multi species mode
      if (use_Nbar==0) {                    // Use N in the beginning of a period to calc M2
         get_N_bar_stom_at_age(y,q);        // Calc N_bar_stom (which in this case is N) 
         calc_M2(y,q);
         calc_Z(y,q);
         get_N_bar_at_age(y,q);             // Calc N within the period
       } 
       else {                               // Use Nbar to calc M2
         tmp=100.0; iter=0;
         while ((tmp>max_M2_sum2) && (iter<=max_M2_iteration)) {
           iter++;
           calc_Z(y,q);     
           get_N_bar_at_age(y,q);       // Calc N within the period   
           get_N_bar_stom_at_age(y,q);  // Calc N_bar_stom (which in this case is Nbar) 

           if ((current_phase()>=stom_phase) && (multi>=2)){
             old_M2(yqMaster)=M2(yqMaster);
             calc_M2(y,q);
             tmp=sum(square(M2(yqMaster)-old_M2(yqMaster)));
             //cout<<"tmp: "<<setprecision(5)<<tmp<<endl;
           }    
         } // end while loop
         calc_Z(y,q);                       // update Z with the latest estimate of M2
         get_N_bar_at_age(y,q);             // Calc N within the period   
         get_N_bar_stom_at_age(y,q);        // Calc N_bar_stom (which in this case is Nbar) 
       }   // end use_Nbar=1
     }     // End Multi species mode 
     get_N_at_age(y,q); //calc N for the next period (q=q+1 or y=y+1 and q=1)
   } // end quarter loop
 }  // end year-loop
 if (!mceval_phase()) evaluate_the_objective_function(); else get_biomass();
 
 // test_out_test();  // print test_output
 // check_print_par();
 // print_survey_residuals();
 
 if ((test_output==3 || test_output==19) && !sd_phase() && !mceval_phase() ) cout <<endl<<setw(8)<<
                 setfixed()<<setprecision(1)<<"obj_func:       "<<obf<<endl<<obj_func<<endl;
 calc_avg_F();
 if (sd_phase()) {
       calc_hist_SSB();
       calc_term_N();
       calc_term_F();
       move_recruitment_to_sdreport();
       if (multi>=2) move_M2_to_sdreport(); 
 }

 if (sd_phase() && do_short_term_forecast==1) make_short_term_forecast_F();
 
 else if (mceval_phase() ) {
   if (do_short_term_forecast==0) {
     if (multi==2) calc_eaten_M2_hist();
     predict();
   }
   else if (do_short_term_forecast==1) make_short_term_forecast_F();
   else if (do_short_term_forecast==2) make_short_term_forecast_TAC();
 }
 //********************************************************************************************* 
 


FUNCTION void make_short_term_forecast_TAC(); 
 int s,a,y,Fm,yqr;
 double dif,x,yy,upper,lower,target;
 int iter;
 dvar_vector tmp(first_VPA,nsp);
  
 y=lyModel;
 
 tmp=0; // Calc F as sum of seasonal F's
 for (s=first_VPA;s<=nsp;s++) {
   for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) {
     for (q=fq;q<=lq;q++){
       CALC_yq 
       tmp(s)+=value(F(yq,s,a));
     }
   } 
   tmp(s)=tmp(s)/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
  }

 short_term_SSB=0;
 short_term_SSB0=0;
 short_term_yield=0;
 short_term_F=0;
 
 cout<<"short forecast number:"<<no_short_term_forecast<<endl;
 y=lyModel+1; q=recq; CALC_yq; yqr=yq;
 
 for (Fm=1;Fm<=no_F_multipliers;Fm++) {
   for (s=first_VPA;s<=nsp;s++) {
      N(yqr,s,fa)=rec_TAC_year(s);  //  Recruits in the TAC year
  
     dif=100.0;
     iter=0;
     x=1.0;
     upper=10;
     lower=0;
     target=F_multipliers(s,Fm);  // TAC, ignore the name of the variable
     y=lyModel+1; 
     while ((dif>1E-8) && (iter<100) && (x >=1E-8)) {
       x=(upper+lower)/2;
       yy=0;
       for (q=fq;q<=lq;q++) {
          CALC_yq
          F(yq,s)=F(yq-lq,s)* x;    // prediction F, use F in the previous year as forecast exploitation pattern
          calc_Z(y,q);  
          get_N_bar_at_age(y,q);             // Calc N within the period  
          for (a=faq(q);a<=la(s);a++)  yy=yy+value(weca(yq,s,a)*N_bar(yq,s,a)*F(yq,s,a));
          get_N_at_age(y,q); 
       }
       
       if (yy>=target) upper=x; else lower=x;
       dif=fabs(upper-lower);
       iter++;
     }
     //cout<<"iter:"<<iter<<" TAC input:"<<target<<"  yield:"<<yy<<" test:"<<Mean_F(s,lyModel)<<endl;
     short_term_yield(s,Fm)=yy;
     short_term_F(s,Fm)=x;
     y=lyModel+1; q=fq; CALC_yq
     for (a=faq(q);a<=la(s);a++) short_term_SSB0(s,Fm)+=N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a);

     y=lyModel+2; q=fq; CALC_yq  
     for (a=faq(q);a<=la(s);a++) if (propmat(yq,s,a)>0) short_term_SSB(s,Fm)+=N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a);
   } // species
  } // Fm

 ofstream res("short_term.out",ios::app);
 for (s=first_VPA;s<=nsp;s++){
   y=lyModel+1;
   for (Fm=1;Fm<=no_F_multipliers;Fm++) {
     res<<no_short_term_forecast<<" "<<s<<" "<<Fm<<" "<<F_multipliers(s,Fm)<<" "<<tmp(s)*short_term_F(s,Fm)<<" "<<short_term_SSB0(s,Fm)<<" "<<short_term_SSB(s,Fm)<<" "<<short_term_yield(s,Fm)<<endl;
   }
 }
 no_short_term_forecast++;
 res.close();
   
 //********************************************************************************************* 


FUNCTION void make_short_term_forecast_F(); 
 int s,a,y,Fm,yqr;

 short_term_SSB=0;
 short_term_SSB0=0;
 short_term_yield=0;
 dvar_vector tmp(first_VPA,nsp);
  
 y=lyModel;
 
 tmp=0; // Calc F as sum of seasonal F's
 for (s=first_VPA;s<=nsp;s++) {
   for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) {
     for (q=fq;q<=lq;q++){
       CALC_yq 
       tmp(s)+=value(F(yq,s,a));
     }
   } 
   tmp(s)=tmp(s)/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
  }
 
 y=lyModel+1; q=recq; CALC_yq; yqr=yq;
 
 for (Fm=1;Fm<=no_F_multipliers;Fm++) {
   for (s=first_VPA;s<=nsp;s++)  N(yqr,s,fa)=rec_TAC_year(s);  //  Recruits in the TAC year
   //cout<<"Nrec: "<<endl<<N(yqr)<<endl;
   
   for (y=lyModel+1;y<=lyModel+1;y++) {
     for (q=fq;q<=lq;q++) {
        CALC_yq
        //cout<<"y:"<<y<<" q:"<<q<<endl;
        for (s=first_VPA;s<=nsp;s++) F(yq,s)=F(yq-lq,s)* F_multipliers(s,Fm);    // prediction F, use F in the prvious year as forecast exploitation pattern
        //cout<<" F: "<<F(yq)<<endl;
        calc_Z(y,q);  
        // cout<<" Z: "<<Z(yq)<<endl;                           
        get_N_bar_at_age(y,q);             // Calc N within the period  
        // cout<<" N: "<<N(yq)<<endl;
        get_N_at_age(y,q); //calc N for the next period (q=q+1 or y=y+1 and q=1)
        // cout<<" N_bar: "<<N_bar(yq)<<endl;
     }
   }
   y=lyModel+1;
   for (q=fq;q<=lq;q++) {
     CALC_yq
     for (s=first_VPA;s<=nsp;s++){
      for (a=faq(q);a<=la(s);a++) {
         short_term_yield(s,Fm)+=weca(yq,s,a)*N_bar(yq,s,a)*F(yq,s,a);
         if (q==fq) {
           if (propmat(yq,s,a)>0) short_term_SSB0(s,Fm)+=N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a);
           //if (a==2) stock_N_likeprof=N(yq,s,a);
         }
   } }}   

   y=lyModel+2; q=fq; CALC_yq  
   for (s=first_VPA;s<=nsp;s++){
     for (a=faq(q);a<=la(s);a++) if (propmat(yq,s,a)>0) short_term_SSB(s,Fm)+=N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a);
     log_short_term_SSB(s,Fm)=log(short_term_SSB(s,Fm));
     if (Fm==1 && s==first_VPA) SSB_likeprof=short_term_SSB(s,Fm);
   }    

  } // Fm

 ofstream res("short_term.out",ios::app);
 for (s=first_VPA;s<=nsp;s++){
   y=lyModel+1;
   for (Fm=1;Fm<=no_F_multipliers;Fm++) {
     //res<<no_short_term_forecast<<" "<<s<<" "<<Fm<<" "<<F_multipliers(s,Fm)<<" "<<avg_F(s,lyModel)*F_multipliers(s,Fm)<<" "<<short_term_SSB0(s,Fm)<<" "<<short_term_SSB(s,Fm)<<" "<<log_short_term_SSB(s,Fm)<<" "<<short_term_yield(s,Fm)<<endl;
      res<<no_short_term_forecast<<" "<<s<<" "<<Fm<<" "<<F_multipliers(s,Fm)<<" "<< tmp(s)*F_multipliers(s,Fm)<<" "<<short_term_SSB0(s,Fm)<<" "<<short_term_SSB(s,Fm)<<" "<<log_short_term_SSB(s,Fm)<<" "<<short_term_yield(s,Fm)<<endl;
   }
 }
 cout<<"short cast number:"<<no_short_term_forecast<<endl;
 no_short_term_forecast++;
 res.close();
   
 //********************************************************************************************* 


 // Calculate N at age and size class from N at age and ALK all
FUNCTION void N_at_length(int y,int q,dvar_matrix N_bar, dvar3_array& N_l_bar);
 int s,a;
 int Lq,Ls,Ld,La,Ll;    //ALKS index counters
 Lq=index_Lq(y,q);
 //cout<<"N_at_length Lq:"<<Lq;
 for (Ld=ALKS_yq(Lq,2);Ld<=ALKS_yq(Lq,3);Ld++) {  // area 
   for (Ls=ALKS_yqd(Lq,2);Ls<=ALKS_yqd(Lq,3);Ls++) { // species
     s=ALKS_yqds(Ls,1);
     N_l_bar(s)=0.0;
     //cout<<" s:"<<s<<" start La:"<<ALKS_yqds(Ls,2)<<" endLa:"<<ALKS_yqds(Ls,3)<<endl;
     for (La=ALKS_yqds(Ls,2);La<=ALKS_yqds(Ls,3);La++) {
       a=ALKS_yqdsa(La,1);
       //cout<<" age:"<<a<<" start:"<<ALKS_yqdsa(La,2)<<" end:"<<ALKS_yqdsa(La,3)<<endl;
       if (s>=first_VPA) {
         if (L50(s)>0 && active(init_s1) ) {  
           for (Ll=ALKS_yqdsa(La,2);Ll<=ALKS_yqdsa(La,3);Ll++) {
             ALKS_adjusted(La,Ll)=ALKS_input(La,Ll)*(1+exp(s1(s)-s1(s)/L50(s)*ALKS_length(La,Ll)));
           }
           ALKS_adjusted(La)/=sum(ALKS_adjusted(La));  //adjust proportion at size to sum up to 1
           //cout <<"sp:"<<s<<" age:"<<a<<" Q:"<<q<<" s1:"<<s1(s)<<endl<<setfixed()<<setprecision(3)<< ALK(La)<<endl<<ALK_adjusted(La)<<endl<<endl;;
         } 
       }
       for (Ll=ALKS_yqdsa(La,2);Ll<=ALKS_yqdsa(La,3);Ll++) {
         N_l_bar(s,a,Ll)= ALKS_adjusted(La,Ll)*N_bar(s,a);
         //cout<<"N_l_bar(s,a,Ll):"<<N_l_bar(s,a,Ll)<<" ALKS_adjusted(La,Ll):"<<ALKS_adjusted(La,Ll)<<" N_bar_stom(yq,s,a):"<<N_bar_stom(yq,s,a)<<" sum key:"<<sum(ALKS_adjusted(La))<<endl; 
       }
 }}}
 
  
 //********************************************************************************************* 

FUNCTION void calc_M2_simple(int y, int q, int d, dvector other_food, dmatrix size, dmatrix prey_w, dmatrix consum, dvar_matrix N_bar, dvar_matrix& avail_food, dvar_matrix& M2);
 int observed=0;   // not observed stomach contents data
 int pred,pred_a,prey,prey_a,pred_l,prey_l;
 double pred_size,prey_size;
 dvariable tmp,tmp2;
 M2=0.0;
 avail_food=0.0;

 for (pred=1;pred<=npr;pred++) if (pred_area_present(pred,d)==1){
  for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
    if (N_bar(pred,pred_a)>0) { 
      //Calc available food
      pred_size=size(pred,pred_a);
      for (prey=first_VPA;prey<=nsp;prey++) {
        if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
            prey_size=size(prey,prey_a);
            if  ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                avail_food(pred,pred_a)+=N_bar(prey,prey_a)*prey_w(prey,prey_a)*suit(y,q,d,pred,prey,pred_size,prey_size,observed); 
      }}}}
      //cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" pred:"<<pred<<" age:"<<pred_a<<endl;
     avail_food(pred,pred_a)+=other_food(pred)*other_suit(pred,pred_size,y,q,d);  // add other food     
     tmp=N_bar(pred,pred_a)*consum(pred,pred_a)/avail_food(pred,pred_a);
       //calc M2
      for (prey=first_VPA;prey<=nsp;prey++) {
        if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {        
            prey_size=size(prey,prey_a);
             
             // just for testing
            // if (  (pred_size*prey_pred_size_fac(pred)>=prey_size) & (pred==14) & (prey==16)) {
            //   tmp2=tmp*suit(y,q,d,pred,prey,pred_size,prey_size,observed);
            //   if ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
            //     cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" pred:"<<pred<<" age:"<<pred_a<<"  available food minus other:"<<setprecision(0)<<avail_food(pred,pred_a)-other_food(pred);
             //    cout<<setprecision(2)<<" other_suit:"<<other_suit(pred,pred_size,y,q,d)<<setprecision(5)<<endl;
             //    cout<<"   partial M2, Prey:"<<prey<<" prey age:"<<prey_a<<" prey_size:"<< prey_size<<" part M2: "<<setprecision(3)<<tmp2<<endl<<endl;
             //   //cout<<"      suit:"<<suit(y,q,d,pred,prey,pred_size,prey_size,observed)<<"   tmp:"<<tmp<<setprecision(0)<<endl;
             //  }
             // }
            
            
            if ((pred_size*prey_pred_size_fac(pred))>=prey_size) {           
              M2(prey,prey_a)+=tmp*suit(y,q,d,pred,prey,pred_size,prey_size,observed);
              
              //if ((pred==14) & (prey==16) & (prey_a==3))  {
              //   cout<<"Year:"<<y<<" Q:"<<q<<"  Prey:"<<prey<<" prey age:"<<prey_a<<" prey_size:"<< prey_size <<"  pred_size: "<<pred_size;
              //   cout<<"  partial M2 "<< setprecision(4)<<tmp*suit(y,q,d,pred,prey,pred_size,prey_size,observed)<<endl;
              // }
            }
      }}}    
     }  
   }
 } 
  
FUNCTION void calc_M2_ALK(int y, int q, int d, dvector other_food, d3_array size,d3_array consum, dvar_matrix N_bar, dvar_matrix N, dvar_matrix F, dvar_matrix M1, dvar_matrix& M2);
 int pred,pred_a,prey,prey_a,pred_l,prey_l;
 int observed=0;   // not observed stomach contents data

 double pred_size,prey_size;
 dvar3_array   N_l_bar(1,nsp,minmaxAll_fa,minmaxAll_la,minlAll,maxlAll);           // average N at age and size class  SKAL LAVES OM TIL RAGGED
 dvar3_array   M2_l(first_VPA,nsp,minmaxVPA_fa,minmaxVPA_la,minlVPA,maxlVPA);   // M2 at age and size class          samme
 dvar3_array    Z_l(first_VPA,nsp,minmaxVPA_fa,minmaxVPA_la,minlVPA,maxlVPA);    // Z at age and size class           samme
 dvar3_array   avail_food_l(1,npr,minmaxPred_fa,minmaxPred_la,minlPred,maxlPred);   // available food at age and size group

 dvar_matrix    deadM1F(first_VPA,nsp,fa+0,la_VPA);
 dvar_matrix    deadM2(first_VPA,nsp,fa+0,la_VPA);
 dvar_matrix    deadM1M2F(first_VPA,nsp,fa+0,la_VPA);
 dvariable tmp, M1F;

 M2=0.0;
 for (prey=first_VPA;prey<=nsp;prey++) M2_l(prey)=0.0;
 N_at_length(y,q,N_bar,N_l_bar);  // get N at length group
 //for (prey=first_VPA;prey<=nsp;prey++) if (is_prey(prey)==1) cout<<"N_l_bar: prey:"<<prey<<endl<<setprecision(0)<<N_l_bar(prey)<<endl;
 //cout<<"year:"<<y<<" q:"<<q<<endl;
 for (pred=1;pred<=npr;pred++) if (pred_area_present(pred,d)==1) {
  for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
    //cout<<"pred:"<<pred<<" age:"<<pred_a<<endl;
    if (N_bar(pred,pred_a)>0) {                 
      //cout<<"pred:"<<pred<<" pred_a:"<<pred_a;;
      //Calc available food
      avail_food_l(pred)=0.0;
      for (pred_l=int(index_minl(yqd,pred,pred_a));pred_l<=int(index_maxl(yqd,pred,pred_a));pred_l++) {
       pred_size=size(pred,pred_a,pred_l);
        if (pred_size >0) { // test;
        //cout<<" pred_l: "<<pred_l<<" size: "<<setprecision(4)<<pred_size;
        for (prey=first_VPA;prey<=nsp;prey++) if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) { 
            for (prey_l=int(index_minl(yqd,prey,prey_a));prey_l<=int(index_maxl(yqd,prey,prey_a));prey_l++) {
              prey_size=size(prey,prey_a,prey_l);
              //cout<<" prey: "<<prey<<" prey_l:"<<prey_l<<" size:"<<setprecision(4)<<prey_size<<endl;
              if  ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                //cout<<"check: N_l_bar:"<<N_l_bar(prey,prey_a,prey_l)<<" suit: "<<setprecision(3)<<suit(y,q,pred,prey,pred_size,prey_size,observed)<<endl;
                avail_food_l(pred,pred_a,pred_l)+=N_l_bar(prey,prey_a,prey_l)*prey_size*suit(y,q,d,pred,prey,pred_size,prey_size,observed);  
         }}}}
 
         avail_food_l(pred,pred_a,pred_l)+=other_food(pred)*other_suit(pred,pred_size,y,q,d);  // add other food     
         tmp=N_l_bar(pred,pred_a,pred_l)*consum(pred,pred_a,pred_l) /avail_food_l(pred,pred_a,pred_l);  
         
         //calc M2 at age and size class
         for (prey=first_VPA;prey<=nsp;prey++) {
           if (pred_prey_comb(d,pred,prey)>0) {
             for (prey_a=faq(q);prey_a<=la(prey);prey_a++){ 
               for (prey_l=int(index_minl(yqd,prey,prey_a));prey_l<=int(index_maxl(yqd,prey,prey_a));prey_l++) {
                 prey_size=size(prey,prey_a,prey_l);
                 if ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                    //cout<<"pred:"<<pred<<" pred_a:"<<pred_a<<"pred_size:"<<pred_size<<" prey:"<<prey<<" prey_a:"<<prey_a<<" prey_l:"<<prey_l<<"prey_size:"<<prey_size<<" M2:"<< tmp*suit(y,q,d,pred,prey,pred_size,prey_size,observed)<<endl;
                    M2_l(prey,prey_a,prey_l)+=tmp*suit(y,q,d,pred,prey,pred_size,prey_size,observed);
         }}}}}
      }
     } 
   }
  } 
 }
 
 //for (prey=first_VPA;prey<=nsp;prey++) if (is_prey(prey)==1) cout<<"M2_l: prey:"<<prey<<endl<<setprecision(3)<<M2_l(prey)<<endl;

 //calc M2 at age
 deadM1F=0.0;
 deadM2=0.0;
 deadM1M2F=0.0;
 for (prey=first_VPA;prey<=nsp;prey++) if (is_prey(prey)==1) {
   for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
     M1F=M1(prey,prey_a)+F(prey,prey_a);
     for (prey_l=int(index_minl(yqd,prey,prey_a));prey_l<=int(index_maxl(yqd,prey,prey_a));prey_l++) {
       if (use_Nbar==0) {                    // VR傣L ???? Nej OK  Use N in the mean N to calc dead numbers
         Z_l(prey,prey_a,prey_l)=M1F+M2_l(prey,prey_a,prey_l);
         N_l_bar(prey,prey_a,prey_l)=N_l_bar(prey,prey_a,prey_l)*(1-exp(-Z_l(prey,prey_a,prey_l)))/Z_l(prey,prey_a,prey_l);   
       }
       deadM1F(prey,prey_a)+=N_l_bar(prey,prey_a,prey_l)*M1F;
       deadM2(prey,prey_a)+=N_l_bar(prey,prey_a,prey_l)*M2_l(prey,prey_a,prey_l);
     }
     if (deadM2(prey,prey_a)>0) {
        deadM1M2F(prey,prey_a)=deadM1F(prey,prey_a)+deadM2(prey,prey_a);
        M2(prey,prey_a)=log(N(prey,prey_a)/(N(prey,prey_a)-deadM1M2F(prey,prey_a)))*deadM2(prey,prey_a)/deadM1M2F(prey,prey_a);
      }
    }
 }
 
 //for (prey=first_VPA;prey<=nsp;prey++) if (is_prey(prey)==1) cout<<"M2: prey:"<<prey<<endl<<setprecision(3)<<M2(prey)<<endl;
 
FUNCTION void calc_M2_one_area(int y, int q);
 int s,d,pred;
 int yq;  // year quarter index
 int yqd;  // quarter division index
 d3_array size(1,nsp,fa,max_a,minl,maxl);            //length or weight of species at age in the sea 
 d3_array consum_local(1,nsp,fa,max_a,minl,maxl);    //consumption by age and length

 CALC_yq
 d=1; // one area only;
 CALC_yqd
 if (simple_ALK==0) {
   calc_M2_simple(y,q,d,AV_other_food(d),size_sea(yqd),size_sea_prey_w(yqd),consum(yqd),N_bar_stom(yq),avail_food(yqd),M2(yq));
   //if (yq==1) cout<<" M2:"<<endl<<setprecision(3)<<M2(yq)<<endl<<endl;
 
 }
 else {    //simple_ALK==1
   for (s=1;s<=nsp;s++) size(s)=size_l_sea(yqd,s);
   for (pred=1;pred<=npr;pred++) consum_local(pred)=consum_l(yqd,pred);
   calc_M2_ALK(y,q,d,AV_other_food(d),size,consum_local, N_bar_stom(yq),N(yq), F(yq), M1(yq), M2(yq));
 }  
 
  
FUNCTION void calc_M2_many_areas(int y, int q);
 int s,d,a,pred;
 int yq;  // year quarter index
 int yqd;  // quarter division index
 d3_array size(1,nsp,fa,max_a,minl,maxl);    //length or weight of species at age in the sea 
 d3_array consum_local(1,nsp,fa,max_a,minl,maxl);    //consumption by age and length

 dvar_matrix Nd(1,nsp,faq(q),la);    // Stock N by area 
 dvar_matrix Nd_bar(first_VPA,nsp,faq(q),la);    // Stock N by area 
 dvar_matrix M2d(first_VPA,nsp,faq(q),la_VPA);    // Stock N by area 
 dvar_matrix Zd(first_VPA,nsp,faq(q),la_VPA);    // Stock N by area 

 dvar_matrix DeadAll(first_VPA,nsp,faq(q),la);    // dead fish from all sources
 dvar_matrix DeadM2(first_VPA,nsp,faq(q),la);    // dead fish from M2
 DeadAll=0.0;
 DeadM2=0.0;
 
 CALC_yq

 for (d=1;d<=no_areas;d++) {
   CALC_yqd
   //cout<<"calc_M2_many_areas. y:"<<y<<" q:"<<q<<" area:"<<d<<" yqd:"<<yqd<<endl;
   //cout<<"N(yq):"<<endl<<setprecision(0)<<N(yq)<<endl;
   //cout<<"N_dist(yqd):"<<endl<<setprecision(3)<<N_dist(yqd)<<endl;

   Nd=elem_prod(N_dist(yqd),N(yq));
   //cout<<"Nd:"<<endl<<setprecision(0)<<Nd<<endl;

   if (simple_ALK==0) {
     calc_M2_simple(y,q,d,AV_other_food(d),size_sea(yqd),size_sea_prey_w(yqd),consum(yqd),Nd,avail_food(yqd),M2d);
   }
   else {   
       for (s=1;s<=nsp;s++) size(s)=size_l_sea(yqd,s);
       for (pred=1;pred<=npr;pred++) consum_local(pred)=consum_l(yqd,pred);
       calc_M2_ALK(y,q,d,AV_other_food(d),size,consum_local,Nd,Nd,F(yq),M1(yq),M2d);
   }
   
   //cout<<"M2d:"<<endl<<setprecision(3)<<M2d<<endl;
   Zd=M1(yq)+M2d+F(yq);
   //cout<<"Zd:"<<endl<<setprecision(3)<<Zd<<endl;
     
   for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) Nd_bar(s,a)=Nd(s,a)*(1.0-exp(-Zd(s,a)))/Zd(s,a);
   //cout<<"Nd_bar:"<<endl<<setprecision(0)<<Nd_bar<<endl;
   DeadM2+=elem_prod(Nd_bar,M2d);
   //cout<<"DeadM2:"<<endl<<setprecision(0)<<DeadM2<<endl;
   DeadAll+=elem_prod(Nd_bar,Zd);
   //cout<<"DeadAll:"<<endl<<setprecision(0)<<DeadAll<<endl;
 }
   
 M2(yq)=0.0;
 for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) for (a=faq(q);a<=la(s);a++) {
   if (DeadM2(s,a)>0) M2(yq,s,a)=log(N(yq,s,a)/(N(yq,s,a)-DeadAll(s,a)))*DeadM2(s,a)/DeadAll(s,a);
 }

 
FUNCTION void calc_M2(int y, int q);
 if (multi>=2) {
   if (no_areas==1) calc_M2_one_area(y,q);
   else calc_M2_many_areas(y,q);
 }

 // ***********************************************************************************  
FUNCTION void calc_part_M2(int y,int q);
 int yq, yqd;
 int pred,pred_a,prey,prey_a;
 int observed=0;
 double pred_size,prey_size;
 dvariable tmp;

 
 CALC_yq 
 d=1; // skal rettes
 CALC_yqd
 //cout<<endl<<endl<<"Year:"<<y<<" Quarter:"<<q<<endl;
 if (simple_ALK==0) {
   for (pred=1;pred<=npr;pred++){
     //cout<<setfixed()<<"N_bar_stom: pred:"<<pred<<" ";
     //cout<<N_bar_stom(yq,pred)<<endl;
     for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
      part_M2(pred,pred_a)=0.0;
      pred_size=size_sea(yqd,pred,pred_a);
      if ( (pred>=first_VPA)|| (pred<first_VPA && N_bar_stom(yq,pred,pred_a)>0)) { 
        //cout<<"pred:"<<pred<<" pred_a:"<<pred_a<<" Pred_size:"<<pred_size<<setprecision(3)<<" N_bar_stom:"<< N_bar_stom(yq,pred,pred_a)<<" Consum:"<<consum(yqd,pred,pred_a)<<" avail:"<<avail_food(yq,pred,pred_a);
        tmp=N_bar_stom(yq,pred,pred_a)*consum(yqd,pred,pred_a)/avail_food(yqd,pred,pred_a);
        //cout<<" tmp:"<<tmp<<endl;
        for (prey=first_VPA;prey<=nsp;prey++) {
          //cout<<"prey:"<<prey<<" pred_prey_comb:"<<pred_prey_comb(d,pred,prey)<<endl;
          if (pred_prey_comb(d,pred,prey)>0) {
            for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
              prey_size=size_sea(yqd,prey,prey_a);
              //cout<<" prey_a:"<<prey_a<<" prey_size:"<< prey_size<<" ratio:"<<pred_size/prey_size<<" pred_size*prey_pred_size_fac(pred):"<<pred_size*prey_pred_size_fac(pred);
              if ((pred_size*prey_pred_size_fac(pred))>=prey_size) part_M2(pred,pred_a,prey,prey_a)=tmp*suit(y,q,d,pred,prey,pred_size,prey_size,observed);
              //cout<<" M2:"<<setscientific()<<part_M2(pred,pred_a,prey,prey_a)<<" suit:"<<suit(y,q,d,pred,prey,pred_size,prey_size,observed)<<endl;
   }}}}}}
 }  

 //********************************************************************************************* 

FUNCTION dvariable suit(int y, int q, int d, int pred,int prey,double pred_size,double prey_size, int observed)
 dvariable tmp,size_sel,vul,ratio, log_ratio;

 //cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" pred:"<<pred<<" prey:"<<prey<<" ";
 vul=season_overlap(d,pred,q,prey)*vulnera(pred_prey_comb(d,pred,prey));

 if (use_overlap==1 && y<=lyModel) vul=vul*overlap(d,pred,y,q,prey);
 else if (use_overlap==2 && y>lyModel) vul=vul*overlap_forecast(d,pred,y,q,prey); 
 
 ratio=pred_size/prey_size;

 if (size_selection(pred)==0) {         // (uniform) no size selection
  if (observed==1) return vul; 
  else if ((ratio >= min_pred_prey_size_ratio(pred,prey)) && (ratio <= max_pred_prey_size_ratio(pred,prey))) return vul;
  else return 0.0;
 }
 else if (size_selection(pred)==4) {         // (confined uniform) no size selection, but within limits
  if (observed==1) return vul; else 
  { log_ratio=log(ratio);
  // if  (pred==14  && y==1995 && prey==16) {
  //   cout<<"prey:"<<prey<<"   pred size:"<<setprecision(3)<<pred_size<<"  prey size:"<<prey_size<<"  ratio:"<<ratio<<"  log_ratio:"<<log_ratio<<endl;
  //   cout<<"1:"<<size_range_a_b(1,pred,prey)<<" 2:"<<size_range_a_b(2,pred,prey)<<" 3:"<<size_range_a_b(3,pred,prey)<<" 4:"<<size_range_a_b(4,pred,prey);
  //   cout<<" lower:"<<size_range_a_b(1,pred,prey)+ size_range_a_b(2,pred,prey)*log(pred_size);
  //   cout<<" upper:"<<size_range_a_b(3,pred,prey)+ size_range_a_b(4,pred,prey)*log(pred_size)<<" vulnerability:";
  //   if ((log_ratio >= size_range_a_b(1,pred,prey)+ size_range_a_b(2,pred,prey)*log(pred_size))
  //   && (log_ratio <= size_range_a_b(3,pred,prey)+ size_range_a_b(4,pred,prey)*log(pred_size))) cout<<vul<<endl; else cout<<" 0"<<endl;
  //   cout<<endl;
  // }
   if ((log_ratio >= size_range_a_b(1,pred,prey)+ size_range_a_b(2,pred,prey)*log(pred_size))
     && (log_ratio <= size_range_a_b(3,pred,prey)+ size_range_a_b(4,pred,prey)*log(pred_size))) { return vul;}
   else { return 0.0;}
  }
 }
 else {           //  size selection
  if (ratio >= min_pred_prey_size_ratio(pred,prey) &&  ratio <= max_pred_prey_size_ratio(pred,prey)){    
      
    if (size_selection(pred)==1  || size_selection(pred)==2 ) { //normal distribution or assymmetric normal distribution
      tmp=log(ratio)-(pref_size_ratio(pred)*prey_size_adjustment(prey)+pref_size_ratio_correction(pred)*log(pred_size));     
      return vul*exp(-square(tmp)/(2.0* var_size_ratio(pred)));
    }
    else if (size_selection(pred)==3) {      //Gamma     1.0/(b^a*gamma(a))*x^(a-1)*exp(-x/b)
                                             // use gammln function: exp(-(log(b)*a+log(gamma(a)))+log(x)*(a-1)-x/b)
      tmp=log(ratio);
     // cout<<"gamma var:"<<var_size_ratio(pred)<<" pref: "<<pref_size_ratio(pred)<<" tmp:"<<tmp<<endl;                                                   
      size_sel= exp(-(log(var_size_ratio(pred))*pref_size_ratio(pred)+gammln(var_size_ratio(pred)))+log(tmp)*(pref_size_ratio(pred)-1.0)-tmp/pref_size_ratio(pred));
      return vul*size_sel;
    }
    else if (size_selection(pred)==5 || size_selection(pred)==6) {      //beta     gamma(a+b)/gamma(a)/gamma(b)*x^(a-1)*(1-x)^(b-1)
                                             // use gammln function: exp(lgamma(a+b)-lgamma(a)-lgamma(b)+log(x)*(a-1)+log(1-x)*(b-1))                                             
     //rescale ratio to [0;1]                                             
      ratio=(log(ratio)-all_min_pred_prey_size_ratio(pred))/all_range_pred_prey_size_ratio(pred); 
      size_sel=exp(gammln(pref_size_ratio(pred)+var_size_ratio(pred))-gammln(pref_size_ratio(pred))-gammln(var_size_ratio(pred))+log(ratio)*(pref_size_ratio(pred)-1.0)+log(1.0-ratio)*(var_size_ratio(pred)-1));
      //cout<<"pred:"<<pred<<" prey:"<<prey<<setprecision(3)<<" ratio:"<<ratio<<" size_sel:"<<size_sel<<" suit:"<<vul*size_sel<<endl;
      return vul*size_sel;
    }

    else return -1000.0;  // error
  }
  else return 0.0;
 }  
 

 //********************************************************************************************* 

FUNCTION dvariable other_suit(int pred,double pred_size,int y, int q, int d)
 dvariable tmp;

 tmp=exp(stl_other_suit_slope(pred)*log(pred_size/AV_other_food_size(pred)))*season_overlap(d,pred,q,0);
 //if (y==2011 and q==1) cout<<"pred: "<<pred<<" pred_size:"<<pred_size<<" AV_other_food_size:" <<AV_other_food_size(pred)<<" Suit:"<<tmp<<endl;

 if (use_overlap>1 && y<=lyModel) tmp*=overlap(d,pred,y,q,0); 
 else if (use_overlap==2 && y>lyModel) tmp*=overlap_forecast(d,pred,y,q,0); 
 
 return tmp;

//********************************************************************************************* 
  
FUNCTION N_at_length_like
 int s,y,q,d,a,l,ll;
 int sy,sq,sp,spl,splp; //stomach index counters
 int Ly,Lq,Ld,Ls,La,Ll;    //ALK index counters
 int yq;
 //if (y==2005 & q==3) cout<<"N_at_length_like"<<endl;
 for (Ly=1;Ly<=n_ALK_y;Ly++) {
   y=ALK_y(Ly,1);
   for (Lq=ALK_y(Ly,2);Lq<=ALK_y(Ly,3);Lq++) {
     q=ALK_yq(Lq,1);
     CALC_yq 
     for (Ld=ALK_yq(Lq,2);Ld<=ALK_yq(Lq,3);Ld++) {
       d=ALK_yqd(Ld,1);
       CALC_yqd
       //if (y==2005 & q==3) cout<<endl<<" yqd:"<<yqd<<" y:"<<y<<" q:"<<q<<" d:"<<d<<endl;
       N_l_bar_like(yqd)=0.0;
       for (Ls=ALK_yqd(Ld,2);Ls<=ALK_yqd(Ld,3);Ls++) {
         s=ALK_yqds(Ls,1); 
         // if (y==2005 & q==3) cout<<" s:"<<s;
         for (La=ALK_yqds(Ls,2);La<=ALK_yqds(Ls,3);La++) {
           a=ALK_yqdsa(La,1);
            //if (y==2005 & q==3) cout<<" a:"<<a<<" tot N_bar_stom:"<<setprecision(3)<<setfixed()<<N_bar_stom(yq,s,a)<<" tot N_bar:"<<N_bar(yq,s,a)<<setprecision(0)<<endl;
           if (L50(s)>0 && active(init_s1) ) {  
             for (Ll=ALK_yqdsa(La,2);Ll<=ALK_yqdsa(La,3);Ll++) ALK_adjusted(La,Ll)=ALK(La,Ll)*(1+exp(s1(s)-s1(s)/L50(s)*ALK_length(La,Ll)));
             ALK_adjusted(La)/=sum(ALK_adjusted(La));  //adjust proportion at size to sum up to 1
             //cout <<"sp:"<<s<<" age:"<<a<<" Q:"<<q<<" s1:"<<s1(s)<<endl<<setfixed()<<setprecision(3)<< ALK(La)<<endl<<ALK_adjusted(La)<<endl<<endl;;
           }
           // if (y==2005 & q==3) cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" s:"<<s<<" a:"<<a<<" minl:"<<ALK_yqdsa(La,2)<<" maxl:"<<ALK_yqdsa(La,3)<<endl;
           for (Ll=max(ALK_yqdsa(La,2),min_prey_length(d,s));Ll<=min(ALK_yqdsa(La,3),max_prey_length(d,s));Ll++) {     // CHECK BRUG AF min og max ????
             //if (y==2005 & q==3) cout<<" Ll:"<<Ll;
             N_l_bar_like(yqd,s,Ll)+= ALK_adjusted(La,Ll)*N_bar_stom(yq,s,a);
             //if (y==2005 & q==3) cout<<" N_l_bar:"<<N_l_bar_like(yqd,s,Ll)<<endl;
           }
        }
      }
      //cout<<" N_l_bar_like:"<<endl<<N_l_bar_like(yqd)<<endl;
     }
   }
 } 
 
 //cout<<"N_bar_stom:"<<endl<<N_bar_stom<<endl;
 //cout <<"N_l_bar_like:"<<endl<<setw(7)<<setprecision(0)<<setfixed()<<N_l_bar_like<<endl;
 //cout<<"N_bar_stom:"<<endl<<N_bar_stom(25,8,1)<<endl;
 //cout <<"N_l_bar_like:"<<endl<<setw(7)<<setprecision(0)<<setfixed()<<N_l_bar_like(8,1981,1)<<endl;
 
 // reorganise data
 stl_N_bar=0.0;
 for (sy=1;sy<=n_stl_y;sy++) { 
   y=stl_y(sy,1);
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
     q=stl_yq(sq,1); 
     for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
      d=stl_yqd(sd,1);
      CALC_yqd
      //cout<<endl<<" yqd:"<<yqd<<" y:"<<y<<" q:"<<q<<" d:"<<d<<endl;
      for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
       for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
         ll=0;
         for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
           s=stl_yqdplp(splp,1);
           for(l=stl_yqdplp(splp,2);l<=stl_yqdplp(splp,3);l++) {
              ll++;
              if (s>0)stl_N_bar(spl,ll)=N_l_bar_like(yqd,s,l);        
           }
         }
        // if (y==2005 & q==3) cout<<"stl_N_bar(spl):"<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" sp="<<sp<<" predator:"<<stl_yqdp(sp,1)<<" spl="<<spl<<" pred l:"<<stl_yqdpl(spl,1)<<endl<<stl_N_bar(spl)<<endl;
        }
      }
     
    }
   }
 }
 //cout <<endl<<"stl_N_bar N-bar at length for prey species:"<<endl<<setw(11)<<setprecision(0)<<setfixed()<<stl_N_bar<<endl;
 //********************************************************************************************* 


FUNCTION calc_expected_stomach_content
 int y,q,pred,prey,iprey_l;
 int observed=1;  // calculation of suitability of observations
 int first_prey_ll;
 double prey_s, pred_s;
 int sy,sq,sp,spl,splp,ll,first_ll;  //stomach index counters
 dvariable tmp,wstom;
 N_at_length_like();
 //calculate available food
 for (sy=1;sy<=n_stl_y;sy++) { 
   y=stl_y(sy,1);
   //cout<<"y:"<<y;
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
     q=stl_yq(sq,1); 
     //cout<<"  q:"<<q;
     for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) {
       d=stl_yqd(sd,1);
       //cout<<" area:"<<d;
       for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
         pred=stl_yqdp(sp,1);
         //cout<<" Pred:"<<pred;
         for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
           pred_s=pred_size(spl);
           //cout <<" Pred_size:"<<pred_s;
           ll=0;
           
           stl_avail_food(spl)=0.0;
           stl_NSumprey_avail(spl)=0.0;  // PL
           for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
             prey=stl_yqdplp(splp,1);
             first_prey_ll=ll+1;
             first_ll=-1;
             //cout<<"  prey:"<<prey<<" start L:"<<stl_yqdplp(splp,2)<<" End L:"<<stl_yqdplp(splp,3)<<endl;
             for (iprey_l=stl_yqdplp(splp,2);iprey_l<=stl_yqdplp(splp,3);iprey_l++) {
               ll++;
               if (prey>0) {
                if(stl_stom_use_avail(spl,ll)==1) { 
                    // if (iprey_l==stl_yqdplp(splp,2)) first_ll=ll;
                   if (first_ll== -1) first_ll=ll;

                   prey_s=prey_size(spl,ll);
                   if ( (size_select_model==1) || (size_select_model==2) || (size_select_model==5)) wstom=stl_wstom(spl,ll);
                   else wstom=prey_s;
                 
                   tmp=stl_N_bar(spl,ll)*wstom*suit(y,q,d,pred, prey, pred_s,prey_s,observed);
   
                   if (tmp==0) {
                        cout<<"Something is wrong !!! (prey availeble food=0.0  y:"<<y<<" q:"<<q<<" pred:"<<pred<<" pred_s:"<<pred_s;
                        cout<<" prey:"<< prey<<" prey_s:"<<setprecision(5)<<prey_s<<setprecision(3)<<" log(pred size/prey size):"<<log(pred_s/prey_s)<<endl;
                        cout<<"Number prey at length:"<<stl_N_bar(spl,ll)<<" mean weight:"<<setprecision(3)<<wstom<<" suitability "<<suit(y,q,d,pred, prey, pred_s,prey_s,observed)<<endl;;
                   }

                   stl_prey_avail_part(spl,ll)=tmp;   // just for output file "summary_stom.out"               
                   stl_avail_food(spl)+=tmp;

                   if (sumStomLike(pred)==0) {    // likelihood by observation, do not sum
                      stl_prey_avail(spl,ll)=tmp;
                   } else {   // likelihood by sum of prey lengths
                      if (ll==first_ll)  stl_prey_avail(spl,first_ll)=tmp;
                      else  stl_prey_avail(spl,first_ll)+=tmp;           // add within the full size range
                   }
 
                   if (do_number_like(pred)==1) {   // number model
                       stl_Nprey_avail(spl,ll)=stl_N_bar(spl,ll)*suit(y,q,d,pred, prey, pred_s,prey_s,observed);  // PL
                       stl_NSumprey_avail(spl,first_prey_ll)+=stl_Nprey_avail(spl,ll); 
                   } 
                 } // end use_avail
               } // end prey>0
               else if (prey==0) {
                 stl_prey_avail(spl,ll)=AV_other_food(d,pred)*other_suit(pred,pred_s,y,q,d);  // Other food
                 stl_prey_avail_part(spl,ll)=stl_prey_avail(spl,ll);
                 stl_avail_food(spl)+=stl_prey_avail(spl,ll); 
              }
           } // end prey size
         } //end prey species

         // We have now available food by prey length and total available food by pred length, all in weight (and in Number if needed)
         
         if (stl_avail_food(spl) ==0) {
           cout<<"something might be wrong, available food=0. y:"<<y<<" q:"<<q<<" pred:"<<pred<<" pred_s:"<< pred_s<<" prey:"<< prey<<endl;
         }
        
         ll=0;
         //if (do_number_like(pred)==1) cout<<"stl_E_Nstom(spl), pred="<<pred<<" size cl:"<<stl_yqdpl(spl,1)<<" y:"<<y<<" q:"<<q<<endl;
         for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
           first_prey_ll=ll+1;
           prey=stl_yqdplp(splp,1);
           for (iprey_l=stl_yqdplp(splp,2);iprey_l<=stl_yqdplp(splp,3);iprey_l++) {
              ll++;
              stl_E_stom(spl,ll)=stl_prey_avail(spl,ll)/stl_avail_food(spl);
              if (prey >0 && do_number_like(pred)==1) {
                if (stl_NSumprey_avail(spl,first_prey_ll)>0) stl_E_Nstom(spl,ll)=stl_Nprey_avail(spl,ll)/stl_NSumprey_avail(spl,first_prey_ll); 
                else {
                  cout<<"something might be wrong, availeble prey number=0. y:"<<y<<" q:"<<q<<" pred:"<<pred<<" pred_s:"<<stl_yqdpl(spl,1)<<" prey:"<<stl_yqdplp(splp,1)<<" prey s:"<<iprey_l<<endl;
                  stl_E_Nstom(spl,ll)=0;
                }
              }
           }
         }
          //cout<<" spl:"<<spl<<" Pred:"<<pred<<" Pred_size:"<<pred_s<<endl;
          //cout<<"stl_prey_avail(spl):"<<stl_prey_avail(spl)<<endl;
          //cout<<"stl_avail_food(spl):"<<stl_avail_food(spl)<<endl;
          //cout<<"stl_E_stom(spl):"<<stl_E_stom(spl)<<endl<<endl;
          //cout<<"stl_E_Nstom(spl):"<<endl<<stl_E_Nstom(spl)<<endl<<endl;
       }
      }
     }
   }
 }
 //cout<<"stl_avail_food: "<<stl_avail_food<<endl;
 //cout<<"stl_prey_avail_part: "<<stl_prey_avail_part<<endl;
 //cout<<"stl_E_stom:"<<endl<<stl_E_stom<<endl;
 //cout<<"stl_E_Nstom:"<<endl<<stl_E_Nstom<<endl;
 //********************************************************************************************* 


FUNCTION CPUE_parm_adm;
 int i,j;
  if (est_calc_sigma(2)==0) {
    for (i=1;i<=no_sp_fl;i++) for (j=1;j<=v_n_CPUE_s2_group(i);j++) qq_s2(i,j)=qq_s2_ini(i,j);   // move catchability uncertanties
  }
 

FUNCTION Rec_parm_adm;
 int ri,s;
 ri=0;
 for (s=first_VPA;s<=nsp;s++) if (use_beta_SSB_Rec(s)==1) {
     ri=ri+1;
    SSB_R_beta(s)=SSB_R_beta_ini(ri);
 }
 ri=0;
 for (s=first_VPA;s<=nsp;s++) {
    if (SSB_Rec_model(s)==51) { ri=ri+1; RecTempVar(s)=RecTempVar_ini(ri);}
    if (SSB_Rec_model(s)==52) RecTempVar(s)=Rec_add_inf(s,1);
 }

 // STN if (n_use_opt61_Rec>0) {  // STN
 // STN    if (R61Parms(1) !=0) alfa_61=R61Parms(1);   
 // STN    if (R61Parms(2) !=0) beta_61=R61Parms(2);
 // STN }
 // STN if (n_use_opt71_Rec>0) {  // STN
 // STN    if (R71Parms(1) !=0) alfa_71=R71Parms(1);   
 // STN    if (R71Parms(2) !=0) beta_71=R71Parms(2);
 // STN }



 if (est_calc_sigma(3)==0) SSB_R_s2=SSB_R_s2_ini;

  
FUNCTION predation_parm_adm;
 int pred,prey,q,i,j;
   
 if  (current_phase()>=stom_phase) {
   i=0; j=0;
   // reorganise size selection parameters
   for (pred=1;pred<=npr;pred++){
     if (size_selection(pred)!=0 && size_selection(pred)!=4) {
       i++;
       var_size_ratio(pred)=var_size_ratio_ini(i);
       j++;
       if (phase_pref_size_ratio_correction>-1) pref_size_ratio_correction(pred)=init_pref_size_ratio_correction(j);
       pref_size_ratio(pred)=init_pref_size_ratio(j);
     } 
   }

   if (active(init_prey_size_adjustment)) for (prey=first_VPA_prey;prey<=nsp-1;prey++) {
     prey_size_adjustment(prey)=init_prey_size_adjustment(prey);
   }

   // reorganise season_overlap parameters
   for(prey=0;prey<=nsp;prey++) if (prey==0 || prey>=first_VPA) for(d=1;d<=no_areas;d++) for (pred=1;pred<=npr;pred++) for (q=fq;q<=lq;q++)  {
     if (season_overlap_index(d,pred,q,prey)>0) {
      season_overlap(d,pred,q,prey)=init_season_overlap(int(season_overlap_index(d,pred,q,prey)));
     }
   }

  // reorganise Other food size dependency
   i=0;
   for (pred=1;pred<=npr;pred++){
     if (size_other_food_suit(pred)>0) {
       i++;
       stl_other_suit_slope(pred)=init_stl_other_suit_slope(i);
     }
   }

   if (mesh_size_active==1) {
     // reorganise mesh selction parameters
     i=0;
      for(prey=first_VPA;prey<=nsp;prey++) if (L50_mesh(prey)==0) {
       i++;
       L50(prey)=init_L50(i); 
     }
     
     i=0;
     for(prey=first_VPA;prey<=nsp;prey++) if (L50_mesh(prey)>=0) {
       i++;
       s1(prey)=init_s1(i); 
     }
   }

   if  (!(sd_phase()))  {
    if ((test_output==13 || test_output==19) && active(vulnera)) { 
     cout <<"################"<<endl<<setprecision(3) << setfixed()<<setw(12)<<endl; 
     cout <<"vulnera: "<<active(vulnera)<<endl<<vulnera<<endl;
     if (init_pref_size_ratio.indexmin()>0) cout <<"Prefered size ratio:          "<<active(init_pref_size_ratio(1))<<"     "<< pref_size_ratio<<endl;
       cout <<"Variance of size ratio      : "<<"     "<<var_size_ratio<<endl;
 
     cout <<setprecision(6) << setfixed()<<setw(12)<<endl; 
     cout <<"Other food suitability slope: "; 
     if (active(init_stl_other_suit_slope)) cout<<" 1 "; else cout<<" 0 ";
     cout<<endl<<init_stl_other_suit_slope<<endl;;
     cout<<"prey_size_adjustment:"<<active(init_prey_size_adjustment)<<endl<<prey_size_adjustment<<endl;
     cout <<setprecision(4) << setfixed()<<setw(12)<<endl; 
     cout <<"init_season_overlap:" <<active(init_season_overlap)<< endl<<init_season_overlap;
     for (pred=1;pred<=npr;pred++) { if (active(Stom_var(pred))) cout<<" 1 "; else cout<<" 0 ";}
     cout <<endl<<Stom_var;
     cout <<endl;
    }
     if (1==3) {
      cout << "Vulnerability pred - prey"<<endl;;
      cout<<"---------------------------";
      for (d=1;d<=no_areas;d++) {
       if (no_areas>1) cout<<endl<<"Area: "<<area_names[d]<<endl; else cout<<endl;
       cout<<"           Other-food     ";
       for (s=first_VPA;s<=nsp;s++) cout<<species_names[s];
       for (pred=1;pred<=npr;pred++) {
         cout <<endl<<species_names[pred]<<"    1.000";
         for (prey=first_VPA;prey<=nsp;prey++) {
           if (pred_prey_comb(d,pred,prey)==0) cout << setw(11) << 0;
           else cout << setw(11) << setprecision(3)  << vulnera(pred_prey_comb(d,pred,prey));
         }  
       }
       }
       cout <<endl<<endl;
    }
   }
  }

 //********************************************************************************************* 
FUNCTION void calc_F(int do_exploitaion_pattern)
 int s,y,q,a;
 int yq;
 int sp_sag_syg,sag_fa,sag,sag_la;
 int ly_group,syg,syg_fyModel,syg_ly;

 // Copy F_y              
  for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==0) {
    if (Nknots(s)>1) {   // splines are used
      vcubic_spline_function splinefn=vcubic_spline_function(knotsX(s),F_y_spline(s),0,0);
      for (y=fyModel+1;y<=lyModel;y++) F_y(s,y)=splinefn(y);
    } else {  // one value per year
      // copy F_y_ini to F_y
      for (y=fyModel+1;y<=lyModel;y++) {
        F_y(s,y)=F_y_ini(F_y_species_index(s),y);  // F_y(s,fyModel) is constant 1;}
        for(syg=1;syg<=n_catch_sep_year_group(s);syg++) {
          if (catch_sep_year(s,syg)==y) F_y(s,y)=1.0 ;
        }
      }
    }
  }
  // Copy F_q. F_q(s,g,lq_F(s,a)) is constant
 if (lq>fq) { // only seasonal data
    sp_sag_syg=0;
    for (s=first_VPA;s<=nsp;s++){
     for (sag=1;sag<=n_catch_season_age_group(s);sag++) {
      a=catch_season_age(s,sag);
      for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {                
         sp_sag_syg++;
         if (seasonal_annual_catches(s)==0 )     { for(q=fq_F(s,a);q<lq_F(s,a);q++) F_q(s,sag,syg,q)=F_q_ini(sp_sag_syg,q);}
         //else if (seasonal_annual_catches(s)==1){ for(q=fq_F(s,a);q<lq_F(s,a);q++) F_q(s,sag,syg,q)=F_q_ini_read(sp_sag_syg,q);}
      }
     }
    }
 }
      
  // Copy F_a, 
  for (s=first_VPA;s<=nsp;s++){
    for (a=cfa(s);a<=las(s);a++){
      for(syg=1;syg<=n_catch_sep_year_group(s);syg++) {
        if (syg==n_catch_sep_year_group(s)) ly_group=lyModel;
        else ly_group=catch_sep_year(s,syg+1)-1;
        //cout<<"s:"<<s<<" ly_group: "<<ly_group<<"  catch_sep_year(s,syg):"<<catch_sep_year(s,syg)<<endl;  
        for (y=catch_sep_year(s,syg);y<=ly_group;y++) {
            F_a(s,y,a)=exp(log_F_a_ini(s,a,syg-1));
            //if (do_effort(s)==1) if (y>catch_sep_year(s,syg)) {
              // if (use_creep(s)==2)      F_a(s,y,a)=F_a(s,y,a) + log(creep(s,a,syg-1))* (y-catch_sep_year(s,syg));  // creep line, expotential growth, kan slettes
              // else if (use_creep(s)==1) F_a(s,y,a)=F_a(s,y,a) + log(creep(s,cfa(s),syg-1)) * (y-catch_sep_year(s,syg));  // creep line, kan slettes
            //}
            //F_a(s,y,a)=exp(F_a(s,y,a));
         }
       }
     }
  // give the same selection pattern for ages with identical selection pattern
   if (las(s)<la(s)){
     for(a=las(s)+1;a<=la(s);a++) {
       for (y=fyModel;y<=lyModel;y++) {
         F_a(s,y,a)=F_a(s,y,las(s));
       }
     }
   }  
 }
 /*
 for (s=first_VPA;s<=nsp;s++){
   cout<<"Species:"<<s<<" "<<F_a(s,fyModel,fa)<<endl;
 } cout<<endl;
 */
 
 // Calc F, Separable model
  for (s=first_VPA;s<=nsp;s++){
    for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {         
      syg_fyModel=catch_sep_year(s,syg);
      if(syg==n_catch_sep_year_group(s)) syg_ly=lyModel;
      else syg_ly=catch_sep_year(s,syg+1)-1; 

      for (sag=1;sag<=n_catch_season_age_group(s);sag++) { 
        sag_fa=catch_season_age(s,sag);
        if (sag==n_catch_season_age_group(s)) sag_la=la(s);
        else sag_la=catch_season_age(s,sag+1)-1;
        for (y=syg_fyModel;y<=syg_ly;y++) {
          lqLocal=(y==lyModel)?lqly:lq;
          for (q=fq;q<=lqLocal;q++) {
           for (a=max(faq(q),sag_fa);a<=sag_la;a++)  {
               if (y==(lyModel-1)) F_q_last_year(s,q,a)=F_q(s,sag,syg,q); // copy for use in prediction 
              CALC_yq
              if (zero_catch_y_season(s,y,q)==1 || do_exploitaion_pattern==1) {
                 if (do_effort(s)==0) F(yq,s,a)=F_y(s,y)*F_q(s,sag,syg,q)*F_a(s,y,a);  // normal SMS
                 else  F(yq,s,a)=effort(s,q,y)*F_q(s,sag,syg,q)*F_a(s,y,a);         // effort model
              }
              else F(yq,s,a)=0.0;
             }
          }
        } 
      } 
    }
  } 
 //********************************************************************************************* 
 
FUNCTION void calc_Z(int y, int q)
 int yq;
 CALC_yq
 if ( ((current_phase()>=stom_phase) && (multi>=2))) {
   for (s=first_VPA;s<=nsp;s++) {
     if (is_prey(s)) Z(yq,s)=F(yq,s)+M1(yq,s)+M2(yq,s);
     else Z(yq,s)=F(yq,s)+M(yq,s);
   }
 }
 else {        // Calc Z single species
  for (s=first_VPA;s<=nsp;s++) Z(yq,s)=F(yq,s)+M(yq,s);
 }
 //cout<<"y:"<<y<<" q:"<<q<<endl<<setprecision(3)<<"M1:"<<endl<<M1(yq)<<endl<<"M2:"<<endl<<M2(yq)<<endl<<"F:"<<endl<<F(yq)<<endl<<"Z"<<Z(yq)<<endl;
 //********************************************************************************************* 

FUNCTION get_initial_N_at_age
 int s,y,a,q;
 int yq;

 for (s=first_VPA;s<=nsp;s++){
   // get recruits all year
   q=recq;
   for (y=fyModel;y<=lyModel;y++) {
     CALC_yq
     if (known_recruitment(s,y)>1)  N(yq,s,fa)=known_recruitment(s,y);
     else N(yq,s,fa)=mfexp(log_rec(s,y)+log_rec_scale(s));
   }
 
   // all ages first year
   y=fyModel;
   q=fq;
   CALC_yq
   for (a=fa+1;a<=la_like(s);a++) N(yq,s,a)=mfexp(log_rec_older(s,a)+log_rec_scale(s));

   //all ages older than la_like, first year
   // Estimate N for ages larger than la_like(s), assume 30% of younger age
    for (a=la_like(s)+1;a<=la(s);a++) N(yq,s,a)=N(yq,s,a-1)*0.3;   
 }



 //********************************************************************************************* 
 
FUNCTION void get_N_at_age(int y, int q) 
 //calc N for the next period (q=q+1 or y=y+1 and q=1)
 int s,a;
 int yq;
  
 CALC_yq
 // calculate N values (excluding recruits)
 for (s=first_VPA;s<=nsp;s++){
   if (q==lq) { // birthday
     for (a=fa;a<la(s);a++)  N(yq+1,s,a+1)=N(yq,s,a)*exp(-Z(yq,s,a));  
     if (nplus(s)==1) N(yq+1,s,la(s))+=N(yq,s,la(s))*exp(-Z(yq,s,la(s))); // plusgroup
   }
   else {  // quarter step
     //if (faq(q)==faq(q+1)) N(yq+1,s)=elem_prod(N(yq,s),exp(-Z(yq,s)));
     //else 
     for (a=faq(q);a<=la(s);a++) N(yq+1,s,a)=N(yq,s,a)*exp(-Z(yq,s,a));
   }
 }

 //********************************************************************************************* 

FUNCTION void get_N_bar_at_age(int y,int q)  
  //Calc N within the period
 int yq,s,a;
 CALC_yq
 
  //if (y==1974) {
    //cout<<endl<<endl<<"##### N y:"<<y<<" q:"<<q<<endl;
    //for (s=first_VPA;s<=nsp;s++)  cout<<"s:"<<s<<endl<<N(yq,s)<<endl;
   
    //cout<<"##### F y:"<<y<<" q:"<<q<<endl;
    //forf (s=first_VPA;s<=nsp;s++)  cout<<"s:"<<s<<endl<<setprecision(3)<<F(yq,s)<<endl<<setprecision(0);
  
    //cout<<"##### M2 y:"<<y<<" q:"<<q<<endl;
    //or (s=first_VPA;s<=nsp;s++)  cout<<"s:"<<s<<endl<<setprecision(3)<<M2(yq,s)<<endl<<setprecision(0);
  
    //cout<<"##### Z y:"<<y<<" q:"<<q<<endl;
    //for (s=first_VPA;s<=nsp;s++)  cout<<"s:"<<s<<endl<<setprecision(3)<<Z(yq,s)<<endl<<setprecision(0);
  // }
  
 // equation: N_bar=(1-exp(-Z))*N/Z;
 //for (s=first_VPA;s<=nsp;s++) N_bar(yq,s)= elem_prod(1-exp(-Z(yq,s)),elem_div(N(yq,s),Z(yq,s)));
   
  for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) N_bar(yq,s,a)=(1-exp(-Z(yq,s,a)))*N(yq,s,a)/Z(yq,s,a);
 

  
 //********************************************************************************************* 
 
FUNCTION void get_N_bar_stom_at_age(int y,int q)  
 int yq;
 CALC_yq
 

 //Calc N to be used to estimate M2
  if (use_Nbar==1) {for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) N_bar_stom(yq,s,a)=N_bar(yq,s,a)*N_prop_M2(yq,s,a);}
  else             {for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) N_bar_stom(yq,s,a)=N(yq,s,a)    *N_prop_M2(yq,s,a);}
 
 // if (y==1974 & q==1) {
 //  cout<<endl<<endl<<setprecision(0)<<"##### get_N_bar_stom_at_age   N  y:"<<y<<" q:"<<q<<endl;
 //  for (s=first_VPA;s<=nsp;s++)  cout<<"s:"<<s<<endl<<N(yq,s)<<endl; 
 //
 //  cout<<endl<<"##### get_N_bar_stom_at_age   N_bar_stom  y:"<<y<<" q:"<<q<<endl;
 //  for (s=first_VPA;s<=nsp;s++)  cout<<"s:"<<s<<endl<<N_bar_stom(yq,s)<<endl; 
 // }


 //cout<<"get_N_bar_stom_at_age: y="<<y<<" Q="<<q<<endl<<setprecision(0)<<N_bar_stom(yq)<<endl;
 
 //********************************************************************************************* 
 
FUNCTION get_biomass
 // calculate  TSB  and SSB
  int s,y,q,a;
  int yq,yqn;
  dvariable tmp;
  TSB=0.0;
  SSB=0.0;
  q=fq;
  for (s=first_VPA;s<=nsp;s++){ 
    for (y=fyModel;y<=lyModel+1;y++){
      CALC_yq
      yqn=yq; if (y>lyModel) yq=yq-lq;;
      for (a=faq(q);a<=la(s);a++) {
        // cout<<"CHECK: s"<<s<<" y:<"<<y<<" a:"<<a<<" yq:"<<yq<<" yqn:"<<yqn; cout<<" west:"<<west(yq,s,a)<<" propmat:"<< propmat(yq,s,a)<<" N:"<<N(yqn,s,a)<<endl;
        if ((a==fa) && (fq==recq)) tmp=N(yqn,s,a)*west(yq,s,a);
        else if (a>fa) tmp=N(yqn,s,a)*west(yq,s,a);
        else tmp=0.0;
        TSB(s,y)+=tmp;
        tmp=tmp*exp(-(prop_F(s)*F(yq,s,a)+prop_M(s)*M(yq,s,a)));
        SSB(s,y)+=tmp*propmat(yq,s,a); 
      }
    }
  }
 //cout<<"SSB:"<<endl<<SSB<<endl; 
 //********************************************************************************************* 
 
FUNCTION get_expected_catch_at_age
 int yq,s,y,q,a;
 
 // for (yq=1;yq<=(lyModel-fyModel+1)*lq;yq++) for (s=first_VPA;s<=nsp;s++) C_hat(yq,s)=elem_prod(F(yq,s),N_bar(yq,s));
 
 for (y=fyModel;y<=lyModel;y++) for (q=fq;q<=lq;q++) {
   CALC_yq
   for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) C_hat(yq,s,a)=F(yq,s,a)*N_bar(yq,s,a);
 }
  
 if (seasonal_annual_catches_any==1) {  // add seasonal catches, if needed
   for (y=fyModel;y<=lyModel;y++) {
     C_hat_annual(y)=0;
     for (q=fq;q<=lq;q++) {
       CALC_yq
       for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) if (zero_catch_y_season(s,y,q)==1) for (a=faq(q);a<=la(s);a++) C_hat_annual(y,s,a)+=C_hat(yq,s,a);
     }
    }
  }
   
 //********************************************************************************************* 

FUNCTION catchability_move
 int s, a, f, y, sp_fl;
 if (active(qq_ini) || (test_output==-1)) {
   sp_fl=0;
    // Move log_qq_ini to qq
     for (s=first_VPA;s<=nsp;s++){
       for (f=1; f<=n_fleet(s);f++) {
        sp_fl++;
        //cout<<"s:"<<s<<" f:"<<f<<endl;
           for (a=first_fleet_age(s,f);a<=last_fleet_age_q(s,f);a++) {
             qq(s,f,a)=qq_ini(sp_fl,a);
             if (last_fleet_age_power(s,f)>=a) {
               qq_power(s,f,a)=qq_power_ini(int(qq_power_index(s,f,a)));
             }
          }
          for (a=last_fleet_age_q(s,f)+1;a<=last_fleet_age(s,f);a++) {
            qq(s,f,a)=qq_ini(sp_fl,last_fleet_age_q(s,f));
            if (last_fleet_age_power(s,f)>=a) qq_power(s,f,a)=qq_power_ini(int(qq_power_index(s,f,a)));
         }
         
         //if (fleet_year_effect(s,f)==1) for (y=first_fleet_year(s,f)+1;y<=last_fleet_year(s,f);y++) {
         //   if (log_year_effect_index(s,f,y)>0) log_year_effect(s,f,y)=log(log_year_effect_ini(int(log_year_effect_index(s,f,y))));
         //}
       }
     } 
 
 }


 //********************************************************************************************* 

FUNCTION evaluate_catch_contributions
 int s, y, q, a, s2_fa, s2_la, s2_group, s2_common, yq, i;
 dvariable sum, tmp,bias_tmp,penalty,sum_penalty;
 dvariable x;
 i=lq-fq+1;
 dvar_vector sumx(1,i);
 dvar_vector sumx2(1,i);
 ivector no(1,i);   // number of observations
 double mins,epsilon,minsPlusEpsilon;

 if (active(F_y_ini) || active(log_rec) || active(log_F_a_ini)|| (test_output==-1)) {
 get_expected_catch_at_age();
 // Catch observation contributions
 mins=min_catch_CV*min_catch_CV;
 epsilon=mins/10;
 minsPlusEpsilon=mins+epsilon;

 for (s=first_VPA;s<=nsp;s++){
   //cout <<"s:"<<s<<"  F_q:"<<endl<<setfixed()<<setprecision(3)<<F_q(s)<<endl;
   tmp=0.0; 
   sum_penalty=0.0;
   s2_common=1;
   for (s2_group=1;s2_group<=n_catch_s2_group(s);s2_group++) {
     sumx=0.0; sumx2=0.0; no=0;sum=0;
     s2_fa=catch_s2_group(s,s2_group);
     if (s2_group==n_catch_s2_group(s)) s2_la=la_like(s);
     else s2_la=catch_s2_group(s,s2_group+1)-1;
     //if (s==28) cout<<"s2_fa:"<<s2_fa<<"  s2_la:"<<s2_la<<endl;

     for (y=fyModel;y<=lyModel;y++) {
        if (seasonal_annual_catches(s)==0) { 
          for (q=fq;q<=lq;q++) if (zero_catch_y_season(s,y,q)==1){ 
            CALC_yq
            //if (s==28)  cout<<"s: "<<s<<" y:"<<y<<" q:"<<q<<" C_hat: "<<C_hat(yq,s);

           if (seasonal_combined_catch_s2(s)>1) s2_common=q;    // the catch variance is calculated by season
           if (seasonal_annual_catches(s)==0) {
             for (a=s2_fa;a<=s2_la;a++) if (incl_catch_season_age(s,q,a)>0 && log_obs_C(yq,s,a)>1e-5) {
               lqLocal=(y==lyModel)?lqly:lq_F(s,a);
               if (q<=lqLocal) {
                 x=log(C_hat(yq,s,a))-log_obs_C(yq,s,a);
                 //if (s==28) cout<<"   X:"<<x<<endl;
                 sumx2(s2_common)+=square(x);
                 sumx(s2_common)+=x;
                 no(s2_common)+=1;
               } //lqLocal
             }  //a-loop
            } // Seasonal catches
          }  // zero catch
         }  // seasonale catches and quarter loop
         else {     // use annual catches in objective function, k鷢t er det ikke
           for (a=s2_fa;a<=s2_la;a++) {
             if (C_hat_annual(y,s,a) >0) {
               x=log(C_hat_annual(y,s,a))-log_obs_C_annual(y,s,a);
               //if (s==28)  cout<<"s: "<<s<<" y:"<<y<<" q:"<<q<<" C_hat: "<<C_hat_annual(y,s,a);
               //if (s==28) cout<<"  X:"<<x<<endl;
               sumx2(1)+=square(x);
               sumx(1)+=x;
               no(1)+=1;
             }
           }  //a-loop
         }  // annual catches 
      }   // year-loop
      //if (s==28) cout<<"sumx2: "<<sumx2<<"  sumx:"<<sumx<<endl;
      for (i=1;i<=seasonal_combined_catch_s2(s);i++) if (no(i)>0) {  
        if (est_catch_sigma ==1) {  // estimate sigma as a real parameter
           catch_s2(s,i,s2_group)=catch_s2_ini(s,i,s2_group);
           sum=no(i)*log(sqrt(catch_s2(s,i,s2_group)))+sumx2(i)*0.5/catch_s2(s,i,s2_group);    
           if (s>=min_first_VPA && s<=max_last_VPA) obf+=obj_weight(s,1)*sum;

        } 
        else {   //calculate sigma
          catch_s2(s,i,s2_group)=(no(i)*sumx2(i)-square(sumx(i)))/square(no(i));
          if (est_calc_sigma(1)==1) {  // truncate estimated variance if below limit            
              if (catch_s2(s,i,s2_group)<mins) {
                catch_s2(s,i,s2_group)=mins;
              }
              sum=no(i)*log(sqrt(catch_s2(s,i,s2_group)))+sumx2(i)*0.5/catch_s2(s,i,s2_group);    
              if (s>=min_first_VPA && s<=max_last_VPA) obf+=obj_weight(s,1)*sum;
          } 
          else {    // use penalty function
              if (catch_s2(s,i,s2_group)<minsPlusEpsilon) {
               penalty=pow(minsPlusEpsilon-catch_s2(s,i,s2_group),3)*1E12;
               //penalty=(minsPlusEpsilon-catch_s2(s,i,s2_group))*1E3;
                sum_penalty+=penalty;
               }
              else penalty=0.0;
  
              sum=no(i)*log(sqrt(catch_s2(s,i,s2_group)))+sumx2(i)*0.5/catch_s2(s,i,s2_group);    
              if (s>=min_first_VPA && s<=max_last_VPA)  obf+=obj_weight(s,1)*(sum+penalty);
           }
         }
        tmp+=sum;
     }  
   }  //s2_group
   //cout<<" obj function: species:"<<s<<" "<<tmp<<endl;
   obj_func(s,1)=tmp;
   obj_func(s,6)=sum_penalty;  
   
  } //species loop
 }  // if active

 
FUNCTION  evaluate_CPUE_contributions
 int s, y, q, a, f, s2_fa, s2_la, s2_group, no, sp_fl, nobs,localMaxFleetYear;
 int yq;
 dvariable sum,tmp,penalty,sum_penalty;;
 dvariable x,sumx2,sumx,N_survey;
 double mins,epsilon,minsPlusEpsilon,duration;

 if (active(qq_ini) || (test_output==-1)) {
  catchability_move();
  mins=CPUE_min_s2;
  epsilon=mins/10;
  minsPlusEpsilon=mins+epsilon;
  sp_fl=0;
  
  for (s=first_VPA;s<=nsp;s++){
   tmp=0.0; sum_penalty=0.0;
   for (f=1; f<=n_fleet(s);f++) {
      localMaxFleetYear=(int)min(last_fleet_year(s,f),lyModel+1); // to allow model last year +1 to be used
      q=fleet_season(s,f);
      if (lqly+1<lq && last_fleet_year(s,f)> lyModel) localMaxFleetYear=lyModel;
      sp_fl++;
      nobs=0;
      fleet_contribution(sp_fl)=0.0;
      duration=fleet_beta(s,f)-fleet_alfa(s,f);
      //cout<<"Species:"<<s<<" fleet:"<<f<<" dur:"<<duration<<"  last year used:"<<localMaxFleetYear<<endl;
      if (s>=first_VPA) for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
         sum=0.0; no=0; 
         sumx2=0.0; sumx=0.0;
         s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++) {
            for (y=max(fyModel,first_fleet_year(s,f));y<=localMaxFleetYear;y++){
                 CALC_yq
                if ((y<lyModel)|| (y==lyModel && q<=lqly) || 
                   (y==lyModel && q==(lqly+1) && duration==0) ||
                   (y>lyModel && q==fq && duration==0 && a>fa)) {
               //cout<<"survey obs from last assessment year plus 1 is used"<<endl;
               //if ((log_CPUE(sp_fl,y,a) < 998.0) && (!((a==fa) && (q <recq))) && (!((y==localMaxFleetYear)))) {
               if ( (log_CPUE(sp_fl,y,a) < 998.0) && (!((a==fa) && (q <recq))) )  {
                  if (duration==1) {N_survey=N_bar(yq,s,a); }
                  else if (duration>0) {
                    N_survey=N(yq,s,a)*exp(-Z(yq,s,a)*fleet_alfa(s,f)); // N as start of the survey period
                    N_survey=N_survey*(1-exp(-Z(yq,s,a)*duration))/(Z(yq,s,a)*duration); // mean N in the survey period
                  }
                  else if (fleet_alfa(s,f)==0 && fleet_beta(s,f)==0) {
                    N_survey=N(yq,s,a);
                    //cout <<"CPUE Y:"<<y<<" q:"<<q<<" a:"<<a<<" fleet:"<<f<<" "<<"  N_survey:"<<N_survey<<
                    //  " qq:"<<qq(s,f,a)<< " qq_power:"<< qq_power(s,f,a)<<" log_CPUE:"<<log_CPUE(sp_fl,y,a)<<endl;;
                  }
                  else if (fleet_alfa(s,f)==1 && fleet_beta(s,f)==1) N_survey=N(yq,s,a)*exp(-Z(yq,s,a));

                  x=log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a))-log_CPUE(sp_fl,y,a);
                  //x=log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a))+log_year_effect(s,f,y)-log_CPUE(sp_fl,y,a);
                  //x=log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a))+ log(qq_efficiency(s,f))*(y-first_fleet_year(s,f))-log_CPUE(sp_fl,y,a);
                  //cout<<"x: "<<x<<endl;
                  nobs++; 
                  no++;
                  sumx2+=square(x);
                  //cout <<"CPUE Y:"<<y<<" q:"<<q<<" a:"<<a<<" fleet:"<<f<<" "<<"  N_survey:"<<N_survey<<" qq:"<<qq(s,f,a)<< " qq_power:"<< qq_power(s,f,a)<<" log_CPUE:"<<log_CPUE(sp_fl,y,a)<<endl;;
                  //cout<<"s:"<<s<<" f:"<<f<<" sp_fl:"<<sp_fl<<" s2_group:"<<s2_group<<" age:"<<a<<" y:"<<y<<endl;
                  sumx+=x; 
                  CPUE_residuals(sp_fl,y,a)=-x;
               }
             }
           }
         }

         if (est_calc_sigma(2)!=0) qq_s2(sp_fl,s2_group)=(no*sumx2-square(sumx))/square(no);  // calc sigma
        
         if (est_calc_sigma(2)==1)   {  // truncate if below limit
            if (qq_s2(sp_fl,s2_group)<mins) qq_s2(sp_fl,s2_group)=mins;
         }
  
        if (est_calc_sigma(2)!=2) {  // option 0 and 1
          sum=no*log(sqrt(qq_s2(sp_fl,s2_group)))+sumx2*0.5/qq_s2(sp_fl,s2_group); // likelihood
          if (s>=min_first_VPA && s<=max_last_VPA) obf+=obj_weight(s,2)*sum;      // objective function
          obj_func(s,2)=sum;             
        }
                
        if (est_calc_sigma(2)!=0) {    // use penalty function
             if (qq_s2(sp_fl,s2_group)<(minsPlusEpsilon)) {
             // penalty=pow(minsPlusEpsilon-qq_s2(sp_fl,s2_group),3)*1E12;
             penalty=(minsPlusEpsilon-qq_s2(sp_fl,s2_group))*1E3;
               qq_s2(sp_fl,s2_group)= minsPlusEpsilon;
              sum_penalty+=penalty;
            }
            else penalty=0.0; 
            sum=no*log(sqrt(qq_s2(sp_fl,s2_group)))+sumx2*0.5/qq_s2(sp_fl,s2_group); // likelihood                  
            if (s>=min_first_VPA && s<=max_last_VPA)  obf+=obj_weight(s,2)*(sum+penalty);
          } 
          
          tmp+=sum;
          fleet_contribution(sp_fl)+=sum;

         }  // variance group end

       fleet_contribution_mean(sp_fl)=fleet_contribution(sp_fl)/nobs;   //mean contribution

   }  // fleet end
   obj_func(s,2)=tmp;
    
   obj_func(s,6)=obj_func(s,6)+sum_penalty;
  }  // species end
 }


FUNCTION  evaluate_SSB_recruitment_contributions
 int s, y,q;
 dvariable sum,penalty, delta,spawnersB, log_spawnersB,log_recruit;
 dvariable x,sumx,sumx2,sumxy,sumy,tmp,gamma,gamma2div4,beta;
 int no;
 double mins,epsilon;

  gamma=10; // constant from Benoit Mesnil and Marie-Jo螔le Rochet, A continious hocky stick stock-recruit model for estimating MSY reference points. ICES Journal of Marine Science, 67: June 8, 2010
  gamma2div4=gamma*gamma/4;
  
 if (active(SSB_R_alfa) || (test_output==-1)) {
   get_biomass(); 

   mins=min_SR_s2;
   epsilon=mins/10;

   for (s=first_VPA;s<=nsp;s++) if (use_known_rec_option_by_sp(s)==0) {
      
     sum=0.0;
     penalty=0.0;
     sumx=0.0; sumx2=0.0;
     delta=0.5;
     //no=SSB_year_last(s)-SSB_year_first(s)-fa+1;    
     no=0; 
      
      for (y=SSB_year_first(s);y<=SSB_year_last(s)-fa;y++) if (recruitment_years(s,y)==1) {       
       no++;
       spawnersB=SSB(s,y);     // SSB 
       log_spawnersB=log(SSB(s,y));
       log_recruit=log_rec(s,y+fa)+log_rec_scale(s);     // recruitment produced by SSB in year y 
       // x is log(observed recruit) minus log(predicted recruitment)
       
      // Ricker
       if (SSB_Rec_model(s)==1) x=log_recruit-(log(SSB_R_alfa(s))+log_spawnersB-SSB_R_beta(s)/SSB_R_beta_cor(s)*spawnersB);
       //if (s==28) cout<<"y:"<<y<<" "<<"SSB:"<<spawnersB<<" rec:"<<exp(log_recruit)<<" x:"<<x<<endl;
       // Ricker with Temperature (Sprat Baltic Sea)
       else if (SSB_Rec_model(s)==51 || SSB_Rec_model(s)==52 ) x=log_recruit-(log(SSB_R_alfa(s))+log_spawnersB-SSB_R_beta(s)/SSB_R_beta_cor(s)*spawnersB+RecTempVar(s)*temperature(y));

       // Ricker with Temperature Stefan STN (Sprat Baltic Sea)
       // else if (SSB_Rec_model(s)==61) {
       //   x=log_recruit-(log(alfa_61)+log_spawnersB-beta_61/SSB_R_beta_cor(s)*spawnersB+R61Parms(3)*T_61(y));
       // }
       
       
      // Speciial cod - Oxygen model, STN (Baltic Sea)
       //else if (SSB_Rec_model(s)==71) {
       //   x=log_recruit-(-alfa_71*spawnersB + beta_71/SSB_R_beta_cor(s)*spawnersB*O_71(y)+R71Parms(3));
       // }

        
       // Beverton
       else if (SSB_Rec_model(s)==2) {  
          x=log_recruit-(log(SSB_R_alfa(s))+log_spawnersB-log(1+SSB_R_beta(s)/SSB_R_beta_cor(s)*spawnersB));
        }
     
      //Geom mean. alfa is geo mean
       else if (SSB_Rec_model(s)==3) x=log_recruit-SSB_R_alfa(s);  
     
      // Hockey stick model  (alfa is used as log(slope), beta as log breakpoint)
       else if (SSB_Rec_model(s)==4) {   
           if (log_spawnersB<=SSB_R_beta(s)) x=log_recruit-(SSB_R_alfa(s)+log_spawnersB); else x=log_recruit-(SSB_R_alfa(s)+SSB_R_beta(s));
       }
       
      // Quadratic Hockey stick model  (alfa is used as slope, beta as log breakpoint)
      // Barrowman and Meyers, Can J. F. Aquat. Sci. 57: 665-676 (2000)
       else if (SSB_Rec_model(s)==5) {          
         if (log_spawnersB<=SSB_R_beta(s)+log(1.0-delta)) x=log_recruit-(SSB_R_alfa(s)+log_spawnersB);
         else if (log_spawnersB<SSB_R_beta(s)+log(1.0+delta)) 
            x=log_recruit-(SSB_R_alfa(s) + 
              log( spawnersB - ( (square(spawnersB-exp(SSB_R_beta(s))*(1.0-delta))/(4.0*delta*exp(SSB_R_beta(s)))))));           
         else x=log_recruit-(SSB_R_alfa(s)+SSB_R_beta(s));
       }
             
      // Hockey stick model with known breakpoint (alfa is used as log slope)
       else if (SSB_Rec_model(s)==100) {       
         if (SSB(s,y)<=SSB_Rec_hockey_breakpoint(s)) 
             x=log_recruit-(SSB_R_alfa(s)+log_spawnersB);      
         else x=log_recruit-(SSB_R_alfa(s)+log(SSB_Rec_hockey_breakpoint(s)));
       }
        
       sumx2+=square(x);
       sumx+=x;
       
       if (save_SSB_Rec_residuals==1) { // save residuals 
          SSB_Rec_nobs(s)=no;
          SSB_Rec_residuals(s,no)=value(x);
       }
     }
     

    if (save_SSB_Rec_residuals==0) {
       if (est_calc_sigma(3)!=0) SSB_R_s2(s)=(no*sumx2-square(sumx))/square(no);    // calc sigma
  
       if (est_calc_sigma(3)==1) {  // truncate
         if (SSB_R_s2(s)<mins) SSB_R_s2(s)=mins;            // truncate
       }
       
       if (est_calc_sigma(3)!=2) {  // option 0 and 1
         sum=no*log(sqrt(SSB_R_s2(s)))+sumx2*0.5/SSB_R_s2(s);   // likelihood   
         if (s>=min_first_VPA && s<=max_last_VPA) obf+=obj_weight(s,3)*sum;      // objective function
         obj_func(s,3)=sum;             
       }
       
       if (est_calc_sigma(3)==2) {   // calc sigma and use penalty function
          if (SSB_R_s2(s)<(mins+epsilon)) {
        //    penalty=pow(mins+epsilon-SSB_R_s2(s),3)*1E8;
            penalty=(mins+epsilon-SSB_R_s2(s))*1E3;
  
            SSB_R_s2(s)=mins;
         } else penalty=0.0; 
         sum=no*log(sqrt(SSB_R_s2(s)))+sumx2*0.5/SSB_R_s2(s);   // likelihood  
         if (s>=min_first_VPA && s<=max_last_VPA) obf+=obj_weight(s,3)*(sum+penalty);
         obj_func(s,6)+=penalty;
         obj_func(s,3)=sum;             
       }
     }  // end (NOT) save recruitment residual
      // cout<<"SR contribution, s:"<<s<<" "<<setfixed()<<setprecision(4)<<sum<<endl; 
   }  // end species loop
 }

  
FUNCTION dvariable calc_stom_var(int pred,int spl,int ll)
 dvariable tmp,stom;
 stom=stl_E_stom(spl,ll);
 if (stom>0.99) stom=0.99;

 if (stomach_variance==1) {         // log-normal error
   //cout<<" stom:"<<stom<<" Stom_var:"<<Stom_var(pred)<<"  Stom_var_fac:"<<Stom_var_fac(pred)<<"  no samples:"<<stl_no_samples(spl,ll)<<endl;
   if (StomObsVar(pred)==1)         tmp=log(0.5+sqrt(0.25+(1.0/stom-1.0)/(Stom_var(pred)*Stom_var_fac(pred)*stl_no_samples(spl,ll))));
   else if (StomObsVar(pred)==0)    tmp=log(0.5+sqrt(0.25+(1.0/stom-1.0)/(                                  stl_no_samples(spl,ll))));
   else if (StomObsVar(pred)==2)    tmp=log(0.5+sqrt(0.25+(1.0/stom-1.0)));

 }
 else if (stomach_variance==2) {    //normal distributed error
   if (StomObsVar(pred)==1)        tmp=Stom_var(pred)*Stom_var_fac(pred)*stom*(1.0-stom)/stl_no_samples(spl,ll);
   else if (StomObsVar(pred)==0)   tmp=                                  stom*(1.0-stom)/stl_no_samples(spl,ll);
   else if (StomObsVar(pred)==2)   tmp=                                  stom*(1.0-stom);
 }
 else if (stomach_variance==3) {    //structure suitable for Dirichlet
   if (StomObsVar(pred)==1)       tmp=Stom_var(pred)*Stom_var_fac(pred)*stl_no_samples(spl,ll)-1;
   else if (StomObsVar(pred)==0)  tmp=                                  stl_no_samples(spl,ll)-1;
   else if (StomObsVar(pred)==2)  tmp=                                                         1;
  
   if (tmp<=0) {
     cout<<"ERROR: Negative variance in Dirichlet distributions."<<endl;
     cout<<"pred:"<<pred<<" spl:" <<spl<<" no of samples:"<<stl_no_samples(spl,ll)<<" Stom_var:"<<setprecision(4)<<Stom_var(pred)<<" product:"<<Stom_var(pred)*stl_no_samples(spl,ll)<<endl;
     exit(9);
   }  
 }
 return tmp;

 
//********************************************************************************************* 
FUNCTION  cacl_like_stom_number_multinomial;
 int s,q,d,first_prey_ll;
 int sy,sq,sp,spl,splp,ll,pred,prey_l,prey;  //stomach index counters

 dvariable sum,p;


 // done already for (s=1;s<=nsp;s++) obj_func(s,5)=0.0;

 for (sy=1;sy<=n_stl_y;sy++) {       
     //cout<<"Year:"<<stl_y(sy,1)<<endl;
    if (stl_y(sy,1)>=fyModel) for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
       q=stl_yq(sq,1); 
       // cout<<" quarter:"<<q<<endl;
       for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) {
         d=stl_yqd(sd,1);
         // cout<<"  area:"<<d<<endl;
         for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) if (do_number_like(stl_yqdp(sp,1))==1) {
          pred=stl_yqdp(sp,1);
          // cout<<"    pred:"<<pred<<endl;
          if (do_number_like(pred)==1) for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
            // cout<<"     pred_l:"<<stl_yqdpl(spl,1)<<endl;

           if (incl_stom(d,pred,q,stl_yqdpl(spl,1),sy)>=1) { //for the predator and predator length group, include stomach observations in likelihood 0=no inlusion, >=1 include data
             ll=0;
             for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
               prey=stl_yqdplp(splp,1);
               // cout << "       Prey:"<<stl_yqdplp(splp,1);

               if (prey>0) { 
                   first_prey_ll=ll+1; 
                   sum=Prey_number_fac_term(spl,first_prey_ll);  // fixed part of multinomial likelihood  (already negative)
               }
               for(prey_l=stl_yqdplp(splp,2);prey_l<=stl_yqdplp(splp,3);prey_l++) {
                 ll++;
                 // cout <<"      Len:"<<prey_l<< " ";
                 //if (stl_stom_use_like(spl,ll)==1) {
                 if (prey>0) {
                    if (stl_E_Nstom(spl,ll)> 0) sum-= stl_nopreystom(spl,ll)*log(stl_E_Nstom(spl,ll));
                    else if (stl_nopreystom(spl,ll)>0) cout<<"Something is wrong, prey number>0, but none estimated! pred:"<<pred<<" pred_l:"
                          <<stl_yqdpl(spl,1)<<" y:"<<stl_y(sy,1)<<" q:"<<q<<" prey:"<<prey<<" prey_l:"<<prey_l<<setfixed()<<setprecision(0)<<"  stl_nopreystom="<<stl_nopreystom(spl,ll)<< "  stl_E_Nstom="<<setfixed()<<setprecision(6)<<stl_E_Nstom(spl,ll)<<endl;
                 }
               }  // end prey length
               if (prey>0) {                                                                          
                 Prey_number_like(spl,first_prey_ll)=value(sum); 
                 obf+=obj_weight(pred,5)*sum; 
                 obj_func(pred,5)+=sum;
                //cout<<endl<<"Likelihood contribution:"<<sum<<endl;
              }
             } //end prey species
            }  // inclusion of stomach samples
          }   // end predator length
       }      // end incl predator
      }       // end area
    }         // end Quarter
 }            // end year
 
FUNCTION  evaluate_stomach_contributions
 int s,q,d,first_Dirichlet;
 int sy,sq,sp,spl,splp,ll,pred,prey_l;  //stomach index counters

 dvariable sum,samp_var,sum_of_squares,p;
 calc_expected_stomach_content();
 // allready done. for (s=1;s<=nsp;s++) obj_func(s,4)=0.0;

 for (sy=1;sy<=n_stl_y;sy++) {       
     //cout<<"Year:"<<stl_y(sy,1)<<endl;
    if (stl_y(sy,1)>=fyModel) for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) {
       q=stl_yq(sq,1); 
       // cout<<" quarter:"<<q<<endl;
       for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) {
         d=stl_yqd(sd,1);
         // cout<<"  area:"<<d<<endl;
         for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
          pred=stl_yqdp(sp,1);
          // cout<<"    pred:"<<pred<<endl;
          for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
             //cout<<"     pred_l:"<<stl_yqdpl(spl,1)<<endl;
         
           if (incl_stom(d,pred,q,stl_yqdpl(spl,1),sy)>=1) { //for the predator and predator length group, include stomach observations in likelihood 0=no inlusion, >=1 include data
             ll=0;
             sum=0.0;
             first_Dirichlet=1;

             for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
               //cout << "       Prey:"<<stl_yqdplp(splp,1);
               for(prey_l=stl_yqdplp(splp,2);prey_l<=stl_yqdplp(splp,3);prey_l++) {
                 ll++;
                  //cout <<"      Len:"<<prey_l<< " ";
                  if (first_Dirichlet==1 && (stomach_variance==3) ) {
                   samp_var=calc_stom_var(pred,spl,ll); 
                   first_Dirichlet=0;
                 }
                 
                 if (stl_stom_use_like(spl,ll)==1) {
                   if (stomach_variance==1){ 
                     samp_var=calc_stom_var(pred,spl,ll); 
                     // cout<<"Observed stom: "<<setprecision(10)<<exp(log_stl_stom(spl,ll))<<" Expected stom:"<<stl_E_stom(spl,ll)<<" samp_var:"<<samp_var;
                     
                     sum_of_squares=square(log_stl_stom(spl,ll)-log(stl_E_stom(spl,ll)));
                     sum+=log(sqrt(samp_var))+sum_of_squares*0.5/samp_var;
                      //cout<<"  contribution:"<<log(sqrt(samp_var))+sum_of_squares*0.5/samp_var<<endl;
                   }
                   else if (stomach_variance==2){
                     samp_var=calc_stom_var(pred,spl,ll); 
                     sum_of_squares=square(stl_stom(spl,ll)-stl_E_stom(spl,ll));
                     sum+=log(sqrt(samp_var))+sum_of_squares*0.5/samp_var;
                   }
                   else if (stomach_variance==3){   //Dirichlet
                      if (stl_E_stom(spl,ll)<1E-7) {
                     //   cout<<"Something might be wrong (Expected stomach contents<1E-7) !!! Year:"<<stl_y(sy,1)<<" quarter:"<<q<<" pred:"<<pred<<" pred_l:"<<stl_yqdpl(spl,1)<<" Prey:"<<stl_yqdplp(splp,1)<<" Len:"<<prey_l<<endl;
                      //  cout<<"  min no of samples: "<<setprecision(0)<<min_no_samples(pred)<<"  no of stomachs:"<<stl_no_samples(spl,ll)<<setprecision(6);
                      //  cout<<" Stom_var_fac: "<<Stom_var_fac(pred);
                      //  cout <<" Observed stom: "<<setprecision(10)<<exp(log_stl_stom(spl,ll))<<" Expected stom:"<<stl_E_stom(spl,ll)<<" samp_var:"<<setprecision(5)<<samp_var<<endl;
                       stl_E_stom(spl,ll)=1E-6;
                       }
                      p=samp_var*stl_E_stom(spl,ll);
                      sum+=gammln(p)-(p-1)*log_stl_stom(spl,ll);
                      if (p==0) {
                        cout<<"Something is wrong (p=0.0) !!! Year:"<<stl_y(sy,1)<<" quarter:"<<q<<" pred:"<<pred<<" pred_l:"<<stl_yqdpl(spl,1)<<" Prey:"<<stl_yqdplp(splp,1)<<" Len:"<<prey_l<<endl;
                        cout<<"  min no of samples: "<<setprecision(0)<<min_no_samples(pred)<<"  no of stomachs:"<<stl_no_samples(spl,ll)<<setprecision(6);
                        cout<<" Stom_var_fac: "<<Stom_var_fac(pred);
                        cout <<" Observed stom: "<<setprecision(5)<<exp(log_stl_stom(spl,ll))<<" Expected stom:"<<stl_E_stom(spl,ll)<<" samp_var:"<<samp_var<<" p:"<<p<<endl;
                      }
                   }
                 }
                 
               }  // end prey length
             } //end prey species
             //cout<<endl<<"Likelihood contribution:"<<sum<<endl;
             if (stomach_variance==3){
                 sum += -gammln(samp_var);    //Dirichlet  
                 sum_p_Dirichlet(spl)=samp_var;                  
                 like_Dirichlet(spl)=sum;
             }
             obf+=obj_weight(pred,4)*sum; 
             obj_func(pred,4)+=sum;
            }  // inclusion of stomach samples
          }   // end predator length
       }      // end predator
      }       // end area
    }         // end Quarter
 }            // end year
 if (do_number_like_any==1) cacl_like_stom_number_multinomial();
 


FUNCTION test_out_test
 int y,q,s;
 int yq;
 
 ofstream res("test.out",ios::app);
  for (s=first_VPA;s<=nsp;s++){
   res<<"############################"<<endl<<"Species:"<<s<<endl;
   for (y=fyModel;y<=lyModel;y++){
    res<<"Year:"<<y;
    for (q=fq;q<=lq;q++){
     res<<" q:"<<q<<endl;
     CALC_yq
     res<<"obs_C:"<<obs_C(yq,s)<<endl;
     res<<"C_hat:"<<C_hat(yq,s)<<endl;
     res<<"M    :"<<M(yq,s)<<endl;
     res<<"F    :"<<F(yq,s)<<endl;
     res<<"Z    :"<<Z(yq,s)<<endl;
     res<<"N    :"<<N(yq,s)<<endl;
     res<<"N_bar:"<<N_bar(yq,s)<<endl;

   }}}
  res.close(); 

FUNCTION evaluate_the_objective_function
 obj_func=0.0;
 evaluate_catch_contributions();
 evaluate_CPUE_contributions();
 evaluate_SSB_recruitment_contributions();
 if  ((multi>=1 && current_phase()>=stom_phase) || (test_output==-1)) evaluate_stomach_contributions();

 //********************************************************************************************* 

FUNCTION move_M2_to_sdreport
   dvariable tmp;     // sum of M2 (forkert avg M2 rettes senere)
   int s,y,a,q,i;
   int yq;
   
   a=fa; // set age and change first quarter
   i=0;
   for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) {
       i++;
      for (y=lyModel-sdReportYear;y<=lyModel;y++){
        tmp=0.0;
        for (q=recq;q<=lq;q++){
          CALC_yq
          tmp+=M2(yq,s,a);
        }
       M2_sd0(i,y)=tmp;
      }
    }
   // age 1 M2   
   a=fa+1;
   i=0;
   for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) {
       i++;
      for (y=lyModel-sdReportYear;y<=lyModel;y++){
        tmp=0.0;
        for (q=1;q<=lq;q++){
          CALC_yq
          tmp+=M2(yq,s,a);
        }
       M2_sd1(i,y)=tmp;
      }
    }
     // age 2 M2  ( I am lazy, I know!) 
   a=fa+2;
   i=0;
   for (s=first_VPA;s<=nsp;s++) if (is_prey(s)==1) {
       i++;
      for (y=lyModel-sdReportYear;y<=lyModel;y++){
        tmp=0.0;
        for (q=1;q<=lq;q++){
          CALC_yq
          tmp+=M2(yq,s,a);
        }
       M2_sd2(i,y)=tmp;
      }
    }
 //********************************************************************************************* 
 
FUNCTION move_recruitment_to_sdreport
 int s,y,yq,q;
  q=recq;
  for (y=lyModel-sdReportYear;y<=lyModel;y++) {
    CALC_yq
    for (s=first_VPA;s<=nsp;s++) rec_sd(s,y)=N(yq,s,fa);

  }
 
    
 //********************************************************************************************* 

FUNCTION calc_avg_F
 int s,y,a,q;
 int yq;
 dvariable tmp,deadM1,deadM2,deadM, deadC, sumZ, meanN;
 int rc;
 for (s=first_VPA;s<=nsp;s++){
   for (y=fyModel;y<=lyModel;y++){
     tmp=0.0;
     for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) {
      deadM1=0.0; deadM2=0.0; deadC=0.0; deadM=0.0; sumZ=0.0;
      if (a==fa)rc=recq;
      else rc=fq;

      for (q=rc;q<=lq;q++){
         CALC_yq 
         if (Z(yq,s,a)>0.0) meanN=N(yq,s,a)*(1-exp(-Z(yq,s,a)))/Z(yq,s,a);
         else meanN=N(yq,s,a);
         if (multi>=1) {
            deadM1+=meanN*M1(yq,s,a);
            if (use_Nbar==0) deadM2+=N(yq,s,a)*M2(yq,s,a);
            else deadM2+=meanN*M2(yq,s,a);
         }
         else deadM+=meanN*M(yq,s,a);
         deadC+=meanN*F(yq,s,a);
         sumZ+=Z(yq,s,a);
       }
     
       if (multi>=1) tmp+=deadC/(deadM1+deadM2+deadC)*sumZ;
       else tmp+=deadC/(deadM+deadC)*sumZ;
     }
     Mean_F(s,y)=tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
     Mean_F_percieved(s,y)=Mean_F(s,y);
     if (sd_phase() && (y>=lyModel-sdReportYear)) {       
       avg_F(s,y)=Mean_F(s,y);
      }
   }
 }

               
  // *****************************************************************************************
FUNCTION dvariable SSB_recruit_last(int s);
  dvariable stock;
  if (lqly==lq) stock=SSB(s,lyModel+1-fa); else stock=0;
  switch(SSB_Rec_model(s)) {
           case 1:  return(SSB_R_alfa(s)*stock*exp(-SSB_R_beta(s)/SSB_R_beta_cor(s)*stock));   // Ricker
                    //break;
           case 51:  return(SSB_R_alfa(s)*stock*exp(-SSB_R_beta(s)/SSB_R_beta_cor(s)*stock+RecTempVar(s)*temperature(lyModel)));  // Ricker with Temperature (Sprat Baltic Sea)
           case 52:  return(SSB_R_alfa(s)*stock*exp(-SSB_R_beta(s)/SSB_R_beta_cor(s)*stock+RecTempVar(s)*temperature(lyModel)));  // Ricker with Temperature (Sprat Baltic Sea)

           case 2: return(SSB_R_alfa(s)*stock/(1+SSB_R_beta(s)/SSB_R_beta_cor(s)*stock));     // Beverton & Holt
                     //break;
           case 3: return(exp(SSB_R_alfa(s)));                                                // Geom mean
                    //break;
           case 4:  if (stock>=SSB_R_beta(s)/SSB_R_beta_cor(s)) {
                      return(SSB_R_beta(s)/SSB_R_beta_cor(s)*SSB_R_alfa(s));
                    }  
                    else return(exp(SSB_R_alfa(s))*stock);
                    //break;

                     
           case 100: if (stock>SSB_Rec_hockey_breakpoint(s)) {
                       return(SSB_Rec_hockey_breakpoint(s)*exp(SSB_R_alfa(s)));
                     }
                     else return(exp(SSB_R_alfa(s))*stock);
                     //break;
           default: return(-1.0);  // error
           //break;
  }  
             
 
FUNCTION calc_term_F
 int s,y,a,q,sa;
 int yq;
 dvariable tmp,deadM1,deadM2,deadM, deadC, sumZ, meanN;
 int rc,i;

 sa=0;   
 y=lyModel;
 for (s=first_VPA;s<=nsp;s++){
     tmp=0.0;
     for (a=a=cfa(s);a<=la(s);a++) {
      deadM1=0.0; deadM2=0.0; deadC=0.0; deadM=0.0; sumZ=0.0;
      if (a==fa) rc=recq;
      else rc=fq;

      for (q=rc;q<=lq;q++){
         CALC_yq
         if (Z(yq,s,a)>0.0) meanN=N(yq,s,a)*(1-exp(-Z(yq,s,a)))/Z(yq,s,a);
         else meanN=N(yq,s,a);
         if (multi>=1) {
            deadM1+=meanN*M1(yq,s,a);
            if (use_Nbar==0) deadM2+=N(yq,s,a)*M2(yq,s,a);
            else deadM2+=meanN*M2(yq,s,a);
         }
         else deadM+=meanN*M(yq,s,a);
         deadC+=meanN*F(yq,s,a);
         sumZ+=Z(yq,s,a);
      }

      if (multi>=1) tmp=deadC/(deadM1+deadM2+deadC)*sumZ;
      else tmp=deadC/(deadM+deadC)*sumZ;
      sa++;
      term_F(sa)=tmp;
      if (tmp>0) log_term_F(sa)=log(tmp);
     }
 }
 
 
 exploi_pattern=0;
 i=0;
 y=lyModel;
 for (q=fq;q<=lq;q++){
   i=0;
   CALC_yq
   for (s=first_VPA;s<=nsp;s++){
     for (a=cfa(s);a<=la(s);a++) {
       i++;
       if (!(a==fa && q<recq)) {
          exploi_pattern(q,i)=F(yq,s,a);
       }
      }
    }
  }   
 

 


FUNCTION calc_term_N
   int y,q,s,a,sa,yq;
   
   // N in the terminal year, first quarter
   sa=0;
   y=lyModel;
   q=fq;
   CALC_yq

   for (s=first_VPA;s<=nsp;s++){
     for (a=faq(q);a<=la_VPA(s);a++) {
       sa++;  // to get rid of non recruited ageclass
       term_N(sa)=N(yq,s,a);
     }
   }
  // calculate SSB in  year lyModel+1, or alternatively in year lyModel last q +1 
  if (lqly==lq) {
    y=lyModel+1;
    q=fq;
  } else {
    y=lyModel;
    q=lqly+1;
  }
  CALC_yq
  for (s=first_VPA;s<=nsp;s++) {
    SSB(s,lyModel+1)=0.0;
    TSB(s,lyModel+1)=0.0;
    for (a=faq(q);a<=la_VPA(s);a++) {
      if (lqly==lq) {
        SSB(s,lyModel+1)+=N((lyModel+1-fyModel)*lq+fq,s,a)*west((lyModel-fyModel)*lq+fq,s,a)*propmat((lyModel-fyModel)*lq+fq,s,a);
        TSB(s,lyModel+1)+=N((lyModel+1-fyModel)*lq+fq,s,a)*west((lyModel-fyModel)*lq+fq,s,a);
      } else {
        SSB(s,lyModel+1)+=N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a);
        TSB(s,lyModel+1)+=N(yq,s,a)*west(yq,s,a);
      }
    }
  }

  //
  sa=0;
  if (lqly==lq) {
    y=lyModel+1;
    q=fq;
  } else q=lqly+1;
  
  CALC_yq
  for (s=first_VPA;s<=nsp;s++){
    //next_SSB(s,lyModel+1)=SSB(s,lyModel+1);
    //next_TSB(s,lyModel+1)=TSB(s,lyModel+1);
    for (a=fa;a<=la_VPA(s);a++){
      sa++;
      //cout<<"s:"<<s<<" a:"<<a<<" sa:"<<sa<<endl;
      if (a==fa && lqly==lq) term_N_next(sa)=SSB_recruit_last(s); else term_N_next(sa)=N(yq,s,a);
      term_logN_next(sa)= log(term_N_next(sa));
    }
  }


 

FUNCTION calc_hist_SSB
  int y,s,a,q,yq1;
  int yq;
  hist_SSB=0.0;
  q=fq;
  for (y=lyModel-sdReportYear;y<=lyModel;y++){
   CALC_yq
    for (s=first_VPA;s<=nsp;s++){  
      for (a=faq(q);a<=la(s);a++) {
        if (N(yq,s,a)>0 && propmat(yq,s,a)>0) {
           hist_SSB(s,y)+=N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a);
         }
       }
     }    
   }
   
   y=lyModel+1;
   CALC_yq
   if (any_do_effort) yq1=yq;  else yq1=yq-lq;
   
   for (s=first_VPA;s<=nsp;s++){ 
      next_SSB(s)=0; 
      for (a=faq(q);a<=la(s);a++) {
        if (N(yq,s,a)>0 && propmat(yq1,s,a)>0) {
           next_SSB(s,lyModel+1)+=N(yq,s,a)*west(yq1,s,a)*propmat(yq1,s,a);
         }
       }
    }   
   
 

FUNCTION calc_eaten_M2_hist
  // Calc biomass eaten per prey species
 int prey,prey_a,y,q;
 int yq;
 for (y=fyModel;y<=lyModel;y++) for (prey=first_VPA;prey<=nsp;prey++){

   eaten_M2(prey,y)=0.0;
   for (q=fq;q<=lq;q++) {
     CALC_yq
     if (is_prey(prey)==1) for (prey_a=faq(q);prey_a<=la(prey);prey_a++) { 
     eaten_M2(prey,y)+=N_bar(yq,prey,prey_a)*M2(yq,prey,prey_a)*west(yq,prey,prey_a);

     }
   }

 }
 
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中�
 //  functions for prediction
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中�
 

FUNCTION void out_predict_raw2(char intype[], int iter, dmatrix tmp_in)  
 int s,y;
 char txt[40];
 char txt2[40];

 // write data for each MCMC repetition
 strcpy(txt,intype);
 strcpy(txt2,"mcout_");
 strcat(txt2,txt);
 strcat(txt2,".out");

 ofstream mc (txt2,ios::app);
 if (!(mc)) {
     cerr << "Error trying to open file "<<txt2<< " for output "<<endl;
     exit(1);
 } 
 for (s=first_VPA;s<=nsp;s++) {
   for (y=lyModel+1;y<=lpy;y++){
     for (q=fq;q<=lq;q++){
       mc <<s<<" "<<y<<" "<<MCMC_prediction<<" "<<iter<<" "<<tmp_in(s,y)<<endl ;
  }}}


// Output detailed values for predictions

FUNCTION void out_predict_raw3(char intype[], int y, int iter,d3_array tmp_in)  
 int s,q,a;
 char txt[40];
 char txt2[40];

  // write data for each MCMC repetition
  strcpy(txt,intype);
  strcpy(txt2,"mcout_");
  strcat(txt2,txt);
  strcat(txt2,".out");

  ofstream mc (txt2,ios::app);
  if (!(mc)) {
     cerr << "Error trying to open file "<<txt2<< " for output "<<endl;
     exit(1);
  } 
  for (s=first_VPA;s<=nsp;s++) {
    for (q=fq;q<=lq;q++){
     for (a=faq(q);a<=la(s);a++){   
       mc <<s<<" "<<a<<" "<<y<<" "<<q<<" "<<MCMC_prediction<<" "<<iter<<" "<<tmp_in(s,q,a)<<endl ;
  }}}
  
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

 
 
FUNCTION void write_other_pred();
  int s,y,q;
  if (multi==2) {
   ofstream op("other_predator_prediction.out",ios::out);
   op <<"Year Quarter Species.n Other.bio"<<endl;

   for (s=1;s<=nOthPred;s++) {
     for (y=fyModel;y<=lpy;y++) {
       for (q=fq;q<=lq;q++) { 
            op<<y<<' '<<q<<' '<<s<<' '<< other_bio(s,y,q)<<endl;;
        }
      } 
    }
    op.close();
  }

 // Calculate N at age and size class from N at age and ALK all

FUNCTION void pred_N_at_length(int y,int q,d3_array pred_N_bar_stom, d3_array& pred_N_l_bar );
 int s,a;
 int Lq,Ls,Ld,La,Ll;    //ALKS index counters
 int yq;
 CALC_yq
 
 Lq=index_Lq(y,q);
 //cout<<"N_at_length Lq:"<<Lq;
 for (Ld=ALKS_yq(Lq,2);Ld<=ALKS_yq(Lq,3);Ld++) {  // area 
   for (Ls=ALKS_yqd(Lq,2);Ls<=ALKS_yqd(Lq,3);Ls++) { // species
     s=ALKS_yqds(Ls,1);
     pred_N_l_bar(s)=0.0;
     //cout<<" s:"<<s<<" start La:"<<ALKS_yqds(Ls,2)<<" endLa:"<<ALKS_yqds(Ls,3)<<endl;
     for (La=ALKS_yqds(Ls,2);La<=ALKS_yqds(Ls,3);La++) {
       a=ALKS_yqdsa(La,1);
       //cout<<" age:"<<a<<" start:"<<ALKS_yqdsa(La,2)<<" end:"<<ALKS_yqdsa(La,3)<<endl;
       for (Ll=ALKS_yqdsa(La,2);Ll<=ALKS_yqdsa(La,3);Ll++) {
         pred_N_l_bar(s,a,Ll)= ALKS_input(La,Ll)*pred_N_bar_stom(s,q,a);
         //cout<<"N_l_bar(s,a,Ll):"<<N_l_bar(s,a,Ll)<<" ALKS_adjusted(La,Ll):"<<ALKS_adjusted(La,Ll)<<" N_bar_stom(yq,s,a):"<<N_bar_stom(yq,s,a)<<" sum key:"<<sum(ALKS_adjusted(La))<<endl; 
      }
 }}}


FUNCTION void calc_growth(d3_array pred_N);
 int s, gs, q;
 double stock_N;
 for (s=first_VPA;s<=nsp;s++) {
   if (growth_model(1,s)==1) {
     // calc stock density 1 January
     stock_N=0;
     for (gs=growth_model(4,s);gs<=growth_model(5,s);gs++) stock_N+=sum(pred_N(s,fq));
     // Calc weight 1. January
     for (a=growth_model(2,s)+1;a<=growth_model(3,s);a++) {
       pred_west(s,fq,a)= growth_type1_regr(s,a,1)+growth_type1_regr(s,a,2)*pred_west_old(s,fq,a-1)+growth_type1_regr(s,a,3)*stock_N;
     }
     a=growth_model(2,s); // first age
     pred_west(s,fq,a)= growth_type1_regr(s,a,1)+growth_type1_regr(s,a,3)*stock_N;
     
     // adjust Q1 w and calc W for quarter 2-4
     for (q=fq;q<=lq;q++) for (a=growth_model(2,s);a<=growth_model(3,s);a++) {
       if (q==fq) {
         if (pred_west(s,fq,a) < growth_min_w(s,a)) pred_west(s,fq,a)=growth_min_w(s,a);
         else if (pred_west(s,fq,a) > growth_max_w(s,a)) pred_west(s,fq,a)=growth_max_w(s,a);
       }
       else pred_west(s,q,a)=pred_west(s,fq,a)*growth_type1_in_year(s,a,q);
       pred_weca(s,q,a)=pred_west(s,q,a)*growth_ratio_weca_west(s,q,a);
     }
     pred_size_sea(s)=pred_west(s);

     //cout<< "pred_west:"<<endl<<pred_west(3,1)<<endl<<"pred_weca:"<<endl<<pred_weca(3,1)<<endl<<"pred_size_sea:"<<endl<<pred_size_sea(3,1)<<endl<<endl;
   }
 }

FUNCTION void calc_pred_M2(int yy, int q, d3_array& pred_M1,d3_array& pred_M2, d3_array& pred_F, d3_array& pred_N, d3_array& pred_N_bar_stom);
 int pred, pred_a, prey, prey_a, pred_l, prey_l;
 int y,d;
 int observed=0; // it is not observed stomach contents data we use here
 double pred_size,prey_size,part_M2;
 double tmp;
 int minl=min(f_ALKS_yqdsal);   // minimum and maximum size class   RETTES: kan laves til vector for at spare plads i size_l_sea variable
 int maxl=max(l_ALKS_yqdsal);   
 if (multi==0) maxl=-1;

 d3_array   pred_avail_food(1,npr,fq,lq,fa,max_a);      
 
 d3_array   pred_M2_l(first_VPA,nsp,fa,la_VPA,minl,maxl);   // M2 at age and size class
 d3_array   pred_Z_l(first_VPA,nsp,fa,la_VPA,minl,maxl);    // Z at age and size class
 d3_array   pred_avail_food_l(1,npr,fa,la_pred,minl,maxl);   // available food at age and size group
 d3_array   pred_N_l_bar(1,nsp,fa,la,minl,maxl);           // average N at age and size class
 
 dmatrix    pred_deadM1F(first_VPA,nsp,fa+0,la_VPA);
 dmatrix    pred_deadM2(first_VPA,nsp,fa+0,la_VPA);
 dmatrix    pred_deadM1M2F(first_VPA,nsp,fa+0,la_VPA);
 double pred_M1F;
 
 ofstream res2("partial_m2_prediction.out",ios::app);

 y=lyModel; // to obtain size at age etc.
 d=1;
 CALC_yqd

 //if (yy==2012 && q==1) {
 //  cout<<"yy:"<<yy<<"  y:"<<y<<" q:"<<q<<" d:"<<d<<" yqd:"<<yqd<<endl;
 //  cout<<"size_sea"<<endl<<size_sea(yqd)<<endl;
 //  cout<<"pred_west"<<endl<<pred_west<<endl;
 //}

 if (use_overlap==0 || use_overlap==1) y=lyModel; 
 else if (use_overlap==2) y=yy; 
 else { cout<<"ERROR. wrong value of use.overlap:"<<use_overlap<<endl; exit(9);}
 
 if (simple_ALK==0) {   // only one sizeclass per age
   for (prey=first_VPA;prey<=nsp;prey++) pred_M2(prey,q)=0.0;
   
   for (pred=1;pred<=npr;pred++){
    for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
      if (pred_N_bar_stom(pred,q,pred_a)>0) { 
        //Calc available food
        pred_avail_food(pred,q,pred_a)=0.0;
  
        pred_size=pred_size_sea(pred,q,pred_a);
        
        for (prey=first_VPA;prey<=nsp;prey++) {
          if (pred_prey_comb(d,pred,prey)>0) {
            for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
               prey_size=pred_size_sea(prey,q,prey_a);
  
               if  ((pred_size*prey_pred_size_fac(pred))>=prey_size) 
                pred_avail_food(pred,q,pred_a)+=pred_N_bar_stom(prey,q,prey_a)
                * pred_west(prey,q,prey_a)*value(suit(y,q,d,pred,prey,pred_size,prey_size,observed));
                //if (pred==14 && prey==19) {
                //cout<<"y:"<<y<<" q:"<<q<<" pred_age:"<<pred_a<<" prey_age:"<<prey_a<<" pred_size:"<<pred_size
                //        <<"pred_west:"<<pred_west(prey,q,prey_a)<<" prey_size:"<<prey_size <<endl;
                // }
             }
           }
         }
  
         pred_avail_food(pred,q,pred_a)+=AV_other_food(d,pred)*value(other_suit(pred,pred_size,y,q,d));  // add other food SKAL RETTES
         tmp=pred_N_bar_stom(pred,q,pred_a)*consum(yqd,pred,pred_a)/pred_avail_food(pred,q,pred_a);

         // if (pred==14 ) {
         //   cout <<"avail food: q="<<q<<" pred="<<pred<<" a:"<<pred_a<<"  "<<pred_avail_food(pred,q,pred_a)<<"  tmp:"<<tmp
         //   <<"  consum:"<<consum(yqd,pred,pred_a)<<"  N_bar:"<<pred_N_bar_stom(pred,q,pred_a)<<endl;
         //  }
         
         //calc M2
         for (prey=first_VPA;prey<=nsp;prey++) {
           if (pred_prey_comb(d,pred,prey)>0) {
             for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
               if (size_select_model==1) prey_size=pred_size_sea(prey,q,prey_a);
               else prey_size=pred_west(prey,q,prey_a);
               if ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                 part_M2 =tmp*value(suit(y,q,d,pred,prey,pred_size,prey_size,observed));
                 pred_M2(prey,q,prey_a)+=part_M2;
                 if (1==7 && part_M2>0) {
                    res2<<yy<<" "<<q<<" "<<pred<<" "<<pred_a<<" "<<prey<<" "<<prey_a<<" "<<part_M2<<" "<<suit(y,q,d,pred,prey,pred_size,prey_size,observed)<<endl;
                 }
               }
             }
           }
         }
       } 
     }
   }  // pred loop
 } //  end (simple_ALK==0),  only one sizeclass per age
 
 else { // simple_ALK==1, more than one sizeclass per age
   for (prey=first_VPA;prey<=nsp;prey++) pred_M2_l(prey)=0.0;   
   pred_N_at_length(lyModel,q,pred_N_bar_stom,pred_N_l_bar);   // Calculate N at age and size class from N at age and ALK all
   //cout<<"year:"<<y<<" q:"<<q<<endl;
   for (pred=1;pred<=npr;pred++){  
    for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
      if (pred_N_bar_stom(pred,q,pred_a)>0) { 
         //cout<<"pred:"<<pred<<" pred_a:"<<pred_a;;
     
        //Calc available food
        pred_avail_food_l(pred)=0.0;
        for (pred_l=int(index_minl(yqd,pred,pred_a));pred_l<=int(index_maxl(yqd,pred,pred_a));pred_l++) {
          pred_size=size_l_sea(yqd,pred,pred_a,pred_l); 
          //cout<<" pred_l: "<<pred_l<<" size: "<<setprecision(4)<<pred_size;
          for (prey=first_VPA;prey<=nsp;prey++) if (pred_prey_comb(d,pred,prey)>0) {
            for (prey_a=faq(q);prey_a<=la(prey);prey_a++) { 
              for (prey_l=int(index_minl(yqd,prey,prey_a));prey_l<=int(index_maxl(yqd,prey,prey_a));prey_l++) {
                prey_size=size_l_sea(yqd,prey,prey_a,prey_l);
                //cout<<" prey: "<<prey<<" prey_l:"<<prey_l<<" size:"<<setprecision(4)<<prey_size<<endl;
                if  ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                  //cout<<"check: N_l_bar:"<<N_l_bar(prey,prey_a,prey_l)<<" suit: "<<setprecision(3)<<suit(y,q,pred,prey,pred_size,prey_size,observed)<<endl;
                  pred_avail_food_l(pred,pred_a,pred_l)+=pred_N_l_bar(prey,prey_a,prey_l)*prey_size*value(suit(y,q,d,pred,prey,pred_size,prey_size,observed));  
           }}}}
   
           pred_avail_food_l(pred,pred_a,pred_l)+=AV_other_food(d,pred)*value(other_suit(pred,pred_size,y,q,d));  // add other food     SKAL RETTES
           tmp=pred_N_l_bar(pred,pred_a,pred_l)*consum_l(yqd,pred,pred_a,pred_l) /pred_avail_food_l(pred,pred_a,pred_l);  
           //cout<<" availeble food: "<<avail_food_l(pred,pred_a,pred_l)<<endl;
           
           //calc M2 at age and size class
           for (prey=first_VPA;prey<=nsp;prey++) {
             if (pred_prey_comb(d,pred,prey)>0) {
               for (prey_a=faq(q);prey_a<=la(prey);prey_a++) { 
                 for (prey_l=int(index_minl(yqd,prey,prey_a));prey_l<=int(index_maxl(yqd,prey,prey_a));prey_l++) {
                   prey_size=size_l_sea(yqd,prey,prey_a,prey_l);
                   if ((pred_size*prey_pred_size_fac(pred))>=prey_size) {  
                      pred_M2_l(prey,prey_a,prey_l)+=tmp*value(suit(y,q,d,pred,prey,pred_size,prey_size,observed));
           }}}}}        
       } 
     }
    } 
   }
     
   //calc M2 at age
   pred_deadM1F=0.0;
   pred_deadM2=0.0;
   pred_deadM1M2F=0.0;
   for (prey=first_VPA;prey<=nsp;prey++) if (is_prey(prey)==1) {
     for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
       pred_M1F=pred_M1(prey,q,prey_a)+pred_F(prey,q,prey_a);
       //cout<<"prey:"<<prey<<" prey_a:"<<prey_a<<" M1F:"<<M1F<<endl;
       for (prey_l=int(index_minl(yqd,prey,prey_a));prey_l<=int(index_maxl(yqd,prey,prey_a));prey_l++) {
         pred_Z_l(prey,prey_a,prey_l)=pred_M1F+pred_M2_l(prey,prey_a,prey_l);
         if (use_Nbar==0) {                    // Use N in the beginning of a period to calc M2
            pred_N_l_bar(prey,prey_a,prey_l)=pred_N_l_bar(prey,prey_a,prey_l)*(1-exp(-pred_Z_l(prey,prey_a,prey_l)))/pred_Z_l(prey,prey_a,prey_l);   
         }
         pred_deadM1F(prey,prey_a)+=pred_N_l_bar(prey,prey_a,prey_l)*pred_M1F;
         pred_deadM2(prey,prey_a)+=pred_N_l_bar(prey,prey_a,prey_l)*pred_M2_l(prey,prey_a,prey_l);
       }
       if (pred_deadM2(prey,prey_a)>0) {
          pred_deadM1M2F(prey,prey_a)=pred_deadM1F(prey,prey_a)+pred_deadM2(prey,prey_a);
          //cout<<"N-deadM1M2F:"<<N(prey,y,q,prey_a)-deadM1M2F(prey,prey_a)<<endl;
          pred_M2(prey,q,prey_a)=log(pred_N(prey,q,prey_a)/(pred_N(prey,q,prey_a)-pred_deadM1M2F(prey,prey_a)))*pred_deadM2(prey,prey_a)/pred_deadM1M2F(prey,prey_a);
          //cout<<setprecision(3)<<M2(prey,y,q,prey_a)<<endl;
       }
      }
   }
 
 }  // end simple_ALK==1
  
  


  //********************************************************************************************* 

FUNCTION double calc_SSB_from_Fscaling( int s, double F_scaling, int first_q, dmatrix M, dmatrix M1, dmatrix M2,dmatrix F_sq, dmatrix N_obs, double rec, dmatrix west, dmatrix propmat);
  // calc SSB in the year after the fishery with F calculated from F_scaling
  dmatrix    Z(fq,lq,fa,max_a );
  dmatrix    F(fq,lq,fa,max_a );
  dmatrix    N(fq,lq,fa,max_a );

  int a, q;
  double SSB;

  for (q=first_q;q<=lq;q++)  F(q)=F_sq(q)*F_scaling;
  N=N_obs;
  if (multi==2) Z=F+M1+M2;
  else Z=F+M;
  //calc prediction N 1.jan next year 
    
  for (q=first_q;q<=lq;q++){                     
     //predict next season's N   
     for (a=la(s);a>=fa;a--) {
        if (!(a==fa && q<recq)) {
          if (q==lq) {
            if (a==la(s) && nplus(s)==1) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1))+N(lq,a)*exp(-Z(lq,a)); 
            else if (a==la(s) && nplus(s)==0) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1)); 
            else if (a>fa) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1)); 
          }
          else N(q+1,a)=N(q,a)*exp(-Z(q,a)); 
        }
        else { 
          if (a==fa && q==recq && fa==1) N(recq,fa)=rec;
        }      
     }
  }
  //Calc SSB 
  SSB=0.0;    
  for (a=fa; a<=la(s);a++) SSB+=N(fq,a)*west(fq,a)*propmat(fq,a);
 return(SSB);                                                   

 //********************************************************************************************* 


FUNCTION double calc_Yield_from_Fscaling( int s, int firstQ, double F_scaling, dmatrix M, dmatrix M1, dmatrix M2, dmatrix F_sq, dmatrix N_obs, dmatrix weca, dmatrix pred_prop_landed);
  dmatrix    F(fq,lq,fa,max_a );
  dmatrix    Z(fq,lq,fa,max_a );
  dmatrix    N(fq,lq,fa,max_a );
  int a, q;
  double yield=0.0;

  F=F_sq*F_scaling;
  if (multi==2) Z=F+M1+M2;
  else  Z=F+M;
  N=N_obs;
   
  //calc prediction N and Yield within year 
  for (q=firstQ;q<=lq;q++){
    //predict next season's N    
    for (a=la(s);a>=fa;a--) if (!(a==fa && q<recq)) {
      yield+=N(q,a)*(1.0-exp(-Z(q,a)))/Z(q,a)*F(q,a)*weca(q,a)*pred_prop_landed(q,a);
      if (q<lq) N(q+1,a)=N(q,a)*exp(-Z(q,a)); 
    }
  }  
 
 return(yield); 
 
 //*********************************************************************************************    

FUNCTION double find_Fscaling_from_target_SSB(int s, int first_q, double lower, double upper, double target_SSB, dmatrix M, dmatrix M1, dmatrix M2, dmatrix pred_F, dmatrix N_obs, dmatrix west, dmatrix propmat,int year);
  double dif,x,y;
  double SSB0,rec1;
  int iter,a;
  dmatrix    dummy(1,1,1,1);

  if (test_output==53) cout<<"input target SSB:"<<target_SSB<<endl; 

  dif=100.0;
  iter=0;
  x=1.0;
  
  // for fa=1, fa might be included in the SSB. rec1 is observed recruits for the next year
  if (fa==1 && propmat(fq,fa)>0) {  
    SSB0=0.0;    
    for (a=fa; a<=la(s);a++) SSB0+=N_obs(fq,a)*west(fq,a)*propmat(fq,a);
    rec1=SSB_recruit(s,SSB0,0.0,dummy,year);    
    if (test_output==53) cout<<"N start of the year:"<<N_obs<<endl;
  } else rec1=0; // it is assumed that the recruits (at age 0) do not contribute to SSB in the same year

  while ((dif>1E-5) && (iter<100) && (x >=1E-8)) {
    x=(upper+lower)/2;

    //calc prediction N 1.jan next year 
    y=calc_SSB_from_Fscaling(s, x, first_q,M, M1, M2, pred_F, N_obs,rec1, west, propmat);
    if (y>=target_SSB) lower=x; else upper=x;
    dif=fabs(upper-lower);
 
    iter++;
  }
  if (test_output==53) cout<<"realised target SSB:"<<y<<"  F scaling:"<<setprecision(3)<<x<<endl; 
  if ((iter<100) || (x<=1E-6))  return(x);
  else return(-1000.0);
                                                  
 //********************************************************************************************* 
 
FUNCTION double find_Fscaling_from_target_SSB_two_years(int s, int first_q, double lower, double upper, double target_SSB, dmatrix M, dmatrix M1, dmatrix M2, dmatrix pred_F, dmatrix pred_F_year2, dmatrix N_obs, dmatrix west, dmatrix weca, dmatrix propmat,int year,double& yieldQ4, double& yieldQ1Q3);
  double dif,x,y;
  double SSB0,rec2;
  int iter,a;
  dmatrix    dummy(1,1,1,1);      // recruitment noize
  dmatrix    Z(fq,lq,fa,max_a );
  dmatrix    F(fq,lq,fa,max_a );
  dmatrix    N(fq,lq,fa,max_a );


  if (test_output==53) cout<<"input target SSB:"<<target_SSB<<endl; 
  
  N=N_obs;

  dif=100.0;
  iter=0;
  x=1.0;
  
   
  if (test_output==53) cout<<"N at start of quarter "<<first_q<<"   "<<N(first_q)<<endl;

  while ((dif>1E-5) && (iter<100) && (x >=1E-8)) {
    x=(upper+lower)/2;

    yieldQ4=0;
    yieldQ1Q3=0;
    F=pred_F*x;
    if (multi==2) Z=F+M1+M2;
    else Z=F+M;

    //calc prediction N 1.jan next year and yield in first year 
    
    for (q=first_q;q<=lq;q++){                     
      //predict next season's N   
      for (a=la(s);a>=fa;a--) {
        yieldQ4+=N(q,a)*(1.0-exp(-Z(q,a)))/Z(q,a)*F(q,a)*weca(q,a); 
        if (!(a==fa && q<recq)) {
          if (q==lq) {
            if (a==la(s) && nplus(s)==1) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1))+N(lq,a)*exp(-Z(lq,a)); 
            else if (a==la(s) && nplus(s)==0) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1)); 
            else if (a>fa) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1)); 
          }
          else {
            N(q+1,a)=N(q,a)*exp(-Z(q,a)); 
          }
        }      
      }
    }

    // if (test_output==53) cout<<"N start at second year:"<<setprecision(0)<<N(fq)<<endl;

    // SSB start of second year
    SSB0=0.0;    
    for (a=faq(fq); a<=la(s);a++) SSB0+=N(fq,a)*west(fq,a)*propmat(fq,a);
    rec2=SSB_recruit(s,SSB0,0.0,dummy,1);
    //if (test_output==53) cout<<setprecision(0)<<"SSB at start of second year:"<<SSB0<<" and resulting recruitment:"<<rec2<<endl;
    N(recq,fa)<-rec2;    
     
    //calc prediction next year 
    F=pred_F_year2*x;
    y=calc_SSB_from_Fscaling(s, x,fq,M,M1,M2, F, N,rec2, west, propmat);

    //if (test_output==53) cout<<"Scaling of F:"<<setprecision(3)<<x<<"  Realised SSB:"<<setprecision(0)<<y<<endl;
    
    if (y>=target_SSB) lower=x; else upper=x;
    dif=fabs(upper-lower);
 
    iter++;
  }
  if (test_output==53) cout<<"realised target SSB:"<<setprecision(0)<<y<<"  F scaling:"<<setprecision(3)<<x<<endl;
   
  if (x>0) yieldQ1Q3=calc_Yield_from_Fscaling(s,fq,x,M,M1,M2,F,N,weca,pred_prop_landed(s));
  if (test_output==53) cout<<"yieldQ1Q3:"<<yieldQ1Q3<<endl;     
 
  if ((iter<100) || (x<=1E-6))  return(x);
  else return(-1000.0);
                                                  
 //********************************************************************************************* 

 
FUNCTION double find_Fscaling_from_target_yield_two_years(int s, int firstQ, double lower, double upper, double target, dmatrix M, dmatrix M1, dmatrix M2, dmatrix pred_F, dmatrix pred_F_year2, dmatrix N_obs, dmatrix weca,dmatrix west, dmatrix propmat, dmatrix pred_prop_landed,double& yieldQ4, double& yieldQ1Q3);
  // find a scaling factor to mean F status quo to obtain a target yield
  dmatrix    dummy(1,1,1,1);      // recruitment noize
  dmatrix    Z(fq,lq,fa,max_a );
  dmatrix    F(fq,lq,fa,max_a );
  dmatrix    N(fq,lq,fa,max_a );
  
  double dif,x,y,SSB0,rec2;
  double yQ4,yQ1Q3;
  int iter;


  N=N_obs;

  dif=100.0;
  iter=0;
  x=1.0;
  
  while ((dif>1E-8) && (iter<100) && (x >=1E-12)) {
    x=(upper+lower)/2;
 
 
    F=pred_F*x;
    if (multi==2) Z=F+M1+M2;
    else Z=F+M;
    yQ4=0;
    yQ1Q3=0;
    
    for (q=firstQ;q<=lq;q++){                     
      //predict next season's N   
      for (a=la(s);a>=fa;a--) {
        //Calc yield within the year
        yQ4+=N(q,a)*(1.0-exp(-Z(q,a)))/Z(q,a)*F(q,a)*weca(q,a); 
        if (!(a==fa && q<recq)) {
          if (q==lq) {
            if (a==la(s) && nplus(s)==1) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1))+N(lq,a)*exp(-Z(lq,a)); 
            else if (a==la(s) && nplus(s)==0) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1)); 
            else if (a>fa) N(fq,a)=N(lq,a-1)*exp(-Z(lq,a-1)); 
          }
          else {
            N(q+1,a)=N(q,a)*exp(-Z(q,a)); 
          }
        }     
      }
    }

    // if (test_output==53) cout<<"N start at second year:"<<setprecision(0)<<N(fq)<<endl;

    // SSB start of second year
    SSB0=0.0;    
    for (a=faq(fq); a<=la(s);a++) SSB0+=N(fq,a)*west(fq,a)*propmat(fq,a);
    
    rec2=SSB_recruit(s,SSB0,0.0,dummy,1);
    //if (test_output==53) cout<<setprecision(0)<<"SSB at start of second year:"<<SSB0<<" and resulting recruitment:"<<rec2<<endl;
    F=pred_F_year2*x;
    // if (multi==2) Z=F+M1+M2; else Z=F+M;
    N(recq,fa)=rec2;
    
    yQ1Q3=calc_Yield_from_Fscaling(s,fq,x,M, M1, M2, F, N, weca,pred_prop_landed);
    y=yQ4+yQ1Q3; //annual TAC
    if (y>=target) upper=x; else lower=x;
    dif=fabs(upper-lower);
    iter++;
  }
  if (test_output==53) cout<<species_names[s]<<" target yield:"<<setprecision(0)<<target<<" realised:"<<y<<" Q4 catch:"<<yQ4<<" Q1Q3 catch:"<<yQ1Q3<<"  F scaling:"<<setprecision(3)<<x<<endl; 
  if ((iter<100) || (x<=1E-8)) {
    yieldQ4=yQ4;
    yieldQ1Q3=yQ1Q3;
    return(x);
  }
  else return(-1000.0);
                                                  
 //********************************************************************************************* 

FUNCTION double find_Fscaling_from_target_yield(int s, int firstQ, double lower, double upper, double target, dmatrix M, dmatrix M1, dmatrix M2, dmatrix pred_F, dmatrix N_obs, dmatrix weca, dmatrix pred_prop_landed);
  // find a scaling factor to mean F status quo to obtain a target yield
  double dif,x,y;
  int iter;

  dif=100.0;
  iter=0;
  x=1.0;
  
  while ((dif>1E-8) && (iter<100) && (x >=1E-12)) {
    x=(upper+lower)/2;
    y=calc_Yield_from_Fscaling(s,firstQ,x,M, M1, M2, pred_F, N_obs, weca,pred_prop_landed);
    if (y>=target) upper=x; else lower=x;
    dif=fabs(upper-lower);
    iter++;
  }
  //if (test_output==53) cout<<species_names[s]<<" target yield:"<<setprecision(0)<<target<<" realised:"<<y<<"  F scaling:"<<setprecision(3)<<x<<endl; 
  if ((iter<100) || (x<=1E-8))  return(x);
  else return(-1000.0);
                                                  
 //********************************************************************************************* 


FUNCTION void get_pred_Nbar_stom_at_age(int q, d3_array pred_N, d3_array& pred_Nbar, d3_array& pred_Nbar_stom)  
 int s,a;
  //Calc N to be used to estimate M2
  for (s=first_VPA;s<=nsp;s++){
    for (a=fa;a<=la(s);a++) {
      if (!((a==fa) && (q <recq))) {
         if (use_Nbar==1) pred_Nbar_stom(s,q,a)=pred_Nbar(s,q,a);
         else pred_Nbar_stom(s,q,a)=pred_N(s,q,a);
      }
    }
  }


 // *****************************************************************************************
FUNCTION double SSB_recruit(int s, double SSB, double noise, dmatrix hist_rec_noise,int year);
  double noiseFac, rec;

  if (noise==0.0 || (rec_noise_trunc(s,1)==0 && rec_noise_trunc(s,2)==0)) noiseFac=1.0; else
  {
   if (recruit_adjust_CV(s)==1) noiseFac=value(exp(sqrt(SSB_R_s2(s))*noise-SSB_R_s2(s)/2.0));
   else   noiseFac=value(exp(sqrt(SSB_R_s2(s))*noise));
  }
  
  
  if (no_recruit_autocor(s)>0 && noise != 0.0) {
    int i;
    //cout<<"start noise:"<<noise<<"  hist_rec_noise:"<<hist_rec_noise<<endl;
    for (i=1;i<=no_recruit_autocor(s);i++) noise+=hist_rec_noise(s,i)*recruit_autocor(s,i);
    
    for (i=no_recruit_autocor(s);i>1;i--) hist_rec_noise(s,i)=hist_rec_noise(s,i-1); 
    hist_rec_noise(s,1)=noise;
    //cout<<"end hist_rec_noise:"<<noise<<",  "<<hist_rec_noise<<endl<<endl;
  }

  switch(SSB_Rec_model(s)) {
           case 1:  return(recruit_adjust(s)*value(SSB_R_alfa(s)*SSB*exp(-SSB_R_beta(s)/SSB_R_beta_cor(s)*SSB)*noiseFac));  // Ricker       
                    //break;
          case 51:  return(recruit_adjust(s)*value(SSB_R_alfa(s)*SSB*exp(-SSB_R_beta(s)/SSB_R_beta_cor(s)*SSB+RecTempVar(s)*Rec_add_inf(s,2))*noiseFac));  // Ricker with Temperature (Sprat Baltic Sea)
          case 52:  return(recruit_adjust(s)*value(SSB_R_alfa(s)*SSB*exp(-SSB_R_beta(s)/SSB_R_beta_cor(s)*SSB+Rec_add_inf(s,1)*Rec_add_inf(s,2))*noiseFac));  // Ricker with Temperature (Sprat Baltic Sea)

           case 2: return(recruit_adjust(s)*value(SSB_R_alfa(s)*SSB/(1+SSB_R_beta(s)/SSB_R_beta_cor(s)*SSB)*noiseFac));     // Betverton & Holt                                 
                    // break;
            case 3: return(recruit_adjust(s)*value(exp(SSB_R_alfa(s))*noiseFac));                 // geometric mean
                    //break;
           case 4:  if (SSB>=SSB_R_beta(s)/SSB_R_beta_cor(s)) {
                      return(recruit_adjust(s)*value(SSB_R_beta(s)/SSB_R_beta_cor(s)*SSB_R_alfa(s)*noiseFac));
                    }  
                    else return(recruit_adjust(s)*value(exp(SSB_R_alfa(s))*SSB*noiseFac));
                    //break;  
           case 10:  rec=exp(-2.42285E-5*SSB+0.29133E-5*SSB*environment(year,1)+12.18724);
                     if (rec>SSB_R_alfa(s)) return(value(SSB_R_alfa(s)));
                     else return(rec);
                    //break;  
            case 11: rec=5.84E6*pow(environment(year,2),2)-1.74E8*environment(year,2)+1.33E9;
                     return(rec);
                    //break;  
            case 100: if (SSB>SSB_Rec_hockey_breakpoint(s)) {
                       return(recruit_adjust(s)*value(SSB_Rec_hockey_breakpoint(s)*exp(SSB_R_alfa(s))*noiseFac));
                     }
                     else return(recruit_adjust(s)*value(exp(SSB_R_alfa(s))*SSB*noiseFac));
                     //break;
           default: return(-1.0);  // error
           //break;
  }  


 

 // *****************************************************************************************
 
FUNCTION void estimate_recruits(int y, int first_s, int last_s, int add_noise, int true_N, d3_array& pred_N, dmatrix& recruit, dmatrix& SSB_obs, d3_array pred_west, d3_array pred_propmat, dmatrix rn, dmatrix hist_rec_noise);
 double noise=0.0;
 double ssb,tmp;
 int s,a;
    // recruits, recruiting at age 1 or older
    if (fa>0) for (s=first_s;s<=last_s;s++){
      if (add_noise) noise=rn(s,y);
      if (true_N) ssb=value(SSB(s,y-fa));
      else ssb=SSB_obs(s,y-fa);
      pred_N(s,recq,fa)=SSB_recruit(s,ssb,noise,hist_rec_noise,y);
      if (true_N==1) {         
        // special case for overwriting recruits or predicted stock numbers, used mainly for FLR simulations
        if (y>=read_predict_N_first_year && y<=read_predict_N_last_year && read_predict_N==1) {
          if (init_predict_N(s,y,init_pop,fa)>=0) pred_N(s,recq,fa)=init_predict_N(s,y,init_pop,fa);
        }
      recruit(s,y)=pred_N(s,recq,fa);   // otput only
      }  
    }     

    // calc SSB in start of the first season
    for (s=first_s;s<=last_s;s++) {
      if (true_N) SSB(s,y)=0.0;
      else SSB_obs(s,y)=0.0;
      for (a=fa;a<=la(s);a++) {
        tmp=pred_N(s,fq,a)*pred_west(s,fq,a)*pred_propmat(s,fq,a);
        if (true_N) SSB(s,y)+=tmp;
        else    SSB_obs(s,y)+=tmp;
      }
    }
     
    // recruits, recruiting at 0-group
    if (fa==0) for (s=first_s;s<=last_s;s++){
      if (add_noise) noise=rn(s,y);
      if (true_N) ssb=value(SSB(s,y-fa));
      else ssb=SSB_obs(s,y-fa);
      pred_N(s,recq,fa)=SSB_recruit(s,ssb,noise,hist_rec_noise,y);
      if (true_N==1) {
        // special case for overwriting recruits or predicted stock numbers, used mainly for FLR simulations
        if (y>=read_predict_N_first_year && y<=read_predict_N_last_year && read_predict_N==1) {
          if (init_predict_N(s,y,init_pop,fa)>=0) pred_N(s,recq,fa)=init_predict_N(s,y,init_pop,fa);
        }
      recruit(s,y)=pred_N(s,recq,fa);   // otput only
      }
       
    }      
   
 
    // calc TSB in start of the first season
    if (true_N) for (s=first_s;s<=last_s;s++) for (a=fa;a<=la(s);a++) {
        TSB(s,y)+=pred_N(s,fq,a)*pred_west(s,fq,a); 
     }

    if (test_output==53) {
     if (true_N)  for (s=first_s;s<=last_s;s++) {
       if (nsp>1) cout<<species_names[s]<<":  ";
         cout<<"TSB("<<y<<"):"<<setfixed()<<setprecision(0)<<TSB(s,y)
         <<"   SSB("<<y<<"):"<<setfixed()<<setprecision(0)<<SSB(s,y)<<
          " reruits("<<y<<"):"<<recruit(s,y);
       if (y>=read_predict_N_first_year && y<=read_predict_N_last_year && read_predict_N==1) {
         cout<<" produced from input file"<<endl;
       } 
       else{ 
         cout <<"  produced by SSB("<<y-fa<<"):"<<SSB(s,y-fa)<<" and ";
         if (add_noise) cout<<"noise"<<endl;
         else cout<<"no noise"<<endl;
       }

     }
     else for (s=first_s;s<=last_s;s++) {
       if (nsp>1) cout<<species_names[s]<<":  ";
       cout<<"SSB_obs("<<y-fa<<"):"<<setfixed()<<setprecision(0)<<SSB_obs(s,y-fa)<<
          " reruits_obs("<<y<<"):"<<pred_N(s,recq,fa)<<"  produced by SSB_obs("<<y-fa<<"):"<<SSB_obs(s,y-fa)<<" and ";
       if (add_noise) cout<<" recruitment noise"<<endl;
       else cout<<"no recruitment noise"<<endl;     
     }
    }
    
 // *****************************************************************************************
   
FUNCTION double uncertanty(int s, dvector un, double noise);
  double x;
  if (un(1)<0) return 1.0;
  
  //cout<<setprecision(3)<<obs_noise_trunc<<endl<<un<<endl<<" noise1 :"<<noise;
  if (noise<obs_noise_trunc(s,1)) noise=obs_noise_trunc(s,1);   
  else if (noise>obs_noise_trunc(s,2)) noise=obs_noise_trunc(s,2);
  //cout<<"noise 2:"<<noise<<endl;
  
  if (un(1)==0) {
    x=un(2)+un(3)*noise;               // normal distributed noise       
    if (x<0) return 0.01; else return x;
  }
  else if (un(1)==1) return un(2)*exp(un(3)*noise)*exp(-un(3)*un(3)/2);     // log-normal distributed noise
  else if (un(1)==10) return un(2)*exp(un(3)*noise);     // log-normal distributed noise  without correction

  else return -1000;  //error

 //********************************************************************************************* 

 // assessment uncertanties from age dependen CV
             
     
FUNCTION void age_uncertanty(int s, dvector un, dvector& N, dvector CV);
 dvector noise(fa,la(s));

 int a;
 
 //cout<<"dim cov:"<< cov.colmin()<<" " <<cov.colmax()<<" "<< cov.rowmin()<<" "<< cov.rowmax() <<" "<<endl;
 
 random_number_generator rng(seed);
 seed++;
 
 noise.fill_randn(rng);
  //cout<<"noise: "<<setprecision(3)<<noise<<endl;
 
 // decrease "outlier" noise
 for (a=fa;a<=la(s);a++) {
    if      (noise(a)<obs_noise_trunc(s,1)) noise(a)=obs_noise_trunc(s,1);   
    else if (noise(a)>obs_noise_trunc(s,2)) noise(a)=obs_noise_trunc(s,2);
    
    N(a)=N(a)*un(2)*exp(CV(a)*noise(a))*exp(-CV(a)*CV(a)/2);     // log-normal distributed noise 
    //    N(a)=N(a)*un(2)*exp(CV(a)*noise(a));     // log-normal distributed noise , no correction


 }
 
    
 //********************************************************************************************* 

  
 // assessment uncertanty from var-co-variance matrix
     
FUNCTION void cov_uncertanty(int s, dvector un, dvector& N, dmatrix cov);
 dmatrix result(1,1,fa,la(s));
 dmatrix noise(1,1,fa,la(s));
 dmatrix chol(fa,la(s),fa,la(s));

 int a,i;
 
 //cout<<"dim cov:"<< cov.colmin()<<" " <<cov.colmax()<<" "<< cov.rowmin()<<" "<< cov.rowmax() <<" "<<endl;
 
 random_number_generator rng(seed);
 seed++;
 
 noise.fill_randn(rng);
 
 // decrease "outlier" noise
 for (a=fa;a<=la(s);a++) {
    if (noise(1,a)<obs_noise_trunc(s,1)) noise(1,a)=obs_noise_trunc(s,1);   
    else if (noise(1,a)>obs_noise_trunc(s,2)) noise(1,a)=obs_noise_trunc(s,2);
 }
 /*
 cout<<"cov_uncertanty:"<<endl<<"N:"<<endl<<setfixed()<<setprecision(0)<<setw(10)<<N<<endl<<
     "COV:"<<endl<<setfixed()<<setprecision(3)<<setw(6)<<cov<<endl<<
      "noise:"<<endl<<setfixed()<<setprecision(3)<<setw(6)<<noise<<endl;
 */
 
 chol=trans(choleski_decomp(cov));      // it is an invariant - should go out of this function
 
 //cout<<"chol:"<<endl; cout<<setfixed()<<setprecision(3)<<setw(6)<<chol<<endl;
 
 result=noise*chol;
 //cout<<"    result: "<<endl<<setprecision(3)<<setw(6)<<result<<endl;
 
 i=0; 
 if (un(1)==2) {for (a=fa;a<=la(s);a++) {i++; N(a)=exp(result(1,a)-cov(i,i)/2)*N(a)*un(2);}}    // reduce by half of variance
 else if (un(1)==3) {for (a=fa;a<=la(s);a++) { N(a)=exp(result(1,a))*N(a)*un(2);}}              // do not reduce 
 //cout<<"N:"<<endl<<setfixed()<<setprecision(0)<<setw(10)<<N<<endl;
  
  
 // same as above, but no recruits involved   
FUNCTION void cov_uncertanty_no_rec(int s, dvector un, dvector& N, dmatrix cov);
 dmatrix result(1,1,fa+1,la(s));
 dmatrix noise(1,1,fa+1,la(s));
 dmatrix chol(fa+1,la(s),fa+1,la(s));
 
 int a,i;
 
 //cout<<"dim cov:"<< cov.colmin()<<" " <<cov.colmax()<<" "<< cov.rowmin()<<" "<< cov.rowmax() <<" "<<endl;
 
 random_number_generator rng(seed);
 seed++;
 
 noise.fill_randn(rng);
 /*
 cout<<"cov_uncertanty:"<<endl<<"N:"<<endl<<setfixed()<<setprecision(0)<<setw(10)<<N<<endl<<
     "COV:"<<endl<<setfixed()<<setprecision(3)<<setw(6)<<cov<<endl<<
      "noise:"<<endl<<setfixed()<<setprecision(3)<<setw(6)<<noise<<endl;
 
 */
 
 chol=trans(choleski_decomp(cov));      // it is an invariant - should go out of this function
 
 //cout<<"chol:"<<endl<<setfixed()<<setprecision(3)<<setw(6)<<chol<<endl;
 result=noise*chol;
 //cout<<"    result: "<<endl<<setprecision(3)<<setw(6)<<result<<endl;
 i=0; 
 if (un(1)==2) {for (a=fa+1;a<=la(s);a++) {i++; N(a)=exp(result(1,a)-cov(i,i)/2)*N(a)*un(2);}}
 else {for (a=fa+1;a<=la(s);a++) { N(a)=exp(result(1,a))*N(a)*un(2);}}
 
 //cout<<"N:"<<endl<<setfixed()<<setprecision(0)<<setw(10)<<N<<endl;
  
 
 //********************************************************************************************* 

 

FUNCTION double real_time_estimate(int s, int TAC_year, d3_array N_true, d3_array Z, double noise);
 double N1;
 // estimate observed N(year, first season, first age+1) from true N(year-1,last season, first age)
 //   and real-time uncertanties

    N1=N_true(s,lq,fa)*exp(-Z(s,lq,fa));
    if (test_output==53) cout<<"Real time. N_true(first age), last season of "<<setprecision(0)<<TAC_year-1<<":"
                                <<N_true(s,lq,fa)<<" N_true(second age,y+1)("
                                <<TAC_year<<"):"<<N1;
    N1=N1*uncertanty(s,real_time_uncertanty(s),noise);
    if (test_output==53) cout<<" N_obs(second age):"<<N1<<endl;
    return(N1);
 //*********************************************************************************************

 //*********************************************************************************************
FUNCTION double survey_estimate_last_year(int s, int q, d3_array N,  double noise);
 double index;
 // estimate observed N(year, first season, first age+1) from true N(year-1,last season, first age)
 //   and real-time uncertanties

    index=N(s,q,fa);  // example first age (recruitment) index
    index=index*uncertanty(s,survey_uncertanty(s),noise);
    if (test_output==53) cout<<"survey_estimate_last_year"<<":"<<setprecision(0)<<" with noise:"<<index<<endl;
    return(index);
 //*********************************************************************************************


FUNCTION double do_HCR_trigger(int s,double x);  // Estimate F or TAC 
 double tmp;
 
 if (test_output==53) cout<<setprecision(3)<<setscientific()<<"HCR trigger: "<<x<<"  Result="; 
 if (x < T1(s)) {
   tmp=maxFT1(s,1)  +x*maxFT1(s,2);
   if (test_output==53) cout<<maxFT1(s,1)<<" + "<<maxFT1(s,2)<<"*"<<x<<"="<<tmp<<endl; 
 }   
 else  if (x < T2(s)) {
   tmp=maxFT1T2(s,1)+(x-T1(s))*maxFT1T2(s,2); 
   if (test_output==53) cout<<maxFT1T2(s,1)<<" + "<<maxFT1T2(s,2)<<"*"<<x-T1(s)<<"="<<tmp<<endl; 
 }
 else  if (x >= T2(s)) {
   tmp=maxFT2(s,1)  +(x-T2(s))*maxFT2(s,2);
   if (test_output==53) cout <<maxFT2(s,1)<<" + "<<maxFT2(s,2)<<"*"<<x-T2(s)<<"="<<tmp<<endl; 
 }  
 if (tmp<0) tmp=0;
 if (test_output==53) cout<<setfixed();
 return tmp;

 //********************************************************************************************* 
FUNCTION void predict_year(int y, int first_sp, int last_sp, int do_M2, int do_catch, int new_year, d3_array& pred_N, d3_array& pred_N_other, d3_array pred_F, d3_array& pred_C, d3_array pred_M, d3_array pred_M1, d3_array& pred_M2, d3_array& pred_Z);
  // take stock numbers, first season and predict N and C for the remaining quarters of the year
  // and predict first season of the next year (option new_year).
  // input year (y) is the year for catches and yield 
 int s,a,q,iter;
 double sum2;
 dmatrix     pred_tmp_M2(first_VPA,nsp,fa,max_a);
 dmatrix     pred_M1F(first_VPA,nsp,fa,max_a);
 d3_array    pred_Nbar_stom(1,nsp,fq,lq,fa,max_a);
 d3_array    pred_Nbar(1,nsp,fq,lq,fa,max_a);
 
 if (multi==0) {        // single species prediction
    for (s=first_sp;s<=last_sp;s++) pred_Z(s)=pred_F(s)+pred_M(s);
 } 

 if (multi==2) {
     for (s=1;s<first_VPA;s++) {
        pred_Nbar(s)=pred_N_other(s);
        pred_Nbar_stom(s)=pred_Nbar(s);
     }
 }
 
 for (q=fq;q<=lq;q++){
   if (multi==2) {                        // multispecies species mode
     if (do_M2==0) for (s=first_sp;s<=last_sp;s++) pred_Z(s)=pred_F(s)+pred_M1(s)+pred_M2(s);
     else { //re-calc M2
       for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) pred_M1F(s,a)=pred_M1(s,q,a)+pred_F(s,q,a);
       if(use_Nbar==0 && do_growth_all==0) {   //use N in the start of the season for calc. of M2
            get_pred_Nbar_stom_at_age(q, pred_N, pred_Nbar, pred_Nbar_stom) ;
            calc_pred_M2(y,q, pred_M1, pred_M2, pred_F, pred_N, pred_Nbar_stom);
       }           
       else {  //(use_Nbar==1 or growth), //use mean N in the season for calc. of M2. Iterations are needed
         iter=0; sum2=100;
         for (s=first_VPA;s<=nsp;s++) for(a=fa;a<=la(s);a++)pred_tmp_M2(s,a)=pred_M2(s,q,a);          
 
         while (iter<20 && sum2>1e-6 ) {
           iter++;
           //cout<<"Y:"<<y<<" Q:"<<q<<" iter:"<<iter<<endl;
           if (use_Nbar==1) for (s=first_VPA;s<=nsp;s++) for(a=fa;a<=la(s);a++) {
              pred_Z(s,q,a)=pred_M1F(s,a)+pred_tmp_M2(s,a);
              pred_Nbar(s,q,a)=pred_N(s,q,a)*(1-exp(-pred_Z(s,q,a)))/pred_Z(s,q,a); 
           }
           get_pred_Nbar_stom_at_age(q, pred_N, pred_Nbar, pred_Nbar_stom) ;
           if (q==fq && do_growth_all==1) calc_growth(pred_N);
           calc_pred_M2(y,q,pred_M1, pred_M2, pred_F, pred_N, pred_Nbar_stom);
                
           sum2=0.0;
           for (s=first_VPA;s<=nsp;s++) for(a=faq(q);a<=la(s);a++) {
             if (pred_M2(s,q,a) != pred_tmp_M2(s,a)) sum2=sum2+pow(pred_M2(s,q,a)-pred_tmp_M2(s,a),2);
             pred_tmp_M2(s,a)=pred_M2(s,q,a);
           }
         }       // end while   
        }         // end (use_Nbar==1)

        if (test_output==953) {
           for (s=first_VPA;s<=nsp;s++)  if (sum(pred_M2(s,q))>0){
             cout<<"M2:"<<species_names[s]<<" season:"<<q<<endl<<setfixed()<<setprecision(3)<<pred_M2(s,q)<<endl;
           }    
        }
        for (s=first_VPA;s<=nsp;s++) for(a=faq(q);a<=la(s);a++) pred_Z(s,q,a)=pred_M1F(s,a)+pred_M2(s,q,a);         
      }     // end if do_M2==1
    }       // end if multi==2   
  
 
   if (do_catch) for (s=first_sp;s<=last_sp;s++) 
   {
    for (a=faq(q);a<=la(s);a++) {
      // catch
      pred_Nbar(s,q,a)=pred_N(s,q,a)*(1-exp(-pred_Z(s,q,a)))/pred_Z(s,q,a); 
      pred_C(s,q,a)=pred_Nbar(s,q,a)*pred_F(s,q,a);   
      // Yield
      yield(s,y)+=pred_C(s,q,a)*pred_weca(s,q,a)*pred_prop_landed(s,q,a);
      // eaten biomass
      if (multi==2 && is_prey(s)==1) eaten_M2(s,y)+=pred_Nbar(s,q,a)*pred_M2(s,q,a)*pred_west(s,q,a);
     }
    }

    //predict next season's N 
    for (s=first_sp;s<=last_sp;s++) for (a=la(s);a>=fa;a--) if (!(a==fa && q<recq)) {
      if (q==lq && new_year) {
        if (a==la(s) && nplus(s)==1) {
            pred_N(s,fq,a)=pred_N(s,lq,a-1)*exp(-pred_Z(s,lq,a-1))+pred_N(s,lq,a)*exp(-pred_Z(s,lq,a));
        }
        else if (a==la(s) && nplus(s)==0) {
            pred_N(s,fq,a)=pred_N(s,lq,a-1)*exp(-pred_Z(s,lq,a-1)); 
        }
        else if (a>fa) pred_N(s,fq,a)=pred_N(s,lq,a-1)*exp(-pred_Z(s,lq,a-1)); 
      }
      else if (q!=lq) pred_N(s,q+1,a)=pred_N(s,q,a)*exp(-pred_Z(s,q,a));  
    }
     
    // clean up with previous years values
    //if (new_year && q>fq && q!=lq) for (s=first_sp;s<=last_sp;s++) pred_N(s,q)=0.0; 
    if (new_year && q>fq ) for (s=first_sp;s<=last_sp;s++) pred_N(s,q)=0.0; 
  }  // end season loop 

 // update weight in the stock
 for (s=first_sp;s<=last_sp;s++) if (do_growth_sp(s)) pred_west_old(s)=pred_west(s);
 // *****************************************************************************************
    
FUNCTION predict
  int s,y,q,d,a,i,step;
  int yq;
  double sumW;

  if (test_output==53 && multi==2) {
    cout<<"stl_other_suit_slope used in forecast:"<<endl<<setfixed()<<setprecision(3)<<stl_other_suit_slope<<endl<<endl;
     cout<<"season_overlap used in forecast:"<<endl<<setfixed()<<setprecision(3);
     for(d=1;d<=no_areas;d++){
       if (no_areas>1) cout<<"Area:"<<d<<endl;
       for (s=1;s<=npr;s++) {
         cout<<"Predator: "<<species_names[s]<<endl;
         cout<<season_overlap(d,s)<<endl;
       }
     }   
  }
    //4darray     season_overlap(1,no_areas,1,npr,fq,lq,0,nsp)     //predator prey overlap

 // size in the sea from  size_sea(yqd,pred,pred_a);
 y=lyModel; // to obtain size at age etc.
 d=1;
 for (q=fq;q<=lq;q++) {
   CALC_yqd
  for (s=1;s<=nsp;s++) for (a=faq(q);a<=la(s);a++)   pred_size_sea(s,q,a)= size_sea(yqd,s,a);
 }
 
  // calc prediction mean weigth in the catch
  for (s=first_VPA;s<=nsp;s++) {
    for (q=fq;q<=lq;q++){
      for (a=faq(q);a<=la(s);a++){
          i=0;
          sumW=0.0;
          for (y=first_year_weca(s);y<=last_year_weca(s);y++) {
            CALC_yq
            if ( weca(yq,s,a) >0) {         
              sumW+=weca(yq,s,a);
              i++;
            }
         }
         if (i>0) pred_weca(s,q,a)=sumW/i; else pred_weca(s,q,a)=0.0;
       }
     } 
     // adjust in case of annual cataches is used in likelihood option seasonal_annual_catches =1
     if (seasonal_annual_catches(s)==1) for (a=fa;a<=la(s);a++) for (q=fq+1;q<=lq;q++) pred_weca(s,q,a)=pred_weca(s,fq,a);    
   }  

  // calc mean proportion landed
  for (s=first_VPA;s<=nsp;s++) {
    for (q=fq;q<=lq;q++){
      for (a=faq(q);a<=la(s);a++){
          i=0;
          sumW=0.0;
          for (y=first_year_prop_landed(s);y<=last_year_prop_landed(s);y++) {
            CALC_yq
              sumW+=prop_landed(yq,s,a);
              i++;
         }
         pred_prop_landed(s,q,a)=sumW/i;
       }
     }
   }

  // adjust seasonal_annual_catches
   
  // calc prediction mean weigth in the sea
  for (s=1;s<=nsp;s++) {
   pred_west(s)=0.0;
   for (q=fq;q<=lq;q++) { 
    for (a=faq(q);a<=la(s);a++){
        y=lyModel;
        CALC_yq
         if (s<first_VPA)pred_west(s,q,a)=west(yq,s,a);       
         else {
           i=0;
           sumW=0.0;
           for (y=first_year_wsea(s);y<=last_year_wsea(s);y++) {
             CALC_yq
             if ( west(yq,s,a) >0) {         
               sumW+=west(yq,s,a);
               i++;
             }
           }
           if (i>0) pred_west(s,q,a)=sumW/i; else pred_west(s,q,a)=0.0;
         }
       }
     }  
   }

   
  // calc prediction proportion mature
  for (s=first_VPA;s<=nsp;s++) {
    for (q=fq;q<=lq;q++) { 
      for (a=faq(q);a<=la(s);a++){
            i=0;
           sumW=0.0;
           for (y=first_year_propmat(s);y<=last_year_propmat(s);y++) {
             CALC_yq
             sumW+=propmat(yq,s,a);
             i++;
           }
           if (i>0) pred_propmat(s,q,a)=sumW/i; else pred_propmat(s,q,a)=0.0;
       }
     }  
   }   
   
   
  if (at_age_output[6]) {
  ofstream FLR("flr_cnf.out",ios::out);
  FLR<<"Species.n Year Quarter Age C N F M west weca propmat";
  if (multi==2) FLR<<" M1 M2";
  FLR<<endl;
  for (y=fyModel;y<=lyModel;y++){
    for (s=first_VPA;s<=nsp;s++) {
      for (a=fa;a<=la(s);a++) {
       for (q=fq;q<=lq;q++) {
         CALC_yq 
         FLR<<s<<" "<<y<<" "<<q<<" "<<a<<" "<<obs_C(yq,s,a)<<" "<<N(yq,s,a)<<" "<<F(yq,s,a)<<
              " "<<M(yq,s,a)<<" "<<west(yq,s,a)<<" "<<weca(yq,s,a)<<
              " "<<propmat(yq,s,a);
         if (multi==2) FLR<<" "<<M1(yq,s,a)<<" "<<M2(yq,s,a);
         FLR <<endl;
       }
     }
    }
   }
  }
 
  // various initialization
  MCMC_prediction++;
  do_prediction_mean=1;

  ofstream res2("partial_m2_prediction.out",ios::out);
  res2 << "Year Quarter Predator.no Predator.age Prey.no Prey.age Part.M2 suit "<<endl;

  if( do_growth_all==1) {
    ofstream g1("mean_weights.out",ios::out);
    g1 << "Year Quarter Species.n Age west weca size N "<<endl;
    g1.close();
  }
  // read SSB/R parameters from file according to option
  if (read_SSB_R==1) for (s=first_VPA;s<=nsp;s++) {
    SSB_Rec_model(s)=int(SSB_R_in(s,1));
    SSB_R_alfa(s)=SSB_R_in(s,2);
    SSB_R_beta(s)=SSB_R_in(s,3);
    if (SSB_Rec_model(s)==100) SSB_Rec_hockey_breakpoint(s)=SSB_R_in(s,3);
    SSB_R_s2(s)=pow(SSB_R_in(s,4),2);
   } else {
     if (est_calc_sigma(3)!=0) {
       cout<<"WARNING: option calc.est.sigma [recruit] should be 0 for a correct estimate of recruitment variance in predictions"<<endl<<
       "a value of: "<<SSB_R_s2_ini<<"has been used"<<endl<<endl;
   }}
  //cout <<"SSB_Rec_model:" << SSB_Rec_model<<endl<<"SSB_R_alfa:"<<SSB_R_alfa<<endl<<"SSB_R_beta:"<<SSB_R_beta<<endl<<"SSB_R_beta_cor:"<<endl<<SSB_R_beta_cor<<endl;
  
  // repeat for each MCMC iteration
  step=no_MCMC_iterations/10;    // counter for sceen output
  for (MCMC_iteration=1;MCMC_iteration<=no_MCMC_iterations;MCMC_iteration++) {
    if (MCMC_iteration<10 || MCMC_iteration==no_MCMC_iterations || MCMC_iteration % step==0) {
      cout<<"start on predicion. MCMC="<<MCMC_prediction<<"  iterration="<<MCMC_iteration;
      if (read_SSB_R==1 && MCMC_iteration==1) cout<<" Using recruitment parameters from file"<<endl; else cout<<endl;
     
    }
    if (n_init_pop>1) init_pop= MCMC_prediction;
    // refresh variables that might be changed
    TAC_constraint=TAC_constraint_copy;
    real_time_uncertanty=real_time_uncertanty_copy;
    HCR=HCR_copy;
    if (make_sim_data==0) do_predict(MCMC_iteration);
  }
  if (make_sim_data==1) make_sim_data_proc(MCMC_prediction);

FUNCTION void make_sim_data_proc(int iteration)
  int s,y,a,q, no_set;
  no_set=no_MCMC_iterations;
  get_expected_catch_at_age();
  print_catch_data_noise(no_set,2);
  
  CPUE_parm_adm();
  catchability_move();
  print_survey_data_noise(no_set,2);

  // calc_expected_stomach_content();
  // print_stomach_data_sim(iteration);
  exit(9);
  
FUNCTION void do_predict(int MCMC_iteration)
  int s,y,a,q,TAC_year;
  int yq;
  int do_M2, do_catch, add_rec_noise, true_N, new_year;

  double tmp, upper_F,max_F_scale,capF_local;
  
  int BW_init_phase=1;  // initial conditions according to para. 4 in Blue whiting HCR
  
  dmatrix rn(first_VPA,nsp,lyModel,lpy+2);  // random numbers for SSB/R noise

  dvector meanFsq(first_VPA,nsp);   // mean F stautus quo
  dvector meanFsq_copy(first_VPA,nsp);
  double meanFsq_tmp;
  double meanFsq_tmp2;
  
  dvector min_noise(first_VPA,nsp);
  dvector TACcurrent(first_VPA,nsp);

  ivector has_changed_exploitation_pattern(first_VPA,nsp);
  ivector sub_HCR(first_VPA,nsp);
   
  dmatrix TAC_true(first_VPA,nsp,lyModel,lpy+1);      // real (implemented) TAC
  dmatrix TAC_F_true(first_VPA,nsp,lyModel,lpy+1);    // real (implemented) F 
  dmatrix TAC_obs(first_VPA,nsp,lyModel,lpy+1);       //  TAC estimated from HCR
  dmatrix TAC_F_obs(first_VPA,nsp,lyModel,lpy+1);     //  F estimated from HCR

  d3_array TAC_true_half(first_VPA,nsp,lyModel,lpy+1,1,2);
  
  dmatrix SSB_obs(first_VPA,nsp,lyModel,lpy+1);

  double TSB0, yield1,  change,tacMax;
  double yieldQ4, yieldQ1Q3;
  double Fmax, Fmin, Fscaling;

  d3_array    pred_Z_true(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_Z_obs(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_M(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_M1(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_M2_true(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_M2_obs(first_VPA,nsp,fq,lq,fa,max_a);

  d3_array    pred_F_true(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_F_true_tmp(first_VPA,nsp,fq,lq,fa,max_a);
  d3_array    pred_F_obs(first_VPA,nsp,fq,lq,fa,max_a);

  d3_array    pred_F_sq(first_VPA,nsp,fq,lq,fa,max_a); // F status quo (exploitation pattern)
  d3_array    pred_F_sq_copy(first_VPA,nsp,fq,lq,fa,max_a);
  dmatrix     pred_F_sq_tmp(fq,lq,fa,max_a);
  dmatrix     pred_F_sq_tmp_year2(fq,lq,fa,max_a);

  d3_array    pred_N_assess(1,nsp,fq,lq,fa,max_a);              // N in the last assessment year
  d3_array    pred_N_true(first_VPA,nsp,fq,lq,fa,max_a);       // true stock numbers
  d3_array    pred_N_true_tmp(first_VPA,nsp,fq,lq,fa,max_a);       // true tmp stock numbers
  d3_array    previous_year_N_true(first_VPA,nsp,fq,lq,fa,max_a);  // true stock numbers, previous year

  d3_array    pred_N_obs(first_VPA,nsp,fq,lq,fa,max_a);        // "observed" N  that is real stock N (pred_N_true)
                                                       // and optional some "observation noise"

  d3_array    pred_N_other(1,first_VPA,fq,lq,fa,max_a);  // other predator stock numbers
  
  d3_array    pred_N_real_time(first_VPA,nsp,fq,lq,fa,max_a);  // N estimated from real time (within quota year)
                                                       //  estimation

   
  d3_array    pred_C(first_VPA,nsp,fq,lq,fa,max_a);
  dmatrix     recruit(first_VPA,nsp,fyModel,lpy);
  dmatrix     hist_rec_noise(first_VPA,nsp,1,no_recruit_autocor); // recruitment noise for the most recent 
                                                                 //   years, used for autocorrelation
  dmatrix     cov_noise(fa+1,max_a,fa+1,max_a);
  
  char txt[2];
  char txtL[15];
                                                                   
  int FLRW;
  FLRW=0;
  
  // various initialization  

  max_F_scale=10.0;
  meanFsq=0.0;
  SSB_obs=0.0;

  TAC_true=-1.0; 
  TAC_F_true=-1.0; 
  TAC_obs=0.0;
  TAC_F_obs=0.0;
  HCR_state=1;
  
  has_changed_exploitation_pattern=0;
  
  random_number_generator rng(seed);
  //cout<<"seed: "<<seed<<endl;
  rn.fill_randn(rng);  // random numbers for SSB-recuit noise 
  // truncate noise
  for (s=first_VPA;s<=nsp;s++) if (rec_noise_input(s)==0 && !( rec_noise_trunc(s,1)==0 && rec_noise_trunc(s,2)==0)) {
    for (y=lyModel;y<=lpy+2;y++) {
      while (rn(s,y) <rec_noise_trunc(s,1) || rn(s,y)>rec_noise_trunc(s,2)) rn(s,y)=randn(rng);
    }
  }
    
  // overwrite with recruitment residuals
  if (use_rec_noise_input==1) for (s=first_VPA;s<=nsp;s++) if (rec_noise_input(s)==1) {
    cout<<"rec_residuals(:"<<rec_residuals(s)<<endl;
    for (y=lyModel;y<=lpy+2;y++)rn(s,y)=rec_residuals(s,y)/sqrt(value(SSB_R_s2(s)));  // so residuals fit in as noise
    cout<<"rn:"<<rn(s)<<endl;
  }
     
  ofstream obj_func_file("obj_func_file.out",ios::app);
  if (!(obj_func_file)) {
     cerr << "Error trying to open obj_func_file.out for appending output "<<endl;
     exit(1);
  }

  print_summary_MCMC();

  for (s=1;s<=nsp;s++){
   obj_func_file<<MCMC_prediction<<" "<<s<<" "<<obj_func(s)<<" "<<obf<<endl;
  }
  
  //Set biomasses and yield to zero for forecast years
  for (s=first_VPA;s<=nsp;s++){
    for (y=lyModel+1;y<=lpy;y++){
      SSB(s,y)=0.0;
      TSB(s,y)=0.0;
      yield(s,y)=0.0;
      eaten_M2(s,y)=0.0;
    }
  }
    
  // save historical M2
  if (multi==2){
    for (y=fyModel;y<=lyModel;y++) {
      for (q=fq;q<=lq;q++) {
        CALC_yq
        for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++){
           pred_M2_true(s,q,a)=value(M2(yq,s,a));
        }
      } 
      if (at_age_output[2]) {strcpy(txt,"M2"); out_predict_raw3(txt,y, MCMC_iteration,pred_M2_true); }
    } 
  }

  // move historic recruitment and M2 to a new array
  for (s=first_VPA;s<=nsp;s++){
    for (y=fyModel;y<=lyModel;y++){
      q=recq;
      CALC_yq
      recruit(s,y)=value(N(yq,s,fa));  //for output only
    }
  }
  // Move last year other predator N to prediction variable
  if (multi==2) {
    y=lyModel;
    for (s=1;s<=nOthPred;s++) for (q=fq;q<=lq;q++) {
       CALC_yq
      for (a=faq(q);a<=la(s);a++) { 
        pred_N_other(s,q,a)=value(N(yq,s,a));
      }
    }
  } 
  // Move last assessment year N, F, M and Z to prediction variables 
  // (and predict remaining seasons' N, if missing) calc last year Yield
  for (s=first_VPA;s<=nsp;s++){
     SSB_obs(s,lyModel)=value(SSB(s,lyModel));
     for (q=fq;q<=lq;q++){
       y=lyModel;
       CALC_yq      
       for (a=faq(q);a<=la(s);a++){
         pred_M(s,q,a)=M(yq,s,a);
          if (multi==2) {
           pred_M1(s,q,a) =M1(yq,s,a);
           pred_M2_true(s,q,a) =value(M2(yq,s,a));
         }
        
        // Fishing mortality
        if (q<=lqly) {
          pred_F_sq(s,q,a)=value(F(yq,s,a));             // use last year's F as F status quo 
         }
        else { 
          // calculate F using most recent F-year factor and historical selection pattern 
          //   and input adjusment factor
          pred_F_sq(s,q,a)=value(F_y(s,lyModel)* F_q_last_year(s,q,a)*F_a(s,lyModel,a)
                           *last_year_season_F_adjustment(s));     
        }
        //overwrite with input exploitation pattern, if specified
        if (use_read_expl_pat==1 && (lyModel+1)>=read_expl_pat_first_year
                                     && (lyModel+1)<=read_expl_pat_last_year && read_expl_pat==1) {
          pred_F_sq(s,q,a)=init_exploitation_pattern(s,lyModel+1,q,a);
        }


        // Total mortality
        if (multi==2) pred_Z_true(s,q,a)=pred_F_sq(s,q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
        else  pred_Z_true(s,q,a)=pred_F_sq(s,q,a)+pred_M(s,q,a);  
       
        // stock numbers
        if (q==fq) pred_N_assess(s,q,a)=value(N(yq,s,a));
        if ((q==recq) && (a==fa)) {    // recruits 
          if (q>lqly) {  // no recruis in last year's assessment           
            // overwrite rec estimate if specified by input option 
            if (lyModel>=read_predict_N_first_year && lyModel<=read_predict_N_last_year && read_predict_N==1) {
               if (init_predict_N(s,y,init_pop,a)>=0) {
                  pred_N_assess(s,recq,fa)=init_predict_N(s,y,init_pop,fa);
                 recruit(s,lyModel)=pred_N_assess(s,recq,fa);
               }
            }
            else {
               pred_N_assess(s,recq,fa)=SSB_recruit(s,value(SSB(s,lyModel-fa)),rn(s,y),hist_rec_noise,y);
            }
          } else  pred_N_assess(s,q,a)=value(N(yq,s,a));
        }
        else if (q>fq && q<=(lqly+1)) pred_N_assess(s,q,a)=pred_N_assess(s,q-1,a)*exp(-pred_Z_true(s,q-1,a));  
         

        // yield        
        if (q<=lqly) {
          TAC_obs(s,lyModel)+=obs_C(yq,s,a)*weca(yq,s,a)*prop_landed(yq,s,a);
         }
        //else {
        //  TAC_obs(s,lyModel)+=pred_F_sq(s,q,a)/pred_Z_true(s,q,a)*pred_N_assess(s,q,a)
        //                       *(1-exp(-pred_Z_true(s,q,a)))*pred_weca(s,q,a)*pred_prop_landed(s,q,a);
        // }
        
       // calc mean F status quo
       if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq(s)+=pred_F_sq(s,q,a);
  
       } //age loop
    }  // q loop
    
    meanFsq(s)=meanFsq(s)/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
    meanFsq_copy(s)=meanFsq(s);
    
    pred_F_sq_copy(s)=pred_F_sq(s);
     
    //Copy assessment values to N_true (later projected forward one year) 
    pred_N_true(s)=pred_N_assess(s);
    
  }   //end species loop
 
  
  for (s=first_VPA;s<=nsp;s++) if (sum(pred_F_sq(s))==0 && read_expl_pat!=1) { 
    cout<<"ERROR: exploitation pattern is zero for species "<<species_names[s]<<endl;
    cout<<pred_F_sq(s)<<endl;
    cout<<"As an alternative, you can give exploitation pattern as input."<<endl<<"Program stopped"<<endl;
    exit(9);
  }
  
  pred_F_true=pred_F_sq; 
  for (s=first_VPA;s<=nsp;s++) TAC_F_true(s,lyModel)=meanFsq(s);
  
  for (s=first_VPA;s<=nsp;s++) if (HCR(s)>=101  && HCR(s)<=111) {
    if (test_output>=52) cout<<"Special case. Norway pout 2012 MSE"<<endl;
    for (q=fq;q<=lq;q++){
       for (a=faq(q);a<=la(s);a++){
         pred_F_true(s,q,a)=pred_F_true(s,q,a)*0.01;
       }
       }
  } 
    
  if (test_output>=52) cout<<endl<<"YEAR (for which the catches are known) :"<<lyModel<<endl<<endl;
  if (test_output==53) {
    cout<<"assessment N:"<<setw(12) << setprecision(0) << setfixed()<<endl;;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<pred_N_assess(s)<<endl;
    }
    cout<<"assessment year yield:"<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<TAC_obs(s,lyModel)<<endl;
    }
    cout<<"assessment year SSB:"<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<SSB(s,lyModel)<<endl;
    }
    cout<<"Forecast exploitation pattern:"<<endl<<setprecision(3);
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<pred_F_sq(s)<<endl;
    }
    cout<<"assessment year Mean F:"<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<meanFsq(s)<<endl;
    }

    cout<<endl<<"Fixed values used in forecast:"<<endl;
    
    cout<<"west:"<<setw(8) << setprecision(4) << setfixed()<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<pred_west(s)<<endl;
    }
   cout<<"weca:"<<setw(8) << setprecision(4) << setfixed()<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<pred_weca(s)<<endl;
    }
    cout<<"proportion mature:"<<setw(8) << setprecision(4) << setfixed()<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<pred_propmat(s)<<endl;
    }
    cout<<"single species M:"<<setw(8) << setprecision(4) << setfixed()<<endl;
    for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<endl;
      cout<<pred_M(s)<<endl;
    }
   if (read_expl_pat==1) {
      if (use_read_expl_pat==1) cout<<"input exploitation pattern from file exploitation_pattern.in:";
      else cout<<"F values:";
      cout<<setw(8) << setprecision(4) << setfixed()<<endl;
      for (y=read_expl_pat_first_year;y<=read_expl_pat_last_year;y++) {
        cout<<"# "<<y<<endl;
        for (s=first_VPA;s<=nsp;s++) {
          if (nsp>1) cout<<species_names[s]<<endl;
          if (read_expl_pat==1) cout<<init_exploitation_pattern(s,y)<<endl;
          else cout<<pred_F_sq(s)<<endl; 
        }
      }
    }
    if (multi==2 && first_VPA>1 ) {
    cout<<"Other predator stock number:"<<setw(12) << setprecision(0) << setfixed()<<endl;
     y=lyModel;
     for (s=1;s<first_VPA;s++) {
       cout<<species_names[s]<<endl;
       for (q=fq;q<=lq;q++){
         CALC_yq
         cout<<(N(yq,s))<<endl;
       }
     }
  }
 }

   hist_rec_noise=0.0;  //reset autocorrelation

   // init value, no closure or constraints applied of the future fisheries
   closure=0;
   constraints=1;
  
  
   
  // *****************************************************************  
  // #### We are now ready to do the predictions;  
  //calc prediction N, and C  values for prediction years
  
  for (y=lyModel+1;y<=lpy;y++){      

    if (test_output>=53) {
      cout<<"*****************************************************************"<<endl;
      cout<<endl<<"YEAR (for which the TAC or F are known already):"<<y<<endl<<endl;
    }
    
    // update other predator stock N
    if (multi==2) {
      for (s=1;s<=nOthPred;s++) {
        other_bio(s,y)=other_bio(s,y-1);
        if (other_pred_N_first_year(s)>0) {
         if ((other_pred_N_first_year(s)<=y-lyModel) && (other_pred_N_last_year(s)>=y-lyModel)) {
           for (q=fq;q<=lq;q++) for (a=fa;a<=la(s);a++) { 
             pred_N_other(s,q,a)=pred_N_other(s,q,a)*other_pred_N_change(s);
             other_bio(s,y,q)=other_bio(s,y-1,q)*other_pred_N_change(s);
           }
         } 
        } 
      }
    }
    
    // estimate true stock numbers 1. Jan using previous year's N and F & M
    do_M2=0;     // do not re-calc M2
    do_catch=0;  // do not calc catch and yield 
    new_year=1;  // project stock N one year ahead
    predict_year(0,first_VPA, nsp, do_M2, do_catch, new_year, pred_N_true,pred_N_other, pred_F_true, 
                   pred_C, pred_M, pred_M1, pred_M2_true, pred_Z_true);
   
    //cout<<"pred_F_true"<<endl<<pred_F_true<<endl;
    //cout<<"pred_N_true:"<<endl<<pred_N_true<<endl; 
    // estimation of recruits
    add_rec_noise=1;
    true_N=1;         // update true SSB and estimate true Rec from true SSB. (SSB_obs is not used)
    estimate_recruits(y, first_VPA, nsp, add_rec_noise, true_N, pred_N_true, recruit, SSB_obs,
                      pred_west,  pred_propmat, rn, hist_rec_noise);
  
    if (test_output==53) for (s=first_VPA;s<=nsp;s++) {
       if (nsp>1) cout<<species_names[s]<<" ";
       cout <<"N_true("<<y<<"):"<<setw(10) 
                                << setfixed()<<setprecision(0)<<pred_N_true(s,fq);
       if (recq != fq) cout<<" recruitment:"<<pred_N_true(s,recq,fa)<<endl;  
    }
   
    // special case for overwriting recruits or predicted stock numbers, used mainly for FLR simulations
    if (y>=read_predict_N_first_year && y<=read_predict_N_last_year && read_predict_N==1) {
      for (s=first_VPA;s<=nsp;s++){
        if (init_predict_N(s,y,init_pop,fa)>=0) pred_N_true(s,recq,fa)=init_predict_N(s,y,init_pop,fa);
        for (a=fa+1;a<=la(s);a++) if (init_predict_N(s,y,init_pop,a)>=0) pred_N_true(s,fq,a)=init_predict_N(s,y,init_pop,a);
         // calc SSB in start of the first season
        SSB(s,y)=0.0;
        TSB(s,y)=0.0;
        for (a=fa;a<=la(s);a++) SSB(s,y)+=pred_N_true(s,fq,a)*pred_west(s,fq,a)*pred_propmat(s,fq,a);
        for (a=fa;a<=la(s);a++) TSB(s,y)+=pred_N_true(s,fq,a)*pred_west(s,fq,a);

        SSB_obs(s,y)=value(SSB(s,y));
      }
      //if (MCMC_iteration==1) cout<<"Predicted N changed from input for year: "<<y<<endl;
      if (test_output==53) for (s=first_VPA;s<=nsp;s++) {
        if (nsp>1) cout<<species_names[s]<<" ";
        cout <<"From Input, N_true("<<y<<"):"<<setw(10)
                                << setfixed()<<setprecision(0)<<pred_N_true(s,fq)<<"  Recruitment:"<<pred_N_true(s,recq,fa)<<endl;
      }
    }
   
    //make table of single species input values
    if ((y==lyModel+1) && (MCMC_iteration==1) ) {
      ofstream sht("short-term-input.out",ios::out);
      for (s=first_VPA;s<=nsp;s++){

        sht<<endl<<species_names[s]<<endl;
        sht<<"Age,Weight in the stock (kg),Weight in the catch (kg),Proportion Mature,F,Stock Numbers (thousands)"<<endl;

        for (a=fa;a<=la(s);a++){
          sht<<setw(5)<<a<<','<<setfixed()<<setprecision(5)<<setw(10)<<pred_west(s,fq,a)<<','<<pred_weca(s,fq,a)<<','<<pred_propmat(s,fq,a)<<','
              <<pred_F_sq(s,fq,a)<<',' <<setprecision(0)<<setw(10)<<pred_N_true(s,fq,a)<<endl;
        }
        sht<<endl<<endl;
      }
      sht.close();
    }

    //make table of single species input values by season
    if ((y==lyModel+1) && (MCMC_iteration==1) ) {
      ofstream sht("forecast-input-detailed.out",ios::out);
      sht<<"Species, Quarter, Age,West,Weca,Prop_Mature,FI,M,N"<<endl;
      for (s=first_VPA;s<=nsp;s++){
        for (q=fq;q<=lq;q++) {
          for (a=faq(q);a<=la(s);a++){
            sht<<setw(5)<<s<<','<<q<<','<<a<<','<<setfixed()<<setprecision(5)<<setw(10)<<pred_west(s,q,a)<<','<<pred_weca(s,q,a)<<','<<
            pred_propmat(s,q,a)<<','<<pred_F_sq(s,q,a)<<','<<pred_M(s,q,a)<<',';
            if (q==fq) sht<<setprecision(1)<<pred_N_true(s,fq,a)<<endl;
            else sht<<'0'<<endl;
          }
        }
      }
      sht.close();
    }

    // first year only. Implement externally given TAC and F without error
    if (y==lyModel+1) {
      
      for (s=first_VPA;s<=nsp;s++) {
        TAC_true_half(s)=0.0; 
        if (TAC_first(s)>0) TAC_true(s,lyModel+1)=TAC_first(s);
        if (HCR(s)>=102 && HCR(s)<110) {
           TAC_true_half(s,lyModel+1,1)= TAC_first(s)*TAC_second(s);
           TAC_true_half(s,lyModel+1,2)= TAC_first(s)*(1.0-TAC_second(s));
           if (test_output==53) cout<< "TAC_true_half first and second half-year of "<<lyModel+1<< ": "<<TAC_true_half(s,lyModel+1)<<"  from input"<<endl;
        }  
        
        if (HCR(s)==100 || HCR(s)==101){
          TAC_true_half(s,lyModel+2,1)=TAC_second(s);
          if (test_output==53) cout<<"TAC for year "<<lyModel+2<<" has been set to "<< TAC_true_half(s,lyModel+2,1)<<" from input "<<endl; 
        }  
         
        if (F_first(s)>0) TAC_F_true(s,lyModel+1)=F_first(s)*meanFsq(s);
        if  (interYear(s)==2) {
           if (F_second(s)>0) TAC_F_true(s,lyModel+2)=F_second(s)*meanFsq(s);
           if (TAC_second(s)>0) TAC_true(s,lyModel+2)=TAC_second(s);
        }
      }
      TAC_obs=TAC_true;
      TAC_F_obs=TAC_F_true;
      //cout<<"TAC_obs:"<<TAC_obs<<endl;
      //cout<<"TAC_F_obs:"<<setprecision(3)<<TAC_F_obs<<endl     
    }

    // first year only. Change TAC constraints in recovery phase
    if (y==lyModel+1) {
      for (s=first_VPA;s<=nsp;s++) {
        if (HCR(s)==334 || HCR(s)==335) {
           TAC_constraint(s,1)=TAC_constraint(s,1)*real_time_uncertanty(s,3);
           TAC_constraint(s,2)=TAC_constraint(s,2)/real_time_uncertanty(s,3);
           if (test_output==53) cout<<" HCR=334&335 initialization. TAC constraints changed to:"<<setprecision(3)<< TAC_constraint(s)<<endl;
        }
       } 
     }
    
    // ##############################################
    //estimate F from previously set of TACs (or F)
   
    if (use_read_expl_pat==0 && y>=read_expl_pat_first_year && y<=read_expl_pat_last_year && read_expl_pat==1) {

      // special case for overwriting input exploitaion pattern (and F)
      // overwrite prediction F if  year is within specified years
      cout<<"F taken from input file for year: "<<y<<endl;
      for (s=first_VPA;s<=nsp;s++){
        meanFsq(s)=0.0;
        for (q=fq;q<=lq;q++){
          for (a=fa;a<=la(s);a++){
            pred_F_sq(s,q,a)=init_exploitation_pattern(s,y,q,a);
            // calc mean F status quo for new exploitation pattern
            if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq(s)+=pred_F_sq(s,q,a);
          } //age loop
        }  // q loop
        meanFsq(s)=meanFsq(s)/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
        meanFsq_copy(s)=meanFsq(s);
        pred_F_true(s)=pred_F_sq(s);
        pred_F_sq_copy(s)=pred_F_sq(s);
        Mean_F(s,y)=meanFsq(s);
        Mean_F_percieved(s,y)=meanFsq(s);

        if (test_output==53) cout<<species_names[s]<<": mean F: "<< setfixed()<<setprecision(3)<<meanFsq(s)<<endl;
      }     // species loop
     }
     
     else for (s=first_VPA;s<=nsp;s++) {
        //cout<<"TAC_F_true:"<<setprecision(3)<<TAC_F_true<<endl;
        //cout<<"TAC_true:"<<setprecision(0)<<TAC_true<<endl;

     // Estimate F, using HCR F or HCR TAC from previously year, or from first year input
       if (TAC_F_true(s,y)>-1.0) { //calc F from HCR F

         // adjust true F according to cap TAC (F has been adjusted earlier according to cap F )
         if (TAC_cap(s)>0 && (y-lyModel>interYear(s))) {
            //calc yield from F and true stock numbers
            yield1=calc_Yield_from_Fscaling(s,fq,TAC_F_true(s,y)/meanFsq(s),pred_M(s), 
                  pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_true(s),pred_weca(s),pred_prop_landed(s));
            if (yield1>TAC_cap(s)  ) {
              if (test_output==53) cout<<"Cap TAC adjustment:  ";
              Fscaling=find_Fscaling_from_target_yield(s,fq,real_time_F(s)/meanFsq(s),TAC_F_true(s,y)/meanFsq(s),TAC_cap(s), 
                        pred_M(s), pred_M1(s), pred_M2_true(s),pred_F_sq(s),
                        pred_N_true(s),pred_weca(s),pred_prop_landed(s));
              if (Fscaling>0) TAC_F_true(s,y)=meanFsq(s)*Fscaling;
            }
         } 
        if (TAC_min(s)>0 && (y-lyModel>interYear(s))) {
            //calc yield from F and true stock numbers
            yield1=calc_Yield_from_Fscaling(s,fq,TAC_F_true(s,y)/meanFsq(s),pred_M(s), 
                  pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_true(s),pred_weca(s),pred_prop_landed(s));
            if (yield1<TAC_min(s)  ) {
              if (test_output==53) cout<<"Min TAC adjustment:  ";
              Fscaling=find_Fscaling_from_target_yield(s,fq,real_time_F(s)/meanFsq(s),TAC_F_true(s,y)/meanFsq(s),TAC_min(s), 
                        pred_M(s), pred_M1(s), pred_M2_true(s),pred_F_sq(s),
                        pred_N_true(s),pred_weca(s),pred_prop_landed(s));
              if (Fscaling>0) TAC_F_true(s,y)=meanFsq(s)*Fscaling;
            }
         } 

         if (real_time_F(s)>TAC_F_true(s,y)) TAC_F_true(s,y)=real_time_F(s);   
         pred_F_true(s)=pred_F_sq(s)*TAC_F_true(s,y)/meanFsq(s);  
         Mean_F(s,y)=TAC_F_true(s,y);
         
         if (y-lyModel<=interYear(s)) {
             TAC_obs(s,y)=calc_Yield_from_Fscaling(s,fq,TAC_F_true(s,y)/meanFsq(s),pred_M(s),
                pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_true(s),pred_weca(s),pred_prop_landed(s));
             if (test_output==53) cout<<"Year ("<<y<<") with given input fixed F:"<<setprecision(3)<<TAC_F_true(s,y)
                        <<setprecision(0)<<", TAC_obs: "<<TAC_obs(s,y)<<endl;
         }
         
         if (test_output==53) {
            if (nsp>1) cout<<species_names[s]<<" ";
            cout<<"F("<<y<<") is estimated from mean F="<<setprecision(3)<<TAC_F_true(s,y)<<endl;
         }
      }
      
      else if (TAC_true(s,y)>0) { //calc F from TAC and N. F cannot exceed F cap 

        if (F_cap(s)>0) upper_F=F_cap(s)/meanFsq(s); else upper_F=max_F_scale; 
        if (TAC_true(s,y)>0) Fscaling=find_Fscaling_from_target_yield(s,fq,real_time_F(s)/meanFsq(s),upper_F, TAC_true(s,y), pred_M(s), pred_M1(s),
                  pred_M2_true(s), pred_F_sq(s), pred_N_true(s), pred_weca(s),pred_prop_landed(s));
        else Fscaling=0.0;
        Mean_F(s,y)=Fscaling*meanFsq(s);
        if (Mean_F(s,y)<real_time_F(s)) {
          Mean_F(s,y)=real_time_F(s);
          //cout <<"min F:"<<real_time_F(s)<<" Fscale before:"<<Fscaling;
           Fscaling=value(Mean_F(s,y))/meanFsq(s);
          //cout <<"Fscaling after:"<<Fscaling<<endl;
        }
        pred_F_true(s)=pred_F_sq(s)*Fscaling;
        if (y-lyModel<=interYear(s)) {
          TAC_F_obs(s,y)=value(Mean_F(s,y));
          if (test_output==53) {
            if (nsp>1) cout<<species_names[s]<<" ";
            cout<<"Special inital condition: TAC_F_obs("<<y<<") is set to: "<<setprecision(3)<<TAC_F_obs(s,y)<<endl;  
          }
        }

        if (test_output==53) {
           if (nsp>1) cout<<species_names[s]<<" "; 
           cout<<"F("<<y<<") is estimated from TAC="<<setprecision(0)<<TAC_true(s,y)<< " test tmp:"<<setprecision(3)<<tmp<<endl;;
           if (tmp>=F_cap(s) && F_cap(s)>0 ) cout<<" and F cap "<<setprecision(3)<<F_cap(s)<<" ";
           cout<<" resulting in mean F_true:"<<setprecision(3)<<Mean_F(s,y)<<endl;
         }   
      }
      else {
        cout<<"Error: neither TAC or F have been estimated for year("<<y<<"), Species:"<<s<<". program stopped."<<endl;
        exit(9);
      } 
      if (test_output==53) {
        if (nsp>1) cout<<species_names[s]<<" ";
        cout<<"F_true("<<y<<"):"<<setprecision(3)<<endl<<pred_F_true(s)<<endl;
      }
      
      if  (y==lyModel+1) { 
       if (HCR(s)==334) {  // special case for rule 334
         if (TAC_F_obs(s,y)>constantF(s)) {
           TAC_F_true(s,y+1)=TAC_F_obs(s,y)*(1.0-real_time_uncertanty(s,1)/100);   // reduce F by x%
           TAC_F_obs(s,y+1)=TAC_F_true(s,y+1);
           TAC_true(s,y+1)=-1;
           if (test_output==53) cout<<"BW (HCR=334) Init condition:  TAC_F_true("<<y+1<<")="<<setprecision(3)<<
                           TAC_F_true(s,y+1)<<" from TAC_F_obs("<<y<<")="<<TAC_F_obs(s,y)<<" and "<<setprecision(0)<<real_time_uncertanty(s,1)<<"% reduction"<<endl;
         }          
       } 
      }
      
      if (has_changed_exploitation_pattern(s)==1) {
         pred_F_sq(s)=pred_F_sq_copy(s);
         meanFsq(s)=meanFsq_copy(s);
         has_changed_exploitation_pattern(s)=0;
      }
      
    }  // end estimate F from previously set of TACs (or F)
    
    // we have now true stock numbers (the 1. Jan of the year=y) in variable pred_N_true,
    // true F (year=y) in variable pred_F_true and SSB 1. Jan in variable SSB

    // calculate (seasonal) N, C, M2, and Z for year y. Do not project into next year
    do_M2=1;    //  calc M2
    do_catch=1; //  calc catch and yield 
    new_year=0; // do not project stock N one year ahead  

    predict_year(y,first_VPA, nsp, do_M2, do_catch, new_year, pred_N_true, pred_N_other, pred_F_true, 
                   pred_C, pred_M, pred_M1, pred_M2_true, pred_Z_true);
    if (FLRW==1) {
      ofstream FLR("flr_cnf.out",ios::app);
      for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la(s);a++) for (q=fq;q<=lq;q++) {
         FLR<<s<<" "<<y<<" "<<q<<" "<<a<<" "<<pred_C(s,q,a)<<" "<<pred_N_true(s,q,a)<<" "<<pred_F_true(s,q,a)<<
              " "<<pred_M(s,q,a)<<" "<<pred_M1(s,q,a)<<" "<<pred_M2_true(s,q,a)<<" "<<pred_west(s,q,a)<<
              " "<<pred_weca(s,q,a)<<" "<<pred_propmat(s,q,a)<<endl;
    }
    }


   if( do_growth_all==1) {
     ofstream g1("mean_weights.out",ios::app);
     for (s=first_VPA;s<=nsp;s++)for (q=fq;q<=lq;q++) for (a=faq(q);a<=la(s);a++)  {
       g1<<y<<" "<<q<<" "<<s<<" "<<a<<" "<<pred_west(s,q,a)<<" "<<pred_weca(s,q,a) <<" "<<pred_size_sea(s,q,a)<<" "<<pred_N_true(s,q,a)<<endl;
     }
     g1.close();
   }
    // write detailed output files
   if (at_age_output[1])             {strcpy(txt,"F"); out_predict_raw3(txt, y, MCMC_iteration,pred_F_true);}
   if (at_age_output[2] && multi==2) {strcpy(txt,"M2"); out_predict_raw3(txt,y, MCMC_iteration,pred_M2_true); }
   if (at_age_output[3])             {strcpy(txt,"Z");out_predict_raw3(txt, y, MCMC_iteration,pred_Z_true);  }
   if (at_age_output[4])             {strcpy(txt,"N");out_predict_raw3(txt, y, MCMC_iteration,pred_N_true); }
   if (at_age_output[5])             {strcpy(txt,"C"); out_predict_raw3(txt, y, MCMC_iteration,pred_C); }

    // save N_true for later use
    for (s=first_VPA;s<=nsp;s++) previous_year_N_true(s)=pred_N_true(s);

    // #############################################################
    // all data for year=y are now saved and we are ready 
    //  to make and "assessment" and come up with F (or TAC) for next year
 
    // adjust exploitation pattern from input if such is specified (used for y=years from input
    if (use_read_expl_pat==1 && (y+1)>=read_expl_pat_first_year &&
                                (y+1)<=read_expl_pat_last_year && read_expl_pat==1) {
      if (MCMC_iteration==1) cout<<"Exploitation pattern changed from input for year:"<<y+1<<endl;
      for (s=first_VPA;s<=nsp;s++){
        meanFsq(s)=0.0;
        for (q=fq;q<=lq;q++){
          for (a=fa;a<=la(s);a++){
            pred_F_sq(s,q,a)=init_exploitation_pattern(s,y+1,q,a);
            // calc mean F status quo for new exploitation pattern
            if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq(s)+=pred_F_sq(s,q,a);
          } //age loop
        }  // q loop
        meanFsq(s)=meanFsq(s)/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
        meanFsq_copy(s)=meanFsq(s);
        pred_F_sq_copy(s)=pred_F_sq(s);
        if (test_output==53) {
            if (nsp>1) cout<<species_names[s]<<" ";
            cout<<"Exploitation pattern("<<y+1<<" - ):"<<setprecision(3)<<endl<<pred_F_sq(s)<<endl;
            cout<<"new mean F: "<< setfixed()<< setprecision(3)<<meanFsq(s)<<endl;
        }
      }     // species loop
    }   // end read exploitation pattern


    if (use_read_expl_pat==0 && (y+1)>=read_expl_pat_first_year &&
                                (y+1)<=read_expl_pat_last_year && read_expl_pat==1) {
      cout<<"F given as input for year:"<<y+1<<" no HCR calcs done"<<endl;
    }
    else {  

    // copy N_true to N_obs( used for "assessment"); noise will be put on later
    for (s=first_VPA;s<=nsp;s++) pred_N_obs(s)=pred_N_true(s);
    
    //cout<<"pred_N_true(1):"<<setprecision(0)<<pred_N_true(1)<<endl;
    //cout<<"pred_F_true(1):"<<setprecision(3)<<pred_F_true(1)<<endl;
    
    // project pred_N_obs one year forward to y=year+1
    do_M2=0;        // do not re-calc M2
    do_catch=0;     // do not calc catch and yield 
    new_year=1;     // do project stock N one year ahead giving N(y)
    predict_year(0,first_VPA, nsp, do_M2, do_catch, new_year, pred_N_obs, pred_N_other, pred_F_true, 
                   pred_C, pred_M, pred_M1, pred_M2_true, pred_Z_true);
                   
    for (s=first_VPA;s<=nsp;s++) if (HCR(s)>=98 && HCR(s)<=125)   {   // special Norway pout cases, save the true N
      pred_N_true_tmp(s)=pred_N_obs(s);
      if (test_output==53) for (s=first_VPA;s<=nsp;s++)cout<<"NOP HCR, pred_N_true_tmp ("<<y+1<<")"<<endl<<setprecision(0)<<pred_N_true_tmp(s)<<endl;
    }
    
    if (test_output==53) for (s=first_VPA;s<=nsp;s++) {
      if (nsp>1) cout<<species_names[s]<<" "; 
      cout <<"N_obs("<<y+1<<") before noise:"<<setw(10) 
                                << setfixed()<<setprecision(0)<<pred_N_obs(s,fq)<<endl;  
    }
    
    for (s=first_VPA;s<=nsp;s++)   {      // assessment noise and bias on stock numbers, first season (this simulates an assessment)
      SSB_obs(s,y+1)=0.0;
      if (assess_uncertanty(s,1)==1 ) {
         if (assess_uncertanty(s,4)==1) pred_N_obs(s,fq)=                           // same noise on all ages
                       pred_N_obs(s,fq)*uncertanty(s,assess_uncertanty(s),randn(rng));
         else for (a=fa;a<=la(s);a++) pred_N_obs(s,fq,a)=                                // different noise on all ages
                 pred_N_obs(s,fq,a)*uncertanty(s,assess_uncertanty(s),randn(rng));
      } 
      else  if (assess_uncertanty(s,1)==2 || assess_uncertanty(s,1)==3) {
           cov_uncertanty(s, assess_uncertanty(s), pred_N_obs(s,fq),coVariance(s).sub(fa,la(s)));
      }
      else  if (assess_uncertanty(s,1)==4) {
          //cout<<"assessment_CV_age(s):"<<setprecision(3)<<assessment_CV_age(s)<<endl;
          age_uncertanty(s, assess_uncertanty(s),pred_N_obs(s,fq), assessment_CV_age(s));
      } 
      else {
        cout<<"ERROR in chosen assessment noise option Option="<<assess_uncertanty(s,1)<<" is not valid."<<endl;
        cout<<"  program stopped"<<endl;
        exit(9);
      }
      
      for (a=fa;a<=la(s);a++){
         SSB_obs(s,y+1)+=pred_N_obs(s,fq,a)*pred_west(s,fq,a)*pred_propmat(s,fq,a); 
      }
      
      if (test_output==53) if (assess_uncertanty(s,1)>=0){
          if (nsp>1) cout<<species_names[s]<<" ";
          cout <<"N_obs after noise("<<y+1<<"):"<<setprecision(2)<< assess_uncertanty(s)<<endl<<setw(10)
                                << setfixed()<<setprecision(0)<<pred_N_obs(s,fq)<< "  SSB_obs("<<y+1<<"):"<<SSB_obs(s,y+1)<<endl;
      }
    }
    
   // estimation of "observed" recruits
    add_rec_noise=0;
    true_N=0;           // update obs SSB and estimate Rec from obs SSB.
    estimate_recruits(y+1, first_VPA, nsp, add_rec_noise, true_N, pred_N_obs, recruit, SSB_obs,
                      pred_west, pred_propmat,rn, hist_rec_noise);
    
     
    if (test_output==53 && recq==fq) for (s=first_VPA;s<=nsp;s++) 
         cout <<"N_obs after calc of recruit("<<y+1<<") and noise "<<endl<<"N: "<<setw(10)<<setfixed()<<setprecision(0)<<pred_N_obs(s,fq)<<endl;

     for (s=first_VPA;s<=nsp;s++) if (y<lpy) SSB_percieved(s,y+1)=SSB_obs(s,y+1);
              
   // we have now observed N 1. Jan year y+1, which is the basis for HCR 
   
   
    for (s=first_VPA;s<=nsp;s++) if (HCR(s)>=98 && HCR(s)<=111) {  // that night be wrong !!?
      pred_N_true_tmp(s,recq,fa)=pred_N_obs(s,recq,fa);
    }
    // and we have now true N 1. Jan year y+1, and guess on true recruitment which is the basis for HCR 
     
     
    // estimate percieved F, used in some HCR
    for (s=first_VPA;s<=nsp;s++) {
      if (y-lyModel<=interYear(s)) {           // initial condition
        if (TAC_F_true(s,y+1)>-1.0) Mean_F_percieved(s,y+1)= TAC_F_true(s,y+1);
        else if (TAC_true(s,y+1)>-1.0) Mean_F_percieved(s,y+1)=meanFsq(s)*
               find_Fscaling_from_target_yield(s,fq,0.0,max_F_scale, TAC_true(s,y+1), pred_M(s), pred_M1(s),
               pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
      }
      else {
        //cout<< "XXX TEST TAC_obs(s,y+1): "<<TAC_obs(s,y+1)<<endl;
        if (y<lpy) Mean_F_percieved(s,y+1)=meanFsq(s)*find_Fscaling_from_target_yield(s,fq,0.0,max_F_scale, TAC_obs(s,y+1), pred_M(s), pred_M1(s),
                pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
      }
      if (test_output==53) {
         if (y<lpy) {
           if (nsp>1) cout<<species_names[s]<<" ";
           cout<<setprecision(3)<<"Mean_F_percieved("<<y+1<<"): "<<Mean_F_percieved(s,y+1)<<endl;
         }  
      }
     } // end percieved F
    
    

   if (test_output==53) cout<<endl<<"Start to calc next year(i.e."<<y+1<<") TAC or F from HCR "<<endl;
            
   // ##### calc new TAC or F according to HCR and constraints       
   for (s=first_VPA;s<=nsp;s++) if (y+interYear(s)<lpy+1) {
     TAC_year=y+interYear(s);      
     // project through intermediate year
     if(interYear(s)>1 && TAC_true(s,TAC_year)<0.0 && TAC_F_true(s,TAC_year)<0.0){      

       // start to find F for intermediate year
 
        if (test_output==53) {
           cout<<endl<<"Start intermediate year calc:";
           if (nsp>1) cout<<species_names[s]<<endl; else cout<<endl;
           //cout<<"TAC_F_obs:"<<TAC_F_obs<<endl;
           //cout<<"TAC_obs:"<<TAC_obs<<endl;
        }
        // project stock according to TAC or TAC_F "observed"
        if (inter_F_TAC(s)==0  && TAC_F_obs(s,y+1)>-1.0) { 
            pred_F_obs(s)=pred_F_sq(s)*TAC_F_obs(s,y+1)/meanFsq(s);
           
           if (test_output==53) {
              cout<<"F is estimated from TAC_F_obs("<<y+1<<")="<<setprecision(3)<<TAC_F_obs(s,y+1)<<endl;
           }
         //  if ((HCR(s)==44 || HCR(s)==45) && y==lyModel+1) {
         //      TAC_obs(s,y+1)=66000;
         //       //TAC_obs(s,y+1)=80500;
         //       // TAC_obs(s,y+1)=44300;
         //      if (test_output==53) cout<<"Special case HCR=44 or 45. TAC("<<y+1<<")"<<TAC_obs(s,y+1)<<endl;
         //   }
        }
        else if (inter_F_TAC(s)==1  && TAC_obs(s,y+1)>=0) { //calc F from TAC and "observed" N 
          Fscaling=find_Fscaling_from_target_yield(s,fq,0.0,max_F_scale, TAC_obs(s,y+1), pred_M(s), pred_M1(s), 
                 pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
          pred_F_obs(s)=pred_F_sq(s)*Fscaling;
                    
          if (test_output==53) cout<<"Intermidiate year F("<<y+1<<")="<<Fscaling*meanFsq(s)<<" is estimated from TAC_obs="<<setprecision(0)<<TAC_obs(s,y+1)<<endl;

          if (y-lyModel<=interYear(s)) {
            TAC_F_obs(s,y+1)=Fscaling*meanFsq(s);
            if (test_output==53) cout<<"Special inital condition: TAC_F_obs("<<y+1<<") set to "<< setprecision(3)<<TAC_F_obs(s,y+1)<<endl;
          }    
          //cout<<"pred_M2_true:"<<endl<<setprecision(3)<<pred_M2_true(s)<<endl;
        }
        else {
          cout<<"Error: no TAC or F for year "<<y+1<<" exist for species "<<s<<", "<<endl;
          cout<<" or option inter.F.TAC. is not in agreement with input TAC or F for the first years"<<endl;
          cout<<"Program stopped"<<endl; 
          exit(-9);
        }

        //if (test_output==53) cout<<"F("<<y+1<<"):"<<setprecision(3)<<endl<<pred_F_obs(s)<<endl; 
 
        // project one year ahead  
        do_M2=0;  // do not calc M2, used last year's value
        do_catch=0;  // do not recalc catch and yield 
        new_year=1;  // project stock N one year ahead giving N(y+2)
        predict_year(-1,s,s,do_M2,do_catch,new_year, pred_N_obs, pred_N_other, pred_F_obs, pred_C, pred_M, pred_M1, pred_M2_true, pred_Z_obs);

        // estimate recruit in year y+2                                               
        add_rec_noise=0;    // make a determenistic estimate of recruitment
        true_N=0;            // update obs SSB and estimate Rec from obs SSB.
        estimate_recruits(y+2, s, s, add_rec_noise, true_N, pred_N_obs, recruit, SSB_obs, pred_west, 
                            pred_propmat,rn, hist_rec_noise);

        if (test_output==53) {
           cout<<"observed N("<<y+2<<"):"<<setw(12) << setprecision(0) << setfixed()<<endl
                   <<pred_N_obs(s)<<endl<<"SSB_obs("<<y+2<<"): "<<SSB_obs(s,y+2)<<endl;; 
        }
        if (test_output==53) {
           cout<<"End intermediate year calc:";
           if (nsp>1) cout<<species_names[s]<<endl; else cout<<endl;
        }

        
     }  // end interYear     
        
             
      Fmin=0.0;     // minimum average F to obtain HCR result  
      
      if (test_output==53) cout<<"HCR option "<<species_names[s]<<":"<<HCR(s)<<endl;
      
      switch (HCR(s)) {
       case  1: // constant F, from Fconstant given as input 
                //     (Fconstant>0 abs value, Fconstant<0 F=Fconstant*Fsq
                if (constantF(s)>0) Fmax=constantF(s);
                else if (constantF(s)<0) Fmax=-constantF(s)*meanFsq(s);
                else Fmax=0;
                break;

        case 2: // Constant TAC
                tacMax=constantTAC(s); 
                //find F from input TAC and obs stock numbers 
                Fscaling=find_Fscaling_from_target_yield(s,fq,0.0,max_F_scale/meanFsq(s),tacMax,pred_M(s),pred_M1(s), 
                          pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                 break;
               
       case 10: // Estimate F from trigger biomass in the beginning of the TAC year
                Fmax=do_HCR_trigger(s,SSB_obs(s,TAC_year));
                break;

      case 11: // Estimate TAC from trigger biomass in the TAC year
                tacMax=do_HCR_trigger(s,SSB_obs(s,TAC_year));
                Fscaling=find_Fscaling_from_target_yield(s,fq,Fmin/meanFsq(s),max_F_scale/meanFsq(s),tacMax,
                          pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), 
                          pred_weca(s),pred_prop_landed(s));
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                break;

      case 15: // Estimate F from a target SSB in the TAC year+1
                Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                    pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                break;             
       
       case 20: //Estimate F from real-time estimate of stock number (N age 1 or more precisely age fa+1) 
                //     and trigger rule
                //estimate observed N1 from true N and real-time uncertanties
                pred_N_obs(s,fq,fa+1)=real_time_estimate(s, TAC_year, pred_N_true, pred_Z_true, randn(rng));

                Fmax=do_HCR_trigger(s,pred_N_obs(s,fq,fa+1));   // F according to HCR
                Fmin=real_time_F(s);                // F used to obtaine N1
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                break;
 
      case 21:  //Estimate TAC from real-time estimate of stock number (N age 1 or more precisely age fa+1) 
                //     and trigger rule
                //Estimate observed N1 from true N and real-time uncertanties
                pred_N_obs(s,fq,fa+1)=real_time_estimate(s, TAC_year, pred_N_true, pred_Z_true, randn(rng));

                Fmin=real_time_F(s);
                tacMax=do_HCR_trigger(s,pred_N_obs(s,fq,fa+1));   // TAC according to HCR
                Fscaling=find_Fscaling_from_target_yield(s,fq,Fmin/meanFsq(s),max_F_scale/meanFsq(s),tacMax,
                                pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s), 
                                pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling;
                else  Fmax=0.0; 
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                break;
       
       case 22: //Estimate TAC from real-time estimate of stock number (N age 1 or more precisely age fa+1) 
                //     and target SSB for SSB after the TAC year
       case 23: //Estimate F from real-time estimate of stock number (N age 1 or more precisely age fa+1) 
                //     and target SSB for SSB after the TAC year
                
                //Estimate observed N1 from true N and real-time uncertanties
                pred_N_obs(s,fq,fa+1)=real_time_estimate(s, TAC_year, pred_N_true, pred_Z_true, randn(rng));
                
                Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                           pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                Fmin=real_time_F(s);
                if (test_output==53) cout<<"Real time F from SSB(TAC year+1):"<<Fmax<<endl;
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                break;
       case 24: //Estimate F from real-time estimate of stock number (N age 1 or more precisely age fa+1) 
                //     and target SSB for SSB after the TAC year However F <= Fconstant
                
                //Estimate observed N1 from true N and real-time uncertanties
                pred_N_obs(s,fq,fa+1)=real_time_estimate(s, TAC_year, pred_N_true, pred_Z_true, randn(rng));
                
                Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                           pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                if (Fmax>constantF(s)) Fmax=constantF(s);
                Fmin=real_time_F(s);
                if (test_output==53) cout<<"Real time F from SSB(TAC year+1):"<<Fmax<<endl;
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                break;
                                
      case 30: //Estimate F from real-time estimate of TSB in the start of TAC year and trigger rule
                if (real_time_uncertanty(s,4)==1) 
                   pred_N_obs(s,fq)=pred_N_true(s,fq)*uncertanty(s,real_time_uncertanty(s),randn(rng));
                else for (a=fa;a<=la(s);a++)
                   pred_N_obs(s,fq,a)=pred_N_true(s,fq,a)*uncertanty(s,real_time_uncertanty(s),randn(rng));
                TSB0=0.0;
                for (a=fa;a<=la(s);a++)TSB0+=pred_N_obs(s,fq,a)*pred_west(s,fq,a);      
                Fmax=do_HCR_trigger(s,TSB0);
                Fmin=real_time_F(s);
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                break;

      case 31: //Estimate TAC from real-time estimate of TSB and trigger rule
                if (real_time_uncertanty(s,4)==1) 
                   pred_N_obs(s,fq)=pred_N_true(s,fq)*uncertanty(s,real_time_uncertanty(s),randn(rng));
                else for (a=fa;a<=la(s);a++)
                   pred_N_obs(s,fq,a)=pred_N_true(s,fq,a)*uncertanty(s,real_time_uncertanty(s),randn(rng));
                 TSB0=0.0;
                for (a=fa;a<=la(s);a++)TSB0+=pred_N_obs(s,fq,a)*pred_west(s,fq,a);
                tacMax=do_HCR_trigger(s,TSB0);
                Fmin=real_time_F(s);
                Fscaling=find_Fscaling_from_target_yield(s,fq,Fmin/meanFsq(s),max_F_scale/meanFsq(s),tacMax,
                               pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s), 
                               pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                break;
                
     case  42: // SPECIAL CASE, F is the highest value of FMSY (constant F input) and F last year
                 Fmax=constantF(s);
                 if (TAC_F_obs(s,y-1) >Fmax) Fmax=TAC_F_obs(s,y-1);
                 cout<<setfixed()<<setprecision(3)<<TAC_F_obs<<endl;
                break;
                
     case  43: // SPECIAL CASE, Commission "Stocks outside biological limits"

                 // Estimate F from a target SSB (Bpa) in the TAC year+1
                 // use variable target_SSB as Bpa
                Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                    pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);

                    
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;


                break;

     case  44: // SPECIAL CASE, Blatic Sea cod: reduce F by 10% per year compared to F estimated
               // for the proceeding year until F=0.3 is reached. After that apply a +-15% constraints on TAC, except for cases where F>0.6
               
                 if (TAC_F_obs(s,TAC_year-1)>constantF(s)) {
                   Fmax=TAC_F_obs(s,TAC_year-1)*(1.0-maxFT1(s,1)); // reduce F by x%
                   if (Fmax<=constantF(s)) {
                     Fmax=constantF(s);
                   }
                 } else Fmax=constantF(s);
 
               break;
  
    case  47: // SPECIAL CASE, North Sea,and Kattegat Cod
              // a) reduce F by 25% if SSB < Blim
              // b) reduce F by 15 if SSB>Blim and SSB <Bpa, min F at 0.4, apply 20 TAC constraint
              // c) reduce F by 10 if SSB >Bpa, min F at 0.4, apply 20 TAC constraint
              
                 if (SSB_obs(s,TAC_year) <=T1(s)) {Fmax=TAC_F_obs(s,TAC_year-1)*0.75;  } 
                 else if (SSB_obs(s,TAC_year) <=T2(s)) {Fmax=TAC_F_obs(s,TAC_year-1)*0.85; if (Fmax<=constantF(s)) Fmax=constantF(s); } 
                 else if (SSB_obs(s,TAC_year) >T2(s)) {Fmax=TAC_F_obs(s,TAC_year-1)*0.90;  if (Fmax<=constantF(s)) Fmax=constantF(s);}  
               break;


       case 98: //(only 98) Norway pout, Estimate TAC in quarter 1 from T1 and T2 and survey indices (0-group) in the previous year (using survey.dist error)
       case 99: // (both 98 and 99) & closure if the fishery in second quarter
                // & TAC for quarter 3 and 4 from first quarter 1-group index and assessment results (using real.T.dist error)
                // & and 25% "normal" recruitment with target at SSB at Bpa? the following year

                int season_last_year_survey;
                int  season_for_re_opening_fishery;

                season_last_year_survey=3;

                season_for_re_opening_fishery=3;
              
                has_changed_exploitation_pattern(s)=1;  // yes, we do change expl pattern

             if (HCR(s)==98) {
                // find absolute F for first quarter based on survey index
                tmp=survey_estimate_last_year(s, season_last_year_survey, previous_year_N_true, randn(rng));
                tacMax=do_HCR_trigger(s,tmp);

                meanFsq_tmp=0.0;
                for (q=fq;q<=lq;q++) {
                   if (q<=2) {
                     pred_F_sq_tmp(q)=pred_F_sq(s,q);
                     for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                   }
                   else pred_F_sq_tmp(q)=0;
                }
                meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
                if (test_output==53) {
                   cout<<"survey index:"<<tmp<<". Resulting TAC:"<<tacMax<<endl;
                   cout<<"Exploitation pattern for survey:"<<endl<<pred_F_sq_tmp<<endl;
                }

                Fscaling=find_Fscaling_from_target_yield(s,fq,Fmin/meanFsq_tmp,max_F_scale/meanFsq_tmp,tacMax,
                                pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                                pred_N_true(s), pred_weca(s),pred_prop_landed(s));
                                
                if (Fscaling>1E-4) {
                  pred_F_sq_tmp(1)=pred_F_sq_tmp(1)* Fscaling;
                  pred_F_sq_tmp(2)=pred_F_sq_tmp(2)* Fscaling;
                  if (test_output==53) cout<<"survey resulting pred_F_sq_tmp:"<<endl<<pred_F_sq_tmp<<endl;
                }
                else {
                    pred_F_sq_tmp(1)=0.0;
                    pred_F_sq_tmp(2)=0.0;
                    closure(s,y)=1;    // closure of the fishery
                }
              } // end HCR==98
              
                // HCR==99 starts here
                //Estimate F from real-time estimate of stock number (N age 1 or more precisely age fa+1)
                //     and target SSB for SSB after the TAC year

                // if no survey based TAC for first quarter close it;
                if (HCR(s)==99)   {
                  pred_F_sq_tmp(1)=0.0;
                  pred_F_sq_tmp(2)=0.0;
                }
                
                // Start to prepare F matrix which consists of real F for the first and the second quarter,
                //  and exploitation pattern for third and fourth quarters

                
                meanFsq_tmp=0.0; // used just for third and fourth quarters
                for (q=season_for_re_opening_fishery;q<=lq;q++) {
                     pred_F_sq_tmp(q)=pred_F_sq(s,q);
                     for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                }
                //meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
                if (test_output==53) {
                    cout<<"F pattern (quarter 1 &2) and exploitation pattern (quarter 3 &4) to calc TAC for third and fourth quarter:";
                    cout<<setfixed()<<setprecision(3)<<endl<<pred_F_sq_tmp<<endl;
                }

                //Estimate observed N1 from true N and real-time uncertanties
                pred_N_obs(s,fq,fa+1)=real_time_estimate(s, TAC_year, pred_N_true, pred_Z_true, randn(rng));

                // reduce expected recruitment by input factor
                pred_N_obs(s,recq,fa)=pred_N_obs(s,recq,fa)*recruit_adjust(s);
                
                Fscaling=find_Fscaling_from_target_SSB(s,season_for_re_opening_fishery,0.0,
                              3.0,target_SSB(s),pred_M(s),
                              pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,pred_N_obs(s),pred_west(s),pred_propmat(s),y);

                Fmin=real_time_F(s);

                for (q=season_for_re_opening_fishery;q<=lq;q++) {
                  if (Fscaling>0) pred_F_sq_tmp(q)=pred_F_sq_tmp(q)*Fscaling;
                  else pred_F_sq_tmp(q)=0.0;
                }

                if (test_output==53) cout<<"final absolute F from HCR="<<HCR(s)<<":"<<endl<<pred_F_sq_tmp<<endl;

                // calc new mean F based on new exploitation pattern
                meanFsq_tmp=0.0; // used just for third and fourth quarters
                for (q=fq;q<=lq;q++) for (a=fa;a<=la(s);a++) {
                  if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2))meanFsq_tmp+=pred_F_sq_tmp(q,a);
                }
                meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
                Fmax=meanFsq_tmp;
                //if (Fmax<1E-4) Fmax=0.0;
                if (Fmin>=Fmax) closure(s,y)=closure(s,y)+2;      // closure of the fishery

                if (test_output==53) cout<<"final Fmax from HCR=99:"<<Fmax<<endl;
                //overwrite with new exploitation pattern
                meanFsq(s)=meanFsq_tmp;
                pred_F_sq(s)=pred_F_sq_tmp;
                
                 if (test_output==53) if (Fmax>1.5) {
                   cout <<"HIGH F: "<<endl<<"N_true:"<<endl<<setprecision(0)<<pred_N_true(s,fq)<<endl;
                   cout <<"N_obs:"<<endl<<pred_N_obs(s,fq)<<endl;
                }
                break;
                
     case 101:  // 2012 Norway pout MSE  with two assessment per year
              // cout<<"TAC_true_half"<<endl<<TAC_true_half<<endl;
              has_changed_exploitation_pattern(s)=1;  // yes, we do change expl pattern
              
              if (growth_model(2,s)>0 || growth_model(3,s)>0) {                 // growth_model(2,s) and growth_model(3,s) is used for cap F by half-year
                   upper_F=growth_model(2,s)+growth_model(3,s);
                   if (test_output==53)  cout<<"Upper F (for the whole year):"<<setprecision(3)<<upper_F<<endl;
              } else upper_F=max_F_scale;
              
              if (test_output==53) cout<<"step 3: pred_N_true_tmp("<<y+1<<"):  "<<setprecision(0)<<pred_N_true_tmp(s,fq)<<endl;
              if (test_output==53) cout<<"Step 3: pred_N_obs("<<y+1<<"):  "<<setprecision(0)<<pred_N_obs(s,fq)<<endl;
              if (test_output==53) cout<<"upper_F:"<<setprecision(3)<<upper_F<<endl;
 
              // Adjust the initial TAC such that the real F does not exceed  input Cap-F.
              tacMax=TAC_true_half(s,y+1,1)+TAC_true_half(s,y+1,2);
              if (test_output==53) {
                cout<<"Step 4: TAC_obs("<<y+1<<") before adjustment:"<<setprecision(0)<< tacMax<<" from:"<<TAC_true_half(s,y+1,1)<<"  and second hy TAC:"<<TAC_true_half(s,y+1,2)<<endl;
                cout<<"pred_F_sq(s)"<<endl<<setprecision(3)<<pred_F_sq(s)<<endl<<"meanFsq(s): "<<meanFsq(s)<<endl;
              } 
              Fscaling=find_Fscaling_from_target_yield(s,fq,0,upper_F/meanFsq(s),tacMax,
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s),
                              pred_N_true_tmp(s), pred_weca(s),pred_prop_landed(s));

              if (test_output==53) cout<<" step 4: Mean True F: "<<setprecision(5)<<meanFsq*Fscaling<<
                                  "  Fscaling:"<<Fscaling<<"  Cap-F:"<<upper_F<<endl;
              
              // Calc adjusted (realised) first half-year TAC  
              // prepare explotion pattern for first half year only
              meanFsq_tmp=0.0;
              for (q=fq;q<=lq;q++) {
                 if (q<=2) {
                   pred_F_sq_tmp(q)=pred_F_sq(s,q);
                   for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                 }
                 else pred_F_sq_tmp(q)=0;
              }
              meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);

              if (Fscaling>0) { 
                  yield1=calc_Yield_from_Fscaling(s,fq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                         pred_N_true_tmp(s),pred_weca(s),pred_prop_landed(s));
                  tacMax=yield1;
              } 
              if (test_output==53) cout<<" step 4:True TAC(adjusted) first half-year:  "<<setprecision(0)<<tacMax<<endl;
              TAC_true_half(s,y+1,1)=tacMax;
               
              pred_F_sq(s,1)=pred_F_sq_tmp(1)* Fscaling;  // calc and save real F
              pred_F_sq(s,2)=pred_F_sq_tmp(2)* Fscaling;
              if (test_output==53) cout<<" step 3:True F first half-year:  "<<setprecision(3)<<endl<<pred_F_sq(s,1)<<endl<<pred_F_sq(s,2)<<endl;
              
              // udate true stock size to 1st July and calculate TAC for second half-year
              tmp=0;
              for (q=fq;q<recq;q++){
                 for (a=faq(q);a<=la(s);a++){
                 if (q==fq) tmp+=pred_N_true_tmp(s,q,a)*pred_west(s,q,a)*pred_propmat(s,q,a);  // SSB
                  // Total mortality
                  if (multi==2) pred_Z_true(s,q,a)=pred_F_sq(s,q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_true(s,q,a)=pred_F_sq(s,q,a)+pred_M(s,q,a);  
                  pred_N_true_tmp(s,q+1,a)= pred_N_true_tmp(s,q,a)*exp(-pred_Z_true(s,q,a));
                } //age loop
               }  // q loop
 
               // estimation of recruits
               pred_N_true_tmp(s,recq,fa)=SSB_recruit(s,tmp,rn(s,y+1),hist_rec_noise,0);
               if (test_output==53) cout<<"step 4: Q3: pred_N_true_tmp: "<<setprecision(0)<<pred_N_true_tmp(s,recq)<<endl;                
              
              // Step 5 calculate observed F for first half-year
              if (test_output==53) {
                 cout<<"step 5: TAC (realised) first half-year:"<<tacMax<<endl;
                 cout<<"step 5: (obs) Exploitation pattern for first half-year:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
              }

              Fscaling=find_Fscaling_from_target_yield(s,fq,0,2*upper_F/meanFsq_tmp,tacMax,          // the upper F limit is twice any realistic value
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                              
              pred_F_sq_tmp(1)=pred_F_sq_tmp(1)* Fscaling;
              pred_F_sq_tmp(2)=pred_F_sq_tmp(2)* Fscaling;

              if (test_output==53) cout<<" step 5: Mean obs F,first half year: "<<meanFsq_tmp*Fscaling<<endl;

              // udate observed stock size to 1st July 
              for (q=fq;q<recq;q++){
                 for (a=faq(q);a<=la(s);a++){
                  // Total mortality
                  if (multi==2) pred_Z_obs(s,q,a)=pred_F_sq_tmp(q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_obs(s,q,a)=pred_F_sq_tmp(q,a)+pred_M(s,q,a);  
                  // observed stock numbers
                   pred_N_obs(s,q+1,a)= pred_N_obs(s,q,a)*exp(-pred_Z_obs(s,q,a));
                } //age loop
                if (test_output==53) cout<<"step 5: Obs F quarter "<<setprecision(0)<<q<<endl<<setprecision(3)<<pred_F_sq_tmp(q)<<endl; 
               }  // q loop
               if (test_output==53) cout<<"step 5: Q3: pred_N_obs: "<<setprecision(0)<<pred_N_obs(s,recq)<<endl;                
               
              
              // step 6, calculate (observed) TAC for second half-year
              if (growth_model(3,s)>0) {                 // growth_model(3,s) is used for cap F for second half-year TACs
                   upper_F=growth_model(3,s);
                   if (test_output==53)  cout<<"Upper F second half year:"<<setprecision(3)<<upper_F<<endl;
              } else upper_F=max_F_scale;

              pred_F_sq_tmp=pred_F_sq_copy(s);   // fresh copy of exploitation pattern
              meanFsq_tmp=meanFsq_copy(s);

              Fscaling=find_Fscaling_from_target_SSB(s,recq,0.0,5*meanFsq_tmp,          // starts in quarter 3 (recq)
                            target_SSB(s),pred_M(s),
                            pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,pred_N_obs(s),pred_west(s),pred_propmat(s),y);
  
              if (Fscaling>0) yield1=calc_Yield_from_Fscaling(s,recq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                        pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
              else {yield1=0.0; Fscaling=0; }
              TAC_true_half(s,y+1,2)=yield1;
              if (test_output==53)  cout<<setprecision(0)<<"step 6: TAC("<<TAC_year<<")second half-year before TAC constraints:"<<yield1<<endl;

              if ((sum(TAC_true_half(s,y+1)) < growth_model(4,s)) || (sum(TAC_true_half(s,y+1)) > growth_model(5,s))) {   // annual TAC outside TAC constarints 
                 if (sum(TAC_true_half(s,y+1)) < growth_model(4,s)) tmp=growth_model(4,s)-TAC_true_half(s,y+1,1);
                 else if (sum(TAC_true_half(s,y+1)) > growth_model(5,s)) tmp=growth_model(5,s)-TAC_true_half(s,y+1,1);
                 if (test_output==53) cout<<"step 6: target TAC second half-year due to constatints: "<<tmp<<endl; 
                 Fscaling=find_Fscaling_from_target_yield(s,recq,0,2,tmp,pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                                                          pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                if (Fscaling>1E-4) {
                  pred_F_sq_tmp(recq)=pred_F_sq_tmp(recq)* Fscaling;
                  pred_F_sq_tmp(recq+1)=pred_F_sq_tmp(recq+1)* Fscaling;
                }
                if (test_output==53) cout<<"Step 6: pred_F_sq_tmp after TAC constraints:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl<<
                        "Fscaling: "<<Fscaling<<"  mean F: "<<meanFsq_tmp*Fscaling<<endl;;
                if (Fscaling>0) yield1=calc_Yield_from_Fscaling(s,recq,1,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                      pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                 else {yield1=0.0; Fscaling=0; } 
                 if (test_output==53) cout<<"step 6: TAC second half year after TAC constraints:"<<yield1<<endl;
                 TAC_true_half(s,y+1,2)=yield1;
              }  
                    
            // Step 7: Calculate true F (and true TAC) for second half-year from observed TAC and true N
              meanFsq_tmp=0.0;
              for (q=fq;q<=lq;q++) {
                 if (q>2) {
                   pred_F_sq_tmp(q)=pred_F_sq_copy(s,q);
                   for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                 }
                 else pred_F_sq_tmp(q)=0;
              }
              meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
             
             if (test_output==53) {
               cout<<"Step 7: pred_F_true before scaling:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
               cout<<"Step 7: target yield:"<<setprecision(0)<<TAC_true_half(s,y+1,2)<<endl;
             }
             Fscaling=find_Fscaling_from_target_yield(s,recq,0,upper_F/meanFsq_tmp,TAC_true_half(s,y+1,2),
                                pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                                pred_N_true_tmp(s), pred_weca(s),pred_prop_landed(s));
             
             if (test_output==53) cout<<" step 7: Mean True F, second half year: "<<setprecision(3)<<meanFsq_tmp*Fscaling<<
                                  "  Fscaling:"<<Fscaling<<"  Cap-F second half-year:"<<upper_F<<endl;
                                 
             if (Fscaling>1E-4) {
                pred_F_sq_tmp(recq)=pred_F_sq_tmp(recq)* Fscaling;
                pred_F_sq_tmp(recq+1)=pred_F_sq_tmp(recq+1)* Fscaling;
              }
              else {
                pred_F_sq_tmp(recq)=0.0;
                pred_F_sq_tmp(recq+1)=0.0;
              }
              if (test_output==53) cout<<"Step 7: pred_F_sq_tmp after scaling:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
              
              if (Fscaling>0) yield1=calc_Yield_from_Fscaling(s,recq,1,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                      pred_N_true_tmp(s),pred_weca(s),pred_prop_landed(s));
               else {yield1=0.0; Fscaling=0; } 
               if (test_output==53) cout<<"step 7: realised TAC second half year:"<<yield1<<endl;
               TAC_true_half(s,y+1,2)=yield1;
               
              // update pred_F_sq to include the absolute F for the year
              pred_F_sq(s,recq)=pred_F_sq_tmp(recq);
              pred_F_sq(s,recq+1)=pred_F_sq_tmp(recq+1);
              
              // calc new mean F 
              meanFsq_tmp=0.0; 
              for (q=fq;q<=lq;q++) for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) meanFsq_tmp+=pred_F_sq(s,q,a);
              meanFsq(s)=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);

              Fmax=meanFsq(s);
              if (test_output==53) cout<<"step 7: true F for year "<<y+1<<endl<<setprecision(3)<< pred_F_sq<<endl;
              if (test_output==53) cout<<"step 7: realised TAC for year "<<y+1<<":  "<<setprecision(0)<< 
               " first half:"<<TAC_true_half(s,y+1,1)<<" second half:"<<TAC_true_half(s,y+1,2)<<"  sum:"<<sum(TAC_true_half(s,y+1))<<endl;
          
             // Step 8, simulate the September assessment
              // assessment noise and bias on stock numbers, first season (this simulates an assessment)
             if (real_time_uncertanty(s,1)==2 || real_time_uncertanty(s,1)==3) {
                ; 
                //cov_uncertanty(s, assess_uncertanty(s),pred_N_obs(s,fq).sub(fa+1,la(s)),coVariance(s).sub(fa+1,la(s)));
              }
            else if (real_time_uncertanty(s,4)==1) pred_N_obs(s,recq)=
                             pred_N_true_tmp(s,recq)*uncertanty(s,real_time_uncertanty(s),randn(rng));
            else for (a=fa;a<=la(s);a++) pred_N_obs(s,recq,a)=
                       pred_N_true_tmp(s,recq,a)*uncertanty(s,real_time_uncertanty(s),randn(rng));
          
            if (test_output==53) cout<<"Step 8: pred_N_obs:  "<<setprecision(0)<<endl<<pred_N_obs(s)<<endl;
      
            // step  9; Update observed stock to 1 Jan next year 
            // estimate observed F second half year
            pred_F_sq_tmp=pred_F_sq_copy(s); meanFsq_tmp=meanFsq_copy(s); // fresh copy
            Fscaling=find_Fscaling_from_target_yield(s,recq,0,upper_F/meanFsq_tmp*2,TAC_true_half(s,y+1,2),
                                pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                                pred_N_obs(s), pred_weca(s),pred_prop_landed(s));

             if (test_output==53) cout<<" step 9: Target TAC second half:"<<setprecision(0)<<TAC_true_half(s,y+1,2)<<"  Mean observed F, second half year: "<<setprecision(3)<<meanFsq_tmp*Fscaling<<
                                  "  Fscaling:"<<Fscaling<<endl;
                                 
             if (Fscaling>1E-4) {
                pred_F_obs(s,recq)=pred_F_sq_tmp(recq)* Fscaling;
                pred_F_obs(s,recq+1)=pred_F_sq_tmp(recq+1)* Fscaling;
              }
              else {
                pred_F_obs(s,recq)=0.0;
                pred_F_obs(s,recq+1)=0.0;
              }
              if (test_output==53) cout<<"Step 9: pred_F_obs after scaling:"<<endl<<setprecision(3)<<pred_F_obs<<endl;
              // update obs_N to 1 st Jan
              for (q=recq;q<=lq;q++){
                 for (a=faq(q);a<=la(s);a++){
                  // Total mortality
                  if (multi==2) pred_Z_obs(s,q,a)=pred_F_obs(s,q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_obs(s,q,a)=pred_F_obs(s,q,a)+pred_M(s,q,a);  
                  if  (q<lq) pred_N_obs(s,q+1,a)= pred_N_obs(s,q,a)*exp(-pred_Z_true(s,q,a));
                  else {
                    if (a<la(s)) pred_N_obs(s,fq,a+1)=pred_N_obs(s,q,a)*exp(-pred_Z_true(s,q,a));
                    if (a==la(s) && nplus(s)==1)  pred_N_obs(s,fq,a)=pred_N_obs(s,q,a)*exp(-pred_Z_true(s,q,a))+pred_N_obs(s,fq,a);
                  }
                } //age loop
               }  // q loop
               tmp=0;
               for (a=faq(fq);a<=la(s);a++) tmp+=pred_N_obs(s,fq,a)*pred_west(s,fq,a)*pred_propmat(s,fq,a);
               pred_N_obs(s,recq,fa)=SSB_recruit(s,tmp,0,hist_rec_noise,0);
               if (test_output==53)  cout<<setprecision(0)<<"step 9: Observed stock N:"<<endl<< pred_N_obs(s,fq)<<"  Recruitment:"<< pred_N_obs(s,recq,fa)<<endl;
              
               // calc TAC for next year
              // Start to prepare F matrix which consists of exploitation pattern for the full year
              meanFsq_tmp=meanFsq_copy(s);  
              pred_F_sq_tmp=pred_F_sq_copy(s);
              Fscaling=find_Fscaling_from_target_SSB(s,recq,0.0,5*meanFsq_tmp,
                                target_SSB(s),pred_M(s),
                                pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,pred_N_obs(s),pred_west(s),pred_propmat(s),y);
  
               if (Fscaling>1E-4) yield1=calc_Yield_from_Fscaling(s,recq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                      pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
               else {yield1=0.0; Fscaling=0; }
               if (test_output==53)  cout<<setprecision(0)<<"step 9: observed TAC (full year) before TAC constraints :"<<yield1<<endl;
             
               if (yield1 < growth_model(4,s) || yield1> growth_model(5,s)) {   // annual TAC outside TAC constarints 
                 if (yield1 < growth_model(4,s)) tmp=growth_model(4,s);
                 else if (yield1 > growth_model(5,s)) tmp=growth_model(5,s);
                  
                 Fscaling=find_Fscaling_from_target_yield(s,recq,0,upper_F/meanFsq_tmp*5,tmp,
                    pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                    pred_N_obs(s), pred_weca(s),pred_prop_landed(s));

                 if (Fscaling>1E-4) {
                  pred_F_sq_tmp(recq)=pred_F_sq_tmp(recq)* Fscaling;
                  pred_F_sq_tmp(recq+1)=pred_F_sq_tmp(recq+1)* Fscaling;
                 }
                 if (test_output==53) cout<<"Step 9: pred_F_sq_tmp after TAC constraints:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
                 if (Fscaling>0) yield1=calc_Yield_from_Fscaling(s,recq,1,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                      pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                 else {yield1=0.0; Fscaling=0; } 
                 if (test_output==53) cout<<"step 9: TAC("<<y+2<<") after TAC constraints:"<<yield1<<endl;
               } 
               if (test_output==53) cout<<"step 9: final TAC("<<y+2<<"):"<<yield1<<endl; 
               TAC_true_half(s,y+2,1)=yield1;

       break;  // end Norway pout MSE 101

      case 102: //simulate an September assessment (where recruitment index is availeble) and esimate TAC for the next calender year  from escapement strategy
                // adjust observed recruitment index from real recruiment and noise 

                if (test_output==53) cout<<"HCR 102: pred_N_obs:  "<<setprecision(0)<<endl<<pred_N_obs(s)<<endl;
                
                Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                    pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                if (test_output==53) cout<<"Fscaling after esacpement strategy:"<<setprecision(3)<<Fscaling<<"  Fmax:"<<Fmax<<endl;
                yield1=calc_Yield_from_Fscaling(s,fq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq(s),
                         pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                if (test_output==53) cout<<"Escapement strategy TAC before TAC adjustment:"<<setprecision(0)<<yield1<<endl;
                         
                if (yield1< growth_model(2,s))  {      //HCR@growth.model[2,1] is used as option for minimum observed TAC 
                  if (test_output==53) cout<<"Escapement strategy gave TAC:"<<setprecision(0)<<yield1<<" which is below min TAC:"<<growth_model(2,s)<<endl;
                     Fscaling=find_Fscaling_from_target_yield(s,fq,0,max_F_scale/meanFsq(s),growth_model(2,s),
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s),
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s)); 
                  if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                  if (test_output==53) cout<<"updated Fscaling after min TAC:"<<setprecision(3)<<Fscaling<<"  Fmax:"<<Fmax<<endl;   
                } 
                else if (yield1> growth_model(3,s))  {      //HCR@growth.model[3,1] is used as option for maximum observed TAC 
                  if (test_output==53) cout<<"Escapement strategy gave TAC:"<<setprecision(0)<<yield1<<" which is above max TAC:"<<growth_model(3,s)<<endl;
                     Fscaling=find_Fscaling_from_target_yield(s,fq,0,max_F_scale/meanFsq(s),growth_model(3,s),
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s),
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s)); 
                  if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                  if (test_output==53) cout<<"updated Fscaling after max TAC:"<<setprecision(3)<<Fscaling<<"  Fmax:"<<Fmax<<endl;   
                }                                                                 
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                
                
       break;
 

      case 103: //simulatet a MAY assessment (where recruitment index is not availeble) and esimate TAC for the next calender year  from escapement strategy
                // adjust observed recruitment index from real recruiment and noise 

                if (test_output==53) cout<<"HCR 103: pred_N_obs:  "<<setprecision(0)<<endl<<pred_N_obs(s)<<endl;
                
                Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                    pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                if (test_output==53) cout<<"Fscaling after esacpement strategy:"<<setprecision(3)<<Fscaling<<"  Fmax:"<<Fmax<<endl;
                yield1=calc_Yield_from_Fscaling(s,fq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq(s),
                         pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                if (test_output==53) cout<<"Escapement strategy TAC before TAC adjustment:"<<setprecision(0)<<yield1<<endl;
                         
                if (yield1< growth_model(2,s))  {      //HCR@growth.model[2,1] is used as option for minimum observed TAC 
                  if (test_output==53) cout<<"Escapement strategy gave TAC:"<<setprecision(0)<<yield1<<" which is below min TAC:"<<growth_model(2,s)<<endl;
                     Fscaling=find_Fscaling_from_target_yield(s,fq,0,max_F_scale/meanFsq(s),growth_model(2,s),
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s),
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s)); 
                  if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                  if (test_output==53) cout<<"updated Fscaling after min TAC:"<<setprecision(3)<<Fscaling<<"  Fmax:"<<Fmax<<endl;   
                } 
                else if (yield1> growth_model(3,s))  {      //HCR@growth.model[3,1] is used as option for maximum observed TAC 
                  if (test_output==53) cout<<"Escapement strategy gave TAC:"<<setprecision(0)<<yield1<<" which is above max TAC:"<<growth_model(3,s)<<endl;
                     Fscaling=find_Fscaling_from_target_yield(s,fq,0,max_F_scale/meanFsq(s),growth_model(3,s),
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s),
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s)); 
                  if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                  if (test_output==53) cout<<"updated Fscaling after max TAC:"<<setprecision(3)<<Fscaling<<"  Fmax:"<<Fmax<<endl;   
                }                                                                 
                if (Fmin>Fmax) closure(s,y)=1;      // closure of the fishery
                
                
       break;
 
       case 110: // TAC for first half year is fixed (input option constant.TAC.) . The TAC is assummed taken however limited by Cap F 
                 // TAC for the full year is based on the May assessment (which provides Stock numbers by 1. January) and an anual target F of 0.35

       case 111: // TAC for first half year is fixed (input option constant.TAC.) . The TAC is assummed taken however limited by Cap F 
                 // TAC for the full year is based on the May assessment (which provides Stock numbers by 1. January) and the Escapement strategy
                 
 
              
              has_changed_exploitation_pattern(s)=1;  // yes, we do change expl pattern
              
              if (growth_model(2,s)>0) {                 // growth_model(2,s) is used for cap F for first half-year TACs
                   upper_F=growth_model(2,s);
                   if (test_output==53)  cout<<"Upper F:"<<setprecision(3)<<upper_F<<endl;
              } else upper_F=max_F_scale;
              
              // input TAC for first half-year
              tacMax=constantTAC(s);
              
              // Adjust the initial TAC such that the real F does not exceed the input Cap-F.
              // change exploitation pattern to include only the first half year 
              
              //if (test_output==53) cout<<"step 3: initial exploitation pattern:"<<setprecision(3)<<endl<<pred_F_sq(s)<<endl;
               if (test_output==53) cout<<"pred_N_true_tmp("<<y+1<<"):  "<<setprecision(0)<<pred_N_true_tmp(s,fq)<<endl;

              meanFsq_tmp=0.0;
              for (q=fq;q<=lq;q++) {
                 if (q<=2) {
                   pred_F_sq_tmp(q)=pred_F_sq(s,q);
                   for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                 }
                 else pred_F_sq_tmp(q)=0;
              }
              meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
              if (test_output==53) {
                 cout<<"step 3: Initial Fixed TAC:"<<setprecision(0)<<tacMax<<endl;
                 cout<<"step 3: Exploitation pattern for first half-year:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl<<"  meanFsq_tmp:"<<meanFsq_tmp<<endl;
              }
             //cout<<"check:"<<upper_F*maxFT1(s,2)/meanFsq_tmp<<endl;
             Fscaling=find_Fscaling_from_target_yield(s,fq,0,upper_F/meanFsq_tmp,tacMax,
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                              pred_N_true_tmp(s), pred_weca(s),pred_prop_landed(s));
                              

              if (test_output==53) cout<<" step 3: Mean True F,first half year: "<<setprecision(3)<<meanFsq_tmp*Fscaling<<
                                  "  Fscaling:"<<Fscaling<<"  Cap-F first half-year:"<<upper_F<<endl;
              
              // Calc adjusted (realised) TAC for first half-year 
              if (Fscaling>0) { 
                  yield1=calc_Yield_from_Fscaling(s,fq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                         pred_N_true_tmp(s),pred_weca(s),pred_prop_landed(s));
                  tacMax=yield1;
              } 
              if (test_output==53) cout<<" step 3:True fixed TAC (adjusted):  "<<setprecision(0)<<tacMax<<endl;
              TAC_true_half(s,y,1)=tacMax;
              pred_F_sq(s,1)=pred_F_sq_tmp(1)* Fscaling;  // calc and save real F
              pred_F_sq(s,2)=pred_F_sq_tmp(2)* Fscaling;
              if (test_output==53) cout<<" step 3:True fixed F first half-year:  "<<setprecision(3)<<endl<<pred_F_sq(s,1)<<endl<<pred_F_sq(s,2)<<endl;
              
              // udate true stock size to 1st July and calculate TAC for second half-year
              tmp=0;
              for (q=fq;q<recq;q++){
                 for (a=faq(q);a<=la(s);a++){
                 if (q==fq) tmp+=pred_N_true_tmp(s,q,a)*pred_west(s,q,a)*pred_propmat(s,q,a);  // SSB
                  // Total mortality
                  if (multi==2) pred_Z_true(s,q,a)=pred_F_sq(s,q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_true(s,q,a)=pred_F_sq(s,q,a)+pred_M(s,q,a);  
                  pred_N_true_tmp(s,q+1,a)= pred_N_true_tmp(s,q,a)*exp(-pred_Z_true(s,q,a));
                } //age loop
               }  // q loop
 
               // estimation of recruits
               pred_N_true_tmp(s,recq,fa)=SSB_recruit(s,tmp,rn(s,y+1),hist_rec_noise,0);
               if (test_output==53) cout<<"step 3: Q3: pred_N_true_tmp: "<<setprecision(0)<<pred_N_true_tmp(s,recq)<<endl;                
               
              // step 4, simulate the May assessment 
              
              // Step 5 calculate observed F for first half-year
               if (test_output==53) {
                 cout<<"step 5: Fixed TAC (realised):"<<tacMax<<endl;
                 cout<<"step 5: (obs) Exploitation pattern for first half-year:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
              }

              Fscaling=find_Fscaling_from_target_yield(s,fq,0,2*upper_F/meanFsq_tmp,tacMax,          // the upper F limit is twice any realistic value
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                              
              pred_F_sq_tmp(1)=pred_F_sq_tmp(1)* Fscaling;
              pred_F_sq_tmp(2)=pred_F_sq_tmp(2)* Fscaling;

              if (test_output==53) cout<<" step 5: Mean obs F,first half year: "<<meanFsq_tmp*Fscaling<<endl;

              // udate observed stock size to 1st July 
              for (q=fq;q<recq;q++){
                 for (a=faq(q);a<=la(s);a++){
                  // Total mortality
                  if (multi==2) pred_Z_obs(s,q,a)=pred_F_sq_tmp(q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_obs(s,q,a)=pred_F_sq_tmp(q,a)+pred_M(s,q,a);  
                  // observed stock numbers
                   pred_N_obs(s,q+1,a)= pred_N_obs(s,q,a)*exp(-pred_Z_obs(s,q,a));
                } //age loop
                if (test_output==53) cout<<"step 5: Obs F quarter "<<setprecision(0)<<q<<endl<<setprecision(3)<<pred_F_sq_tmp(q)<<endl; 
               }  // q loop
               if (test_output==53) cout<<"step 5: Q3: pred_N_obs: "<<setprecision(0)<<pred_N_obs(s,recq)<<endl;                
               
              
              // step 6, calculate (observed) TAC for second half-year
               if (growth_model(3,s)>0) {                 // growth_model(3,s) is used for cap F for second half-year TACs
                   upper_F=growth_model(3,s);
                   if (test_output==53)  cout<<"Upper F:"<<setprecision(3)<<upper_F<<endl;
              } else upper_F=max_F_scale;

              if (HCR(s)==110) {   // TAC from target F for the full year
                 // Start to calculate F left over for second half year
                  meanFsq_tmp=meanFsq_tmp*Fscaling;  // mean F  (observed) first haf year
                  tmp=growth_model(4,s)-meanFsq_tmp; // growth_model(4,s) includes target F. meanFsq_tmp include mean F for first half year
                  if (tmp<0) tmp=0.0;
                  if (test_output==53) cout<<setprecision(3)<<"step 6: Target F for the full year: "<<growth_model(4,s)<<" mean F for first half year:"<< meanFsq_tmp<<
                                            " mean F for second half year:"<<tmp<<endl;
                  
                  if  (tmp>0) {
                    double mult;
                    mult=0;
                    meanFsq_tmp=0;
                    while (meanFsq_tmp<tmp) {
                      mult+=0.0025;
                      //cout<<"mult: "<<setprecision(3)<<mult<<endl;
                      meanFsq_tmp=0;
                      for (q=recq;q<=lq;q++) {
                        pred_F_sq_tmp(q)=mult*pred_F_sq_copy(s,q);
                        for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                      }
                      meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
                    }
                    if (test_output==53) cout<<"step 6: mean F:"<<setprecision(3)<<meanFsq_tmp<<endl<<"obs F (pred_F_sq_tmp): "<<endl<<pred_F_sq_tmp<<endl;                
                    // calc TAC for second half-year
                    if (meanFsq_tmp>0) yield1=calc_Yield_from_Fscaling(s,recq,1,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                          pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                    else yield1=0.0; 
                    if (test_output==53)  cout<<setprecision(0)<<"step 6: TAC("<<TAC_year<<")second half-year before TAC adjustment :"<<yield1<<endl;
                  } 
                  else {
                    for (q=recq;q<=lq;q++) pred_F_sq_tmp(q)=0.0;
                    yield1=0;
                  }
                  
                  yield1=yield1*growth_model(5,s);   // growth_model(4,s) is used for proportion of the halft year TAC that is used
                  TAC_true_half(s,y,2)=yield1;
                 
                 if (test_output==53) cout<<setprecision(3)<<"step 7: TAC adjustment factor:"<<growth_model(5,s)<<" Final (observed) TAC second half year:"<<setprecision(0)<<yield1<<endl;
              }
               else if (HCR(s)==111) {   // Escapement strategy
                 // Start to prepare F matrix which consists of exploitation pattern for third and fourth quarters
                  meanFsq_tmp=0.0; // used just for third and fourth quarters
                  pred_F_sq_tmp=pred_F_sq_copy(s);
                  for (q=fq;q<recq;q++) pred_F_sq_tmp(q)=0;
                  for (q=recq;q<=lq;q++) {
                       for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                  }
                  if (test_output==53) cout<<"step 6: Q3: pred_F_sq_tmp: "<<setprecision(3)<<endl<<pred_F_sq_tmp<<endl;                
                
                  meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
                  Fscaling=find_Fscaling_from_target_SSB(s,recq,0.0,5*meanFsq_tmp,
                                target_SSB(s),pred_M(s),
                                pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,pred_N_obs(s),pred_west(s),pred_propmat(s),y);
  
                 if (Fscaling>0) yield1=calc_Yield_from_Fscaling(s,recq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                        pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                 else {yield1=0.0; Fscaling=0; }
                 if (test_output==53)  cout<<setprecision(0)<<"step 6: TAC("<<TAC_year<<")second half-year before TAC adjustment :"<<yield1<<endl;
          
                 yield1=yield1*growth_model(5,s);   // growth_model(4,s) is used for proportion of the halft year TAC that is used
                 TAC_true_half(s,y,2)=yield1;
                 
                 if (test_output==53) cout<<setprecision(3)<<"step 7: TAC adjustment factor:"<<growth_model(5,s)<<" Final (observed) TAC second half year:"<<setprecision(0)<<yield1<<endl;
             }
            
            // Step 7: Calculate true F (and true TAC) for second half-year from observed TAC and true N
             if (test_output==53) {
               cout<<"Step 7: pred_F_true before scaling:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
               cout<<"Step 7: target yield:"<<setprecision(0)<<TAC_true_half(s,y,2)<<endl;
             }
             Fscaling=find_Fscaling_from_target_yield(s,recq,0,upper_F/meanFsq_tmp,TAC_true_half(s,y,2),
                                pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                                pred_N_true_tmp(s), pred_weca(s),pred_prop_landed(s));

             if (test_output==53) cout<<" step 8: Mean True F, second half year: "<<setprecision(3)<<meanFsq_tmp*Fscaling<<
                                  "  Fscaling:"<<Fscaling<<"  Cap-F second half-year:"<<upper_F<<endl;
                                 
             if (Fscaling>1E-4) {
                pred_F_sq_tmp(recq)=pred_F_sq_tmp(recq)* Fscaling;
                pred_F_sq_tmp(recq+1)=pred_F_sq_tmp(recq+1)* Fscaling;
              }
              else {
                pred_F_sq_tmp(recq)=0.0;
                pred_F_sq_tmp(recq+1)=0.0;
              }
              if (test_output==53) cout<<"Step 8: pred_F_sq_tmp after scaling:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl;
              
              if (Fscaling>0) yield1=calc_Yield_from_Fscaling(s,recq,1,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                      pred_N_true_tmp(s),pred_weca(s),pred_prop_landed(s));
               else {yield1=0.0; Fscaling=0; } 
               if (test_output==53) cout<<"realised TAC second half year:"<<yield1<<endl;
               TAC_true_half(s,y,2)=yield1;
               
              // update pred_F_sq to include the absolute F for the year
              pred_F_sq(s,recq)=pred_F_sq_tmp(recq);
              pred_F_sq(s,recq+1)=pred_F_sq_tmp(recq+1);
              
              // calc new mean F 
              meanFsq_tmp=0.0; 
              for (q=fq;q<=lq;q++) for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) meanFsq_tmp+=pred_F_sq(s,q,a);
              meanFsq(s)=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);

              Fmax=meanFsq(s);
              if (test_output==53) cout<<"step 7: true F for year "<<y<<endl<<setprecision(3)<< pred_F_sq<<endl;
            
              if (test_output==53) cout<<"TAC for year "<<y+1<<":  "<<setprecision(0)<< TAC_true_half(s,y,1)+TAC_true_half(s,y,2)<<endl<<endl;
              break;
               
     
      // end Norway pout MSE 111
      
       case 112: // TAC for 1 November-31 October (implemented as Q4 & Q1-Q3) from an assessment in September including the first half-year
                 //   and Q3 survey of the last assessment year. 
                 // TAC for Q4 & Q1-Q3 is set by an adapted Escapement strategy, which target SSB>Bpa the 1. January in the year following the Q4 & Q1-Q3 TAC. 
                 //  In the calculation of TAC it is assumed that Q4 Catches after the Q4 & Q1-Q3 TAC is set to zero
                 
              if (test_output==53) cout<<endl<<"Start HCR 112, y="<<y<<endl;
              
              has_changed_exploitation_pattern(s)=1;  // yes, we do change expl pattern
 
              if (growth_model(2,s)>0 || growth_model(3,s)>0) { // growth_model(2,s) and growth_model(3,s) is used for cap F by Q4 and Q1-Q3
                   upper_F=growth_model(2,s)+growth_model(3,s);
                   //if (test_output==53)  cout<<"Upper F (for the whole year):"<<setprecision(3)<<upper_F<<endl;
              } else upper_F=max_F_scale;
             
              // calc recruits
              tmp=0;
              q=1;
              for (a=faq(q);a<=la(s);a++) tmp+=pred_N_true_tmp(s,q,a)*pred_west(s,q,a)*pred_propmat(s,q,a);  // SSB
              pred_N_true_tmp(s,recq,fa)=SSB_recruit(s,tmp,rn(s,y+1),hist_rec_noise,0);    // recruits
   
                       
              if (test_output==53) {
                cout<<endl<<"step 3: pred_N_true_tmp("<<y+1<<") fq:  "<<setprecision(0)<<pred_N_true_tmp(s,fq)<<endl;
                cout<<"   and recruitment:"<<pred_N_true_tmp(s,recq,fa)<<endl;
              }
              
              if (y== lyModel+1) TAC_true_half(s,y+1,1)=TAC_second(s);  
    
             
             // TAC for the Q1-Q3.
             //  Q1Q3 TAC=TAC_true_half(s,y,1);  Q4 TAC=TAC_true_half(s,y,2);
              
             if (test_output==53) cout<<"Initial Q1-Q3 TAC for year "<<y+1<<":  "<<setprecision(0)<< TAC_true_half(s,y+1,1)<<endl;
             tacMax=TAC_true_half(s,y+1,1);
              
              // Adjust the initial TAC Q1-Q3 such that the real F does not exceed the input Cap-F.
              // change exploitation pattern to include only the first quarters
              
              //if (test_output==53) cout<<"step 3: initial exploitation pattern:"<<setprecision(3)<<endl<<pred_F_sq(s)<<endl;
               if (test_output==53) cout<<"pred_N_true_tmp("<<y+1<<"):  "<<setprecision(0)<<pred_N_true_tmp(s,fq)<<endl;
              upper_F=growth_model(3,s); // Cap F Q1-Q3
              
              meanFsq_tmp=0.0;
              for (q=fq;q<=lq;q++) {
                 if (q<=3) {
                   pred_F_sq_tmp(q)=pred_F_sq(s,q);
                   for (a=fa;a<=la(s);a++) if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2)) meanFsq_tmp+=pred_F_sq_tmp(q,a);
                 }
                 else pred_F_sq_tmp(q)=0;
              }
              meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
              if (test_output==53) {
                 cout<<"step 3: Initial Fixed TAC:"<<setprecision(0)<<tacMax<<endl;
                 cout<<"step 3: Exploitation pattern for Q1-Q3:"<<endl<<setprecision(3)<<pred_F_sq_tmp<<endl
                     <<"  meanFsq_tmp:"<<meanFsq_tmp<<endl;
              }
             //cout<<"check:"<<upper_F*maxFT1(s,2)/meanFsq_tmp<<endl;
             Fscaling=find_Fscaling_from_target_yield(s,fq,0,upper_F/meanFsq_tmp,tacMax,
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                              pred_N_true_tmp(s), pred_weca(s),pred_prop_landed(s));
                              
              
              if (test_output==53) cout<<" step 3: Mean True F, Q1-Q3: "<<setprecision(3)<<meanFsq_tmp*Fscaling<<
                                  "  Fscaling:"<<Fscaling<<"  Cap-F Q1-Q3:"<<upper_F<<endl;
              
              // Calc adjusted (realised) TAC for Q1-Q3
              if (Fscaling>0) { 
                  yield1=calc_Yield_from_Fscaling(s,fq,Fscaling,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,
                         pred_N_true_tmp(s),pred_weca(s),pred_prop_landed(s));
                  tacMax=yield1;
              } 
              if (test_output==53) cout<<" step 3:True fixed TAC (adjusted):  "<<setprecision(0)<<tacMax<<endl;
              TAC_true_half(s,y+1,1)=tacMax;
              pred_F_true_tmp(s,1)=pred_F_sq_tmp(1)* Fscaling;  // calc and save real F
              pred_F_true_tmp(s,2)=pred_F_sq_tmp(2)* Fscaling;
              pred_F_true_tmp(s,3)=pred_F_sq_tmp(3)* Fscaling;
              if (test_output==53) cout<<" step 3:True F Q1-Q3:  "<<setprecision(3)<<endl<<pred_F_true_tmp(s,1)<<endl
                                       <<pred_F_true_tmp(s,2)<<endl<<pred_F_true_tmp(s,3)<<endl;
              
              
              // udate true stock size to 1st July (and 1st October)
              tmp=0;
              for (q=fq;q<lq;q++){
                 for (a=faq(q);a<=la(s);a++){
                  // Total mortality
                  if (multi==2) pred_Z_true(s,q,a)=pred_F_true_tmp(s,q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_true(s,q,a)=pred_F_true_tmp(s,q,a)+pred_M(s,q,a); 
                  pred_N_true_tmp(s,q+1,a)= pred_N_true_tmp(s,q,a)*exp(-pred_Z_true(s,q,a));
                  
                } //age loop
               }  // q loop
 
               
               if (test_output==53) cout<<"step 3: Q1-Q3: pred_N_true_tmp: "<<endl<<setprecision(0)<<pred_N_true_tmp(s)<<endl<<endl;                
             
               
              // step 4, simulate the September assessment 
              
              // assessment noise and bias on stock numbers, Q3 (this simulates an assessment)
               q=3;
              if (assess_uncertanty(s,1)==1 ) {
                 //cout<<setfixed()<<setprecision(0)<<"True:"<<pred_N_true_tmp(s,q)<<endl;
                 
                 if (assess_uncertanty(s,4)==1) pred_N_obs(s,q)=                           // same noise on all ages
                       pred_N_true_tmp(s,q)*uncertanty(s,assess_uncertanty(s),randn(rng));
                 else if (assess_uncertanty(s,4)==0) for (a=fa;a<=la(s);a++) pred_N_obs(s,q,a)=      // diffrent noise on all ages
                  pred_N_true_tmp(s,q,a)*uncertanty(s,assess_uncertanty(s),randn(rng));

                //cout<<setprecision(0)<<"Obs: "<<pred_N_obs(s,q)<<endl<<endl;
                //for (a=fa;a<=la(s);a++) pred_N_obs(s,q,a)=pred_N_true_tmp(s,q,a)*uncertanty(s,assess_uncertanty(s),randn(rng));
              }
              else if (assess_uncertanty(s,1)==2 || assess_uncertanty(s,1)==3 ) {
                 for (a=faq(q);a<=la(s);a++) pred_N_obs(s,q,a)= pred_N_true_tmp(s,q,a);
                 cov_uncertanty(s, assess_uncertanty(s), pred_N_obs(s,q),coVariance(s).sub(fa,la(s)));
              }
       
              else if (assess_uncertanty(s,1)==4 ) {
                for (a=faq(q);a<=la(s);a++) pred_N_obs(s,q,a)= pred_N_true_tmp(s,q,a);
                //cout<<setfixed()<<setprecision(0)<<"age True:"<<pred_N_obs(s,q)<<endl;
                age_uncertanty(s, assess_uncertanty(s),pred_N_obs(s,q), assessment_CV_age(s));
                //cout<<setprecision(0)<<"age Obs: "<<pred_N_obs(s,q)<<endl<<endl;
              }
              
             if (test_output==53) cout<<"Step 4: pred_N_obs Q3:  "<<setprecision(0)<<endl<<pred_N_obs(s,q)<<endl<<endl;
                      
             // Step 5 calculate observed F for Q3 and update obs N to 1st October
             // calc Q3 real catch
             q=3;
             if (test_output==53) cout<<"N:   "<<setprecision(0)<<pred_N_true_tmp(s,q)<<endl<<
                   "Z:   "<<setprecision(3)<<pred_Z_true(s,q)<<endl<<
                   "F:   "<< pred_F_true(s,q)<<endl<<
                   "WECA:"<<setprecision(3)<<pred_weca(s,q)<<endl;
             tacMax=0;     
             for (a=faq(q);a<=la(s);a++) tacMax+=pred_N_true_tmp(s,q,a)*(1.0-exp(-pred_Z_true(s,q,a)))/pred_Z_true(s,q,a)* pred_F_true_tmp(s,q,a)*pred_weca(s,q,a); 
             
             if (test_output==53) cout<<"step 5: Q3 catch(realised):"<<tacMax<<endl;
             // cout<<"step 5: pred_F_sq_tmp:"<<setprecision(3)<<endl<<pred_F_sq_tmp<<endl;
              
              // begin in Q3 and set Q4 exploitation to zero the upper F limit is to 2 (which is a unrealistic high value)
             Fscaling=find_Fscaling_from_target_yield(s,3,0,2,tacMax,          
                              pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                              pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
              
              if (test_output==53) cout<<"step 5: Fscaling:"<<setprecision(3)<<Fscaling<<endl;                
              pred_F_sq_tmp(q)=pred_F_sq_tmp(q)* Fscaling;
 
              if (test_output==53) cout<<" step 5:  obs F, Q3 forecast: "<<pred_F_sq_tmp(q)<<endl;

              // udate observed stock size to 1st October 
              for (a=faq(q);a<=la(s);a++){
                  // Total mortality
                  if (multi==2) pred_Z_obs(s,q,a)=pred_F_sq_tmp(q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_obs(s,q,a)=pred_F_sq_tmp(q,a)+pred_M(s,q,a);  
                  // observed stock numbers
                   pred_N_obs(s,q+1,a)= pred_N_obs(s,q,a)*exp(-pred_Z_obs(s,q,a));
               } //age loop
               if (test_output==53) cout<<"step 5: Obs F Q3 "<<setprecision(3)<<pred_F_sq_tmp(q)<<endl; 
               if (test_output==53) cout<<"step 5: Q4: pred_N_obs: "<<setprecision(0)<<pred_N_obs(s,q+1)<<endl;                
               
              
              // step 6, calculate TAC Q4 and Q1-Q3 from escapement strategy
              
               // Start to prepare F matrix which consists of exploitation pattern Q4 in first year and Q1-Q3 in second year
                pred_F_sq_tmp=pred_F_sq_copy(s);
                pred_F_sq_tmp_year2=pred_F_sq_copy(s);
                
                for (q=fq;q<=3;q++) pred_F_sq_tmp(q)=0;  // Q4 only
                pred_F_sq_tmp_year2(4)=0;                // Q1-Q3
                
                meanFsq_tmp=1; 
                upper_F=real_time_uncertanty(s,4); // upper F used to set the TAC from the escapement strategy
                
                if (test_output==53) cout<<endl<<"Step 6: upper scaling for escapemnet strategy set to:"<<upper_F;
                
                Fscaling=find_Fscaling_from_target_SSB_two_years(s,4,0.0,upper_F,
                              target_SSB(s),pred_M(s),
                              pred_M1(s),pred_M2_true(s),pred_F_sq_tmp,pred_F_sq_tmp_year2,pred_N_obs(s),
                              pred_west(s),pred_weca(s), pred_propmat(s),y,yieldQ4,yieldQ1Q3 );
                if (test_output==53) cout<<" realized scaling:"<<Fscaling<<endl;
               tacMax=yieldQ4+yieldQ1Q3;
               if (test_output==53)  cout<<setprecision(0)<<"step 6: TAC Q4:"<<yieldQ4<<"  TAC Q1-Q3:"
                                         <<yieldQ1Q3<<"  sum:"<<tacMax<<endl;

              if ( tacMax < growth_model(4,s) || tacMax > growth_model(5,s)) {   // annual TAC outside TAC constraints 
                 if (tacMax < growth_model(4,s)) tmp=growth_model(4,s);
                 else if (tacMax > growth_model(5,s)) tmp=growth_model(5,s);
                 if (test_output==53) cout<<"step 6: target TAC changed due to constraints: "<<tmp<<endl; 
                
                  find_Fscaling_from_target_yield_two_years(s, 4, 0, 2, tmp, pred_M(s), pred_M1(s), pred_M2_true(s),pred_F_sq_tmp,pred_F_sq_tmp_year2, pred_N_obs(s), pred_weca(s),pred_west(s),pred_propmat(s),pred_prop_landed(s), yieldQ4, yieldQ1Q3);
                if (Fscaling>1E-4) {
                  pred_F_sq_tmp(4)=pred_F_sq_tmp(4)* Fscaling;
                  pred_F_sq_tmp_year2=pred_F_sq_tmp_year2* Fscaling;
                }
                if (test_output==53) cout<<"Step 6: target yield after TAC constraints, Q4:"<<setprecision(0)<<yieldQ4<<" and Q1Q3:"<<yieldQ1Q3<<"  total:"<<yieldQ4+yieldQ1Q3<<endl;
                TAC_true_half(s,y+1,2)=yield1;
              }  

          
            // Step 7: Calculate true F (and true catches) for Q4 such that the real F does not exceed CAP FQ4, and adjust remaining TAC 
             upper_F=growth_model(2,s);  // Cap F Q4
             
             if (test_output==53) {
               cout<<"Step 7: pred_F (Q4)  before scaling:"<<endl<<setprecision(3)<<pred_F_sq_tmp(4)<<endl;
               cout<<"Step 7: target yield Q4:"<<setprecision(0)<<yieldQ4<<endl;
             }
          

             q=3;
             // calculate true stock N in start of Q4
              for (a=faq(q);a<=la(s);a++){
                  // Total mortality
                  if (multi==2) pred_Z_obs(s,q,a)=pred_F_true_tmp(s,q,a)+pred_M1(s,q,a)+pred_M2_true(s,q,a);  //used muli species
                  else  pred_Z_obs(s,q,a)=pred_F_true_tmp(s,q,a)+pred_M(s,q,a);  
                  // observed stock numbers
                   pred_N_true_tmp(s,q+1,a)= pred_N_true_tmp(s,q,a)*exp(-pred_Z_obs(s,q,a));
               } //age loop
             
            
             // calc new Q4 mean F 
             meanFsq_tmp=0.0; 
             for (q=4;q<=lq;q++) for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) meanFsq_tmp+=pred_F_sq_tmp(q,a);
             meanFsq_tmp=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);  
             q=4; 
             if (test_output==53) cout<<"pred_N_true_tmp(s,q=4):"<<pred_N_true_tmp(s,q)<<endl;
             upper_F=growth_model(2,s); //Cap F Q4
             Fscaling=find_Fscaling_from_target_yield(s,q,0,upper_F/meanFsq_tmp,yieldQ4,
                                pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq_tmp,
                                pred_N_true_tmp(s), pred_weca(s),pred_prop_landed(s));

             
                    
             if (Fscaling>1E-4) pred_F_true_tmp(s,q)=pred_F_sq_tmp(q)* Fscaling;
             else pred_F_true_tmp(s,q)=0.0;

              if (test_output==53) cout<<"Step 7: pred_F_true_tmp after scaling:"<<endl<<setprecision(3)<<pred_F_true_tmp(s,q)<<endl;
              
              if (Fscaling>0) {
                yield1=calc_Yield_from_Fscaling(s,q,1.0,pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_true_tmp(s),pred_N_true_tmp(s),pred_weca(s),pred_prop_landed(s));
               }
              else {yield1=0.0; Fscaling=0; } 
              
              if (test_output==53) cout<<"Step 7: realised catch Q4:"<<yield1<<endl;
               
              TAC_true_half(s,y,2)=yield1;
               
              // update pred_F_sq to include the absolute F for the year
              for (q=fq;q<=lq;q++) pred_F_sq(s,q)=pred_F_true_tmp(s,q);
               
              // calc new mean F 
              meanFsq_tmp=0.0; 
              for (q=fq;q<=lq;q++) for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) meanFsq_tmp+=pred_F_sq(s,q,a);
              meanFsq(s)=meanFsq_tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);

              Fmax=meanFsq(s);
              if (test_output==53) {
                cout<<"True mean F for year "<<y<<":"<<meanFsq(s)<<endl; 
                cout<<endl<<"step 7: true F for year "<<y<<endl<<setprecision(3)<< pred_F_sq<<endl;
              } 
              //  Q1Q3 TAC=TAC_true_half(s,y,1);  Q4 TAC=TAC_true_half(s,y,2);
              TAC_true_half(s,y+1,2)=yield1;  // Q4            
              TAC_true_half(s,y+2,1)=yieldQ4+yieldQ1Q3-yield1;   // Q1Q3
             
              //cout<<setprecision(0)<<"true TAC for TAC year "<<y+1<<"  Q4:"<<TAC_true_half(s,y+1,2)<<" and year"<<y+2<<" Q1Q3:"<<TAC_true_half(s,y+2,1)<<" sum:"<<TAC_true_half(s,y+1,2)+TAC_true_half(s,y+2,1)<<endl;
              //cout<<setprecision(0)<<setfixed()<<"true TAC for year "<<y+1<<" Q1Q3:"<<TAC_true_half(s,y+1,1)<<"  Q4:"<<TAC_true_half(s,y+1,2)<<" sum:"<<(TAC_true_half(s,y+1,1)+TAC_true_half(s,y+1,2))<<endl;
             
            
              break;
               
     
      // end Norway pout MSE 112

      case 333: // SPECIAL CASE, Blue whiting, 2005 management plan
                // F should decrease to Fpa (constantF) in initial phase.
                //   If F has not reached Fpa, decrease TAC by xx t. variable constant.TAC is used as input var for xx
                // after initial phase use target.SSB and upper level at F at Fpa
                Fmin=real_time_F(s);
                if (BW_init_phase) {
                  if (test_output==53) cout<<"Init condition"<<endl;
                  //Fmax=constantF(s);

                  yield1=TAC_obs(s,TAC_year-1)-constantTAC(s); // pargraph 4
                  Fscaling=find_Fscaling_from_target_yield(s,fq,Fmin/meanFsq(s),max_F_scale/meanFsq(s),yield1,
                               pred_M(s),pred_M1(s), pred_M2_true(s), pred_F_sq(s),
                               pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
                  tmp=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                      pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);

                  if (tmp<Fscaling) Fscaling=tmp;

                  if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                  if (test_output==53) cout<<"initial condition. TAC("<<setprecision(0)<<TAC_year-1<<"):"<<
                                        TAC_obs(s,TAC_year-1)<<" TAC("<<TAC_year<<"):"<<yield1<<
                                        " at F:"<<setprecision(3)<<Fmax<<endl;
                  if (Fmax<=constantF(s)) {
                    BW_init_phase=0;
                    if (test_output==53) cout<<"End of initial condition."<<endl;
                    //added Sep 2007
                       // if the F for reaching the yield target is lower than Fpa and
                       // Fpa does not lead to a SSB below Bpa then use F=Fpa
                       if (test_output==53) cout<<"Fmax:"<<Fmax<<" constantF:"<<constantF(s)<<"  tmp:"<<tmp<<" tmp*meanFsq:"<<tmp*meanFsq(s)<<endl;
                       if (tmp > (constantF(s)/meanFsq(s))) {
                         if (test_output==53) cout<<"F raised from "<< setprecision(3)<<Fmax;
                         Fmax=tmp*meanFsq(s);
                         if (Fmax>constantF(s)) Fmax=constantF(s);
                         if (test_output==53) cout<<" to "<< setprecision(3)<<Fmax<<endl;
                       }
                    // end added Sep 2007
                  }
                }
                else {
                  // Estimate F from a target SSB in the TAC year+1
                  Fscaling=find_Fscaling_from_target_SSB(s,1,0.0,max_F_scale/meanFsq(s),target_SSB(s),pred_M(s),
                    pred_M1(s),pred_M2_true(s),pred_F_sq(s),pred_N_obs(s),pred_west(s),pred_propmat(s),y);
                if (Fscaling>0) Fmax=meanFsq(s)*Fscaling; else Fmax=0.0;
                if (Fmax>constantF(s)) Fmax=constantF(s);
                 if (test_output==53) cout<<"BW phase2:"<<endl;
                }
               break;
      case 334:
      case 335: // SPECIAL CASE, Blue whiting, 2008 management plan
                // Init phase: Decrease F to Ftarger (constantF) by xx percent each year. xx is given by real_time_uncertanty(s,1)
                //   init phase is reached when F has become <= Ftarget
                // After init phase change HCR option to zz, specified by yy (real_time_uncertanty(s,2)
              
                Fmin=real_time_F(s);
                if (HCR_state(s)==1) {
                  if (test_output==53) cout<<"BW Init condition: TAC_year="<<TAC_year<<"  TAC_F_obs("<<TAC_year-1<<")="<<setprecision(3)<<
                           TAC_F_obs(s,TAC_year-1)<<endl;
                  
                  // reduce F by x%
                  if (TAC_F_obs(s,TAC_year-1)>constantF(s)) {
                   Fmax=TAC_F_obs(s,TAC_year-1)*(1.0-real_time_uncertanty(s,1)/100);
                   if (test_output==53) cout<<"F reduction by "<< setprecision(1)<<real_time_uncertanty(s,1)<<"% giving F="<<setprecision(3)<<Fmax<<endl;
                   if (Fmax<=constantF(s)) {  // target is reached
                     if (test_output==53) cout<<"state 2 reached"<<endl;
                     HCR_state(s)=2;
                     Fmax=constantF(s);
                     TAC_constraint(s,1)=TAC_constraint(s,1)/real_time_uncertanty(s,3);
                     TAC_constraint(s,2)=TAC_constraint(s,2)*real_time_uncertanty(s,3);
                     if (test_output==53) cout<<" HCR=334&335 state two. TAC constraints changed to:"<<setprecision(3)<< TAC_constraint(s)<<endl;
                     HCR(s)=int(real_time_uncertanty(s,2));
                     if (test_output==53) cout<<" HCR changed from 334&335 to:"<<HCR(s)<<endl; 
                    }
                 } else Fmax=constantF(s);
                 }

                break;


                //  ---
 
       default: cout<<"NOT A VALID HCR:"<<HCR(s)<<" for species:"<<s<<endl;
                exit(9);
                break;
      }
      
      // F cannot be lower than Fmin
      if (Fmin>Fmax) Fmax=Fmin;

      if (test_output==53) cout<<"F_TAC("<<TAC_year<<") after basic HCR:"<<setprecision(3)<<Fmax<<endl
                               <<"explotation pattern:"<<endl<<pred_F_sq(s)<<endl;;
        
      if (Fmax>0) yield1=calc_Yield_from_Fscaling(s,fq,Fmax/meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),pred_F_sq(s),
                    pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
      else yield1=0.0;
      
      if (test_output==53) cout<<setprecision(0)<<"TAC("<<TAC_year<<") after basic HCR:"<<yield1<<endl;
     
      // Year to year variation constraints

      //  F constraints
      if (F_constraint(s,1)!=0 || F_constraint(s,2)!=0) {
         change=Fmax/TAC_F_obs(s,TAC_year-1);
         if (change<F_constraint(s,1) && F_constraint(s,1) !=0 ) {
            Fmax=TAC_F_obs(s,TAC_year-1)*F_constraint(s,1);
            constraints(s,y)=constraints(s,y)+1;
         }
         else if (change>F_constraint(s,2) && F_constraint(s,2) !=0) {
           Fmax=TAC_F_obs(s,TAC_year-1)*F_constraint(s,2);
           constraints(s,y)=constraints(s,y)+2;
         }
         yield1=calc_Yield_from_Fscaling(s,fq,Fmax/meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),
                 pred_F_sq(s),pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
         if (test_output==53) cout<<"TAC_F_obs("<<setprecision(0)<<TAC_year-1<<"): "<<setprecision(3)
                   <<TAC_F_obs(s,TAC_year-1)<<" Change:"<<change<<"  constraint:"<<F_constraint(s)<<endl;
         if (test_output==53) cout<<"F_TAC after F constraints:"<<setprecision(3)<<Fmax<<endl;
      } 
       
      // Yield constraints   
      if (TAC_constraint(s,1)>0.0 || TAC_constraint(s,2)>0.0) { 
        if (test_output==53) cout<<"F_TAC before constraints:"<<setprecision(3)<<Fmax<<endl;
        change=yield1/TAC_obs(s,TAC_year-1);    
        if (test_output==53) cout<<setprecision(0)<<"TAC("<<TAC_year-1<<"):"<<TAC_obs(s,TAC_year-1)<<
          "  TAC("<<TAC_year<<"):"<<yield1<<setprecision(3)<<"  Change:"<<change
           <<"  constraint:"<<TAC_constraint(s)<<endl;
        tmp=-1.0;
        Fscaling=-1;
        if (change<TAC_constraint(s,1)) { // decrease in yield is too much raise F
            Fscaling=find_Fscaling_from_target_yield(s,fq,Fmax/meanFsq(s), max_F_scale/meanFsq(s), 
              TAC_obs(s,TAC_year-1)*TAC_constraint(s,1), pred_M(s), pred_M1(s), 
              pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
              constraints(s,y)=constraints(s,y)+2;
        }
        else if (change>TAC_constraint(s,2)) { // increase in yield is too much, decrease F
              Fscaling=find_Fscaling_from_target_yield(s,fq, Fmin/meanFsq(s),max_F_scale/meanFsq(s),
                       TAC_obs(s,TAC_year-1)*TAC_constraint(s,2), pred_M(s), pred_M1(s), 
                       pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
              constraints(s,y)=constraints(s,y)+4;
        }  
        
        if (Fscaling >=0) Fmax=meanFsq(s)*Fscaling;
        yield1=calc_Yield_from_Fscaling(s,fq,Fmax/meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),
                 pred_F_sq(s),pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                 
        if (test_output==53) cout<<"TAC_F("<<setprecision(0)<<TAC_year<<") after catch constraints:"
               <<setprecision(3)<<Fmax<<endl;


       
        //if (HCR(s)==44 && HCR_state(s)==2) if (Fmax>T1(s)) {
        if (HCR(s)==44 ) {
         if (Fmax<constantF(s)) {
            Fmax=constantF(s);
            if (test_output==53) cout<<"HCR=44, F lower than "<< constantF(s)<<" and raised to that value"<<endl;
            // re-calc yield
            yield1=calc_Yield_from_Fscaling(s,fq,Fmax/meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),
                 pred_F_sq(s),pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
         }
         else if (Fmax>T1(s)) {
           if (test_output==53) cout<<"HCR=44. F after application of TAC constraint is:"<<Fmax<<" and higher than upper F:"<<T1(s)<<". F is decreased to:"<<T1(s)<<endl;
           Fmax=T1(s);  // interpretation of article 4 in Cod baltic
           // re-calc yield
           yield1=calc_Yield_from_Fscaling(s,fq,Fmax/meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),
                 pred_F_sq(s),pred_N_obs(s),pred_weca(s),pred_prop_landed(s));

        }
        } // end HCR(s)==44 '
       } // End "normal" Yield constraints 

        // Islandic version. Let the final TAC be the average over X% previous years TAC and Y% this year TAC
        if (TAC_constraint(s,1)<0.0 &&  TAC_constraint(s,2)<0.0) { 
          if (test_output==53) cout<<"F_TAC before constraints:"<<setprecision(3)<<Fmax<<endl;
          change=yield1/TAC_obs(s,TAC_year-1);    
          if (test_output==53) cout<<setprecision(0)<<"Icelandic model: "<<-TAC_constraint(s,1)<<"% of TAC("<<TAC_year-1<<"):"<<TAC_obs(s,TAC_year-1)<<
            " and "<<-TAC_constraint(s,2)<<"% of TAC("<<TAC_year<<"):"<<yield1;
          tmp=(TAC_obs(s,TAC_year-1)*TAC_constraint(s,1)+yield1*TAC_constraint(s,2))/(TAC_constraint(s,2)+TAC_constraint(s,2));
          if (test_output==53) cout<<" Result TAC:"<<tmp<<endl;
          Fscaling=-1;
          Fscaling=find_Fscaling_from_target_yield(s,fq,0, 2, 
                tmp, pred_M(s), pred_M1(s), 
                pred_M2_true(s), pred_F_sq(s), pred_N_obs(s), pred_weca(s),pred_prop_landed(s));
          constraints(s,y)=constraints(s,y)+2;

          if (Fscaling >=0) Fmax=meanFsq(s)*Fscaling;
          yield1=calc_Yield_from_Fscaling(s,fq,Fmax/meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),
                   pred_F_sq(s),pred_N_obs(s),pred_weca(s),pred_prop_landed(s));
                   
          if (test_output==53) cout<<"TAC_F("<<setprecision(0)<<TAC_year<<") after catch constraints:"
                 <<setprecision(3)<<Fmax<<endl;
       }
    
       // SSB constraints
       // ?????


      TAC_F_obs(s,TAC_year)=Fmax;
      TAC_obs(s,TAC_year)=yield1;
      
      if (HCR_F_TAC(s)==1) {       //implement HCR using estimated TAC to estimate true F
        TAC_F_true(s,TAC_year)=-1.0;
        
        if (implemt_uncertanty(s,1)>=0) {
          if (test_output==53)  cout<<"yield before implementation noise:"<<setprecision(0)<<yield1<<endl;
          yield1=yield1*uncertanty(s,implemt_uncertanty(s),randn(rng));  // implementation bias and noise
          if (test_output==53)  cout<<"yield after implementation noise:"<<yield1<<endl;
        }
         // adjust according to max TAC (cap TAC, given as input)
        if (TAC_cap(s)>0) { 
          if (yield1>TAC_cap(s)) {
            if (test_output==53) cout<<"Cap TAC adjustment done."<<endl;
            yield1=TAC_cap(s);           
          }
        }
        if (TAC_min(s)>0) { 
          if (yield1<TAC_min(s)) {
            if (test_output==53) cout<<"Min TAC adjustment done."<<endl;
            yield1=TAC_min(s);           
          }
        }

        
        if (HCR(s)==24 && F_cap(s)>0) { 
           upper_F=F_cap(s)/meanFsq(s); 
           if (test_output==53) cout<<"Yield before F_cap: "<<yield1<<endl;
           if (yield1>0) Fscaling=find_Fscaling_from_target_yield(s,fq,real_time_F(s)/meanFsq(s),upper_F, yield1, pred_M(s), pred_M1(s),
                    pred_M2_true(s), pred_F_sq(s), pred_N_true(s), pred_weca(s),pred_prop_landed(s));
           else Fscaling=0.0; 
           if (test_output==53) cout<<"Fscaling: "<<setprecision(3)<< Fscaling<< "  F:"<< Fscaling*meanFsq(s)<<endl;          
           if (Fscaling*meanFsq(s)>F_cap(s)) yield1=calc_Yield_from_Fscaling(s,fq,Fscaling*meanFsq(s),pred_M(s),pred_M1(s),pred_M2_true(s),
                 pred_F_sq(s),pred_N_true(s),pred_weca(s),pred_prop_landed(s));
           if (test_output==53) cout<<"Yield after F_cap:  "<<setprecision(0)<<yield1<<endl;

        }
        
        TAC_true(s,TAC_year)=yield1;
        TAC_F_true(s,TAC_year)=-1;
      }
      else {     //implement HCR using estimated F as true F
  
         if (implemt_uncertanty(s,1)>=0) {
            if (test_output==53)  cout<<"F before implementation noise:"<<setprecision(3)<<Fmax<<endl;
            Fmax=Fmax*uncertanty(s,implemt_uncertanty(s),randn(rng));  // implementation bias and noise 
            if (test_output==53) cout<<"F after implementation noise:"<<Fmax<<endl;
          }
          
         // adjust to overall species max F (F cap given as input)
         if (F_cap(s)>0) {
           if (Fmax>F_cap(s)) {
             Fmax=F_cap(s);
             if (test_output==53) cout<<"Cap F adjustment done"<<endl;
           }
         }
         TAC_F_true(s,TAC_year)=Fmax;
         TAC_true(s,TAC_year)=-1;
      }
          
       if (test_output==53) {
         if (HCR_F_TAC(s)) cout<<"F_true will be derived from TAC_true("<<
              setprecision(0)<<TAC_year<<"):"<<TAC_true(s,TAC_year)<<endl;
         else cout<<"F_true will be derived from F_true("<<TAC_year<<"):"<<
           setprecision(3)<<TAC_F_true(s,TAC_year)<<endl; 
         cout<<"Finished to calc TAC("<<setprecision(0)<<TAC_year<<") for "<<species_names[s]<<":"<<endl;
       }   
         
        
    }   //end species HCR   
   }     //end  year loop
  }   // do HCR for next year
            
  if (MCMC_prediction==1 &&  MCMC_iteration==1) write_other_pred();


  // seed for next random number t
  seed=seed+1;

  // write temporary data
  tmp_SSB(MCMC_prediction,MCMC_iteration)=value(SSB);
  tmp_SSB_percieved(MCMC_prediction,MCMC_iteration)=value(SSB_percieved);
  tmp_TSB(MCMC_prediction,MCMC_iteration)=value(TSB);
  tmp_mean_F(MCMC_prediction,MCMC_iteration)=value(Mean_F);
  tmp_recruit(MCMC_prediction,MCMC_iteration)=recruit;
  tmp_mean_F_percieved(MCMC_prediction,MCMC_iteration)=value(Mean_F_percieved);
  tmp_yield(MCMC_prediction,MCMC_iteration)=value(yield);
  if (multi==2) tmp_eaten_M2(MCMC_prediction,MCMC_iteration)=value(eaten_M2);
  
  if (at_age_output[7]) {strcpy(txtL,"constraints"); out_predict_raw2(txtL,MCMC_iteration,value(constraints));}
  if (at_age_output[8]) {strcpy(txtL,"closure"); out_predict_raw2(txtL,MCMC_iteration,value(closure));}
 
 //********************************************************************************************* 
 
 
FUNCTION Calc_avg_M2
 int s,y,q,a,no;
 dvariable tmp;

 if (multi>=2) {
    cout << endl <<"Average M2";
    for (s=first_VPA;s<=nsp;s++){
     cout << endl<<species_names[s]<<"   ";
     for (a=fa;a<=la(s);a++) {
       tmp=0.0; no=0;
       for (y=fyModel;y<=lyModel;y++){
         lqLocal=(y==lyModel)?lqly:lq;
         no++;
         for (q=fq;q<=lqLocal;q++) if (!((a==fa) && (q <recq)))tmp+=M2((y-fyModel)*lq+q,s,a);



       }
       cout << setw(7) << setprecision(3) << setfixed()<<tmp/no;
     }
   }
 }


FUNCTION write_warnings
  int s2_group,s2_la,s2_fa, sp_fl;
  int a,s,q,f;
  dvariable mins;
  
  mins=min_catch_CV*min_catch_CV;
  mins=mins+mins/10;
  cout<<endl;
   for (s=first_VPA;s<=nsp;s++){
      for (s2_group=1;s2_group<=n_catch_s2_group(s);s2_group++) {
       s2_fa=catch_s2_group(s,s2_group);
       if (s2_group==n_catch_s2_group(s)) s2_la=la(s);
       else s2_la=catch_s2_group(s,s2_group+1)-1;
       for (a=s2_fa;a<=s2_la;a++) {
         for (q=fq;q<=seasonal_combined_catch_s2(s);q++)
          if (catch_s2(s,q,s2_group)>0 && catch_s2(s,q,s2_group)<=mins) {
            cout<<"Warning: CV of catch obs. below limit. ";
            if (nsp>1) cout<<  species_names[s];
            cout << "season "<<q<<", age "<<a<<endl;
       }
     }
   }
  }
  
 mins=CPUE_minStd*CPUE_minStd;
 mins=mins+mins/10;
 sp_fl=0;
 for (s=first_VPA;s<=nsp;s++){
   for (f=1; f<=n_fleet(s);f++) {
      sp_fl++;
      for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
         s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++)
         if (qq_s2(sp_fl,s2_group)>0 && qq_s2(sp_fl,s2_group)<=mins && s>=first_VPA && multi!=1) {
            cout<<"Warning: CV of CPUE obs. below limit. ";
            if (nsp>1) cout<<  species_names[s];
            cout<< " "<<fleet_names[sp_fl]<<" age "<<a<<endl;
         }
      }
    }
 }
 
 mins=min_SR_CV*min_SR_CV;
 mins=mins+mins/10;

 for (s=first_VPA;s<=nsp;s++){
   if (SSB_R_s2(s)>0 && SSB_R_s2(s)<=mins) {
       cout<<"Warning: CV of SSB-R obs. below limit. ";
       if (nsp>1) cout<<  species_names[s];
       cout<<endl;
   }
 }


 
FUNCTION check_print_par
 int s;
 
   cout << setw(12) << setprecision(5) << setfixed();
    for (s=first_VPA;s<=nsp;s++) if (do_effort(s)==0) cout << endl << "F_y_ini"<<endl<<F_y_ini(s)<<endl;
    cout << endl << "F_q_ini:"<<endl;
    for (i=1;i<=no_sp_sag_syg;i++) {
      for (j=fcq(i);j<=lcq(i);j++) { 
        cout<<F_q_ini(i,j)<<" ";
      }
      cout<<endl;
    }
    cout << endl << "log_F_a_ini"<<endl<<log_F_a_ini<<endl;
    cout << setw(12) << setprecision(5) <<  setfixed() ;
    cout << endl <<  "log rec scale:  " <<log_rec_scale<< endl;
    cout << endl << "log_rec"<<endl<<log_rec<<endl;
    cout << endl << "log_rec_older"<<endl<<log_rec_older<<endl;
    cout << endl << "qq_ini"<<endl<<qq_ini<<endl;
    cout << endl << "SSB_R_alfa"<<endl<<SSB_R_alfa<<endl;
    cout << endl << "SSB_R_beta"<<endl<<SSB_R_beta<<endl;
    if (active(vulnera) && multi>=1) {
        cout <<"vulnera: "<<active(vulnera)<<endl<<vulnera<<endl;
        if (init_pref_size_ratio.indexmin()>0) cout <<"init Prefered size ratio: "<<endl<<init_pref_size_ratio<<endl;
        if (var_size_ratio.indexmin()>0) cout <<"Variance of size ratio: "<<endl<<var_size_ratio<<endl;
        cout <<"Other food suitability slope: "<<endl<<stl_other_suit_slope<<endl;
        cout <<"init_season_overlap: "<< endl<<init_season_overlap<<endl;
        cout <<"Stom_var:"<<endl<<Stom_var<<endl;;
    }
    cout <<endl;

  
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

BETWEEN_PHASES_SECTION

 if (test_output==11 || test_output==19) {
   cout << endl<<endl<< "XXXXXXXXXXXXXXXXXXXXX  Before phase=";
   cout << current_phase()<<" minimizaion XXXXXXXXXXXXXXXXXXXXXXX" <<endl;
 }
 
//initialise S/R parameters, if not read in as a parameter  
 if ((current_phase()==phase_SSB_R_alfa) && (mean(SSB_R_alfa) == 1.0)) { 

   if (current_phase()>1) get_biomass();
   else SSB=1e5;
   
   for (s=first_VPA;s<=nsp;s++) { 
     if (SSB_Rec_model(s)==1) {    // Ricker
       //SSB_R_log_beta[s]=log(1/mean(SSB[s]));
       SSB_R_beta(s)=SSB_R_beta_cor(s)/mean(SSB(s));

       SSB_R_alfa(s)=exp(log_rec_scale(s))*SSB_R_beta(s)/SSB_R_beta_cor(s)/exp(-1.0); 
       //SSB_R_alfa[s]=exp(log_rec_scale(s))*exp(SSB_R_log_beta(s))/exp(-1.0); 
     }
     else if (SSB_Rec_model(s)==2) {   // Beverton & Holt
       //SSB_R_log_beta[s]=log(1/mean(SSB[s]));
       SSB_R_beta(s)=SSB_R_beta_cor(s)/mean(SSB(s));

       SSB_R_alfa(s)=2*exp(log_rec_scale(s))/mean(SSB(s));
     }  
     
     else if (SSB_Rec_model(s)==3) {   // Geometric mean
       SSB_R_alfa(s)=log_rec_scale(s); 
     } 
     else if (SSB_Rec_model(s)==4) {   // Hockey stick
        SSB_R_alfa(s)=log_rec_scale(s)-log(mean(SSB(s)));
        //SSB_R_log_beta[s]=log(mean(SSB[s]));
        SSB_R_beta(s)=mean(SSB(s))/SSB_R_beta_cor(s);
     }  
   }
 }
 
 if (test_output==11 || test_output==19) {
    check_print_par();   
    cout << endl <<" end of   XXXXXXXXXXXXX  Before phase="<< current_phase();
    cout <<" minimization XXXXXXXXXXXX" <<endl;
 }
 //********************************************************************************************* 

FUNCTION print_input_par
  int s,fl;
  cout << endl;
  cout << "First year, last year (data):              " << fyModel<<" " <<lyData <<endl;
  cout << "First year, last year (model):             " << fyModel<<" " <<lyModel <<endl;
  cout << "First season, last season:                 " << fq<<" " <<lq <<endl;
  cout << "Last season in last year:                  " << lqly <<endl;
  cout << "Number of species:                         " << nsp <<endl;
  cout << "Number of preadtors:                       " << npr <<endl;
  cout << "First age all species:                     " << fa <<endl;
  cout << "Recruiment season:                         " << recq <<endl;
  cout << "Max age all species combined:              " << max_a <<endl;
  cout << "Last age by species:                       " << la <<endl;
  cout << "Last age for age indep. selec. by species: " << las <<endl;
  cout << "First age where catch data are used:     " <<  cfa << endl; 
  cout << "Plusgroup by species:                      " << nplus <<endl;
  cout << "No. of separate catch s2 groups by species:" << n_catch_s2_group << endl;
  cout << "First age group in each catch s2 by species:       " << endl;
  cout << catch_s2_group << endl;
  cout << "No of separate seasonal catch groups by species:   " << endl << n_catch_season_age_group << endl;
  cout << "First age group in each seasonal group by specis:  " << endl << catch_season_age << endl; 
  cout << "Weighting factor for objective function contribution:" << endl << obj_weight << endl;
  cout << "First and last ages in calculation of average F:   " << endl << avg_F_ages <<endl;
  cout << "Number of age selection group:            " << endl<< n_catch_sep_year_group << endl;
  cout << "First year for each age selection group:  " << endl<< catch_sep_year <<endl;

  cout << "Minimum CV of CPUE observations :          " << CPUE_minStd << endl;
  
  cout << "Multispecies mode:                         " << multi<<endl;  
  cout << "Stock numbers used in fitting suit.   parm:" << use_Nbar <<endl;
  cout << "Stomach variance model:                    " << stomach_variance<<endl;

  cout << endl << "Fleet info:" <<endl;
  int sp_fl=0;
  for (s=first_VPA;s<=nsp;s++) {
    cout <<"Species: "<< s<<"  "<<species_names[s]<<endl;
     for (fl=1; fl<=n_fleet(s);fl++) {
      sp_fl++;
      cout << setw(10) << setprecision(0) << setfixed();
      cout <<" Fleet info:"<< fleet_names[sp_fl]<<" "<<fleet_info(s,fl) << endl;
      cout <<"   CPUE s2 groups:           "<<CPUE_s2_group(s,fl) << endl;
      cout << setw(10) << setprecision(2) << setfixed();
    }
  }
 //********************************************************************************************* 

FUNCTION void print_summary_MCMC()
 int s,y,q,a;
 int yq;
 
  if (MCMC_prediction==1) {
     ofstream res_MCMC("summary_mcmc.out",ios::out);
     res_MCMC <<"Repetion Year Quarter Species.n Age  M2 F Z  N C.hat"<<endl;
     res_MCMC.close();
  }
  ofstream res_MCMC("summary_mcmc.out",ios::app);
  for (s=first_VPA;s<=nsp;s++){
    for (y=fyModel;y<=lyModel;y++){
      lqLocal=(y==lyModel)?lqly:lq;
      for (q=fq;q<=lqLocal;q++){
        CALC_yq
        for (a=fa;a<=la(s);a++) {
           if (!((a==fa) && (q <recq))) {
             res_MCMC <<MCMC_prediction<<" "<<y<<" "<<q<<" " <<s<<" "<<a<<" ";
             if (multi >=1 && s>=first_VPA) res_MCMC <<M2(yq,s,a)<<" ";
             else res_MCMC << "0.0 ";
             if (s>=first_VPA) res_MCMC <<F(yq,s,a)<<" "; else res_MCMC<<" 0 ";
             if (s>=first_VPA)res_MCMC <<Z(yq,s,a)<<" "; else res_MCMC<<" 0 ";
             res_MCMC <<N(yq,s,a)<<" ";
             if (s>=first_VPA)res_MCMC <<C_hat(yq,s,a)<<" ";else res_MCMC<<" 0 ";
              res_MCMC << endl;;
           }
        }
      }
    }
  }
  res_MCMC.close();

FUNCTION void print_summary()
 int s,y,q,a;
 int yq;
 int d;
 d=1;  // SKAL LAVES OM
 ofstream res("summary.out",ios::out);
 res <<"Year Quarter Species.n Age M1 M2 M F Z N N.bar C.hat C.obs west weca Yield CWsum prop.landed prop.in propmat BIO SSB Lsea ration"<<endl;
  for (s=first_VPA;s<=nsp;s++){
    for (y=fyModel;y<=lyModel;y++){
      lqLocal=(y==lyModel)?lqly:lq; 
      for (q=fq;q<=lqLocal;q++){
        CALC_yq
        CALC_yqd
        for (a=fa;a<=la(s);a++) {
           if (!(a==fa && q <recq)) {
             res <<y<<" "<<q<<" " <<s<<" "<<a<<" ";
             if (multi >=1 ) {
               res <<M1(yq,s,a)<<" ";
               res <<M2(yq,s,a)<<" ";
             }
             else res << "0 0 ";
             res <<M(yq,s,a)<<" "; 
             res <<F(yq,s,a)<<" "; 
             res <<Z(yq,s,a)<<" "; 
             res <<N(yq,s,a)<<" ";
             res <<N_bar(yq,s,a)<<" ";
             res <<C_hat(yq,s,a)<<" ";
             res <<obs_C(yq,s,a)<<" ";
             res <<west(yq,s,a)<<" ";
             res <<weca(yq,s,a)<<" ";
             res <<obs_C(yq,s,a)*weca(yq,s,a)*prop_landed(yq,s,a)<<" ";
             res <<obs_C(yq,s,a)*weca(yq,s,a)<<" ";
             res <<prop_landed(yq,s,a)<<" ";
             res <<N_prop_M2(yq,s,a)<<" ";          
             res <<propmat(yq,s,a)<<" ";
             res <<N(yq,s,a)*west(yq,s,a)<<" ";
             res <<N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a)<<" ";
             if (multi >=1 ) res <<lsea(yqd,s,a)<<" ";  else res << "0 ";
             if (multi >=1  && s<=npr) res <<consum(yqd,s,a)<<" ";  else res << "0 ";
             res << endl;;
           }
        }
      }
    }
  }

  
  
 // other predators
   if (multi >=1) for (s=1;s<=nOthPred;s++){
    for (y=fyModel;y<=lyModel;y++){
      lqLocal=(y==lyModel)?lqly:lq; 
      for (q=fq;q<=lqLocal;q++){
        CALC_yq
        CALC_yqd
        for (a=fa;a<=la(s);a++) if (!(a==fa && q <recq)) {
           if ( N(yq,s,a)>0) {
             res <<y<<" "<<q<<" " <<s<<" "<<a<<" ";
             res<<"-1 -1 -1 -1 0 ";  // M1 M2 M F Z 
             res <<N(yq,s,a)<<" ";
             res <<N(yq,s,a)<<" ";  // Nbar
             res <<"-1 -1 ";  // C_hat obs_C
             res <<west(yq,s,a)<<" ";
             res <<"-1 -1 -1 -1 -1 -1 "; // weca yield CWsum prop.landed prop.in propmat
             res <<N(yq,s,a)*west(yq,s,a)<<" ";
             res <<"-1 ";  // SSB
             res <<lsea(yqd,s,a)<<" "; 
             res <<consum(yqd,s,a)<<" "; 

             res << endl;;
           }
        }
      }
    }
  }

  // extend with data for last assessment year plus one
  // Year Quarter Species.n Age M1 M2 M F Z N N.bar C.hat C.obs west weca Yield CWsum prop.landed propmat BIO SSB Lsea ration
    if (lqly==lq) for (s=first_VPA;s<=nsp;s++){
    y=lyModel+1;
    q=1;
    CALC_yq
    CALC_yqd
        for (a=faq(1);a<=la(s);a++) {
           res <<y<<" "<<q<<" " <<s<<" "<<a<<" ";
           if (multi >=1 && s>=first_VPA) {
             res <<-1<<" ";
             res <<-1<<" ";
           }
           else res << "0 0 ";
           res <<" -1";       //M
           res <<" -1";         // F
           res <<" -1 ";         // Z
           res <<N(yq,s,a)<<" ";
           res <<" -1 ";        // Nbar
           res <<" -1 ";        // C_hat
           res <<" -1 ";        // obs_C
           if (do_effort(s)==1) res <<west(yq,s,a)<<" ";
           else res <<west(yq-lq,s,a)<<" ";
           res <<" -1 ";        // weca
           res <<"-1 -1 -1 -1 ";        // yield CWsum prop.landed   prop.in
           if (do_effort(s)==1) res <<propmat(yq,s,a)<<" ";
           else res <<" -1 ";
           res <<N(yq,s,a)*west(yq-lq,s,a)<<" ";
           if (do_effort(s)==1) res<<N(yq,s,a)*west(yq,s,a)*propmat(yq,s,a)<<" ";
           else res <<N(yq,s,a)*west(yq-lq,s,a)*propmat(yq-lq,s,a)<<" ";
           if (multi >=1 ) res <<lsea(yqd-q_d,s,a)<<" ";  else res << "0 ";
           if (multi >=1  && s<=npr) res <<consum(yqd-q_d,s,a)<<" ";  else res << "0 ";
           res << endl;;
      }
  }

    // extend with data for "next" quarter of the last assessment year plus
    if (lqly <lq) for (s=first_VPA;s<=nsp;s++){
    y=lyModel;
    q=lqly+1;
    CALC_yq
    CALC_yqd
        for (a=faq(q);a<=la(s);a++) {
           res <<y<<" "<<q<<" " <<s<<" "<<a<<" ";
           if (multi >=1 && s>=first_VPA) {
             res <<-1<<" ";
             res <<-1<<" ";
           }
           else res << "0 0 ";
           res <<" -1";       //M
           res <<" -1";         // F
           res <<" -1 ";         // Z
           res <<N(yq,s,a)<<" ";
           res <<" -1 ";        // Nbar
           res <<" -1 ";        // C_hat
           res <<" -1 ";        // obs_C
           res <<west(yq-lq,s,a)<<" ";
           res <<" -1 ";        // weca
           res <<"-1 -1 ";        // yield CWsum
           if (do_effort(s)==1) res <<propmat(yq,s,a)<<" ";
           else res <<" -1 ";
           res <<N(yq,s,a)*west(yq-lq,s,a)<<" ";
           if (do_effort(s)==1) res<<N(yq,s,a)*west(yq-lq,s,a)*propmat(yq,s,a)<<" ";
           else res <<N(yq,s,a)*west(yq-lq,s,a)*propmat(yq-lq,s,a)<<" ";
           if (multi >=1 ) res <<lsea(yqd-q_d,s,a)<<" ";  else res << "0 ";
           if (multi >=1  && s<=npr) res <<consum(yqd-q_d,s,a)<<" ";  else res << "0 ";
           res << endl;;
      }
  }
  res.close();
  
  

  
FUNCTION void print_summary_areas();
 int s,d,a,pred,y,q,i,j,qq;
 int yq;  // year quarter index
 int yqd;  // quarter division index
 double pred_growth_q, pred_growth_a, prey_growth_q, prey_growth_a,avail;
 d3_array size(1,nsp,fa,max_a,minl,maxl);    //length or weight of species at age in the sea
 d3_array consum_local(1,nsp,fa,max_a,minl,maxl);    //consumption by age and length

 ofstream res("summary_areas.out",ios::out);
 res <<"Area Year Quarter Species.n Age M1 M2 F Z N N.bar west BIO SSB weca pred.w prey.w pred.growth.q pred.growth.y prey.growth.q prey.growth.y DeadM2 DeadAll avail.food.q avail.food.y"<<endl;

 i=lq*no_areas;
 for (y=fyModel;y<=lyModel;y++) for (q=fq;q<=lq;q++){
   dvar_matrix Nd(1,nsp,faq(q),la);    // Stock N for a particular year, quarter and area
   dvar_matrix Nd_bar(first_VPA,nsp,faq(q),la);    // Stock Nbar
   dvar_matrix M2d(first_VPA,nsp,faq(q),la_VPA);    // Stock M2
   dvar_matrix Zd(first_VPA,nsp,faq(q),la_VPA);    // Z

   dvar_matrix DeadAll(first_VPA,nsp,faq(q),la);    // dead fish N from all sources
   dvar_matrix DeadM2(first_VPA,nsp,faq(q),la);    // dead fish N from M2
 
   DeadAll=0.0;
   DeadM2=0.0;

   CALC_yq
   
   for (d=1;d<=no_areas;d++) {
     CALC_yqd
      //cout <<y<<" Q:"<<q<<" d:"<<d<<"  yq: "<<yq<<"  yqd: "<<yqd<<endl;
     if (no_areas>1) Nd=elem_prod(N_dist(yqd),N(yq));
     else Nd=N(yq);
     if (simple_ALK==0) {
       //TRACE(AV_other_food(d))
       //cout<<setprecision(3);TRACE(size_sea(yqd))
       //TRACE(size_sea_prey_w(yqd))
       //TRACE(consum(yqd))
       //cout<<setprecision(0);
       //TRACE(Nd)
       //TRACE(avail_food(yqd))
       calc_M2_simple(y,q,d,AV_other_food(d),size_sea(yqd),size_sea_prey_w(yqd),consum(yqd),Nd,avail_food(yqd),M2d);
     }
     else {
       for (s=1;s<=nsp;s++) size(s)=size_l_sea(yqd,s);
       for (pred=1;pred<=npr;pred++) consum_local(pred)=consum_l(yqd,pred);
       calc_M2_ALK(y,q,d,AV_other_food(d),size,consum_local,Nd,Nd,F(yq),M1(yq),M2d);
     }
     //cout<<setprecision(3); TRACE(M2d);
     Zd=M1(yq)+M2d+F(yq);

     for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) Nd_bar(s,a)=Nd(s,a)*(1.0-exp(-Zd(s,a)))/Zd(s,a);
     DeadM2=elem_prod(Nd_bar,M2d);
     // cout<<setprecision(0);TRACE(DeadM2)
     DeadAll=elem_prod(Nd_bar,Zd);

     for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) {
       res <<d<<" "<<y<<" "<<q<<" " <<s<<" "<<a<<" ";
       res <<M1(yq,s,a)<<" "<<M2d(s,a)<<" "<<F(yq,s,a)<<" "<<Zd(s,a)<<" "<<Nd(s,a)<<" "<<Nd_bar(s,a)<<" ";
       res<<west(yq,s,a)<<" "<<west(yq,s,a)*Nd(s,a)<<" "<< west(yq,s,a)*Nd(s,a)*propmat(yq,s,a)<<" "<<weca(yq,s,a)<<" ";
       res <<size_sea(yqd,s,a)<<" "<< size_sea_prey_w(yqd,s,a)<<" ";
       
       pred_growth_q=-9999; pred_growth_a=-9999; prey_growth_q=-9999; prey_growth_a=-9999;
       //cout<<"y:"<<y<<" q:"<<q<<" d:"<<d<<" s:"<<s<<" a:"<<a<<"  " ;
       if (q<lq) {
         pred_growth_q=(size_sea(yqd+no_areas,s,a)-size_sea(yqd,s,a));
         prey_growth_q=(size_sea_prey_w(yqd+no_areas,s,a)-size_sea_prey_w(yqd,s,a));
       }
       else if (q==lq && a<la(s) && y<lyModel) {
         pred_growth_q=(size_sea(yqd+no_areas,s,a+1)-size_sea(yqd,s,a));
         prey_growth_q=(size_sea_prey_w(yqd+no_areas,s,a+1)-size_sea_prey_w(yqd,s,a));
       }
       if (y<lyModel && a<la(s))  {
         pred_growth_a=(size_sea(yqd+i,s,a+1)-size_sea(yqd,s,a));
         prey_growth_a=(size_sea_prey_w(yqd+i,s,a+1)-size_sea_prey_w(yqd,s,a));
       }
       res <<pred_growth_q<<" "<<pred_growth_a<<" "<<" "<< prey_growth_q<<" "<<prey_growth_a;

       res<<" "<<DeadM2(s,a)<<" "<<DeadAll(s,a)<<" ";
       if (s<=npr) {
         res<<avail_food(yqd,s,a)<<" ";
         if (pred_area_present(s,d)==1  && y<lyModel){
           avail=value(avail_food(yqd,s,a)); j=0;
           for (qq=q+1;qq<=q+lq-1;qq++) {     // mean availabe food the next year
              j=j+1;
              if (qq<=lq) avail+=value(avail_food(yqd+j*no_areas,s,a));
              else if (a<la(s)) avail+=value(avail_food(yqd+j*no_areas,s,a+1));
              else avail=-9999;
           }
           if (avail>0) avail=avail/lq;
           res<<avail;
         } else res<<" 0 ";
       } else res<<" 0 0 ";
       res<<endl;
     }
   }
 }
 res.close();
 
FUNCTION void print_summary_table()
 int s,y,q,a;
 dvariable tmp;
 int yq, yq1;


 ofstream res("summary_table.out",ios::out);
 for (s=first_VPA;s<=nsp;s++){
    res<<species_names[s]<<endl<<endl;
    res<<"Year         Recruits          SSB            TSB          Yield           mean-F"<<endl;
    res<<"              (1000)        (tonnes)       (tonnes)       (tonnes)        age "<<avg_F_ages(s,1)<<"-"<<avg_F_ages(s,2)<<endl;

    for (y=fyModel;y<=lyModel;y++){

      q=recq;
      CALC_yq
      res<<y<<"  ";
      res<< setw(15) << setprecision(0) << setfixed() <<N(yq,s,fa);
      res<< setw(15) << setprecision(0) << setfixed() <<SSB(s,y);
      res<< setw(15) << setprecision(0) << setfixed() <<TSB(s,y);
      res<< setw(15) << setprecision(0) << setfixed() <<yield(s,y);
      res<< setw(15) << setprecision(3) << setfixed() <<Mean_F(s,y)<<endl;
    }
 
    if (fq==lq || lqly==lq) {  // annual assessment, calch SSB the year after the last assess. year
      tmp=0.0;
      y=lyModel+1; q=fq;
      CALC_yq
      yq1=yq;
      y=lyModel; q=fq;
      CALC_yq 
      if (any_do_effort==0) yq1=yq;
      for (a=fa+1;a<=la(s);a++){
         tmp+=N((lyModel+1-fyModel)*lq+fq,s,a)*west(yq1,s,a)*propmat(yq1,s,a)*exp(-(prop_M(s)*M(yq,s,a)+prop_F(s)*F(yq,s,a))) ;
      }
      res<<lyModel+1<<"  ";
      res<< setw(30) << setprecision(0) << setfixed() <<tmp;
    }
    res<<endl<<endl;
 }
 res.close();
 if (multi==2) calc_eaten_M2_hist();
 ofstream res2("summary_table_raw.out",ios::out);
 res2<<"Species.n Year  Rec SSB TSB SOP SOP.hat Yield Yield.hat mean.F Eaten"<<endl;


  for (s=first_VPA;s<=nsp;s++){
    for (y=fyModel;y<=lyModel;y++){
      lqLocal=(y==lyModel)?lqly:lq;
      yield_hat(s,y)=0.0;
      CWsum_hat(s,y)=0.0;
      for (q=fq;q<=lqLocal;q++){
        CALC_yq
        for (a=cfa(s);a<=la(s);a++)  if (!(a==fa && q<recq)) {
         CWsum_hat(s,y)+=weca(yq,s,a)*C_hat(yq,s,a);
         yield_hat(s,y)+=weca(yq,s,a)*C_hat(yq,s,a)*prop_landed(yq,s,a);

        }
      }
    }
  }



 for (s=first_VPA;s<=nsp;s++){
    for (y=fyModel;y<=lyModel;y++){
      q=recq;
      CALC_yq
      res2<<s<<" "<<y<<"  "<<N(yq,s,fa)<<" "<<SSB(s,y)<<" "<<
            TSB(s,y)<<" "<<CWsum(s,y)<<" "<<CWsum_hat(s,y)<<" "<<yield(s,y)<<" "<<yield_hat(s,y)<<" "<<Mean_F(s,y)<<" "<<eaten_M2(s,y)<<endl;
    }

    if (fq==lq || lqly==lq) {  // annual assessment, calch SSB the year after the last assess. year
     tmp=0.0;
    y=lyModel+1; q=fq;
      CALC_yq
      yq1=yq;
      y=lyModel; q=fq;
      CALC_yq 
      if (any_do_effort==0) yq1=yq;

     for (a=fa+1;a<=la(s);a++) tmp+=N(yq+lq,s,a)*west(yq1,s,a)*propmat(yq1,s,a)*exp(-(prop_M(s)*M(yq,s,a)+prop_F(s)*F(yq,s,a)));
     res2<<s<<" "<<lyModel+1<<"  NA "<<tmp<<" NA NA NA NA NA NA NA"<< endl;
    }
 }
 res<<endl;
 res2.close();

  
FUNCTION void print_rec_scale()
 int s;
 ofstream res("rec_scale.out",ios::out);
 res <<"species rec_scale"<<endl;
 for (s=first_VPA;s<=nsp;s++) { res <<s<<" "<<log_rec_scale(s)<<endl; }
 res.close();

FUNCTION void print_vulnerab()
 int pred,prey,i,d;
 
 ofstream res("vulnerab.out",ios::out);
 res <<"Area Pred.no Prey.no vulnerability"<<endl;
 i=0;   
 for (d=1;d<=no_areas;d++) for (pred=1;pred<=npr;pred++){
      for (prey=first_VPA;prey<=nsp;prey++) {
        if (pred_prey_comb(d,pred,prey)>0) {
        i++;
        res <<pred<<" "<<prey<<" " << setprecision(4)<<setscientific()<<vulnera(i)<<endl;
        }
      }
  }

  //********************************************************************************************* 
 
FUNCTION void print_summary_stom()
 int y,q,ll,pred,prey,pred_l,prey_l,pred_l_class,size_model,first_prey_ll;
 dvariable stomcon,stomcon_hat,obj_con,avail_food_,N_l;
 dvariable prey_l_class,prey_length_mean,pred_l_mean,suitab,residual,prey_avail,prey_avail_part,N_samples,sum_p,like_dir,like_multinom,p, Obj_N_contrib; 
 int sy,sq,sp,spl,splp;  //stomach index counters
 int observed=1; 

 double pred_w,prey_w,prey_N,pred_siz,prey_siz,N_haul,stom_used_all,samp_var;

 ofstream res("summary_stom.out",ios::out);
 res<<"Year Quarter.no SMS.area Predator.no Size.model Predator.length.class Predator.length ";
 res<<"Predator.length.mean Predator.weight Predator.size Predator.available.food Diri.p Diri.sum.p Diri.like multinom.like Prey.no  ";
 res<<"Prey.length.class Prey.length.mean Prey.weight  Prey.size Prey.number.obs Prey.number.prob Obj.N.contrib avail.prey.number.sum N.haul N.samples stomcon  stomcon.hat ";
 res<<"Residual L.N.bar Prey.avail Prey.avail.part Suit Stom.var obj.contrib stom.input stom.used.all stom.used.like stom.used.avail stom.type";
 res<<endl;
 
 
 for (sy=1;sy<=n_stl_y;sy++) { 
   y=stl_y(sy,1);
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
     q=stl_yq(sq,1);
     for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
      d=stl_yqd(sd,1);
      for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
       pred=stl_yqdp(sp,1);
       size_model=size_selection(pred);
       for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
         //if (do_number_like(pred)==1) cout<<"stl_E_Nstom(spl):"<<endl<<stl_E_Nstom(spl)<<endl;
         pred_l=stl_yqdpl(spl,4);
         pred_l_class=stl_yqdpl(spl,1);    //length group 
         if (incl_stom(d,pred,q,stl_yqdpl(spl,1),sy)>=1) stom_used_all=1; else stom_used_all=0;
         pred_l_mean=pred_length(spl); //mean length per lenght group 
         pred_w=L_W_ab(pred,1)*pow(pred_length(spl),L_W_ab(pred,2));
         pred_siz=pred_size(spl);      //Predator size (weight or length)  
         avail_food_=stl_avail_food(spl);  //available food for a predator-length
         sum_p=sum_p_Dirichlet(spl);
         like_dir=like_Dirichlet(spl);
         ll=0;
         for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
           prey=stl_yqdplp(splp,1);
           first_prey_ll=ll+1;
           like_multinom=Prey_number_like(spl,first_prey_ll);
           for(prey_l=stl_yqdplp(splp,2);prey_l<=stl_yqdplp(splp,3);prey_l++) {
             ll++;
             if (prey >0) { 
              prey_length_mean=stl_lstom(spl,ll);   // prey mean length within class 
              prey_w=stl_wstom(spl,ll);        // prey mean weight within class 
              suitab=suit(y,q,d,pred,prey,pred_siz,prey_size(spl,ll),observed); 
              N_l=stl_N_bar(spl,ll);         //N-bar at length
             } else
             {
               prey_length_mean=-1;
               prey_w=-1;
               suitab=other_suit(pred,pred_siz,y,q,d);
               N_l=-1;
             }  
             prey_N=stl_nopreystom(spl,ll);
             prey_l_class=prey_l;                  // prey length class          
             prey_avail=stl_prey_avail(spl,ll);    // Availeble food of a prey, totally by species or by size
             prey_avail_part=stl_prey_avail_part(spl,ll);    // Availeble food of a prey size

             stomcon=stl_stom(spl,ll);             // stomach content weight (relative)
             stomcon_hat=stl_E_stom(spl,ll);       // Expected stomach content weight (relative)
             if (stl_stom_use_like(spl,ll)==1) {
               if (stomach_variance==1){  // residual  log-normal distributed error
                  residual=log_stl_stom(spl,ll)-log(stl_E_stom(spl,ll)); 
               } else residual=stl_stom(spl,ll)-stl_E_stom(spl,ll);  // Normal or Dirichlet distribution
             
               //if (incl_stom(d,pred,q,stl_yqdpl(spl,1),sy)>=1) samp_var=value(calc_stom_var(pred,spl,ll));   // Calculated sample variance
               samp_var=0;
               if (stomach_variance==3) {
                  samp_var= value(stl_E_stom(spl,ll) * (1.0-stl_E_stom(spl,ll))/(sum_p+1));
                  p=sum_p*stl_E_stom(spl,ll);
               }  else p=0;

               if(stomach_variance!=3){   // normal or log-normal distribution or test
                 if (samp_var>0) obj_con=log(sqrt(samp_var))+square(stl_stom(spl,ll)-stl_E_stom(spl,ll))*0.5/samp_var; else obj_con=0;
               } else obj_con=0;            
              }  else {  // very low and not use stomach contents
                stomcon_hat=0; 
                residual=0;
                samp_var=0;
                p=0;
                samp_var=0;
              } 
              if (stl_E_Nstom(spl,ll)>0) {
                  Obj_N_contrib=log(stl_E_Nstom(spl,ll))*stl_nopreystom(spl,ll);
              } else Obj_N_contrib=0;
              prey_siz=prey_size(spl,ll);            //size (length or weight) of prey per lenght class group
              N_haul=stl_N_haul(spl,ll);             //Number of hauls per predator length,
              N_samples=stl_no_samples(spl,ll);      //Number of hauls including a specific prey or no of hauls
              
              if (size_model==0) {
                 //prey_l_class=-1;
                 //prey_siz=-1;
                 //prey_length_mean=-1;
                 //prey_w=-1;
                 //N_l=-1;
             }
             
             res<<y<<" "<<q<<" "<<d<<" "<<pred<<" "<<size_model<<" "<<pred_l_class<<" "<<pred_l<<" "<<pred_l_mean<<" ";
             res<<pred_w<<" "<<pred_siz<<" "<<avail_food_<<" "<<p<<" "<<sum_p<<" "<<like_dir<<" "<<like_multinom<<" "<<prey<<" "<<prey_l_class<<" "<<prey_length_mean<<" ";
             res<<prey_w<<" "<<prey_siz<<" "<<prey_N<<" "<<stl_E_Nstom(spl,ll)<<" "<<Obj_N_contrib<<" "<<stl_NSumprey_avail(spl,first_prey_ll)<<" "<<N_haul<<" "<<N_samples<<" "<<stomcon<<" "<<stomcon_hat<<" ";
             res<<residual<<" "<<N_l<<" "<<prey_avail<<" "<<prey_avail_part<<" "<<suitab<<" "<<samp_var<<" "<<obj_con<<" ";
             res<<stl_stom_input(spl,ll)<<" "<<stom_used_all<<" "<<stl_stom_use_like(spl,ll)<<" "<<stl_stom_use_avail(spl,ll)<<" "<<stl_stom_type(spl,ll)<< endl;
            }  
          }
        }
       }
      }
    }
  }
  res.close();


 //*********************************************************************************************

 // print residuals from Survey CPUE
FUNCTION void print_survey_residuals()
 int y,s,q,a,sp_fl,f;
 dvariable N_survey,duration;

 ofstream res("survey_residuals.out",ios::out);
 res << "# log(Survey observed CPUE) - log(expected CPUE):" ;
 sp_fl=0;
 for (s=first_VPA;s<=nsp;s++){
   res << endl << "######################################################"<<endl;
   res << "# Species: " << s <<endl;
   res << "# No. of fleets: "<<n_fleet(s);
   for (f=1;f<=n_fleet(s);f++) {
     duration=fleet_beta(s,f)-fleet_alfa(s,f);
     res << endl << "# Fleet no.: " <<f<<endl;
     res << "# First and last year: "<< first_fleet_year(s,f) << "," << last_fleet_year(s,f)<<endl;
     res << "# First and last age: "<< first_fleet_age(s,f)<< ","<< last_fleet_age(s,f);
     sp_fl++;
     q=fleet_season(s,f);
     res << endl<<"# Species, Fleet, Year, ,";
     for (a=first_fleet_age(s,f);a<=last_fleet_age(s,f);a++) res<<" ,Age_"<<a;
     res << setw(8) << setprecision(3) << setfixed();
     if (s>= first_VPA)for (y=first_fleet_year(s,f);y<=min(last_fleet_year(s,f),lyModel+1);y++){
          for (a=first_fleet_age(s,f);a<=last_fleet_age(s,f);a++) {
               if ((y<lyModel)|| (y==lyModel && q<=lqly) || 
                   (y==lyModel && q==(lqly+1) && duration==0) ||
                   //(y>lyModel && q==fq && lq==fq && duration==0)) {
                  (y>lyModel && q==fq  && duration==0)) {

                    if (first_fleet_age(s,f)==a) {
                      res << endl<< s<< "," << f << "," <<y ;   
                      res << setw(8) << setprecision(4) << setfixed();
                    }
                    if ((log_CPUE(sp_fl,y,a) < 998.0) && (!((a==fa) && (q <recq)))) {
                     //   if (duration==1) {N_survey=N_bar(s,y,q,a); }
                     //   else if (duration>0) {
                     //   N_survey=N(s,y,q,a)*exp(-Z(s,y,q,a)*fleet_alfa(s,f)); // N as start of the survey period
                     //   N_survey=N_survey*(1-exp(-Z(s,y,q,a)*duration))/(Z(s,y,q,a)*duration); // N in the mid period
                     //   } else N_survey=N(s,y,q,a);
                     //   if (log_CPUE(sp_fl,y,a)<998.0) res <<","<< log_CPUE(sp_fl,y,a)-log(N_survey)*qq_power(s,f,a)-log(qq(s,f,a));
                     if (log_CPUE(sp_fl,y,a)<998.0) res <<","<< CPUE_residuals(sp_fl,y,a);
                    }
                    else  res << ",-99.9" ;    
            } 
        }
    }
   } // fleet loop
 }  // species loop
 res <<endl;
 res.close();


 //********************************************************************************************* 
 
FUNCTION void print_part_M2();
 int y,q,pred,pred_a,prey,prey_a;

 ofstream res2("partial_m2.out",ios::out);
 res2 << "Year Quarter Predator.no Predator.age Prey.no Prey.age Part.M2"<<endl;

 for (y=fyModel;y<=lyModel;y++){
   for (q=fq;q<=lq;q++){
     calc_part_M2(y,q);
     for (pred=1;pred<=npr;pred++) {
       for (pred_a=fa;pred_a<=la(pred);pred_a++) {
         for (prey=first_VPA;prey<=nsp;prey++){
           for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
             if (part_M2(pred,pred_a,prey,prey_a) !=0.0) res2<<y<<" "<<q<<" "<<pred<<" "<<pred_a<<" "<<prey<<" "<<prey_a<<" "<<part_M2(pred,pred_a,prey,prey_a)<<endl; 
           }
         }
       }
     }
   }
 }
 res2.close();
 

 //********************************************************************************************* 

 
FUNCTION void print_ALKS()
 int s,y,q,d,a;
 int Ly,Lq,Ld,Ls,La,Ll;    
 int yq;
 ofstream res("alk_all",ios::out);
 res<<"year quarter area Species.no Age LengthGroup Length proportion proportion.adjusted "<<endl;
 for (Ly=1;Ly<=n_ALK_y;Ly++) {
   y=ALKS_y(Ly,1);
   for (Lq=ALKS_y(Ly,2);Lq<=ALKS_y(Ly,3);Lq++) {
     q=ALKS_yq(Lq,1);
     CALC_yq
     for (Ld=ALKS_yq(Lq,2);Ld<=ALKS_yq(Lq,3);Ld++) {
       d=ALKS_yqd(Ld,1);
       CALC_yqd
       for (Ls=ALKS_yqd(Ld,2);Ls<=ALKS_yqd(Ld,3);Ls++) {
         s=ALKS_yqds(Ls,1);
         for (La=ALKS_yqds(Ls,2);La<=ALKS_yqds(Ls,3);La++) {
           a=ALKS_yqdsa(La,1);
           for (Ll=ALKS_yqdsa(La,2);Ll<=ALKS_yqdsa(La,3);Ll++) {
             res<<y<<" "<<q<<" "<<d<<" "<<s<<" "<<a<<" "<<Ll<<" " <<ALKS_length(La,Ll)<<" "<<ALKS_input(La,Ll)<<" "<<ALKS_adjusted(La,Ll)<<endl;
           }
         }
       }
     }
   }
 } 
 res.close();

//********************************************************************************************* 
 

 
FUNCTION void print_ALK()
 int s,y,q,d,a;
 int Ly,Lq,Ld,Ls,La,Ll;    
 int yq;
 ofstream res("alk_stom.out",ios::out);
 res<<"year quarter area Species.no Age LengthGroup Length proportion proportion.adjusted Nbar"<<endl;
 for (Ly=1;Ly<=n_ALK_y;Ly++) {
   y=ALK_y(Ly,1);
   for (Lq=ALK_y(Ly,2);Lq<=ALK_y(Ly,3);Lq++) {
     q=ALK_yq(Lq,1);
     CALC_yq
     for (Ld=ALK_yq(Lq,2);Ld<=ALK_yq(Lq,3);Ld++) {
       d=ALK_yqd(Ld,1);
       CALC_yqd
       for (Ls=ALK_yqd(Ld,2);Ls<=ALK_yqd(Ld,3);Ls++) {
         s=ALK_yqds(Ls,1);
         for (La=ALK_yqds(Ls,2);La<=ALK_yqds(Ls,3);La++) {
           a=ALK_yqdsa(La,1);
           for (Ll=ALK_yqdsa(La,2);Ll<=ALK_yqdsa(La,3);Ll++) {
             res<<y<<" "<<q<<" "<<d<<" "<<s<<" "<<a<<" "<<Ll<<" " <<ALK_length(La,Ll)<<" "<<ALK(La,Ll)<<" "<<ALK_adjusted(La,Ll)<<" "<<N_bar_stom(yq,s,a)<<endl;
           }
         }
       }
     }
   }
 } 
 res.close();



FUNCTION void print_catch_residuals()
 int y,s,q,a;
 int yq;
 ofstream res2("catch_residuals.out",ios::out);
 res2 << "#  log(Observed catch)- log(Expected catch)";
  for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==0){
   for (q=fq;q<=lq;q++){
     res2 << endl<<"# Species,Quarter,Year";
     for (a=fa;a<=la(s);a++) res2<<", Age_"<<a;
     for (y=fyModel;y<=lyModel;y++){
       CALC_yq
       res2 << endl << s<< "," <<q<<"," <<y ;
       res2 << setw(6) << setprecision(4) << setfixed();  

       for (a=faq(q);a<=la(s);a++) {
         lqLocal=(y==lyModel)?lqly:lq;
          if ((a>=cfa(s)) && (C_hat(yq,s,a)>=0) && (y<=lyModel) && (log_obs_C(yq,s,a)> 1e-10) 
                && (q<=lqLocal) && incl_catch_season_age(s,q,a)==1  && zero_catch_y_season(s,y,q)==1) res2 <<","<< log_obs_C(yq,s,a)-log(C_hat(yq,s,a));
          else res2 <<",-99.9";
       }
     }
   }
 }
 
 for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) { 
     res2 << endl<<"# Species,Quarter,Year";
     for (a=fa;a<=la(s);a++) res2<<", Age_"<<a;
     for (y=fyModel;y<=lyModel;y++){
        res2 << endl << s<< ",9," <<y ;
       res2 << setw(6) << setprecision(4) << setfixed();  
       for (a=fa;a<=la(s);a++) {
       //cout<<"s:"<<s<<" y:"<<y<<" a:"<<a<<"C_hat_annual(y,s,a):"<<C_hat_annual(y,s,a)<<endl; 
         if ((a>=cfa(s)) && (C_hat_annual(y,s,a)>=0) && (y<=lyModel) && C_hat_annual(y,s,a)>0) res2 <<","<< log_obs_C_annual(y,s,a)-log(C_hat_annual(y,s,a));
         else res2 <<",-99.9";
       }
     }
 }
 

 // print residuals from catch and survey model
FUNCTION void print_catch_survey_residuals()
 int y,s,q,a,fleet,f,sp_fl;
 int yq;
 dvariable N_survey,duration;
 int s2_fa,s2_la,s2_group;
 d3_array catch_var(first_VPA,nsp,fq,lq,fa,max_a);
 d3_array survey_var(first_VPA,nsp,1,10,fa,max_a);          // quick and dirty soulution for running by oone species at time in single species mode
 // d3_array survey_var(first_VPA,nsp,1,n_fleet,fa,max_a);
 ofstream res("catch_survey_residuals.out",ios::out);
 fleet=-9;
 res <<"data Species.n Quarter Year fleet Age observed model residual stand.residual"<<endl;

 for (s=first_VPA;s<=nsp;s++){
    survey_var(s)=0;
   for (s2_group=1;s2_group<=n_catch_s2_group(s);s2_group++) {
     s2_fa=catch_s2_group(s,s2_group);
     if (s2_group==n_catch_s2_group(s)) s2_la=la(s);
     else s2_la=catch_s2_group(s,s2_group+1)-1;
     for (a=s2_fa;a<=s2_la;a++) {
       for (q=fq;q<=seasonal_combined_catch_s2(s);q++) {
          if (catch_s2(s,q,s2_group)>0) catch_var(s,q,a)=value(catch_s2(s,q,s2_group));
       }
     }
   }
 }

 for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==0) {
   for (q=fq;q<=lq;q++){
     for (y=fyModel;y<=lyModel;y++){
        CALC_yq
        for (a=faq(q);a<=la(s);a++) {
         lqLocal=(y==lyModel)?lqly:lq; 
         res << setfixed()<<"catch " << s<< " " <<q<<" " <<y <<' '<<fleet<<' '<<a<<' ';
         if ((a>=cfa(s)) && (C_hat(yq,s,a)>=0) && (y<=lyModel) && (q<=lqLocal) && (log_obs_C(yq,s,a)> 1e-10)
                 && (incl_catch_season_age(s,q,a)==1) && zero_catch_y_season(s,y,q)==1 )
         res <<' '<<setscientific()<<exp(log_obs_C(yq,s,a)) <<' '<<C_hat(yq,s,a)<<' '<<
               log_obs_C(yq,s,a)-log(C_hat(yq,s,a))<<" " <<(log_obs_C(yq,s,a)-log(C_hat(yq,s,a)))/sqrt(catch_var(s,q,a))<<endl;
         else res <<" -99.9 -99.9 -99.9  -99.9"<<endl;
       }
     }
   }
 }
 for (s=first_VPA;s<=nsp;s++) if (seasonal_annual_catches(s)==1) {
   for (y=fyModel;y<=lyModel;y++){
      for (a=fa;a<=la(s);a++) {
       res << setfixed()<<"catch " << s<< " " <<" 9 " <<y <<' '<<fleet<<' '<<a<<' ';
 //       if ((a>=cfa(s)) && (y<=lyModel) && sum(zero_catch_y_season(s,y))>=1 &&  (C_hat_annual(y,s,a)>0))
        if ((a>=cfa(s)) && (y<=lyModel) &&  (C_hat_annual(y,s,a)>0))
          res <<' '<<setscientific()<<exp(log_obs_C_annual(y,s,a)) <<' '<<C_hat_annual(y,s,a)<<' '<< log_obs_C_annual(y,s,a)-log(C_hat_annual(y,s,a))<<
          ' '<<-1<<endl;
       else res <<" -99.9 -99.9 -99.9 -99.9 "<<endl;;
     }
   }
 }
  
  
    
 // survey observations
 sp_fl=0;
 for (s=first_VPA;s<=nsp;s++){
 survey_var(s)=0;
   for (f=1; f<=n_fleet(s);f++) {
      sp_fl++;
      if (s>=first_VPA) for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
         s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++) {
            survey_var(s,f,a)=value(qq_s2(sp_fl,s2_group));
         }
      }
    }
 }

 sp_fl=0;
 for (s=first_VPA;s<=nsp;s++){
   for (f=1;f<=n_fleet(s);f++) {
     duration=fleet_beta(s,f)-fleet_alfa(s,f);
     sp_fl++;
     q=fleet_season(s,f);
     if (s>=first_VPA) for (y=first_fleet_year(s,f);y<=min(last_fleet_year(s,f),lyModel+1);y++){
          for (a=first_fleet_age(s,f);a<=last_fleet_age(s,f);a++) {
               if ((y<lyModel)|| (y==lyModel && q<=lqly) || 
                   (y==lyModel && q==(lqly+1) && duration==0) ||
                   (y>lyModel && q==fq  && duration==0)) {
                   res << setfixed()<<"survey " << s<< " " <<q<<" " <<y <<' '<<f<<' '<<a<<' ';
                   if ((log_CPUE(sp_fl,y,a) < 998.0) && (!((a==fa) && (q <recq)))) {
                      if (log_CPUE(sp_fl,y,a)<998.0)
                          res <<" "<<setscientific()<<exp(log_CPUE(sp_fl,y,a))<<' '<<
                          exp(-CPUE_residuals(sp_fl,y,a)+log_CPUE(sp_fl,y,a) )<<' '<<
                          CPUE_residuals(sp_fl,y,a)<<' ';
                          if (CPUE_residuals(sp_fl,y,a)>-99) res<<CPUE_residuals(sp_fl,y,a)/sqrt(survey_var(s,f,a))<<endl;
                          else res<<" -99.9"<<endl;
                      }
                   else res <<" -99.9 -99.9 -99.9 -99.9"<<endl;;
            } 
        }
    }
   } // fleet loop
 }  // species loop
 res.close();



FUNCTION void print_size_pref();
 int s;
  ofstream sf("size_pref.out",ios::out);
  sf<<"species.n size.model size.ratio size.var pref.size.ratio.correction"<<endl;
  for (s=1;s<=npr;s++) {
    sf <<s<<"  "<<size_selection(s)<<"  "<<pref_size_ratio(s)<<"  "<<var_size_ratio(s)<<" "<<pref_size_ratio_correction(s)<<endl;
  }
  sf.close();



FUNCTION void print_min_max_size_pref();
   int s,pred,prey;
   double x;
  ofstream sf("min_max_size_pref.out",ios::out);
  sf<<"## min pred/prey size ratio"<<endl<<"# ";
  for (s=first_VPA;s<=nsp;s++)  sf<<species_names[s]<<" ";
  sf<<endl;
  for (pred=1;pred<=npr;pred++) {
    for (prey=first_VPA;prey<=nsp;prey++) {
      sf<<setw(11)<<setfixed()<<setprecision(3)<<min_pred_prey_size_ratio(pred,prey)<<" ";
    }
    sf<<endl;
  }

  sf<<"## max pred/prey size ratio"<<endl<<"# ";
  for (s=first_VPA;s<=nsp;s++)  sf<<species_names[s]<<" ";
  sf<<endl;
  for (pred=1;pred<=npr;pred++) {
    for (prey=first_VPA;prey<=nsp;prey++) {
      sf<<setw(11)<<setfixed()<<setprecision(3)<<max_pred_prey_size_ratio(pred,prey)<<" ";
    }
    sf<<endl;
  }
  
  sf<<endl<<"## log of min pred/prey size ratio"<<endl<<"# ";
  for (s=first_VPA;s<=nsp;s++)  sf<<species_names[s]<<" ";
  sf<<endl;
  for (pred=1;pred<=npr;pred++) {
    for (prey=first_VPA;prey<=nsp;prey++) {
      x=min_pred_prey_size_ratio(pred,prey);
      if (x==1000) x=-9999; else x=log(x);
      sf<<setw(11)<<setfixed()<<setprecision(3)<<x<<" ";
    }
    sf<<"  # "<<species_names[pred]<<endl;
  }
  sf<<endl<<"## log of max pred/prey size ratio"<<endl<<"# ";
  for (s=first_VPA;s<=nsp;s++)  sf<<species_names[s]<<" ";
  sf<<endl;
  for (pred=1;pred<=npr;pred++) {
    for (prey=first_VPA;prey<=nsp;prey++) {
      x=max_pred_prey_size_ratio(pred,prey);
      if (x<0) x=-9999; else x=log(x);
      sf<<setw(11)<<setfixed()<<setprecision(3)<<x<<" ";
    }
    sf<<"  # "<<species_names[pred]<<endl;
  }

      
  sf.close();
 
FUNCTION void print_vars(adstring txt,adstring intype)
 int y,s,q,a;
 int yq;
 ofstream details("details.out",ios::app);
 details << endl <<txt<<endl;
 for (s=first_VPA;s<=nsp;s++){
   details <<"# Species: "<<species_names[s]<<"  ################################################################" <<endl;
   for (y=fyModel;y<=lyModel;y++){
     details <<"# Year:"<<y<<endl;
     details <<"# age"; 
     for (a=fa;a<=la(s);a++){
        if (a==fa) details << setw(5) << a;
        else details << setw(11) << a;
     }
     details << " ";
     for (a=la(s)+1;a<=max_a;a++){
       details << setw(11) << -a;
     }

     details << endl;
     lqLocal=(y==lyModel)?lqly:lq;
     for (q=fq;q<=lqLocal;q++){
       CALC_yq
       for (a=fa;a<=la(s);a++) {
        if (!(a==fa && q<recq)) { 
         if      (strcmp(intype,"obs_C") == 0) details << setw(10) << setprecision(0) << setfixed() <<obs_C(yq,s,a)<< " ";
         else if (strcmp(intype,"log_obs_C") ==0)      details << setw(10) << setprecision(0) << setfixed() <<exp(log_obs_C(yq,s,a))<< " ";
         else if (strcmp(intype,"N") ==0)      details << setw(10) << setprecision(0) << setfixed() <<N(yq,s,a)<< " ";
         else if (strcmp(intype,"M1") == 0)    details << setw(10) << setprecision(4) << setfixed() <<M1(yq,s,a)<< " ";
         else if (strcmp(intype,"M2") == 0)    details << setw(10) << setprecision(4) << setfixed() <<M2(yq,s,a)<< " ";
         else if (strcmp(intype,"M") == 0)     details << setw(10) << setprecision(4) << setfixed() <<M(yq,s,a)<< " ";
         else if (strcmp(intype,"F") == 0)     details << setw(10) << setprecision(4) << setfixed() <<F(yq,s,a)<< " ";
         else if (strcmp(intype,"Z") == 0)     details << setw(10) << setprecision(4) << setfixed() <<Z(yq,s,a)<< " ";
         else details <<"ERROR"<<endl;
        } else   details << setw(10) << setprecision(4) << setfixed() <<0<< " ";
       }
       for (a=la(s)+1;a<=max_a;a++) details << setw(11) << 0;
      details <<endl;
     }
   } 
 } 


FUNCTION void print_vars_ICES(adstring txt,adstring intype)
 int y,s,q,a,wi,ls,max_g,g,x,yl,no;
 dvariable tmp;

 wi=10;                       //width of column
 ls=80;                       // line size
 yl=ls/wi;                    // year per line 
 max_g=(lyModel-fyModel)/yl;     // no of sub-tables
 if ((lyModel-fyModel)>(max_g*yl)) max_g++; 

 ofstream det("details_ices.out",ios::app);

 det<< endl <<endl<< txt<< endl;
 for (s=first_VPA;s<=nsp;s++){
   det <<endl<<"Species: "<<species_names[s]<<endl;
   for (g=1;g<=max_g;g++) {
     det <<endl<<endl<<endl<<"Age";
     for (x=1;x<=wi-3;x++) det << " ";
     for (y=fyModel+(g-1)*yl;y<min(fyModel+g*yl,lyModel+1);y++) det <<setw(wi)<< y;
     det << endl; 

     for (a=fa;a<=la(s);a++){
       det << setw(wi) << a;
       for (y=fyModel+(g-1)*yl;y<min(fyModel+g*yl,lyModel+1);y++) {
         tmp=0.0;
         if (strcmp(intype,"obs_C") == 0) {
            for(q=fq;q<=lq;q++) if (!(a==fa && q<recq)) tmp+=obs_C((y-fyModel)*lq+q,s,a);
            det << setw(wi) << setprecision(0) << setfixed()<< tmp;
         }
         else if (strcmp(intype,"N") == 0) { 
            q=fq;
            CALC_yq
            if (!(a==fa && q<recq)) tmp+=N(yq,s,a);


            det << setw(wi) << setprecision(0) << setfixed()<< tmp;
         }
         else if (strcmp(intype,"M2") == 0){
            for(q=fq;q<=lq;q++) if (!(a==fa && q<recq)) {
               CALC_yq 
               tmp+=M2(yq,s,a);
            }


            det << setw(wi) << setprecision(4) << setfixed()<< tmp;
         }
         else if (strcmp(intype,"F") == 0) {
            for(q=fq;q<=lq;q++) if (!(a==fa && q<recq)) tmp+=F((y-fyModel)*lq+q,s,a);
            det << setw(wi) << setprecision(4) << setfixed()<< tmp;
         }
         else det <<"ERROR"<<endl;
       }
       det <<endl;
     }
     if (strcmp(intype,"N") == 0) { 
        det <<endl<<"TSB";
        for (x=1;x<=wi-3;x++) det << " ";
        for (y=fyModel+(g-1)*yl;y<min(fyModel+g*yl,lyModel+1);y++) det<<setw(wi)<<setprecision(0)<<setfixed()<<TSB(s,y);
        det <<endl<<"SSB";
        for (x=1;x<=wi-3;x++) det << " ";
        for (y=fyModel+(g-1)*yl;y<min(fyModel+g*yl,lyModel+1);y++) det<<setw(wi)<<setprecision(0)<<setfixed()<<SSB(s,y);
     }
     else if (strcmp(intype,"F") == 0) {
       det <<endl<<"Avg. F"<<setw(2)<< avg_F_ages(s,1)<< "-"<<avg_F_ages(s,2);
       for (y=fyModel+(g-1)*yl;y<min(fyModel+g*yl,lyModel+1);y++) {
          no=0; tmp=0.0;
          for (q=fq;q<=lq;q++){

            for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) if (!(a==fa && q<recq)) {
              no++;
              tmp+=F((y-fyModel)*lq+q,s,a);
            }
          }
          det<< setw(wi)<<setprecision(3)<<setfixed()<<tmp/no*(lq-fq+1);
       }
     }
     else if (strcmp(intype,"obs_C") == 0) {
       det <<endl<<"Yield";
       for (x=1;x<=wi-5;x++) det << " ";
       for (y=fyModel+(g-1)*yl;y<min(fyModel+g*yl,lyModel+1);y++) det<<setw(wi)<<setprecision(0)<<setfixed()<<yield(s,y);
     }

     else det <<endl;
   }
 } 

FUNCTION void Print_OP_seed()
 ofstream op("op_seed.in",ios::out);
 op<<3<<endl;
 op.close();

FUNCTION void print_Config_parms()
 int i,s,d,pred,prey;
 d3_array vulop(1,no_areas,1,npr,first_VPA,nsp);

 
 ofstream op("op_config.dat",ios::out);
 op<<setprecision(0);

 op<<"########################################"<<endl<<
     "# Single/Multispecies mode (option VPA.mode)"<<endl<<
     "# 0=single species mode"<<endl<<
     "# 2=multi species mode, Z=F+M1+M2"<<endl<<
     multi<<endl;
 op<<"########################################"<<endl;
 op<<"##  N in the beginning of the period or N bar for calculation of M2 (option use.Nbar)"<<endl;
 op<<"#  0=use N in the beginning of the time step (default)"<<endl;
 op<<"#  1=use N bar"<<endl;
 op<<use_Nbar<<endl;
 
 op<<"########################################"<<endl<<
     "# Area explicit options for M2 and F (option area.FM)"<<endl<<
     "# 1=Both F and M2 are calculated for the entire stock area"<<endl<<
     "# 2=M2 is calculated by area, but F (or C) is assumed global"<<endl<<
     "# 3=Both M2 and F (or C) are assumed by area"<<endl;
     if (no_areas==1) op<<1<<endl; else op<<3<<endl;
     
 op<<"######"<<endl<<no_areas<<"  # Number of areas"<<endl;
 op<<"######"<<endl<<nsp<<"  # number of species"<<endl;
 op<<"######"<<endl<<fa<<"  # first age, all species"<<endl;
 op<<"######"<<endl<<max_a<<"  # max age, all species"<<endl;
 op<<"######"<<endl<<lq<<"  # no of seasons"<<endl;
 op<<"######"<<endl<<recq<<"  # recruitment seasons"<<endl;

 op<<"########################################"<<endl<<
     "## various information by species"<<endl<<
     "# 1. last age"<<endl<<
     "# 2. first age where catch data are used (else F=0 assumed)"<<endl<<
     "# 3. plus group, 0=no plus group, 1=plus group"<<endl<<
     "# 4. predator species, 0=no, 1=VPA predator, 2=Other predator"<<endl<<
     "# 5. prey species, 0=no, 1=yes"<<endl<<
     "##"<<endl;
 for (s=1;s<=nsp;s++) {
   op<<la(s)<<" ";
   if (s<first_VPA) op<<"0 "; else op<<cfa(s)<<" ";
   op<<nplus(s)<<" "<<is_pred(s)<<" ";
  if (s<first_VPA) op<<" 0 "; else op<<is_prey(s);
   op<<" # "<<species_names(s)<<endl;
 }

  op<<"########################################"<<endl<<
      "## first and last age in calculation of average F by species"<<endl;
   for (s=first_VPA;s<=nsp;s++) op<<avg_F_ages(s,1)<<" "<<avg_F_ages(s,2)<<" # "<<species_names(s)<<endl;


 op<<setw(12) << setprecision(6)<< setscientific();
 op<<"################################################"<<endl;
 op<<"# Stock recruitment parameters"<<endl;
 op <<"#model alfa  beta std info1 info2"<<endl;
 for (s=first_VPA;s<=nsp;s++){
    op<<SSB_Rec_model(s)<<" ";
   if (SSB_Rec_model(s)==1 || SSB_Rec_model(s)==2 || SSB_Rec_model(s)==51 || SSB_Rec_model(s)==52) op<<SSB_R_alfa(s)<<" " <<SSB_R_beta(s)/SSB_R_beta_cor(s);
   else if (SSB_Rec_model(s)==3)  op<<SSB_R_alfa(s)<<" " <<0.0;
   else if ((SSB_Rec_model(s)==4) || (SSB_Rec_model(s)==5)) op<<SSB_R_alfa(s)<<" " <<SSB_R_beta(s);
   else if (SSB_Rec_model(s)==100) op<<SSB_R_alfa(s)<<" " <<SSB_Rec_hockey_breakpoint(s);
   op<<"  "<<sqrt(SSB_R_s2(s));
   if (SSB_Rec_model(s)==51 || SSB_Rec_model(s)==52) op<<" "<<RecTempVar(s); else op<<" "<<Rec_add_inf(s,1);
   op<<" "<<Rec_add_inf(s,2)<<" # "<<species_names(s)<<endl;

 }
 
 if (multi==2) {
   op<<"##"<<endl;
   op<<"############### Multi species ###################"<<endl;
   op<<simple_ALK<<"  # Usage of age-length-keys for calc of M2 (option simple.ALK))"<<endl;
   op<<consum_op<<"   # Usage of food-rations from input values (option=0) or from size and regression parameters (option=1)(option consum)"<<endl;
   op<<"## Variance of size selection (option size.selection) by predator"<<endl<<size_selection<<endl;
   op<<"## Max prey size/ pred size factor for inclusion in M2 calc by predator"<<setfixed()<<setprecision(3)<<endl<<prey_pred_size_fac<<endl;
   op<<"################################################"<<endl;
   op<<"# Predator prey vulnerability"<<endl;
   i=1;
   for (d=1;d<=no_areas;d++) {
    vulop(d)=0;
    for (pred=1;pred<=npr;pred++) {
      for (prey=first_VPA;prey<=nsp;prey++) {
      if (pred_prey_comb(d,pred,prey)>0) {
         vulop(d,pred,prey)=value(vulnera(i));
         i++;
   }}}}
   for (d=1;d<=no_areas;d++) op<<"# Area "<<d<<endl<< setprecision(5)<<setfixed()<<vulop(d)<<endl;

   op<<"################################################"<<endl;
   op<<"# Seasonal overlap"<<endl;
   for (d=1;d<=no_areas;d++) op<<"# Area "<<d<<endl<< setprecision(5)<<setfixed()<<season_overlap(d)<<endl;

   //op<<"################################################"<<endl;
   //op<<"# Growth model applied"<<endl<<growth_model<<endl;  //  Growth model 0=no growth; food; 2=density dependent

   op<<"################################################"<<endl;
   op<<"# Size selection parameters"<<endl;
   op<<"# size model"<<endl<<setprecision(5)<<setfixed()<<size_selection<<endl;
   op<<"# prefered size"<<endl<<setprecision(5)<<setfixed()<<pref_size_ratio<<endl;
   op<<"# variance of prefered size"<<endl<<var_size_ratio<<endl;
   op<<"# adjustment of prefered size"<<endl<<pref_size_ratio_correction<<endl;
   op<<"# prey_size_adjustment"<<endl<<prey_size_adjustment<<endl;
   op<<"################################################"<<endl;
   op<<"# Other food size dependency"<<endl<< stl_other_suit_slope<<endl;
   op<<"################################################"<<endl;
   op<<"# minimum predator prey size ratio"<<endl;
   op<<min_pred_prey_size_ratio<<endl;
   op<<"# maximum predator prey size ratio"<<endl;
   op<<max_pred_prey_size_ratio<<endl;

   op<<"################################################"<<endl;
   op<<"# Constraint uniform size selection"<<endl;
   op<<"# Intercept, minimum"<<endl;
   op<<size_range_a_b(1)<<endl;
   op<<"# Slope, minimum"<<endl;
   op<<size_range_a_b(2)<<endl;
   op<<"# Intercept, maximum"<<endl;
   op<<size_range_a_b(3)<<endl;
   op<<"# Slope, maximum"<<endl;
   op<<size_range_a_b(4)<<endl;

   op<<"################################################"<<endl;
   op<<"# Predator presence (0=absent, 1=present) in an area"<<endl;
   for (s=1;s<=npr;s++) op<<pred_area_present(s)<<" # "<<species_names[s]<<endl;

   op<<"################################################"<<endl;
   op<<"# Prey eaten by predator (0=no, >=1 means yes) in an area"<<endl;
   for (d=1;d<=no_areas;d++) op<<"# Area "<<d<<setprecision(0)<<endl<<pred_prey_comb(d)<<endl;

   op<<"################################################"<<endl;
   op<<"# Other food by area and predator"<<endl;
   op<<AV_other_food<<endl;

   op<<"################################################"<<endl;
   op<<"# Size of predator for constant available other food"<<endl;
   op<<setprecision(6)<<AV_other_food_size<<endl;
 }
 op.close();


FUNCTION void out_OP_growth()
 if (OP_do_growth_all==1) {
   ofstream op ("op_growth_type1.in",ios::out);
   op<<"#growth_type1.in, Regression parameters:"<<endl<<setprecision(3)<<endl<<growth_type1_regr<<endl;
   op<<"#growth_type1.in, in year increment ratio:"<<endl<<setprecision(3)<<endl<<growth_type1_in_year<<endl;
   op<<"#growth_type1.in, ratio between weca and west"<<endl<<setprecision(2)<<endl<<growth_ratio_weca_west<<endl;
   op<<"#Growth, minimum weight at age Q1"<<endl<<setprecision(2)<<endl<<growth_min_w<<endl;
   op<<"#Growth, maximum weight at age Q1"<<endl<<setprecision(2)<<endl<<growth_max_w<<endl;
   op.close();
  }


FUNCTION void out_OP(char fname[],char text1[],int fsp,int lsp, int precis,d3_array v, imatrix yrange, int nd)
 int s,y,q,d;
 char txt[40];
 char txt2[40];
 ivector yq2(fsp,lsp);

 strcpy(txt,fname);
 strcpy(txt2,"op_");
 strcat(txt2,txt);
 strcat(txt2,".in");

 for (s=fsp;s<=lsp;s++) {
   y=yrange(1,s);
   q=fq;
   CALC_yq
   yq2(s)=yq;  
   
   for (y=yrange(1,s)+1;y<=yrange(2,s);y++) for (q=fq;q<=lq;q++) {
     CALC_yq
     v(yq2(s)+q-1,s)+=v(yq,s);   // sum
   }
   for (q=fq;q<=lq;q++) for (a=faq(q);a<=la(s);a++) v(yq2(s)+q-1,s,a)=v(yq2(s)+q-1,s,a)/(yrange(2,s)-yrange(1,s)+1);
 }
 
 ofstream op (txt2,ios::out);
 op<<"################################################"<<endl;
 op<<"# "<<text1<<setw(12) << setprecision(precis)<< setfixed(); op<<endl;

 for (q=fq;q<=lq;q++) {
   op<<"##### Quarter:"<<q<<endl;
   for (d=1;d<=nd;d++) {
     if (nd>1) op<<"# Area:"<<d<<endl;
     for (s=fsp;s<=lsp;s++){
       if (q<recq && lq>1) op<<" 0 ";
       op<<v(yq2(s)+q-1,s);
       for (a=la(s)+1;a<=max_a;a++) op<<" 0 ";
       op<<" # "<<species_names(s)<<endl;
     }
   }
 }
 op.close();
 if (OP_output==1) cout <<"Operating data file: "<<txt2<<" is done"<<endl;



FUNCTION void print_Operating_input()
 int i,s,d,a,pred,y,q,yy,first;
 char ftext[10];
 char text[40];

 strcpy(ftext,"wsea");  strcpy(text,"Mean weight in the sea");
 out_OP(ftext,text,1,nsp, 4,west,OP_wsea,1);

 strcpy(ftext,"wcatch");  strcpy(text,"Mean weight in the catch");
 out_OP(ftext,text, first_VPA,nsp, 4,weca,OP_weca,no_areas);

 strcpy(ftext,"m");  strcpy(text,"Natural mortality (M)");
 out_OP(ftext,text, first_VPA,nsp, 4,M,OP_M,1);
 
 if (multi>=2) {      
   for (s=first_VPA;s<=nsp;s++) {
     for (y=OP_M(1,s);y<=OP_M(2,s);y++) for (q=fq;q<=lq;q++) {
       CALC_yq
       if (is_prey(s)) M1M2(yq,s)=value(M1(yq,s)+M2(yq,s));
       else M1M2(yq,s)=M(yq,s);
     }
   }
   strcpy(ftext,"m1m2");  strcpy(text,"Multispecies Natural mortality (M1+M2)");
   out_OP(ftext,text, first_VPA,nsp, 4,M1M2,OP_M,no_areas);
 }

 strcpy(ftext,"f");  strcpy(text,"F at age");
 out_OP(ftext,text, first_VPA,nsp, 4,value(F),OP_F,no_areas);

 strcpy(ftext,"propmat");  strcpy(text,"Proportion mature");
 out_OP(ftext,text, first_VPA,nsp, 2,propmat,OP_propmat,1);

 if (multi==2) {
   strcpy(ftext,"m1");  strcpy(text,"Residual natural mortality (M1)");
   out_OP(ftext,text,first_VPA,nsp, 3,M1,OP_M,no_areas);
 }
 
 //if (calc_discard==1) {
   strcpy(ftext,"prop_landed");  strcpy(text,"Proportion of the catch landed");
   out_OP(ftext,text, first_VPA,nsp, 4,prop_landed,OP_prop_landed,no_areas);
 //}
 if (multi==2) {
   out_OP_growth();
    
   if (nOthPred >0) {
     strcpy(ftext,"other_n");  strcpy(text,"Stock numbers of other predator");
     out_OP(ftext,text, 1,nOthPred, 1,other_pred_N,OP_other_N,no_areas);
   }
 }
 
 if (lq==1) yy=0;
 else yy=1;

 ofstream op1("op_n.in",ios::out);
 op1<<"################################################"<<endl;
 op1<<"# Stock numbers at age 1. January, (except for recruits)"<<setw(12) << setprecision(2)<< setfixed()<<endl;
 y=lyModel+1;
 q=1;
 CALC_yq
 for (s=first_VPA;s<=nsp;s++){
   op1<<SSB_recruit_last(s)<<" ";
   for (a=fa+1;a<=la(s);a++) op1<<N(yq,s,a)<<" ";
   for (a=la(s)+1;a<=max_a;a++) op1<<" 0 ";
   op1<<" # "<<species_names(s)<<endl;
 }
 op1.close();
 if (test_output>4) cout <<"Operating data file: op_n.in is done"<<endl;

 if (no_areas>1) {
   ofstream op2("op_stock_distribution.in",ios::out);
   op2<<"################################################"<<endl;
   op2<<"# Stock distribution (proportion by area)"<<setw(12) << setprecision(3)<< setfixed()<<endl;
   for (s=first_VPA;s<=nsp;s++) if (OP_stock_dist(2,s) > OP_stock_dist(1,s)) {
     y=OP_stock_dist(1,s);
     d=1;q=fq;
     CALC_yqd
     first=yqd;

     for (y=OP_stock_dist(1,s)+1; y<=OP_stock_dist(2,s);y++) {
      for (q=fq;q<=lq;q++) {
        for (d=1;d<=no_areas;d++)  {
          CALC_yqd
          N_dist(first+(q-fq)*no_areas+d-1,s)+=N_dist(yqd,s);
        }
      }
     }
     for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++) N_dist(first+(q-fq)*no_areas+d-1,s)/=(OP_stock_dist(2,s)-OP_stock_dist(1,s)+1);
   }

   for (q=fq;q<=lq;q++) {
    op2<<"##### Quarter:"<<q<<endl;
    for (d=1;d<=no_areas;d++)  {
     op2<<"## "<<"area:"<<d<<endl;
     for (s=first_VPA;s<=nsp;s++) {
       y=OP_stock_dist(1,s);
       CALC_yqd
       if (q<recq  && yy==1) op2<<" 0 ";
       op2<<N_dist(yqd,s);
       for (a=la(s)+1;a<=max_a;a++) op2<<" 0 ";
       op2<<" # "<<species_names[s]<<endl;
     }
    }
   }
   op2.close();
   if (test_output>4) cout <<"Operating data file: op_stock_distribution.in is done"<<endl;
 }
 
 ofstream op2a("op_c.in",ios::out);
 op2a<<"################################################"<<endl;
 op2a<<"# Catch at age by area (for testing only))"<<setw(12) << setprecision(3)<< setfixed()<<endl;
 y=lyModel;
 for (q=fq;q<=lq;q++) {
   CALC_yq;
   op2a<<"##### Quarter:"<<q<<endl;
   for (d=1;d<=no_areas;d++)  {
     CALC_yqd;
     op2a<<"# "<<"area:"<<d<<endl;
     for (s=first_VPA;s<=nsp;s++) {
       if (q<recq  && yy==1) op2a<<" 0 ";
       if (no_areas>1) { for (a=faq(q);a<=la(s);a++) op2a<<value(C_hat(yq,s,a))*N_dist(yqd,s,a)<<" ";}
       else for (a=faq(q);a<=la(s);a++) op2a<<value(C_hat(yq,s,a))<<" ";
       for (a=la(s)+1;a<=max_a;a++) op2a<<" 0 ";
       op2a<<" # "<<species_names[s]<<endl;;
     }
   }
 }
 op2a.close();
 if (test_output>4) cout <<"Operating data file: op_c.in is done"<<endl;

 // calc average F based un sum of seasonal F
 dvar_vector tmp(first_VPA,nsp);
  
 y=lyModel;
 

 tmp=0; // Calc F as sum of seasonal F's
 for (s=first_VPA;s<=nsp;s++) {
   for (q=fq;q<=lq;q++){
     CALC_yq 
     for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) {
       tmp(s)+=value(F(yq,s,a));
     }
   } 
   tmp(s)=tmp(s)/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
  }


 ofstream op77("op_exploitation.in",ios::out);
 op77<<"################################################"<<endl;
 op77<<"# Exploitation at age "<<setw(12) << setprecision(4)<< setfixed()<<endl;
 y=lyModel;
 for (q=fq;q<=lq;q++) {
   CALC_yq
   op77<<"##### Quarter:"<<q<<endl;
   for (d=1;d<=no_areas;d++)  {
    op77<<"## "<<"area:"<<d<<endl;
    for (s=first_VPA;s<=nsp;s++){
     if (q<recq && yy==1) op77<<" 0 ";
     op77<<F(yq,s)/tmp(s); 
     for (a=la(s)+1;a<=max_a;a++) op77<<" 0 ";
     op77<<" # "<<species_names(s)<<endl;
   }
  }
 }
 op77.close();
 if (test_output>4) cout <<"Operating data file: op_exploitation.in is done"<<endl;

 if (consum_op==0 && multi==2) {
   ofstream op8("op_consum.in",ios::out);
   op8<<"################################################"<<endl;
   op8<<"# Food ration (consumption) per individual "<<setw(9) << setprecision(3)<< setfixed()<<endl;
   for (s=1;s<=npr;s++) if (OP_consum(2,s) > OP_consum(1,s)) {
     y=OP_consum(1,s);
     d=1;q=fq;
     CALC_yqd
     first=yqd;
     for (y=OP_consum(1,s)+1; y<=OP_consum(2,s);y++) {
      for (q=fq;q<=lq;q++) {
        for (d=1;d<=no_areas;d++)  {
          CALC_yqd
          consum(first+(q-fq)*no_areas+d-1,s)+=consum(yqd,s);
        }
      }
     }
     for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++) consum(first+(q-fq)*no_areas+d-1,s)/=(OP_consum(2,s)-OP_consum(1,s)+1);
   }
   for (q=fq;q<=lq;q++) {
    op8<<"##### Quarter:"<<q<<endl;
    for (d=1;d<=no_areas;d++)  {
     op8<<"## "<<"area:"<<d<<endl;
     for (s=1;s<=npr;s++) {
       y=OP_consum(1,s);
       CALC_yqd
       if (q<recq  && yy==1) op8<<" 0 ";
       op8<<consum(yqd,s);
       for (a=la(s)+1;a<=max_a;a++) op8<<" 0 ";
       op8<<" # "<<species_names[s]<<endl;
     }
    }
   }
   op8.close();
   if (test_output>4) cout <<"Operating data file: op_consum.in is done"<<endl;
 }

 if (consum_op==1 && multi==2) {
   ofstream op8ab("op_consum_ab.in",ios::out);
   op8ab<<"################################################"<<endl;
   op8ab<<"# parameter a and b for: R=a*w^b"<<endl<<"#     a        b"<<endl;
   for (q=fq;q<=lq;q++) {
     op8ab<<" ##### Quarter:"<<q<<endl;
     for (d=1;d<=no_areas;d++)  {
       op8ab<<"# "<<"area:"<<d<<endl;
       for (s=1;s<=npr;s++) {
         op8ab<<setw(7) << setprecision(4)<< setfixed()<<consum_ab(d,s,q)<<" # "<<species_names[s]<<endl;
       }
     }
   }
   op8ab.close();
   if (test_output>4) cout <<"Operating data file: op_consum_ab.in is done"<<endl;
 }

 if (multi==2) {
   ofstream op9("op_size.in",ios::out);
   op9<<"################################################"<<endl;
   op9<<"# Size in the sea, for use in size selction model "<<setw(12) << setprecision(4)<< setfixed()<<endl;
   y=lyModel;
   for (q=fq;q<=lq;q++) {
    op9<<"##### Quarter:"<<q<<endl;
    for (d=1;d<=no_areas;d++)  {
     CALC_yqd;
     op9<<"# "<<"area:"<<d<<endl;
     for (s=1;s<=nsp;s++) {
       if (q<recq && yy==1) op9<<" 0 ";
       op9<<size_sea(yqd,s);
       for (a=la(s)+1;a<=max_a;a++) op9<<" 0 ";
       op9<<" # "<<species_names[s]<<endl;
     }
    }
   }
   op9<<"################################################"<<endl;
   op9<<"# Mean weight of eaten preys "<<setw(12) << setprecision(4)<< setfixed()<<endl;
   y=lyModel;
   for (q=fq;q<=lq;q++) {
    op9<<"##### Quarter:"<<q<<endl;
    for (d=1;d<=no_areas;d++)  {
     CALC_yqd;
     op9<<"# "<<"area:"<<d<<endl;
     for (s=first_VPA;s<=nsp;s++) {
       if (q<recq && yy==1) op9<<" 0 ";
       op9<<size_sea_prey_w(yqd,s);
       for (a=la(s)+1;a<=max_a;a++) op9<<" 0 ";
       op9<<" # "<<species_names[s]<<endl;
     }
    }
   }
   op9<<"################################################"<<endl;
   op9<<"# Length at age "<<setw(12) << setprecision(4)<< setfixed()<<endl;
   y=lyModel;
   for (q=fq;q<=lq;q++) {
    op9<<"##### Quarter:"<<q<<endl;
    for (d=1;d<=no_areas;d++)  {
     CALC_yqd;
     op9<<"# "<<"area:"<<d<<endl;
     for (s=1;s<=nsp;s++) {
       if (q<recq && yy==1) op9<<" 0 ";
       op9<<lsea(yqd,s);
       for (a=la(s)+1;a<=max_a;a++) op9<<" 0 ";
       op9<<" # "<<species_names[s]<<endl;
     }
    }
   }
   op9.close();
   if (test_output>4) cout <<"Operating data file: op_size.in is done"<<endl;
 }  
   
  ofstream op10("op_reference_points.in",ios::out);
  op10<<"################################################"<<endl;
  op10<<"# Reference points (Flim, Fpa, Blim and Bpa) by species "<<setw(12) <<endl;
  for (s=first_VPA;s<=nsp;s++){
    op10<< setprecision(4)<< setfixed()<<reference_points(s,1)<<" "<<reference_points(s,2)<<" "<<setprecision(0)<<reference_points(s,3)<<" "<<reference_points(s,4)<<" # "<<species_names(s)<<endl;
  }
  op10.close();
  if (test_output>4) cout <<"Operating data file: op_reference_points.in is done"<<endl;
  
  ofstream op10b("op_length_weight_relations.in",ios::out);
  op10b<<"# length weight relation W=a * Power(L,b),\n # L in mm, W in kg"<<endl;
  for (s=1;s<=nsp;s++){
      op10b<<"# "<<species_names(s)<<endl;
      op10b<< L_W_ab(s)<<endl;
  }
  op10b.close();
  if (test_output>4) cout <<"Operating data file: op_length_weight_relations.in is done"<<endl;
  
  if (multi!=1) {
	  save_SSB_Rec_residuals=1;
	  evaluate_SSB_recruitment_contributions();  // to get the residuals
	  ofstream op11("op_ssb_rec_residuals.in",ios::out);
	  op11<<"################################################"<<endl;
	  op11<<"# Stock recruitment residuals ##"<<endl<<"#    Number of residuals by species"<<endl;
	  for (s=first_VPA;s<=nsp;s++){
	    op11<<SSB_Rec_nobs(s)<<" # "<<species_names(s)<<endl;
	  }
	  op11<<"# residuals by species"<<endl;
	  op11<<setw(8) << setprecision(4)<< setfixed();
	  for (s=first_VPA;s<=nsp;s++){
	    for (i=1;i<=SSB_Rec_nobs(s);i++){
	      op11<<SSB_Rec_residuals(s,i)<<" ";
	    }
	    op11<<" # "<<species_names(s)<<endl;
	   }
	  op11.close();
	  if (test_output>4) cout <<"Operating data file: op_ssb_rec_residuals.in is done"<<endl;
  }

	   
FUNCTION void print_SSB_R()
 int s;
 ofstream SR("ssb_r.out",ios::out);
 SR<<setw(12) << setprecision(6)<< setscientific();
 SR <<"Species.n model alfa  beta std  info1 info2"<<endl;
 for (s=first_VPA;s<=nsp;s++){
   SR << s << " " <<SSB_Rec_model(s)<<" ";
   if (SSB_Rec_model(s)<=2 || SSB_Rec_model(s)==51 || SSB_Rec_model(s)==52)      SR<<SSB_R_alfa(s)<<" " <<SSB_R_beta(s)/SSB_R_beta_cor(s);
   else if (SSB_Rec_model(s)==3)  SR<<SSB_R_alfa(s)<<" " <<0.0;
   else if ((SSB_Rec_model(s)==4) || (SSB_Rec_model(s)==5)) SR<<SSB_R_alfa(s)<<" " <<SSB_R_beta(s);
   else if (SSB_Rec_model(s)==100) SR<<SSB_R_alfa(s)<<" " <<SSB_Rec_hockey_breakpoint(s);
   SR<<"  "<<sqrt(SSB_R_s2(s));
   if (SSB_Rec_model(s)==51 || SSB_Rec_model(s)==52) SR<<" "<<RecTempVar(s); else SR<<" "<<Rec_add_inf(s,1);
   SR<<" "<<Rec_add_inf(s,2)<<endl;
 }
 SR.close();


  
FUNCTION void print_F_status_quo()
 int s,y,q;
 y=lyModel;
 ofstream fsq("f_status_quo.out",ios::out);
 fsq<<setw(12) << setprecision(6)<< setfixed();
 fsq <<"# F status quo"<<endl;

 for (s=first_VPA;s<=nsp;s++){
   fsq<<"# "<<species_names[s]<<endl;
   for (q=1;q<=lq;q++) {
     CALC_yq
     fsq<< F(yq,s);
     fsq<<endl;
   }
 }
 fsq.close();

FUNCTION void print_M1M2sum()
 int y,s,q,a;
 int yq;

 ofstream mm("natmorm1m2.out",ios::out);

 for (s=first_VPA;s<=nsp;s++){
   mm <<"# Species:"<<s<<"  ################################################################" <<endl;
   for (y=fyModel;y<=lyModel;y++){
     mm <<"# Year:"<<y<<endl;
     mm <<"# age"; 
     for (a=fa;a<=la(s);a++){
        if (a==fa) mm << setw(5) << a;
        else mm << setw(11) << a;
     }
     mm << " ";
     for (a=la(s)+1;a<=max_a;a++){
       mm << setw(11) << -a;
     }

     mm << endl;
     for (q=fq;q<=lq;q++){
       CALC_yq
       if (faq(q)>fa)  mm << setw(10) << setprecision(4) << setfixed() <<0<< " ";
       for (a=faq(q);a<=la(s);a++){ 
         mm << setw(10) << setprecision(4) << setfixed() <<M1(yq,s,a)+M2(yq,s,a)<< " ";
        }
       for (a=la(s)+1;a<=max_a;a++) mm << setw(11) << 0;
      mm <<endl;
     }
   }
 } 
 mm.close();

FUNCTION void print_season_overlap()
 int pred,prey,q;

 ofstream sv("season_overlap.out",ios::out);

  if (phase_season_overlap>0) {
  sv<<"#Predator prey season overlap"<<endl;
  sv<<            "#----------------------"<<endl;
  for(d=1;d<=no_areas;d++) {
    if(no_areas>1) sv<<"### Area: "<<area_names[d]<<endl;
    for (pred=1;pred<=npr;pred++) {
      sv <<"# Predator:"<<species_names[pred]<<endl;
      if (pred==1) {
          sv<<"# Other-food  ";
          for (prey=first_VPA;prey<=nsp;prey++) sv <<species_names[prey]<<" ";
          sv <<endl;
      }
      for (q=fq;q<=lq;q++) {
          for (prey=0;prey<=nsp;prey++) if ((prey==0) || (prey>=first_VPA)) {
          if (season_overlap(d,pred,q,prey)==1) sv<<setw(8) << setprecision(0)<<season_overlap(d,pred,q,prey)<<"    ";
          else sv<<setw(12) << setprecision(3)<<season_overlap(d,pred,q,prey);
          }
      sv<<endl;
      }
    }
  }
  sv.close();
  }

FUNCTION int npar_matrix( dmatrix ma)
 // count number of active parameters in a variable
 int r,npar;
 npar=0;
 for (r=ma.rowmin(); r<=ma.rowmax();r++) npar+=ma(r).indexmax()-ma(r).indexmin()+1; 
 return npar;

FUNCTION int npar_vector( dvector ma)
 // count number of active parameters in a variable
 return ma.indexmax()-ma.indexmin()+1;


FUNCTION void out_predict_mean(char intype[],int kind, d4_array tmp_in)    
 int tmp=no_MCMC_iterations*MCMC_prediction;
 dvector     tmp_out(1,tmp);
 int s,y,r,i,ii,firsty;
 
 char txt[40];
 char txt2[40];

 double std_tmp;
 
 if (kind==1) firsty=fyModel; 
 else if (kind==3) firsty=lyModel+1;
 // write summary statistics 
  
  strcpy(txt,intype);
  strcpy(txt2,"mcout2_average_");
  strcat(txt2,txt);
  strcat(txt2,".out");
  
  ofstream mcdat(txt2,ios::out);
  if (!(mcdat)) {
     cerr << "Error trying to open file "<<txt<< " for output "<<endl;
     exit(1);
  }
  mcdat <<"Species.n Year "<<intype<< " std"<<endl;
 
   // calculate average value
  for (s=first_VPA;s<=nsp;s++) {
    for (y=firsty;y<=lpy;y++){
      ii=0; tmp_out=0.0;
      for (r=1;r<=MCMC_prediction;r++){
        for (i=1;i<=no_MCMC_iterations;i++) {
          ii++;
          tmp_out(ii)=tmp_in(r,i,s,y);
        }
      }
      std_tmp=std_dev(tmp_out);
      if (std_tmp<1.0E-6) std_tmp=0.0;
      mcdat <<s<<" "<<y<<" "<< mean(tmp_out) << " " <<std_tmp<<endl ;
    }
  }
  
   // write data for each MCMC repetition
  strcpy(txt,intype);
  strcpy(txt2,"mcout_");
  strcat(txt2,txt);
  strcat(txt2,".out");

  ofstream mcdat2(txt2,ios::out);
  if (!(mcdat2)) {
     cerr << "Error trying to open file "<<txt2<< " for output "<<endl;
     exit(1);
  } 
  
  mcdat2<< "Species.n Year Repetion Iteration "<<intype<<endl;
  for (s=first_VPA;s<=nsp;s++) {
    for (y=firsty;y<=lpy;y++){
      for (r=1;r<=MCMC_prediction;r++){
        for (i=1;i<=no_MCMC_iterations;i++) {
        mcdat2 <<s<<" "<<y<<" "<<r<<" "<<i<<" "<<tmp_in(r,i,s,y)<<endl ;
  }}}}
 

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中


FUNCTION void write_predict_output()
  char txt [20];
  cout <<"Writing prediction output."<<endl; 
  
  strcpy(txt,"SSB");              out_predict_mean(txt,1,tmp_SSB);
  strcpy(txt,"SSB_percieved");    out_predict_mean(txt,3,tmp_SSB_percieved);
  strcpy(txt,"TSB");              out_predict_mean(txt,1,tmp_TSB);
  strcpy(txt,"mean_F");           out_predict_mean(txt,1,tmp_mean_F);
  strcpy(txt,"mean_F_percieved"); out_predict_mean(txt,1,tmp_mean_F_percieved);
  strcpy(txt,"yield");            out_predict_mean(txt,1,tmp_yield);
  if (multi==2) { strcpy(txt,"eaten_M2"); out_predict_mean(txt,1,tmp_eaten_M2); }
  strcpy(txt,"recruit");          out_predict_mean(txt,1,tmp_recruit);
  
                                           
FUNCTION void  print_objective_func_file()
 int s;
  ofstream obj("objective_function.out",ios::out);
  obj<<"Species.n catch CPUE SSB.Rec  stomachs stomachs.N penalty all n.catch n.CPUE n.SSB.R n.stom n.all.obs n.par"<<endl;
  for (s=1;s<=nsp;s++)  obj<<s<<"  "<<obj_func(s)<<" "<<obf<<" "<<no_obj_obs(s)<<" "<<initial_params::nvarcalc()<<endl;
  obj.close();
   
   
FUNCTION void print_survey_data_noise(int no_set,int methode)
 int s, y, q, a, f, i, s2_fa, s2_la, s2_group, no, sp_fl,localMaxFleetYear;
  dvariable N_survey,duration;
  dmatrix    N_survey_out(fyModel,lyModel+1,fa,max_a);       // survey and noise
  char txt[40];
  char txt2[10];


  random_number_generator rnd(123);  // noise on invented catches from mean catch

  for (no=1;no<=no_set;no++) {
   strcpy(txt,"survey_and_noise");
   sprintf(txt2, "%d", no );
   strcat(txt,txt2);
   strcat(txt,".in");
   ofstream suv(txt,ios::out);
   cout<<"writing file: "<<txt<<endl;
   
   sp_fl=0;
   i=0;
   for (s=first_VPA;s<=nsp;s++){
    for (f=1; f<=n_fleet(s);f++) {
      localMaxFleetYear=(int)min(last_fleet_year(s,f),lyModel+1); // to allow model last year +1 to be used
      duration=fleet_beta(s,f)-fleet_alfa(s,f);
      q=fleet_season(s,f);
      N_survey_out=-99;
      sp_fl++;
      suv<<"# sp:"<<s<<" fleet:"<<f<<" year-range:"<< first_fleet_year(s,f)<<"-"<< localMaxFleetYear<<endl;
      if (s>=first_VPA) for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
         s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++) {
            if (a==s2_fa) i=i+1;
            for (y=first_fleet_year(s,f);y<=localMaxFleetYear;y++){
               CALC_yq
               if ((y<lyModel)|| (y==lyModel && q<=lqly) ||
                   (y==lyModel && q==(lqly+1) && duration==0) ||
                   (y>lyModel && q==fq && duration==0 && a>fa)) {
               if ( (log_CPUE(sp_fl,y,a) < 998.0) && (!((a==fa) && (q <recq))) )  {
                  if (duration==1) {N_survey=N_bar(yq,s,a); }
                  else if (duration>0) {
                    N_survey=N(yq,s,a)*exp(-Z(yq,s,a)*fleet_alfa(s,f)); // N as start of the survey period
                    N_survey=N_survey*(1-exp(-Z(yq,s,a)*duration))/(Z(yq,s,a)*duration); // mean N in the survey period
                  }
                  else if (fleet_alfa(s,f)==0 && fleet_beta(s,f)==0) {
                    N_survey=N(yq,s,a);
                  }
                  else if (fleet_alfa(s,f)==1 && fleet_beta(s,f)==1) N_survey=N(yq,s,a)*exp(-Z(yq,s,a));
                  // cout<<"s:"<<s<<" y:"<<y<<" a:"<<a<<" sp_fl:"<<sp_fl<<" s2_group:"<<s2_group<<" i:"<<i<<" N_survey:"<<N_survey<<endl;
                  if (methode==1) N_survey_out(y,a)=value(exp(log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a)))
                                     *fleet_effort(sp_fl,y))*exp(randn(rnd)* sqrt(value(qq_s2(sp_fl,s2_group))));  //log norm dist noise from model
                  else if (methode==2)  N_survey_out(y,a)=value(exp(log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a)))
                                     *fleet_effort(sp_fl,y))*exp(randn(rnd)* sqrt(qq_s2_input(no,i)));  //log norm dist noise from model                  
                }
             }
           }
         }         
     }  // variance group end      
     for (y=first_fleet_year(s,f);y<=localMaxFleetYear;y++){
         suv<<fleet_effort(sp_fl,y);
         for (a=int(CPUE_s2_group(s,f,1));a<=s2_la;a++) suv<<" "<< N_survey_out(y,a);
         suv<<endl;   
     }
   }  // fleet end
  }  // species end
   suv.close();
 }

   
FUNCTION void print_survey_data_sim(int no)
 int s, y, q, a, f, s2_fa, s2_la, s2_group, sp_fl,localMaxFleetYear;
  dvariable N_survey,duration;
  dmatrix    N_survey_out(fyModel,lyModel+1,fa,max_a);       // survey 
  char txt[40];
  char txt2[10];

   strcpy(txt,"survey_and_noise");
   sprintf(txt2, "%d", no );
   strcat(txt,txt2);
   strcat(txt,".in");
   ofstream suv(txt,ios::out);
   cout<<"writing file: "<<txt<<endl;
   
   random_number_generator rnd(123);  // noise on invented catches from mean catch
   
   sp_fl=0;
   for (s=first_VPA;s<=nsp;s++){
    for (f=1; f<=n_fleet(s);f++) {
      localMaxFleetYear=(int)min(last_fleet_year(s,f),lyModel+1); // to allow model last year +1 to be used
      duration=fleet_beta(s,f)-fleet_alfa(s,f);
      q=fleet_season(s,f);
      N_survey_out=-99;
      sp_fl++;
      suv<<"# sp:"<<s<<" fleet:"<<f<<" year-range:"<< first_fleet_year(s,f)<<"-"<< localMaxFleetYear<<endl;
      if (s>=first_VPA) for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
          s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++) {
            for (y=first_fleet_year(s,f);y<=localMaxFleetYear;y++){
               CALC_yq
               if ((y<lyModel)|| (y==lyModel && q<=lqly) ||
                   (y==lyModel && q==(lqly+1) && duration==0) ||
                   (y>lyModel && q==fq && duration==0 && a>fa)) {
               if ( (log_CPUE(sp_fl,y,a) < 998.0) && (!((a==fa) && (q <recq))) )  {
                  if (duration==1) {N_survey=N_bar(yq,s,a); }
                  else if (duration>0) {
                    N_survey=N(yq,s,a)*exp(-Z(yq,s,a)*fleet_alfa(s,f)); // N as start of the survey period
                    N_survey=N_survey*(1-exp(-Z(yq,s,a)*duration))/(Z(yq,s,a)*duration); // mean N in the survey period
                  }
                  else if (fleet_alfa(s,f)==0 && fleet_beta(s,f)==0) {
                    N_survey=N(yq,s,a);
                  }
                  else if (fleet_alfa(s,f)==1 && fleet_beta(s,f)==1) N_survey=N(yq,s,a)*exp(-Z(yq,s,a));
                  //cout<<"N_survey:"<<N_survey<<" qq_power(s,f,a):"<<qq_power(s,f,a)<< " qq(s,f,a):"<<setprecision(3)<<qq(s,f,a)<<" fleet_effort(sp_fl,y):"<<fleet_effort(sp_fl,y)<<endl;
                  N_survey_out(y,a)=value(exp(log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a)))*fleet_effort(sp_fl,y));
                  N_survey_out(y,a)=value(exp(log(N_survey)*qq_power(s,f,a)+log(qq(s,f,a)))
                                    *fleet_effort(sp_fl,y))*exp(randn(rnd)* sqrt(value(qq_s2(sp_fl,s2_group))));  //log norm dist noise
                }
             }
           }
         }         
     }  // variance group end  
     //cout<<N_survey_out<<endl;    
     for (y=first_fleet_year(s,f);y<=localMaxFleetYear;y++){
         suv<<fleet_effort(sp_fl,y);
         for (a=int(CPUE_s2_group(s,f,1));a<=s2_la;a++) suv<<" "<< N_survey_out(y,a);
         suv<<endl;   
     }
   }  // fleet end
  }  // species end
   suv.close();

    
FUNCTION void print_stomach_data_sim(int no)
 char txt[40];
 char txt2[10];
 int y,q,ll,pred,prey,pred_l,prey_l,pred_l_class,stom_used_all,size_model;
 dvariable stomcon,stomcon_hat;
 dvariable prey_l_class; 
 int sy,sq,sp,spl,splp;  //stomach index counters;

 strcpy(txt,"stom_and_noise");
 sprintf(txt2, "%d", no );
 strcat(txt,txt2);
 strcat(txt,".in");
 ofstream res(txt,ios::out);
 cout<<"writing file: "<<txt<<endl;
  
 for (sy=1;sy<=n_stl_y;sy++) { 
   y=stl_y(sy,1);
   for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
     q=stl_yq(sq,1);
     for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
      d=stl_yqd(sd,1);
      for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
       pred=stl_yqdp(sp,1);
       res<<"#########"<<endl<<"### pred:"<<pred<<" Year:"<<y<<" Q:"<<q<<" area:"<<d<<endl;
       
       size_model=size_selection(pred);
       for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
         pred_l=stl_yqdpl(spl,4);
         pred_l_class=stl_yqdpl(spl,1);    //length group 
         
         if (incl_stom(d,pred,q,stl_yqdpl(spl,1),sy)>=1) stom_used_all=1; else stom_used_all=0;
         res<<"# pred l:"<<pred_l<<" "<<pred_l_class<<endl; 
         ll=0;
         for (splp=stl_yqdpl(spl,2);splp<=stl_yqdpl(spl,3);splp++) {
           prey=stl_yqdplp(splp,1);
           
           for(prey_l=stl_yqdplp(splp,2);prey_l<=stl_yqdplp(splp,3);prey_l++) {
             ll++;

             // stomcon=stl_stom(spl,ll);             // input stomach content weight (relative)
             stomcon_hat=stl_E_stom(spl,ll);       // Expected stomach content weight (relative)
             if (stl_stom_use_like(spl,ll)==0) {
               if (stl_stom_use_avail(spl,ll)==1) stomcon_hat=min_stom_cont(pred)*1.1; else stomcon_hat=0;
             }
             res<<stomcon_hat<<" ";
           }
           res<<"  # prey:"<<prey<<endl;;
         }
       }
      }
    }
   }
 }
 res.close();
 
 


 
FUNCTION void print_catch_data_noise(int no_set,int methode)
  // method=1  use var estimated within SMS
  // mathod=2 use var from file catach_noise.data, data store in catch_s2_input
  
  d3_array   C_hat_noise(fyModel,lyModel,fq,lq,fa,max_a);       // catch and noise
  int s, y, q, a, s2_fa, s2_la, s2_group,no,i;
  char txt[40];
  char txt2[10];
  int yq;

  random_number_generator rnd(123);  // noise on invented catches from mean catch

  for (no=1;no<=no_set;no++) {
   strcpy(txt,"canum_and_noise");
   sprintf(txt2, "%d", no );
   strcat(txt,txt2);
   strcat(txt,".in");
   ofstream suv(txt,ios::out);
   cout<<"writing file: "<<txt<<endl;  
   ofstream can(txt,ios::out);
 
    
    for (s=first_VPA;s<=nsp;s++){
      C_hat_noise=0;
     for (s2_group=1;s2_group<=n_catch_s2_group(s);s2_group++) {
       s2_fa=catch_s2_group(s,s2_group);
       if (s2_group==n_catch_s2_group(s)) s2_la=la_like(s);
       else s2_la=catch_s2_group(s,s2_group+1)-1;
       for (y=fyModel;y<=lyModel;y++) {
         for (q=fq;q<=lq;q++) if (zero_catch_y_season(s,y,q)==1){

           if (seasonal_annual_catches(s)==0) {
             if (seasonal_combined_catch_s2(s)==1) i=1; else i=q;
             for (a=s2_fa;a<=s2_la;a++) if (incl_catch_season_age(s,q,a)>0) {
               lqLocal=(y==lyModel)?lqly:lq_F(s,a);
               if (q<=lqLocal) {
                 CALC_yq
                 //cout<<"sp:"<<s<<" y:"<<y<<" q:"<<q<<" a:"<<a<<" s2_group:"<<s2_group<<" C_hat:"<<C_hat(yq,s,a)<<" catch_s2:"<<catch_s2(s,i,s2_group)<<endl;
                 //cout<<"methode:"<<methode<<" no:"<<no<<" catch_s2_index:"<<catch_s2_index(s,i,s2_group)<<" input noise:"<<setprecision(3)<<catch_s2_input(no,catch_s2_index(s,i,s2_group))<<endl;
                 if (methode==1) C_hat_noise(y,q,a)=value(C_hat(yq,s,a))*exp(randn(rnd)* sqrt(value(catch_s2(s,i,s2_group))));  //log norm dist noise
                 else if (methode==2) C_hat_noise(y,q,a)=value(C_hat(yq,s,a))*exp(randn(rnd)* sqrt(catch_s2_input(no,catch_s2_index(s,i,s2_group))));  //log norm dist noise
                 //cout<<"C_hat_noise(y,q,a): "<<C_hat_noise(y,q,a)<<" rnd:"<<randn(rnd)<<endl;
               } //lqLocal
             }  //a-loop
           } // Seasonal catches                       =
         }  // q-loop
        } //y-loop
      } // s2_group
      //cout<<"C_hat_noise:"<<endl<<C_hat_noise<<endl;
       for (y=fyModel;y<=lyModel;y++) can<<"# sp:"<<s<<" "<<y<<endl<<C_hat_noise(y)<<endl;
    } // species loop
    can.close();
  } 

 
FUNCTION void print_catch_data_sim(int no)
  int s, y, q, a;
  char txt[40];
  char txt2[10];
  int yq;
  strcpy(txt,"canum_and_noise");
  sprintf(txt2, "%d", no );
  strcat(txt,txt2);
  strcat(txt,".in");
  ofstream suv(txt,ios::out);
  cout<<"writing file: "<<txt<<endl;  
  ofstream can(txt,ios::out);

  for (s=first_VPA;s<=nsp;s++){
    for (y=fyModel;y<=lyModel;y++) {
      can<<"# sp:"<<s<<" "<<y<<endl;
      for (q=fq;q<=lq;q++) {
        CALC_yq
        can<<C_hat(yq,s)<<endl;
      }
    }
  }
  can.close();
 


 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

REPORT_SECTION
 save_gradients(gradients);

 int s,y,q,a,f,no,sp_fl,pred,prey,dummy,i,syg;
 dvariable tmp;
 dvar_matrix out(1,nsp,1,4);
 dvar_matrix out2(fyModel,lyModel,first_VPA,nsp);
 dvar_vector out3(1,7);
 
 if (test_output>=1) cout <<"REPORT_SECTION initiated"<<endl;


 // sum number of observations
 for (s=1;s<=nsp;s++)  no_obj_obs(s,5)=sum(no_obj_obs(s)); // row sum
 for (s=1;s<=nsp;s++)  for (i=1;i<=5;i++) no_obj_obs(nsp+1,i)+=no_obj_obs(s,i);  // column sum

 cout<<"report file: "<<(adprogram_name + ad_tmp)<<endl;
 
 time (&end_time);

 report<<dateStr<<"   run time:"<<difftime(end_time,start_time)<<" seconds"<<endl;
 
 
 report << endl<< "objective function (negative log likelihood):  " << obf << endl;
 report << "Number of parameters: "<<initial_params::nvarcalc()<<endl;
 
 
 if (nsp>1) report << "Number of observations used in likelihood: "<< no_obj_obs(nsp+1,5)<<endl;
 report <<"Maximum gradient: "<<objective_function_value::pobjfun->gmax<<endl;
 report<<"Akaike information criterion (AIC):   "<< 2*initial_params::nvarcalc()+2*obf <<endl;
 // WRONG NUMBER OF OBSERVATIONS report<<"Bayesian information criterion (BIC): " << 2*obf+initial_params::nvarcalc()*log(sum(no_obj_obs))<<endl<<endl;

                                                              
 
 report<<"Number of observations used in the likelihood:";
 report<<endl<<"                            Catch    CPUE     S/R Stomach     Sum";
 for (s=1;s<=nsp;s++) {
   report<<endl;
   if (nsp>1) report << "Species:"<< setw(2)<<s << ", "<<species_names[s]<<"  ";
   else report<<"                        ";
   report<<setw(7)<<setfixed()<<setprecision(0)<<no_obj_obs(s); 
  }
 if (nsp>1) {
   report<<endl<<"Sum                      ";
   report<<setw(7)<<setfixed()<<setprecision(0)<<no_obj_obs(nsp+1)<<endl;
 }
 
 report<<endl<<endl<<"objective function weight:";
 report<<endl<<"                          Catch  CPUE   S/R";
 if  (multi>=1) report<<"     Stom.    Stom N.";
 for (s=1;s<=nsp;s++) {
   report<<endl;
   if (nsp>1) report << "Species:"<< setw(2)<<s << ", "<<species_names[s]<<" ";
   else report<<"                        ";
   for (i=1;i<=3;i++) report<<setw(6)<<setprecision(2)<<obj_weight(s,i);
   if  (multi>=1) report<<"       "<<obj_weight(s,4);
   if  (multi>=1) report<<"       "<<obj_weight(s,5);
 }
 

 report<<endl<<endl;
 out3=0.0;
   
 report << "unweighted objective function contributions (total): " <<endl
    <<"                Catch    CPUE    S/R   Stom.  Stom N.  Penalty     Sum"<< endl;
  for (s=1;s<=nsp;s++) {
  tmp=0.0;
    if (nsp>1) report <<species_names[s]<<" ";
    else report        << "           ";
    for (i=1;i<=6;i++) {
       out3(i)=out3(i)+obj_func(s,i);
       tmp=tmp+obj_func(s,i);
       report.setf(ios::right);
       if (i<6) report<< setw(8) << setfixed()<<setprecision(1)<<obj_func(s,i);
       else  report<< setw(10) << setfixed()<<setprecision(2)<<obj_func(s,i);
    }
    out3(7)=out3(7)+tmp;
   report<< setw(8) << setfixed()<<setprecision(0)<<tmp << endl;
 }

 if (nsp>1) {
    report << "Sum         ";
    for (i=1;i<6;i++) report<< setw(8) << setfixed()<<setprecision(1)<<out3(i);
    report<< setw(10) << setfixed()<<setprecision(2)<<out3(6);
    report<< setw(8) << setfixed()<<setprecision(0)<<out3(7);
 }
 
 //for (s=1;s<=nsp;s++) no_obj_obs(s,3)=lyModel-fyModel+1;
 for (s=1;s<=nsp;s++) for (i=1;i<=4;i++) {
    if (no_obj_obs(s,i)>0) out(s,i)=obj_func(s,i)/no_obj_obs(s,i);
    else out(s,i)=0.0;
 }
 report << endl;
 report <<endl<< "unweighted objective function contributions (per observation): " <<endl<<"                Catch   CPUE     S/R   Stomachs"<<endl;
   for (s=1;s<=nsp;s++) {
    if (nsp>1) report <<species_names[s]<<" ";
    else report        << "           ";
    for (i=1;i<=4;i++) {
       report.setf(ios::right);
       report<< setw(8) << setfixed()<<setprecision(2)<<out(s,i);
    }
    report <<endl;
 }

 
 report << endl<< endl<<"contribution by fleet:";
 report << endl <<      "----------------------";
 sp_fl=0;
 
 for (s=first_VPA;s<=nsp;s++){
   if (s>=first_VPA) report <<endl;    
   if (nsp>1 && s>=first_VPA) report <<"Species:"<<s<<", "<<species_names[s]<<" "<<endl;

   for (f=1;f<=n_fleet(s);f++) {
     sp_fl++;
     //report << setw(8) << setprecision(3) << setfixed();
     //report.setf(ios::right);
     if (s>=first_VPA) {
        report <<fleet_names[sp_fl]<<"  total:"<< setw(8) << setprecision(3) << setfixed()<<fleet_contribution(sp_fl);
       report<< "   mean:"<< setw(8) << setprecision(3) << setfixed()<<fleet_contribution_mean(sp_fl)<<endl;
     }  
   }
 }
 
 
 for (s=first_VPA;s<=nsp;s++) {
   for (y=fyModel;y<=lyModel;y++) {
    if (do_effort(s)==1) out2(y,s)=0  ;
    else out2(y,s)=F_y(s,y);
   }
 }
 if (all_do_effort==0) {
   report<<endl<<endl<<"F, Year effect:"<<endl;
   report<<            "---------------"<<endl<<"         ";
   if (nsp>1) for (s=first_VPA;s<=nsp;s++) report<<"sp."<<setw(2)<<s<<"    ";
   report<<endl;
   for (y=fyModel;y<=lyModel;y++) report<<y<<':'<< setw(8) << setprecision(3) << setfixed()<<out2(y)<<endl;
 }
 // if (use_creep(1)>0){ 
 //   report<<endl<<endl<<"Technical creep:"<<endl;
 //  report<<            "---------------"<<endl;
 //  report<<creep<<endl<<endl;
 // }
  
 if (lq-fq>=1) {
 report << endl << "F, season effect:";
 report << endl << "-----------------";

 for (s=first_VPA;s<=nsp;s++) {
   report << endl;
   if (nsp>1) report <<species_names[s]<<" "<<endl;
   if (seasonal_annual_catches(s)==1) report<<"Please note: Season effects are copied from input file"<<endl;
   for (sag=1;sag<=n_catch_season_age_group(s);sag++){
     report << "age: "<< catch_season_age(s,sag);
     if (sag<n_catch_season_age_group(s)) {
       if ((catch_season_age(s,sag)+1)>catch_season_age(s,sag+1)) report<<" - "<<catch_season_age(s,sag+1)-1<<endl;
     }
     else {
       if (catch_season_age(s,sag)!=la(s)) report<<" - "<<la_like(s);
       if (la_like(s) < la(s)) report<<" ("<<la(s)<<")";
     }
     report<<endl;
     for (syg=1;syg<=n_catch_sep_year_group(s);syg++) {
         y=catch_sep_year(s,syg);
     report <<setprecision(0) << setfixed()<<setw(8)<<y<<"-";
     if (syg==n_catch_sep_year_group(s)) report<<lyModel;
     else report<<catch_sep_year(s,syg+1)-1;
     report << ":  "<<setprecision(3) << setfixed()<<F_q(s,sag,syg)<<endl;;
     }
   }
 }}
 
 
 
 report << endl << "F, age effect:";
 report << endl << "--------------"<<endl<<"          ";
 for (a=fa;a<=max_a;a++) report<<setw(7)<<a;

 int ly_group;
  // Copy F_a, 
  for (s=first_VPA;s<=nsp;s++){
    for (a=cfa(s);a<=las(s);a++){
      for(syg=1;syg<=n_catch_sep_year_group(s);syg++) {
        if (syg==n_catch_sep_year_group(s)) ly_group=lyModel;
        else ly_group=catch_sep_year(s,syg+1)-1;

         for (y=catch_sep_year(s,syg);y<=ly_group;y++) {
          if (n_catch_sep_year_group(s)==1) { 
            F_a(s,y,a)=log_F_a_ini(s,a,0);
           }
          else F_a(s,y,a)=log_F_a_ini(s,a,syg-1);

           F_a(s,y,a)=exp(F_a(s,y,a));
         }
       }
     }
   // give the same selection pattern for ages with identical selection pattern
   if (las(s)<la(s)){
     for(a=las(s)+1;a<=la(s);a++) {
       for (y=fyModel;y<=lyModel;y++) {
         F_a(s,y,a)=F_a(s,y,las(s));
       }
     }
   }  
 }
 
 for (s=first_VPA;s<=nsp;s++){
   if (nsp>1)report <<endl<<species_names[s];
   for(syg=1;syg<=n_catch_sep_year_group(s);syg++) {
     y=catch_sep_year(s,syg);
     report <<setprecision(0) << setfixed()<<setw(4)<<endl<<y<<"-";
     if (syg==n_catch_sep_year_group(s)) report<<lyModel;
     else report<<catch_sep_year(s,syg+1)-1;
     report << ":"<<setprecision(3) << setfixed()<<setw(6)<<F_a(s,y);
   }
 }  

 calc_F(1);   // calc F without considering closures
 report << endl<< endl<<endl<<"Exploitation pattern (scaled to mean F=1)";
 report << endl <<            "-----------------------------------------"<<endl;
 if (lq>1) report<<"                  "; else  report<<"          ";
 for (a=fa;a<=max_a;a++) report<<setw(7)<<a;
     
 for (s=first_VPA;s<=nsp;s++){
   if (nsp>1)report <<endl<<species_names[s];

   for(syg=1;syg<=n_catch_sep_year_group(s);syg++) {
     y=catch_sep_year(s,syg); 
     report <<setprecision(0) << setfixed()<<setw(4)<<endl<<y<<"-";
     if (syg==n_catch_sep_year_group(s))  report<<lyModel;
     else report<<catch_sep_year(s,syg+1)-1;
     tmp=0;
     for (q=fq;q<=lq;q++){
       CALC_yq
       for (a=fa;a<=la(s);a++){
         if (a>=avg_F_ages(s,1) && a<=avg_F_ages(s,2) && !(a==fa && q<recq)) tmp+=F(yq,s,a);
       } 
     }  
     tmp=tmp/(avg_F_ages(s,2)-avg_F_ages(s,1)+1);
     for (q=fq;q<=lq;q++){
       CALC_yq
       if (q>1) report<<"         ";
       for (a=fa;a<=la(s);a++){
         if (a==fa && lq>1 ) report <<" season "<<q<<":"; else if (a==fa) report<<":"; 
         if (tmp>0 && !(a==fa && q<recq)) report<<setprecision(3) << setfixed()<<setw(7)<<F(yq,s,a)/tmp;
         else report<<setprecision(3) << setfixed()<<setw(7)<<0;
       }
       if (lq>1) report<<endl;
     }
   }
 }
 calc_F(0); // re-calc real F
 
 int s2_group,s2_la,s2_fa;
 if (phase_single_species_fixed==0) {  
   report << endl<< endl<<"sqrt(catch variance) ~ CV:";
   report << endl      << "--------------------------"<<endl;
   
   for (s=first_VPA;s<=nsp;s++){
     if (nsp>1) report<<endl<<species_names[s];
     if (seasonal_combined_catch_s2(s)>1) {
        report<<endl<<"             ";
        report<<" season"<<endl<<"------";
        for (q=fq;q<=seasonal_combined_catch_s2(s);q++) report<<"--------";
        report<<endl<<"age ";
        for (q=fq;q<=seasonal_combined_catch_s2(s);q++)report<<setw(8)<<q;
        report<<endl;
     }
     for (s2_group=1;s2_group<=n_catch_s2_group(s);s2_group++) {
       if (s2_group==1) report<<endl;
       s2_fa=catch_s2_group(s,s2_group);
       if (s2_group==n_catch_s2_group(s)) s2_la=la(s);
       else s2_la=catch_s2_group(s,s2_group+1)-1;
       for (a=s2_fa;a<=s2_la;a++) {
          report<<setw(2)<<a<<"    ";
         for (q=fq;q<=seasonal_combined_catch_s2(s);q++) { 
          if (a<=la_like(s)) {
            if (catch_s2(s,q,s2_group)>0) report<<setw(8) << setprecision(3) << setfixed()<<sqrt(catch_s2(s,q,s2_group));
            else report<<"        ";
          } else report<<"      NA";
         }
         report<<endl;
       }
     } 
   }
 }
 
 report<<endl;


 report << endl <<"Survey catchability:";
 report << endl <<"--------------------";
 int done; 
 sp_fl=0;
 for (s=first_VPA;s<=nsp;s++){
   if (nsp>1) report<<endl<<species_names[s]<<"                ";
   else report<<endl<<"       "<<"                ";
   if (s==first_VPA) for (a=fa;a<=max(v_last_fleet_age);a++) report<<"    age "<<a;
   
   for (f=1; f<=n_fleet(s);f++) {
      done=1;
      //q=fleet_season(s,f);
      sp_fl++;
      report << endl<<" "<<fleet_names[sp_fl];
      for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
         s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (done==1) for (a=fa;a<s2_fa;a++) report<<"         ";
         done=0;
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++) {
            report<< setw(9) << setprecision(3) <<  setfixed()<<qq(s,f,a);
         }
      }
    }
  }

 

 dummy=0;
 for (s=first_VPA;s<=nsp;s++) for (f=1; f<=n_fleet(s);f++) if (last_fleet_age_power(s,f)>=0) dummy=1; 
 if (dummy==1) {
    report << endl; 
    report << endl <<"Stock size dependent catchability (power model)";
    report << endl <<"-----------------------------------------------";   
    sp_fl=0;
    for (s=first_VPA;s<=nsp;s++){
      if (nsp>1) report<<endl<<species_names[s]<<"                ";
      if (s==1) for (a=fa;a<=max(v_last_fleet_age);a++) report<<"    age "<<a;
      for (f=1; f<=n_fleet(s);f++) {
        sp_fl++;
        report << endl<<fleet_names[sp_fl];
        for (a=fa;a<first_fleet_age(s,f);a++) report<<"            ";
        for (a=first_fleet_age(s,f);a<=last_fleet_age(s,f);a++) report<< setw(9) << setprecision(2) << setfixed()<<qq_power(s,f,a);
        }
    }
 }  
  
 if (phase_single_species_fixed==0) {  
 report << endl<< endl<<"sqrt(Survey variance) ~ CV:";
 report << endl       <<"---------------------------";
 sp_fl=0;
 for (s=first_VPA;s<=nsp;s++){
   if (nsp>1) report<<endl<<species_names[s]<<"                ";
    else             report<<endl<<"       "<<"                ";
   if (s==first_VPA) for (a=fa;a<=max(v_last_fleet_age);a++) report<<"    age "<<a;
   for (f=1; f<=n_fleet(s);f++) {
      done=1;      
      sp_fl++;
      report << endl<<" "<<fleet_names[sp_fl];
      //q=fleet_season(s,f);
      if (s>=first_VPA) for (s2_group=1;s2_group<=n_CPUE_s2_group(s,f);s2_group++) {
         s2_fa=int(CPUE_s2_group(s,f,s2_group));
         if (done==1) for (a=fa;a<s2_fa;a++) report<<"         ";
         done=0;
         if (s2_group==n_CPUE_s2_group(s,f)) s2_la=last_fleet_age(s,f);
         else s2_la=int(CPUE_s2_group(s,f,s2_group+1))-1;
         for (a=s2_fa;a<=s2_la;a++) {
            report<< setw(9) << setprecision(2) <<  setfixed()<<sqrt(qq_s2(sp_fl,s2_group));
          }
      }
    }
  }
 }
 
 //Average F
 for (s=first_VPA;s<=nsp;s++){
   for (y=fyModel;y<=lyModel;y++){
     no=0; tmp=0.0;
     for (q=fq;q<=lq;q++){
       CALC_yq
       for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) {
         no++;
         tmp+=F(yq,s,a);
       }
     }
     out2(y,s)=tmp/no*(lq-fq+1);
   }
 }
 
 if (lq>fq && 1==2) { 
   report<<endl<<endl<<"Average F (sum of sesonal F):"<<endl;
   report<<      "-------------------------------"<<endl<<"         ";
   for (s=first_VPA;s<=nsp;s++) report<<"sp."<<setw(2)<<s<<"    ";
   report<<endl;
   for (y=fyModel;y<=lyModel;y++) report<<y<<':'<< setw(8) << setprecision(3) << setfixed()<<out2(y)<<endl;
 }
 
 report<<endl<<endl<<"Average F:"<<endl;
 report<<            "----------"<<endl<<"         ";
 for (s=first_VPA;s<=nsp;s++)for (y=fyModel;y<=lyModel;y++) out2(y,s)=Mean_F(s,y); 
 
 for (s=first_VPA;s<=nsp;s++) report<<"sp."<<setw(2)<<s<<"    ";
 report<<endl;
 for (y=fyModel;y<=lyModel;y++) report<<y<<':'<< setw(8) << setprecision(3) << setfixed()<<out2(y)<<endl;
  
 
 if (multi>=1) {
   for ( y=fyModel;y<=lyModel;y++) for (q=fq;q<=lq;q++) calc_M2(y,q);
   report << endl <<"Average M2    ";
   report << setw(2) << setprecision(0) << setfixed();
   for (a=fa;a<=max_a;a++) report<<"  age "<<a;

   for ( s=first_VPA;s<=nsp;s++){
     report << endl<<species_names[s]<<"   ";

     for (a=fa;a<=la(s);a++) {
       tmp=0.0; no=0;
       for ( y=fyModel;y<=lyModel;y++){
         no++;
         for (q=fq;q<=lq;q++) {
           CALC_yq
           if (!((a==fa) && (q <recq)))tmp+=M2(yq,s,a);
         }
       }
       report << setw(7) << setprecision(3) << setfixed()<<tmp/no;
     }
   }
 }
 
 report << endl<< endl<<"Recruit-SSB                               alfa      beta       recruit s2     recruit s"<<endl;
 for (s=first_VPA;s<=nsp;s++){
   report << species_names[s]<<" ";
   
   if (use_known_rec_option_by_sp(s)==1)  report<<" Recruitment given as input:";
   else if (SSB_Rec_model(s)==1)               report<<" Ricker:               ";
   else if (SSB_Rec_model(s)==51)  report<<" Ricker Temperature    ";
   else if (SSB_Rec_model(s)==2)   report<<" Beverton & Holt:      ";
   else if (SSB_Rec_model(s)==3)   report<<" Geometric mean:       ";
   else if (SSB_Rec_model(s)==4)   report<<" Hockey stick:         ";
   else if (SSB_Rec_model(s)==5)   report<<" Hockey stick smooth:  ";
   else if (SSB_Rec_model(s)==100) report<<" Hockey stick -break.: ";

   else if (SSB_Rec_model(s)==9)   report<<" Ricker++:             ";
   // else if (SSB_Rec_model(s)==61)  report<<" STN Ricker++:         ";
   // else if (SSB_Rec_model(s)==71)  report<<" STN Cod Oxygen:       ";

   
   if (use_known_rec_option_by_sp(s)==1) report<<endl;
   else {
     report<<setw(12) << setprecision(3)<< setfixed(); 
     if (SSB_Rec_model(s)<=2 || SSB_Rec_model(s)==51)  report<<setw(12) << setprecision(3)<< setfixed()<<SSB_R_alfa(s) <<"   " <<setscientific()<<SSB_R_beta(s)/SSB_R_beta_cor(s);
     else if (SSB_Rec_model(s)==3) report<<setw(12) << setprecision(3)<< setfixed()<<SSB_R_alfa(s)<<"             " ;
     else if ((SSB_Rec_model(s)==4) || (SSB_Rec_model(s)==5)) report<<setw(12) << setprecision(3)<< setfixed()<<exp(SSB_R_alfa(s))<<"   "<<setscientific()<<SSB_R_beta(s);
     else if (SSB_Rec_model(s)==100) report<<setw(12) << setprecision(3)<< setfixed()<<exp(SSB_R_alfa(s))<<"   "<<setscientific()<<SSB_Rec_hockey_breakpoint(s);
     // else if (SSB_Rec_model(s)==61 )  report<<setw(12) << setprecision(3)<< setfixed()<<alfa_61 <<"   " <<setscientific()<<beta_61/SSB_R_beta_cor(s);
     // else if (SSB_Rec_model(s)==71 )  report<<setw(12) << setprecision(3)<< setscientific()<<alfa_71 <<"   " <<setscientific()<<beta_71/SSB_R_beta_cor(s);
      report <<setfixed()<< "   " <<SSB_R_s2(s)<< "          " <<sqrt(SSB_R_s2(s))<<endl;
   }
 }
 
  if (multi>=1) {
     report << endl<<endl<<"Multispecies parameters"<<endl;
     report              <<"========================"<<endl<<endl;
     
     report <<"stomach content variance model: ";
     if (stomach_variance==1) report<<"log normal distribution";
     else if (stomach_variance==2) report<<"normal distribution";
     else if (stomach_variance==3) report<<"Dirichlet distribution";
     else report <<"test distribution:"<<stomach_variance;
     report <<endl<<endl; 
     
 
     report << "Vulnerability pred - prey"<<endl;;
     report<<"---------------------------";
     for (d=1;d<=no_areas;d++) {
       if (no_areas>1) report<<endl<<"Area: "<<area_names[d]<<endl; else report<<endl;
       report<<"           Other-food     ";
       for (s=first_VPA;s<=nsp;s++) report <<species_names[s];
       for (pred=1;pred<=npr;pred++) {
         report <<endl<<species_names[pred]<<"    1.000";
         for (prey=first_VPA;prey<=nsp;prey++) {
           if (pred_prey_comb(d,pred,prey)==0) report << setw(11) << 0;
           else report << setw(11) << setprecision(3)  << vulnera(pred_prey_comb(d,pred,prey));
         }  
       }
      }
     report <<endl<<endl;
 
     //report << "Size dependent vulnerability pred - prey"<<endl;;
     //report<<"---------------------------"<<endl;
     //report<<"           Other-food     ";
     //for (s=first_VPA;s<=nsp;s++) report <<species_names[s];
     
     //for (pred=1;pred<=npr;pred++) {
     //   report <<endl<<species_names[pred]<<"    1.000";
     //  for (prey=first_VPA;prey<=nsp;prey++) {
     //    if (pred_prey_comb(pred,prey)==0) report << setw(11) << 0;
     //    else report << setw(11) << setprecision(3)  << vulnera_size(pred_prey_comb(pred,prey));
     //  }
     //}
     //report <<endl<<endl;

   
     if  (sum(size_selection)==0) {
       report<<"No size selection has been chosen for all predators"<<endl;                 
     }
     else {
        report<<"Size selection parameters:"<<endl;
        report<<"---------------------------           ";
        for (pred=1;pred<=npr;pred++) report <<species_names[pred]<<"    ";
        
        report << endl<<"Size selection model:             ";
        report<<  setw(17) << setprecision(0)<<setfixed();
    
        for (pred=1;pred<=npr;pred++) {
        if (size_selection(pred)==0)      report<<"no size selc.  ";
        else if (size_selection(pred)==1) report<<"log-norm.      ";
        else if (size_selection(pred)==2) report<<"asym log-norm. ";
        else if (size_selection(pred)==3) report<<"Gamma          ";
        else if (size_selection(pred)==4) report<<"no size selc++ ";
        else if (size_selection(pred)==5) report<<"beta           ";
        else if (size_selection(pred)==6) report<<"beta, unimodal ";
        }
        report <<endl<<"Sum prey sizes in likelihood:";
        for (pred=1;pred<=npr;pred++) {
          if (sumStomLike(pred)==0) report<<"             no" ;
          else                      report<<"            yes";
        }
        report << endl<<"Prefered size ratio:         ";
        for (pred=1;pred<=npr;pred++) {
        if (size_selection(pred)==0 || size_selection(pred)==4) report<<setw(15)<<setprecision(3)<<setfixed()<<0.0;
        else report<<setw(15)<<setprecision(3)<<setfixed()<<pref_size_ratio[pred];
        } 
        report << endl<<"Prefered size ratio adjust.: ";
        for (pred=1;pred<=npr;pred++) report<<setw(15)<<setprecision(3)<<setfixed()<<pref_size_ratio_correction(pred);
        report << endl<<"Variance of size ratio:      ";
        for (pred=1;pred<=npr;pred++) report<<setw(15)<<setprecision(3)<<setfixed()<<var_size_ratio(pred);
        report<<endl;
        
        
        report << endl<<"Other food Suitability slope:"<<endl;;
        for (pred=1;pred<=npr;pred++) {
            report<<species_names(pred)<<setw(15)<<setprecision(4)<<setfixed()<<stl_other_suit_slope(pred)<<endl;
        }
        if (active(init_prey_size_adjustment)) {
          report<<endl<<endl<<"Prey species size adjustment factor:"<<endl<<"          ";
          for (prey=first_VPA_prey;prey<=nsp;prey++) if (is_prey(prey)==1) report<<species_names(prey)<<"    ";
          report<<endl;
          for (prey=first_VPA_prey;prey<=nsp;prey++) if (is_prey(prey)==1) report<<setw(15)<<setprecision(3)<<setfixed()<<prey_size_adjustment(prey);
          report<<endl<<endl;
        }

     }
     report<<endl<<endl<<"Stomach variance:    value    internal     ";
     if (stomach_variance==3 ) report<<"max alfa0"<<endl; else report<<endl;
     
     // start to find max alfa0 (equal sumP) 
      if (stomach_variance==3) {  // Dirichlet 
        for (sy=1;sy<=n_stl_y;sy++) { 
         for (sq=stl_y(sy,2);sq<=stl_y(sy,3);sq++) { 
          for (sd=stl_yq(sq,2);sd<=stl_yq(sq,3);sd++) { 
           for (sp=stl_yqd(sd,2);sp<=stl_yqd(sd,3);sp++) {
            pred=stl_yqdp(sp,1);
             for (spl=stl_yqdp(sp,2);spl<=stl_yqdp(sp,3);spl++) {
              if (sum_p_Dirichlet(spl) > max_sumP(pred)) max_sumP(pred)=value(sum_p_Dirichlet(spl));
      }}}}}}
      
     for (pred=1;pred<=npr;pred++) {
        report <<species_names(pred)<<setprecision(3)<<setfixed()<<setw(15)<<Stom_var(pred)*Stom_var_fac(pred)<<"    "<<Stom_var(pred);
         if (stomach_variance==3 ) {
           if (Stom_var_u_save(pred)/Stom_var(pred)<1.02 && Stom_var_u_save(pred)/Stom_var(pred)>0.98) report<<" limit";
           else report<<"      ";
           report<<"  "<<max_sumP(pred)<<endl;
         } else report<<endl;
     } 
     if (phase_season_overlap>0) {
       report<<endl<<endl<<"Predator prey season overlap"<<endl;
       report<<            "----------------------------"<<endl;
       for(d=1;d<=no_areas;d++) {
         if (no_areas>1) report<<endl<<"Area: "<<area_names[d]<<endl;
         for (pred=1;pred<=npr;pred++) {
           report <<"Predator:"<<species_names[pred];
           if (pred==1) {
              report<<" Other-food  ";
              for (prey=first_VPA;prey<=nsp;prey++) report <<species_names[prey]<<" ";
           }
           report <<endl;
           for (q=fq;q<=lq;q++) {
             report<<"  q:" <<q<<"          ";
             for (prey=0;prey<=nsp;prey++) if ((prey==0) || (prey>=first_VPA)) {
               if (season_overlap(d,pred,q,prey)==1) report<<setw(8) << setprecision(0)<<season_overlap(d,pred,q,prey)<<"    ";
               else report<<setw(12) << setprecision(3)<<season_overlap(d,pred,q,prey);
             }
             report<<endl;
           }
         } 
       }
     }
     
     if (mesh_size_active==1 && active(init_s1)) {
       report<<endl<<endl<<"Mesh size selection:"<<endl;
       report             <<"-------------------"<<endl<<"                       "; 
       for (prey=first_VPA_prey;prey<=nsp;prey++)  report<<species_names[prey]<<"    ";
       report<<endl<<"L50            ";
       for (prey=first_VPA_prey;prey<=nsp;prey++) { 
         if (L50_mesh(prey)>=0) report<<setw(15)<<setprecision(0)<<setfixed()<<L50(prey); 
         else report<<setw(15)<<setprecision(0)<<setfixed()<<0;
        }
       
       report<<endl<<"Selection range";
       for (prey=first_VPA_prey;prey<=nsp;prey++) {
         if (L50_mesh(prey)>=0) report<<setw(15)<<setprecision(0)<<setfixed()<<2*((s1(prey)+log(3))/(s1(prey)/L50(prey))-L50(prey)); 
         else report<<setw(15)<<setprecision(0)<<setfixed()<<0; 
       }
         
       report<<endl<<"S1             ";
       for (prey=first_VPA_prey;prey<=nsp;prey++) {
         if (L50_mesh(prey)>=0) report<<setw(15)<<setprecision(3)<<setfixed()<<s1(prey); 
         else report<<setw(15)<<setprecision(0)<<setfixed()<<0; 
       } 
       
       report<<endl<<"S2             ";
       for (prey=first_VPA_prey;prey<=nsp;prey++) {
         if (L50_mesh(prey)>=0) report<<setw(15)<<setprecision(3)<<setfixed()<<s1(prey)/L50(prey); 
         else report<<setw(15)<<setprecision(0)<<setfixed()<<0; 
       } 

       report<<endl;
     }
     
     
 } // end multi


 if (test_output>=1) cout <<"Output file sms.rep is done"<<endl;

 if (do_prediction_mean!=1) { //retrospecive analysis
  if (multi!=1) {
     print_vars("Observed catches","obs_C");
     //print_vars("Obs.used catches","log_obs_C");  
     if (multi>=1) {
       print_vars("Residual Natural Mortality (M1)","M1");
       print_vars("Predation Mortality (M2)","M2");
     }
     else print_vars("Natural single species Mortality (M)","M");
     print_vars("Stock numbers","N");  
     print_vars("Fishing mortality","F");   
     print_vars("Total Mortality (Z)","Z");
  
     if (test_output>=1) cout <<"Output file details.out is done"<<endl;
     print_summary_table();
     if (test_output>=1) cout <<"Output file summary_table.out is done"<<endl;

     if (no_areas>1 && multi>=1) {
       if (test_output>=1) cout <<"Start Output file summary_areas.out"<<endl;
       print_summary_areas();
       if (test_output>=1) cout <<"Output file summary_areas.out is done"<<endl;
     }
     print_rec_scale();
     if (test_output>=1) cout <<"Output file rec_scale.out is done"<<endl;
     
     print_vars_ICES("Observed catches","obs_C");
     print_vars_ICES("Stock numbers","N");      
     print_vars_ICES("Fishing mortality","F");
     
     if (multi==2) print_vars_ICES("Predation mortality","M2");
     if (test_output>4) cout <<"details_ices.out is done"<<endl;
   
     print_survey_residuals();  
     if (test_output>=1) cout <<"Output file survey_residuals.out is done"<<endl;
     print_catch_residuals();
     if (test_output>=1) cout <<"Output file catch_residuals.out is done"<<endl;

     print_catch_survey_residuals();
     if (test_output>=4) cout <<"Output file catch_survey_residuals.out is done"<<endl;
     
   }
   
   if (multi>=1) {
      print_summary_stom();
      print_vulnerab(); 
      if (test_output>=1) cout <<"Output file summary_stom.out is done"<<endl;
   }

   if (multi>1) {
      print_part_M2();
      if (test_output>=1) cout <<"Output file part_m2.out is done"<<endl;
    
      print_size_pref();
      print_min_max_size_pref();
      if (test_output>4) cout <<"output file size_pref.out is done"<<endl;

      print_ALK();
      if (test_output>4) cout <<"output file alk.out is done"<<endl;
 
      if (simple_ALK==1) {
         print_ALKS();
        if (test_output>4) cout <<"output file alk_all.out is done"<<endl;
      }
      
      print_M1M2sum();
      if (test_output>4) cout <<"output file m1m2.out is done"<<endl;

      print_season_overlap();
      if (test_output>4) cout <<"output file season_overlap.out is done"<<endl;
    }

   print_summary();
   if (test_output>=1) cout <<"Output file summary.out is done"<<endl;
   
   print_SSB_R();
   //if (test_output>4) cout <<"output file ssb_r.out is done"<<endl;

   print_F_status_quo();
   if (OP_output==1) cout <<"Output file print_f_status_quo.out is done"<<endl;

   print_objective_func_file();

   if (OP_output==1) {
     print_Config_parms();
     if (OP_output==1) cout <<"Operating configuration file is done"<<endl;

     print_Operating_input();
     if (OP_output==1) cout <<"Operating data files is done"<<endl;
     
     Print_OP_seed();
   }
   
   if (do_prediction_mean!=1 && mcmc!=1) write_warnings();


 }
 
  if (test_output<0) {
     print_catch_data_noise(-test_output,1);
     print_survey_data_noise(-test_output,1);
  }
  
  
  if (test_output>=1) cout <<"REPORT SECTION completed"<<endl;
 

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

RUNTIME_SECTION
  maximum_function_evaluations 4000 

 //convergence_criteria  1E-1 1e-5;

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

TOP_OF_MAIN_SECTION 
  
 arrmblsize=3000000;
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(10000000);
 gradient_structure::set_CMPDIF_BUFFER_SIZE(600000);
 gradient_structure::set_MAX_NVAR_OFFSET(2000);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(3000);

 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中
 // 中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中中

FINAL_SECTION

 
 if (test_output>=1) cout <<"FINAL_SECTION initiated"<<endl;
 
 if (do_prediction_mean==1) write_predict_output();

 cout << endl <<"Successfull completion, HURRAH - HURRAH " << endl;
 
