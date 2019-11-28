// Units:
//  fish numbers (e.g. N, C and Recruits) in 1000 fish (more correctly,
//      all numbers get the same unit as catch numbers)
//  Mean weights (e.g. West, Weca) in Kg
//  Biomass, SSB in ton  (more correctly the product of fish numbers and mean weights)
//  Lengths (predator and prey length) in mm
//  Consumption per fish in Kg
//  Biomass of other food in ton (more correctly, the same unit as the
//      product of fish numbers and consumption)

 // いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい
 // いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい

GLOBALS_SECTION
 #include <admodel.h>
 #include <time.h>

 // test output
 #define TRACE(obj) cout<<"line "<<__LINE__<<", "<<#obj" =\n "<<obj<<endl<<endl;

 //long int seed=10;    // for random number generation

 adstring_array species_names; // vector to species names
 adstring_array area_names;   // vector to area names


 // いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい
 // いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい

DATA_SECTION

 int i;
 int do_optim;
 !! do_optim=1; // make optimization
 
 !! for (i=1;i<argc;i++) {
 !!   if (strstr(argv[i],"-maxfn") !=NULL)  do_optim=0;
 !! }

 int y;      // Year
 int q;      // quarter
 int s;      // species
 int a;      // age
 int d;      // division (SMS area)

 int pred;    // predator
 int prey;    // prey

 init_int test_output       // Produce test output (0=no 1..9.. increasing output)

 !! if (test_output>0) cout<<"SMS Operating model, December 2018, using ADMB version 12.0 "<<endl;
 !! if (do_optim==0 && test_output>0) cout<<"No ";
 !! if (test_output>0) cout<<"Optimization will be done"<<endl;

 !! if (test_output==1) cout<<"test.output: "<<test_output<<endl;


 init_int output       // Produce output
 !! if (test_output==1) cout<<"output: "<<output<<endl;

 init_int MSFD            // do calc of indicatores (e.g. from the MSFD)
 !! if (test_output==1) cout<<"indicator: "<<MSFD<<endl;

 init_int fy                // first year
 !! if (test_output==1) cout<<"first.year: "<<fy<<endl;
 
 init_int fy_out                // first year
 !! if (test_output==1) cout<<"first.year.out: "<<fy_out<<endl;
 
 init_int ly            // last year
 !! if (test_output==1) cout<<"last.year: "<<ly<<endl;
  int iter;

  
 !! ad_comm::change_datafile_name("op_config.dat");
 
 init_int multi             // multispecies mode (multi>=1) or single species(multi=0)
 !! if (test_output==2) cout <<"ms.mode: "<<multi<<endl;
 
  init_int use_Nbar          // Stock numbers used in fitting suitability paramters.
                            // (0: Use N; 1 Use mean N)
 !! if (test_output==2) cout <<"use.Nbar: "<< use_Nbar<<endl;

 init_int area_FM
 !! if (test_output==2) cout <<"area.FM: "<<area_FM<<endl;

 init_int no_areas;
 !! if (test_output==2) cout<<"no.areas: "<<no_areas<<endl;

 init_int nsp               // number of species
 !! if (test_output==2) cout<<"no.species: "<<nsp<<endl;

 init_int fa                // first age all species
 !! if (test_output==2) cout<<"first.age: "<<fa<<endl;

 init_int max_a             // overall maximum age, all species
 !! if (test_output==2) cout<<"max.age: "<<max_a<<endl;

 int fq                     // first season (quarter)
 !! fq=1;

 init_int lq                // last season (quarter)
 !! if (test_output==2) cout<<"last.quarter: "<<lq<<endl;

 init_int recq              // season for recruitment
 !! if (test_output==2) cout<<"rec.season: "<<recq<<endl;

 ivector faq(fq,lq)         // first age by quarter
 !! faq=fa;
 !! for (q=fq;q<recq;q++) faq(q)=fa+1;


 init_matrix tmp(1,nsp,1,5)// various species information on "input" format
                            // 1. last age by species
                            // 2. first age where catch data are used (else F=0 assumed)
                            // 3. plus group, 0=no plus group, 1=plus group
                            // 4. predator species, 0=no, 1=yes VPA, 2=yes "other predator"
                            // 5. prey species, 0=no, 1=yes

 !! if (test_output==2) cout<<"species.info:"<<endl<<tmp<<endl;

 // species information, re-organised
 ivector la(1,nsp)          // last age by species
 ivector nplus(1,nsp)       // plus group, 0=no plus group, 1=plus group
 ivector is_pred(1,nsp)     // is the species a predator? 0=no, 1=yes, 2=yes "other predator"
 ivector is_prey(1,nsp)     // is the species a prey? 0=no, 1=yes

 int nOthPred              // no of "other predators"
 !! nOthPred=0;
 int npr                   // number of predators=VPA preadtors + Other predators
 !! npr=0;

 int first_VPA              // species number for the first species where the stock numbers are estimated
 int first_VPA_prey         // species number for the first species where the stock numbers are estimated and which is a prey
 int nprey;
 

 // Reorganise a bit

 !! for (s=1;s<=nsp;s++) {
 !!  la(s)=int(tmp(s,1));
 !!  if (s<=npr) la_pred(s)=la(s);
 !!  nplus(s)=int(tmp(s,3));
 !!  is_pred(s)=int(tmp(s,4));
 !!  if (is_pred(s)==2) nOthPred=nOthPred+1;
 !!  if (is_pred(s)==1 || is_pred(s)==2) npr=npr+1;
 !!  is_prey(s)=int(tmp(s,5));
 !! }

 !! first_VPA=nOthPred+1;
 !! if (test_output==1) cout<<"no. of species: "<<nsp<<"  no. of predators: "<<npr<<"  no. of other predators:"<<nOthPred<<endl;
 !! if (npr==0) npr=1;

 !!  if (multi<1) is_prey=0;
 !!  nprey=sum(is_prey);



 ivector la_pred(1,npr)              // last age by predator species
 !! for (s=1;s<=npr;s++) la_pred(s)=la(s);
 ivector la_VPA(first_VPA,nsp)      // last ages for VPA species
 ivector cfa(first_VPA,nsp)         // first age where catch data are used (else F=0 assumed)

 !! first_VPA_prey=0;
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   la_VPA(s)=la(s);
 !!   cfa(s)=int(tmp(s,2));
 !!   if (first_VPA_prey==0 && is_prey(s)==1) first_VPA_prey=s;
 !! }
 !! if (test_output==2) cout<<"first VPA species no.:"<<first_VPA<<"  first VPA species as prey:"<<first_VPA_prey<<endl;

 // First and last ages in calculation of average F by species
 init_imatrix avg_F_ages(first_VPA,nsp,1,2)
 !! if (test_output==2) cout<<"avg.F.ages: "<<endl<<avg_F_ages<<endl;

 //Species.n model alfa  beta std
 init_matrix SSB_R_in(first_VPA,nsp,1,6)
 !! if (test_output==2) cout<<"Init SSB_R_in:"<<endl<<SSB_R_in<<endl;
 ivector SSB_Rec_model(first_VPA,nsp)
 vector SSB_Rec_hockey_breakpoint(first_VPA,nsp)
 vector SSB_R_alfa(first_VPA,nsp)
 vector SSB_R_beta(first_VPA,nsp)
 vector SSB_R_s2(first_VPA,nsp)
 vector SSB_R_sd(first_VPA,nsp)
 matrix Rec_add_inf(first_VPA,nsp,1,2)

 !! for (s=first_VPA;s<=nsp;s++) {
 !!    SSB_Rec_model(s)=int(SSB_R_in(s,1));
 !!    if (SSB_Rec_model(s)==100) SSB_Rec_hockey_breakpoint(s)=SSB_R_in(s,3); else SSB_Rec_hockey_breakpoint(s)=0.0;
 !!    SSB_R_alfa(s)=SSB_R_in(s,2);
 !!    SSB_R_beta(s)=SSB_R_in(s,3);
 !!    SSB_R_sd(s)=SSB_R_in(s,4);
 !!    SSB_R_s2(s)=SSB_R_in(s,4)*SSB_R_in(s,4);
 !!    Rec_add_inf(s,1)=SSB_R_in(s,5);
 !!    Rec_add_inf(s,2)=SSB_R_in(s,6);
 !!    //if (do_optim==1) SSB_R_s2(s)=0.0;
 !! }
 !! if (multi==0) ad_comm::change_datafile_name("just_one.in");

 init_int simple_ALK   // use age-size-key for calculation of M2 values
    //  0=Use only one sizegroup per age (file lsea.in or wsea.in)
    //  1=Use size distribution per age (file ALK_all.in)
   !! if (test_output==2 && multi>0) cout <<"simple.ALK: "<<simple_ALK<<endl;

 init_int consum_op   // Use food-rations from input values or from size and regression parameters (option consum)
     //  0=Use input values by age (consum.in)
     // 1=use weight at size and regression parameters (op_consum_ab.in)
   !! if (test_output==2 && multi>0) cout <<"consum: "<<consum_op<<endl;

 init_ivector size_selection(1,npr) // 0: no size selection,
                                    // 1: log-normal distribution,
                                    // 11:normal distribution size selection, but sum of all prey sizes used in likelihood
                                    // 3: Gamma distributed size selection,
                                    // 4:no size selection, but within observed range defined from input quantile regression parameters (intercept an slope) ,
 !! if (test_output==2 && multi>0) cout <<"size.selection: "<<endl<<size_selection<<endl;

 init_vector prey_pred_size_fac(1,npr)    // Max prey size/pred size ratio for inclusion in M2 calc
 !! if (test_output==2 && multi>0) cout<<"max.prey.pred.size.fac: "<<endl<<prey_pred_size_fac<<endl;


 init_3darray vulnerability(1,no_areas,1,npr,first_VPA,nsp)  // prey vulnerability
 !! if (test_output==2 && multi==2) cout<<"vulnerability:"<<endl<<vulnerability<<endl;

 init_4darray season_overlap(1,no_areas,1,npr,fq,lq,0,nsp)  // seasonal predator prey overlap
  !! if (test_output==2 && multi==2) cout<<"season_overlap:"<<endl<<season_overlap<<endl;

 init_ivector size_select_model(1,npr)    // size selection model
 !! if (test_output==2 && multi==2) cout<<"size_select_model:"<<endl<<size_select_model<<endl;

 init_vector pref_size_ratio(1,npr)
 !! if (test_output==2 && multi==2) cout<<"pref_size_ratio:"<<endl<<pref_size_ratio<<endl;

 init_vector var_size_ratio(1,npr)
 !! if (test_output==2 && multi==2) cout<<"var_size_ratio:"<<endl<<var_size_ratio<<endl;

 init_vector pref_size_ratio_correction(1,npr)
 !! if (test_output==2 && multi==2) cout<<"pref_size_ratio_correction:"<<endl<<pref_size_ratio_correction<<endl;

 init_vector prey_size_adjustment(first_VPA,nsp)
 !! if (test_output==2 && multi==2) cout<<"prey_size_adjustment:"<<endl<<prey_size_adjustment<<endl;

 init_vector other_suit_slope(1,npr)
 !! if (test_output==2 && multi==2) cout<<"other_suit_slope:"<<setprecision(3)<<setfixed()<<endl<<other_suit_slope<<endl;

 init_matrix min_pred_prey_size_ratio(1,npr,first_VPA,nsp)             // min predator prey size ratio
 !! if (test_output==2 && multi==2) cout<<"min_pred_prey_size_ratio:"<<endl<<min_pred_prey_size_ratio<<endl;

 init_matrix max_pred_prey_size_ratio(1,npr,first_VPA,nsp)             // max predator prey size ratio
 !! if (test_output==2 && multi==2) cout<<"max_pred_prey_size_ratio:"<<endl<<max_pred_prey_size_ratio<<endl;

 // from sms.tpl  init_3darray size_range_a_b(1,4,1,Mnpr,first_VPA,Mindex)
 init_3darray size_range_a_b(1,4,1,npr,first_VPA,nsp)
 !! if (test_output==2 && multi==2) for (i=1;i<=4;i++) cout<<"size_range_a_b:"<<endl<<"("<<i<<")"<<endl<<size_range_a_b(i)<<endl;

 init_imatrix pred_area_present(1,npr,1,no_areas)
 !! if (test_output==2 && multi==2) cout<<"pred_area_present:"<<endl<<pred_area_present<<endl;

 init_3darray pred_prey_comb(1,no_areas,1,npr,first_VPA,nsp)
 !! if (test_output==2 && multi==2) cout<<"pred_prey_comb:"<<endl<<pred_prey_comb<<endl;

 init_matrix other_food(1,no_areas,1,npr)
 !! if (test_output==2 && multi==2) cout<<"other_food:"<<setprecision(0)<<endl<<other_food<<endl;

 init_vector other_food_size(1,npr)          // size of predator for constant input available food
 !! if (test_output==2 && multi==2) cout<<"other_food_size:"<<setprecision(4)<<endl<<other_food_size<<endl;
 !! if (test_output==2) cout<<"op_config.dat completed"<<endl;


 !! ad_comm::change_datafile_name("op.dat");

 init_vector skip6(1,6)

 init_imatrix OP_wsea(1,2,1,nsp)    // first year and last year for calc of OP mean weight in the sea
 !! if (test_output>=1) cout<<"OP.wsea: "<<endl<<OP_wsea <<endl;

 init_imatrix OP_M(1,2,first_VPA,nsp)    // first year and last year for calc of OP M and OP M1
 !! if (test_output>=1) cout<<"OP.M: "<<endl<<OP_M <<endl;

 init_imatrix OP_propmat(1,2,first_VPA,nsp)    // first year and last year for calc of OP proportion mature
 !! if (test_output>=1) cout<<"OP.propmat: "<<endl<<OP_propmat <<endl;

 init_imatrix OP_F(1,2,first_VPA,nsp)    // first year and last year for calc of OP F and OP exploitation pattern
 !! if (test_output>=1) cout<<"OP.F: "<<endl<<OP_F <<endl;

 init_imatrix OP_weca(1,2,first_VPA,nsp)    // first year and last year for calc of OP mean weight in the catch
 !! if (test_output>=1) cout<<"OP.weca: "<<endl<<OP_weca <<endl;

 init_imatrix OP_prop_landed(1,2,first_VPA,nsp)    // first year and last year for calc of OP proportion of the catach landed
 !! if (test_output>=1) cout<<"OP.prop.landed: "<<endl<<OP_prop_landed <<endl;

  init_imatrix OP_stock_dist(1,2,first_VPA,nsp)    // first year and last year for calc of OP stock distribution
 !! if (test_output>=1) cout<<"OP.stock.dist: "<<endl<<OP_stock_dist <<endl;

  init_imatrix OP_consum(1,2,1,npr)    // first year and last year for calc of food ration (consum)
 !! if (test_output>=1) cout<<"OP.consum: "<<endl<<OP_consum <<endl;

 init_matrix growth_model(1,5,first_VPA,nsp)    //  Growth model 0=no growth; food; 1=density dependent, 10=saithe special growth (Xochitl Cormon)
 !! if (test_output==2 && multi==2) cout<<"growth.model: "<<endl<<growth_model <<endl;
 int do_growth_all 
 int do_growth_type_1
 int do_growth_type_10
 ivector do_growth_sp(first_VPA,nsp)
 !! do_growth_all=0; do_growth_type_1=0;
 !! for (s=first_VPA;s<=nsp;s++) if (growth_model(1,s)>0) {
 !!   do_growth_sp(s)=1; do_growth_all+=1; 
 !!  if (growth_model(1,s)==1) do_growth_type_1=1; if (growth_model(1,s)==10) do_growth_type_10=1; 
 !! } else do_growth_sp(s)=0;
 !! if (do_growth_all>=1) do_growth_all=1;

 !! if (test_output>=1) cout <<"growth.model:"<<endl<<growth_model<<endl;

 init_vector stochastic_recruitment(first_VPA,nsp)
 int rec_covar
 !! rec_covar=0;
 int rec_readIn
 !! rec_readIn=0;
 !! if (test_output>=1) cout <<"stochastic.recruitment:"<<endl<<stochastic_recruitment<<endl;
 !! for (s=first_VPA;s<=nsp;s++) if (stochastic_recruitment(s)==2) rec_covar=1; 
 !! for (s=first_VPA;s<=nsp;s++) if (stochastic_recruitment(s)==3) rec_readIn=1; 
 !! if (rec_covar==1) for (s=first_VPA;s<=nsp;s++) stochastic_recruitment(s)=2; 
  
 ivector do_Equisim(first_VPA,nsp)
 !! for (s=first_VPA;s<=nsp;s++) { if (stochastic_recruitment(s)>=20 && stochastic_recruitment(s)<30) do_Equisim(s)=1; else do_Equisim(s)=0; }
  
 init_vector rec_noise_trunc_a(first_VPA,nsp)
 !!  if (test_output>=1) cout <<"rec.noise.trunc_a:"<<endl<<rec_noise_trunc_a<<endl;
 init_vector rec_noise_trunc_b(first_VPA,nsp)
  !!  if (test_output>=1) cout <<"rec.noise.trunc_b:"<<endl<<rec_noise_trunc_b<<endl;
  
 !! for (s=first_VPA;s<=nsp;s++) {
 !!  if (rec_noise_trunc_a(s)>= rec_noise_trunc_b(s) && rec_noise_trunc_a(s)!=0 && rec_noise_trunc_b(s)!=0) {
 !!   cout<<"ERROR: Recruitment noise truncation. First value must be smaller than second value\nProgram stopped"<<endl;
 !!   exit(9);
 !! }}
 
 // Reorganise a bit
 matrix rec_noise_trunc(first_VPA,nsp,1,2);
 !! for (s=first_VPA;s<=nsp;s++) {
 !!  rec_noise_trunc(s,1)=rec_noise_trunc_a(s); rec_noise_trunc(s,2)=rec_noise_trunc_b(s);
 !!  if (stochastic_recruitment(s)==0) {rec_noise_trunc(s)=0; }
 !! }
 !!  if (test_output>=1) cout <<"rec.noise.trunc:"<<endl<<rec_noise_trunc<<endl;

 init_vector recruit_adjust(first_VPA,nsp);          // factor for adjustment of future recruits
 !! if (test_output>=1) cout<<"recruit_adjust:"<<recruit_adjust<<endl;

 init_ivector recruit_adjust_CV(first_VPA,nsp);          // adjustment of future recruits by to take account of the diffrence between mean and median in a log-normal distribution
 !! if (test_output>=1) cout<<"recruit_adjust_CV:"<<recruit_adjust_CV<<endl;

 init_ivector F_or_C(first_VPA,nsp)  // use F value or Catch at age to update N
 !! if (test_output==1) cout<<"F_or_C:"<< F_or_C<<endl;
 !! if (do_optim>=1) for (s=first_VPA;s<=nsp;s++) F_or_C(s)=0;
 int anyF;
 !! anyF=0; if (do_optim==0) for (s=first_VPA;s<=nsp;s++) if (F_or_C(s)==1 || F_or_C(s)==11) anyF=1;
 int anyF31;
 !! anyF31=0; if (do_optim==0) for (s=first_VPA;s<=nsp;s++) if (F_or_C(s)==31) anyF31=1;

 int allF11;
 !! allF11=1; if (do_optim==0 || anyF==1) for (s=first_VPA;s<=nsp;s++) if (F_or_C(s)!=11) allF11=0;
 int anyC;
 !! anyC=0; if (do_optim==0) for (s=first_VPA;s<=nsp;s++) if (F_or_C(s)==2 || F_or_C(s)==22) anyC=1;

 init_int max_M2_iteration  // Max M2 iterations
 !! if (test_output==1 && multi>0) cout <<"max.M2.iteration: "<<max_M2_iteration<<endl;
 !! if (use_Nbar==0) {
 !!  max_M2_iteration=1;
 !!  if (test_output==1 && multi>0) cout <<"max.M2.iteration changed to  "<<max_M2_iteration<<" due to use of Nbar=0"<<endl;
 !! }
 
 init_number max_M2_sum2    // max M2 sum of square for two iterations
 !! if (test_output==1 && multi>0) cout<<"max.M2.sum2: "<<max_M2_sum2<<endl;


  init_imatrix OP_other_N(1,2,1,nOthPred)    // first year and last year for calc of OP mean stock number of other predators
 !! if (test_output==1 && multi>0) cout<<"OP_other_N: "<<endl<<OP_other_N <<endl;

 // other predator change. 
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
 !! if (test_output==1 && multi>0) {
 !!   cout<<"other_pred_N_change:"<<endl<<other_pred_N_change<<endl;
 !!   cout<<"other_pred_N_first_year:"<<endl<<other_pred_N_first_year<<endl;
 !!   cout<<"other_pred_N_last_year:"<<endl<<other_pred_N_last_year<<endl;
 !! }

 !! if (test_output>=1) cout<<"op_dat.dat completed"<<endl;

 !! if (MSFD==1)  ad_comm::change_datafile_name("op_msfd.dat");
 !! else ad_comm::change_datafile_name("just_one.in");
 

 init_int do_bio_demer
 init_imatrix  bio_demer_ages(1,2,1,nsp)    //first an last age to be included
 // !! cout<<"bio_demer_ages:"<<endl<<bio_demer_ages<<endl;
 
 init_int do_bio_small
 init_imatrix  bio_small_ages(1,2,1,nsp)    //first an last age to be included

 init_int do_bio_pelag
 init_imatrix  bio_pelag_ages(1,2,1,nsp)    //first an last age to be included

 init_int do_bio_forage
 init_imatrix  bio_forage_ages(1,2,1,nsp)    //first an last age to be included

  
  init_int do_M2_bar
   init_imatrix  M2_bar_ages(1,2,first_VPA,nsp)    //first an last age to be included

  init_int  do_mean_weight_at_age   
  init_ivector  mean_weight_at_age_sp(first_VPA,nsp)

  init_int  do_community_mean_age   
  init_ivector  community_mean_weight_sp(first_VPA,nsp)
  
  init_int  do_mean_weight_C   
  init_ivector  mean_weight_C_sp(first_VPA,nsp)

  init_int  do_community_mean_C   
  init_ivector  community_mean_weight_C_sp(first_VPA,nsp)

  init_int  do_community_F   
  init_ivector  community_F_sp(first_VPA,nsp)

  init_int  do_community_M   
  init_ivector  community_M_sp(first_VPA,nsp)

  init_int  do_life_expectancy  
  init_ivector  life_expectancy_age(first_VPA,nsp)
  vector firstN(first_VPA,nsp)  
  
  init_int  do_community_life_expectancy  
  init_ivector   community_life_expectancy_age(first_VPA,nsp)
  init_vector   community_life_expectancy_weighting(first_VPA,nsp)
  vector community_life_expectancy(fy_out,ly)
  
  init_int  do_size_spectra   
  init_ivector  size_spectra_sp(1,nsp)

  init_int  do_LFI   
  init_ivector  LFI_sp(1,nsp)
  init_ivector  LFI_age(1,nsp)
 
 
 // #################### Read data files
  
  
 // co-variance matrix of log stock recruits
 int tmpA;
 !! if (rec_covar==0) { ad_comm::change_datafile_name("just_one.in"); tmpA= -1;}
 !! else { ad_comm::change_datafile_name("covariance_rec.in"); tmpA=nsp;}
 init_matrix coVarianceRec(first_VPA,tmpA,first_VPA,nsp);
 !! if (test_output==3 && rec_covar==1) cout<<"covariance_rec.in:"<<endl<<coVarianceRec<<endl;
  matrix chol_coVarianceRec(first_VPA,tmpA,first_VPA,nsp);
 !! if (rec_covar==1) {
 !!   chol_coVarianceRec=trans(choleski_decomp(coVarianceRec)); 
 !! if (test_output==3 && rec_covar==1) cout<<"(choleski_decomp of Rec Covariance:"<<endl<<chol_coVarianceRec<<endl;    
 !! }

 //  Read S/R residuals from file
 !! if (rec_readIn==0) { ad_comm::change_datafile_name("just_one.in"); tmpA= -1;}
 !! else { ad_comm::change_datafile_name("op_ssb_rec_residuals.in"); tmpA=nsp;}
 init_ivector SSB_Rec_nobs(first_VPA,tmpA)
 init_matrix SSB_Rec_residuals(first_VPA,tmpA,1,SSB_Rec_nobs)
 !! if (test_output==3 && rec_readIn==1) cout<<"op_ssb_rec_residuals.in:"<<endl<<SSB_Rec_residuals<<endl;
 
 // Equisim parameter estimates
 !! if (sum(do_Equisim)==0) ad_comm::change_datafile_name("just_one.in"); else  ad_comm::change_datafile_name("op_eqsim.in");
 init_3darray SSB_Rec_EquiSim(first_VPA,nsp,1,3,1,4)  // 1-4: alfa, beta, CV, weight.

 !! if (sum(do_Equisim)==0) ad_comm::change_datafile_name("just_one.in"); else  ad_comm::change_datafile_name("op_eqsim_stoch.in");
 init_int nEquiSim;
 int iEquiSim   // counter
 !! iEquiSim=0;
 init_3darray SSB_Rec_EquiSim_stoch(first_VPA,nsp,1,nEquiSim,1,4)  // 1-4: alfa, beta, CV, model.
 
  
 !! ad_comm::change_datafile_name("op_seed.in");
 init_int seed
 !! if (test_output==3) cout<<"seed:"<<endl<<seed<<endl;

 int last_age
 !! last_age=max_a;
 
 !!  if (multi>=1) ad_comm::change_datafile_name("op_length_weight_relations.in");
 init_matrix L_W_ab(1,nsp,1,2); //W=a*L**b relations, a and b by species
 !! if (test_output==3 ) cout<<"Length-weight relations parameters from file length_weight_relations.in:"<<endl<<L_W_ab<<endl;


 !! ad_comm::change_datafile_name("op_n.in");
 init_matrix N_init(first_VPA,nsp,fa,last_age)      
 !! if (test_output==3) cout<<"N_init:"<<endl<<N_init<<endl;
 
 !! if (nOthPred==0 || multi==0 ) {
 !!   ad_comm::change_datafile_name("just_one.in");
 !!   last_age=-1;
 !! }
 !! else ad_comm::change_datafile_name("op_other_n.in");
 init_4darray N_other_input(fq,lq,1,no_areas,1,nOthPred,fa,max_a)
 !! if (test_output==3 && nOthPred>0) cout<<"N_other_input:"<<endl<<N_other_input<<endl;

 !! last_age=max_a;
 !! if (no_areas==1) {last_age=-1; ad_comm::change_datafile_name("just_one.in");}
 !! else {ad_comm::change_datafile_name("op_stock_distribution.in");}
 init_4darray N_dist(fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && no_areas>1) cout<<"N_dist:"<<endl<<N_dist<<endl;

 !! if (anyC==1) { ad_comm::change_datafile_name("op_c.in"); last_age=max_a;}
 !! else {ad_comm::change_datafile_name("just_one.in"); last_age=-1; }
 init_5darray C_input(fy,ly,fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && anyC==1) cout<<"C_input:"<<endl<<C_input<<endl;

  //!! if (anyC==1) { ad_comm::change_datafile_name("OP_M1M2.in"); last_age=max_a;}
 // !! else {ad_comm::change_datafile_name("just_one.in"); last_age=-1; }
 // init_4darray M1M2(fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 // !! if (test_output==3) cout<<"M1M2:"<<endl<<M1M2<<endl;


 !! if (anyF==1) {ad_comm::change_datafile_name("op_f.in"); last_age=max_a;}
 !! else { ad_comm::change_datafile_name("just_one.in"); last_age=-1; }
 int lyF;
 !! lyF=ly;
 !! if (allF11==1) lyF=fy; 
 init_5darray F_input(fy,lyF,fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && anyF==1) cout<<"F_input:"<<endl<<F_input<<endl;
 
 !! if (do_optim==1 || anyF31==1) {ad_comm::change_datafile_name("op_exploitation.in"); last_age=max_a;}
 !! else { ad_comm::change_datafile_name("just_one.in"); last_age=-1; }
 init_4darray exploitation_pattern(fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && do_optim==1) cout<<"exploitation_pattern:"<<endl<<exploitation_pattern<<endl;
 
 !! ad_comm::change_datafile_name("op_prop_landed.in"); last_age=max_a;
 init_4darray prop_landed(fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)
 !! if (test_output==3) cout<<"proportion landed:"<<endl<<prop_landed<<endl;

 !! ad_comm::change_datafile_name("op_wcatch.in");
 init_4darray weca_input(fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)
 !! if (test_output==3) cout<<"weca_input:"<<endl<<weca_input<<endl;

 !! ad_comm::change_datafile_name("op_wsea.in");
 init_3darray west_input(fq,lq,1,nsp,fa,max_a)
 !! if (test_output==3) cout<<"west_input:"<<endl<<west_input<<endl;

 !! ad_comm::change_datafile_name("op_propmat.in");
 init_3darray propmat(fq,lq,first_VPA,nsp,fa,max_a)
 !! if (test_output==3) cout<<"propmat:"<<endl<<propmat<<endl;

 !! ad_comm::change_datafile_name("op_m.in");
 init_3darray M_fixed(fq,lq,first_VPA,nsp,fa,max_a)
 !! if (test_output==3) cout<<"M:"<<endl<<M_fixed<<endl;



 !! if (multi==0) {
 !!   ad_comm::change_datafile_name("just_one.in");
 !!   last_age=-1;
 !! } else last_age=max_a;
 
 !! if (multi==2 && consum_op==0) ad_comm::change_datafile_name("op_consum.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 init_4darray consum_input(fq,lq,1,no_areas,1,npr,fa,last_age)
 !! if (test_output==3 && multi==2 && consum_op==0) cout<<"consum_input:"<<endl<<consum_input<<endl;

  
  !! if (multi==2) ad_comm::change_datafile_name("op_n_proportion_m2.in");
 init_3darray n_proportion_m2(fq,lq,first_VPA,nsp,fa,max_a)
 !! if (test_output==3 && multi==2) cout<<"n_proportion_m2:"<<endl<<n_proportion_m2<<endl;

  //*********************************************************************************************
 !! if (multi==2 && consum_op==1) ad_comm::change_datafile_name("op_consum_ab.in");
 !! else ad_comm::change_datafile_name("just_one.in");
 init_4darray consum_ab(fq,lq,1,no_areas,1,npr,1,2)
 !! if (test_output==3 && multi==2 && consum_op==1) cout<<"Consumption parameters from file consum_ab.in:"<<endl<<setprecision(3)<<consum_ab<<endl;

 //*********************************************************************************************

 !! if (multi==2) ad_comm::change_datafile_name("op_size.in");
 init_4darray        size_sea_input(fq,lq,1,no_areas,1,nsp,fa,last_age)
 !! if (test_output==3 && multi==2) cout<<"size_sea_input:"<<endl<<size_sea_input<<endl;

 init_4darray prey_w(fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && multi==2) cout<<"prey_w:"<<endl<<prey_w<<endl;

  init_4darray lsea(fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && multi==2) cout<<"length at age:"<<endl<<lsea<<endl;

 !! if (multi==2) ad_comm::change_datafile_name("op_m1.in");
 init_4darray M1(fq,lq,1,no_areas,first_VPA,nsp,fa,last_age)
 !! if (test_output==3 && multi==2) cout<<"M1:"<<endl<<M1<<endl;

 !! change_datafile_name("op_reference_points.in");
 init_matrix reference_points(first_VPA,nsp,1,4)
 !! if (test_output==3 && multi==2) cout<<" reference_points:"<<endl<< reference_points<<endl;

 !! if (do_growth_type_1==0) {last_age=-1; ad_comm::change_datafile_name("just_one.in");}
 !! else if (do_growth_type_1==1) {last_age=max_a; ad_comm::change_datafile_name("op_growth_type1.in"); }
 
 
 init_3darray  growth_type1_regr(first_VPA,nsp,fa,last_age,1,3); //  Growth in forecast
 !! if (do_growth_type_1==3 && multi==2 && test_output==3) {
 !!  cout<<"growth_type1.in, Regression parameters:"<<endl<<setprecision(3)<<endl<<growth_type1_regr<<endl;
 !! }
 init_3darray  growth_type1_in_year(first_VPA,nsp,fa,last_age,fq,lq); //  Growth in forecast
 !! if (do_growth_type_1==1 && multi==2 && test_output==3) {
 !!  cout<<"growth_type1.in, in year increment ratio:"<<endl<<setprecision(3)<<endl<<growth_type1_in_year<<endl;
 !! }
 init_3darray  growth_ratio_weca_west(first_VPA,nsp,fq,lq,fa,last_age); //  ratio between west and weca
 !! if (do_growth_type_1==1 && multi==2 && test_output==3) {
 !!  cout<<"growth_type1.in, ratio between weca and west"<<endl<<setprecision(2)<<endl<<growth_ratio_weca_west<<endl;
 !! }
 init_matrix  growth_min_w(first_VPA,nsp,fa,last_age); //  minimum west
 !! if (do_growth_type_1==1 && multi==2 && test_output==3) {
 !!  cout<<"Growth, minimum weight at age Q1"<<endl<<setprecision(2)<<endl<<growth_min_w<<endl;
 !! }
 init_matrix  growth_max_w(first_VPA,nsp,fa,last_age); //  minimum west
 !! if (do_growth_type_1==1 && multi==2 && test_output==3) {
 !!  cout<<"Growth, maximum weight at age Q1"<<endl<<setprecision(2)<<endl<<growth_max_w<<endl;
 !! }

 // Saithe model
 !! if (do_growth_type_10==0) {last_age=-1; ad_comm::change_datafile_name("just_one.in");}
 !! else if (do_growth_type_10==1) { ad_comm::change_datafile_name("op_growth_type10.in"); }
 
 init_matrix  growth_type10_regr(first_VPA,nsp,1,5); //  Growth in forecast
 !! if (do_growth_type_10==1 && multi==2 && test_output==3) {
 !!  cout<<"growth_type10.in, Regression parameters:"<<endl<<setprecision(3)<<endl<<growth_type10_regr<<endl;
 !! }


 !! if (test_output==3) cout<<"all data file completed"<<endl;

 int repetition;
 !! if (anyF31==1) ad_comm::change_datafile_name("op_multargetf.in"); else ad_comm::change_datafile_name("just_one.in");
 init_int no_rep;
 init_matrix Fcombination(1,no_rep,first_VPA,nsp); // combination of F
 
 
 !! if (do_optim==1 || anyF31==1) ad_comm::change_datafile_name("op_trigger.dat"); else ad_comm::change_datafile_name("just_one.in");
 
 init_int fy_op;
 init_int ly_op;
 !! if (test_output==4 ) cout<<"First and last year in optimization:  "<<fy_op<<" ,"<<ly_op<<endl;


  init_int first_no_run
 !! if (test_output==4) cout<<"first.run.no: "<<first_no_run<<endl;

 
 init_int first_no_iter
 !! if (test_output==4) cout<<"first.no.iter: "<<first_no_iter<<endl;

 init_int no_iter
 !! if (test_output==4) cout<<"no.iter: "<<no_iter<<endl;
 
 int last_no_iter
 !! last_no_iter=first_no_iter+no_iter-1;     

 init_ivector HCR(first_VPA,nsp);
 !! if (test_output==4 ) cout<<"HCR:  "<<HCR<<endl;

 //init_vector constantF(first_VPA,nsp)
 //!! if (test_output==4) cout<<"constantF:"<<constantF<<endl;

 init_vector T1(first_VPA,nsp);
 !! if (test_output==4) cout<<"T1:  "<<T1<<endl;

 init_vector T2(first_VPA,nsp);
 !! if (test_output==4) cout<<"T2:  "<<T2<<endl;

 init_vector Flow_target(first_VPA,nsp);
 !! if (test_output==4) cout<<"Flow_target:  "<<Flow_target<<endl;
 init_vector Fhigh_target(first_VPA,nsp);
 !! if (test_output==4) cout<<"Fhigh_target: "<<Fhigh_target<<endl;
 init_vector Finit_target(first_VPA,nsp);
 !! if (test_output==4) cout<<"Finit_target: "<<Finit_target<<endl;
 init_ivector phase_target(first_VPA,nsp);
 !! if (test_output==4) cout<<"phase_target:"<<phase_target<<endl;
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   if (Fhigh_target(s)< Flow_target(s)) { cout<<"ERROR in op_trigger.dat, Fhigh must be >= than Flow"<<endl;  exit(9); }
 !!   if (!(Fhigh_target(s)>= Finit_target(s) || Flow_target(s)>= Finit_target(s))  ) { cout<<"ERROR in op_trigger.dat, Fint must be tween Flow and Fhigh"<<endl;  exit(9); }
 !! }  
 
 init_vector Flow_slope(first_VPA,nsp);
 !! if (test_output==4) cout<<"Flow_slope:  "<<Flow_slope<<endl;
 init_vector Fhigh_slope(first_VPA,nsp);
 !! if (test_output==4) cout<<"Fhigh_slope: "<<Fhigh_slope<<endl;
 init_vector Finit_slope(first_VPA,nsp);
 !! if (test_output==4) cout<<"Finit_slope: "<<Finit_slope<<endl;
 init_ivector phase_slope(first_VPA,nsp);
 
 !! if (test_output==4) cout<<"phase_slope:"<<phase_slope<<endl;
 !! for (s=first_VPA;s<=nsp;s++) {
 !!   if (Fhigh_slope(s)< Flow_slope(s)) { cout<<"ERROR in op_trigger.dat, Fhigh_slope must be >= than Flow_slow"<<endl;  exit(9); }
 !!   if (!(Fhigh_slope(s)>= Finit_slope(s) || Flow_slope(s)>= Finit_slope(s))  ) { cout<<"ERROR in op_trigger.dat, Fint_slope must be tween Flow and Fhigh"<<endl;  exit(9); }
 !! } 
  
 !! Flow_slope=log(Flow_slope); Fhigh_slope=log(Fhigh_slope); Finit_slope=log(Finit_slope);
 
  init_vector low_SSB50(first_VPA,nsp);
 !! if (test_output==4) cout<<"low_SSB50:  "<<low_SSB50<<endl;
 init_vector high_SSB50(first_VPA,nsp);
 !! if (test_output==4) cout<<"high_SSB50: "<<high_SSB50<<endl;
 init_vector init_SSB50(first_VPA,nsp);
 !! if (test_output==4) cout<<"init_SSB50: "<<init_SSB50<<endl;
 init_ivector phase_SSB50(first_VPA,nsp);
 !! if (test_output==4) cout<<"phase_SSB50:"<<phase_SSB50<<endl;

  init_vector low_S1(first_VPA,nsp);
 !! if (test_output==4) cout<<"low_S1:  "<<low_S1<<endl;
 init_vector high_S1(first_VPA,nsp);
 !! if (test_output==4) cout<<"high_S1: "<<high_S1<<endl;
 init_vector init_S1(first_VPA,nsp);
 !! if (test_output==4) cout<<"init_S1: "<<init_SSB50<<endl;
 init_ivector phase_S1(first_VPA,nsp);
 !! if (test_output==4) cout<<"phase_S1:"<<phase_S1<<endl;

 init_ivector penalty_use(first_VPA,nsp);
 !! if (test_output==4) cout<<"use penalty:  "<<penalty_use<<endl;
 init_vector penalty_limit(first_VPA,nsp);
 !! if (test_output==4) cout<<"limit: "<<penalty_limit<<endl;
 init_vector penalty_SSB75(first_VPA,nsp);
 !! if (test_output==4) cout<<"SSB75: "<<penalty_SSB75<<endl;
 init_vector penalty_factor(first_VPA,nsp);
 !! if (test_output==4) cout<<"penalty factor: "<<penalty_factor<<endl;

 ivector penalty_number(first_VPA,nsp);
 vector penalty_S1(first_VPA,nsp);
 !! for (s=first_VPA;s<=nsp;s++)  penalty_S1(s)=penalty_limit(s)*log(3.0)/(penalty_SSB75(s)-penalty_limit(s));

  
 init_vector YieldWeight(first_VPA,nsp);
 !! if (test_output==4) cout<<"YieldWeight:"<<YieldWeight<<endl;
 
 init_int use_price;
 !! if (test_output==4) cout<<"use_price:"<<use_price<<endl;
  
 !! if (test_output==4) cout<<"op_trigger.dat completed"<<endl;
 
 !! ad_comm::change_datafile_name("op_price.in");
  
 init_3darray price(1,no_areas,first_VPA,nsp,fa,max_a)
 !! if (test_output==4) cout<<"price:"<<endl<<price<<endl;
 
 LOCAL_CALCS
  for (s=first_VPA;s<=nsp;s++) {
    if (HCR(s)==1 || HCR(s)==2 || HCR(s)==3 || HCR(s)==11 || HCR(s)==22 || HCR(s)==33) {phase_SSB50(s)=-1; phase_S1(s)=-1;}
    if (HCR(s)==1 || HCR(s)==2 || HCR(s)==4 || HCR(s)==11 || HCR(s)==22 || HCR(s)==44) {phase_slope(s)=-1; }
    if (                          HCR(s)==4 || HCR(s)==44) {phase_slope(s)=-1; }
  }
  //if (test_output==4) {
  //  cout<<"phase_target:"<<phase_target<<endl<<
  //        "phase_slope:"<<phase_slope<<endl<<
  //        "phase_SSB50:"<<phase_SSB50<<endl;
  //}
 END_CALCS

  // !! ad_comm::change_datafile_name("environment.in");
 matrix environment(fy,ly,1,2);
 // !! cout<<"environment:"<<endl<<setfixed()<<setprecision(3)<<setw(11)<<environment<<endl;

 3darray rn(first_no_iter,last_no_iter,first_VPA,nsp,fy,ly+2);  // random numbers for SSB/R noise

 int ly_plus_1
 !! ly_plus_1=ly+1;

 matrix M2bar(fy_op,ly_op,first_VPA,nsp)
 vector bio_demer(fy_op,ly_op)
 vector bio_small(fy_op,ly_op) 
 vector bio_pelag(fy_op,ly_op) 
 vector bio_forage(fy_op,ly_op) 
 vector comm_Fland(fy_op,ly_op) 
 vector comm_Fall(fy_op,ly_op) 
 vector comm_M(fy_op,ly_op) 
 vector comm_M2(fy_op,ly_op)


 matrix life_expectancy(first_VPA,nsp,fy_op,ly_op) 
 
 matrix mw_C(fy_out,ly,first_VPA,nsp)
 matrix tot_C(fy_out,ly,first_VPA,nsp)
 vector mw_C_community(fy_out,ly)
 
 vector LFI(fy_op,ly_op) 
 matrix penalty_y(fy_out,ly,first_VPA,nsp);
 !! penalty_y=0;
 
 vector penalty(first_VPA,nsp);

 number sum_yield_value;
 number sum_yield_global; 
 !! if (test_output>0) cout<<"End of DATA SECTION reached:"<<endl;
  
  
PARAMETER_SECTION
  
 !! ofstream parexp("par_exp.out",ios::out);
 !! parexp<<"par species"<<endl;
  
  init_bounded_number_vector Ftarget(first_VPA,nsp,Flow_target,Fhigh_target,phase_target);
  !! for (s=first_VPA;s<=nsp;s++) Ftarget(s)=Finit_target(s);
  !! for (s=first_VPA;s<=nsp;s++) if (phase_target(s)>0) parexp<<"Ftarget "<<s<<endl;

  init_bounded_number_vector log_Fslope(first_VPA,nsp,Flow_slope,Fhigh_slope,phase_slope);
  !! for (s=first_VPA;s<=nsp;s++) log_Fslope(s)=Finit_slope(s);
  !! for (s=first_VPA;s<=nsp;s++) if (phase_slope(s)>0) parexp<<"log_Fslope "<<s<<endl;
  
  init_bounded_number_vector SSB50(first_VPA,nsp,low_SSB50,high_SSB50,phase_SSB50);
  !! for (s=first_VPA;s<=nsp;s++) SSB50(s)=init_SSB50(s);
  !! for (s=first_VPA;s<=nsp;s++) if (phase_SSB50(s)>0) parexp<<"SSB50 "<<s<<endl;

  init_bounded_number_vector S1(first_VPA,nsp,low_S1,high_S1,phase_S1);
  !! for (s=first_VPA;s<=nsp;s++) S1(s)=init_S1(s);
  !! for (s=first_VPA;s<=nsp;s++) if (phase_S1(s)>0) parexp<<"S1 "<<s<<endl;

 
 
  objective_function_value f
  
  vector Factual(first_VPA,nsp);

  4darray     west(fy,ly_plus_1,fq,lq,1,nsp,fa,max_a)  // to allow growth
  5darray     weca(fy,ly_plus_1,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a) // to allow growth
  5darray size_sea(fy,ly_plus_1,fq,lq,1,no_areas,1,nsp,fa,max_a) // to allow growth
  5darray   consum(fy,ly,fq,lq,1,no_areas,1,npr,fa,max_a) // to allow growth and variable food intake
  5darray  N_other(fy,ly,fq,lq,1,no_areas,1,nOthPred,fa,max_a)  // to allow change in population size, growth and variable food intake
   
  4darray N_global(fy,ly_plus_1,fq,lq,first_VPA,nsp,fa,max_a)  
  matrix N_dll(first_VPA,nsp,fa,max_a)       // potential dll
  //dll_matrix F_dll(first_VPA,nsp,fa,max_a)       // potential dll
  
  matrix SSB(fy,ly_plus_1,first_VPA,nsp)
  matrix TSB(fy,ly_plus_1,first_VPA,nsp)
  matrix Fbar(fy,ly_plus_1,first_VPA,nsp)
  // in DATA section matrix M2bar(fy_op,ly_op_1,first_VPA,nsp)
  matrix yield_global(fy,ly,first_VPA,nsp)
  matrix CWsum_global(fy,ly,first_VPA,nsp)
  //matrix yield_value(fy_op,ly_op,first_VPA,nsp)
  matrix yield_value(fy,ly,first_VPA,nsp)
  matrix deadM2w(fy,ly,first_VPA,nsp)
  matrix deadMw(fy,ly,first_VPA,nsp)

  5darray    N(fy,ly_plus_1,fq,lq,1,no_areas,1,nsp,fa,max_a)
  5darray Nbar(fy,ly,fq,lq,1,no_areas,1,nsp,fa,max_a)
  5darray NbarStom(fy,ly,fq,lq,1,no_areas,1,nsp,fa,max_a)
  5darray    F(fy,ly,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)
  5darray    M(fy,ly,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)
  5darray   M2(fy,ly,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)
  5darray    Z(fy,ly,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)
  5darray    C(fy,ly,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a)

  4darray yield(fy,ly,fq,lq,1,no_areas,first_VPA,nsp)
  4darray CWsum(fy,ly,fq,lq,1,no_areas,first_VPA,nsp)

  4darray    part_M2(1,npr,fa,max_a,first_VPA,nsp,fa,max_a)    // partial M2
  3darray   deadM1(fy,ly,first_VPA,nsp,fa,max_a)
  3darray   deadM2(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    deadM(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    deadF(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    deadZ(fy,ly,first_VPA,nsp,fa,max_a)

  3darray   annoM1(fy,ly,first_VPA,nsp,fa,max_a)
  3darray   annoM2(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    annoM(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    annoZ(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    annoF(fy,ly,first_VPA,nsp,fa,max_a)
  3darray    annoC(fy,ly,first_VPA,nsp,fa,max_a)

  3darray    mean_weca(fy,ly,first_VPA,nsp,fa,max_a)

  matrix old_M2(first_VPA,nsp,fa,max_a)
 
  sdreport_vector avg_yield(first_VPA,nsp)
  !! for (s=first_VPA;s<=nsp;s++) parexp<<"avg_yield "<<s<<endl;

  sdreport_vector yield_ly(first_VPA,nsp)
  !! for (s=first_VPA;s<=nsp;s++) parexp<<"yield_ly "<<s<<endl;

  sdreport_vector avg_SSB(first_VPA,nsp)
  !! for (s=first_VPA;s<=nsp;s++) parexp<<"avg_SSB "<<s<<endl;

  sdreport_vector SSB_ly(first_VPA,nsp)
  !! for (s=first_VPA;s<=nsp;s++) parexp<<"SSB_ly "<<s<<endl;
  !! parexp.close();

 !! if (test_output>0) cout<<"End of PARAMETER SECTION reached:"<<endl;
 
INITIALIZATION_SECTION
  Ftarget 0.3

PRELIMINARY_CALCS_SECTION
 if (test_output>0) cout<<"Start of PRELIMINARY_CALCS_SECTION reached:"<<endl;
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

  random_number_generator rng(seed);
  //random_number_generator rng((unsigned)time(0));
  //cout<<"Random seed: "<<(unsigned)time(0)<<endl;
  //cout<<"Random seed: "<<(unsigned) clock()<<endl;
  

  rn.fill_randn(rng);
  // truncate noise
  for (i=first_no_iter;i<=last_no_iter;i++) {
    for (s=first_VPA;s<=nsp;s++)  if (stochastic_recruitment(s)!=3) {
      if (rec_noise_trunc(s,1)!=0 && rec_noise_trunc(s,2)!=0) {
        for (y=fy;y<=ly+2;y++) {
           while (rn(i,s,y) <rec_noise_trunc(s,1) || rn(i,s,y)>rec_noise_trunc(s,2)) rn(i,s,y)=randn(rng);
        }
      } else rn(i,s)=0.0;  // no noise
    }

    if (rec_readIn)  for (s=first_VPA;s<=nsp;s++)  if (stochastic_recruitment(s)==3) {
      for (y=fy;y<=ly+2;y++) {
        rn(i,s,y)=SSB_Rec_residuals(s,1+floor(randu(rng)*SSB_Rec_nobs(s)));    // uniform distribution  [0,1] times nobs
      }
    }
  }
       
 
 for (i=first_no_iter;i<=last_no_iter;i++) {
    for (s=first_VPA;s<=nsp;s++)  {
      // cout<<"iter="<<i<<" s="<<s<<" min rec noise:"<<min(rn(i,s))<<" max noise:"<<max(rn(i,s))<<endl;
    } 
  } 
  //cout<<"CHECK consum_op:"<<consum_op<<endl; 
  //cout<<"CHECK growth_model saithe:"<<growth_model(1,21)<<endl;
  //cout<<"CHECK other_pred_N_change:"<<other_pred_N_change(17)<<endl;
  for (y=fy;y<=ly+1;y++) {
    west(y)=west_input;
    weca(y)=weca_input;
    if (multi==2 ) if (y<=ly) for (q=fq;q<=lq;q++) N_other(y,q)=N_other_input(q);
    if (multi==2 ) size_sea(y)=size_sea_input;
    if (y<=ly) for (q=fq;q<=lq;q++) {
      if (multi==2 ) for (d=1;d<=no_areas;d++) {
        for (s=1;s<=npr;s++) {
          for (a=faq(q);a<=la(s);a++) {
            if (consum_op==1) consum(y,q,d,s,a)= consum_ab(q,d,s,1)* pow(west(y,q,s,a),consum_ab(q,d,s,2));
            else consum(y,q,d,s,a)=consum_input(q,d,s,a);
  }}}}}
  //cout<<"CHECK west 2015, Q4: "<<west(2015,4,17)<<endl;
  //cout<<"CHECK consum_ab, Q4: "<<consum_ab(4,1,17)<<endl;
  //cout<<"CHECK consum 2015, Q4: "<<consum(2015,4,1,17)<<endl;
  //cout<<"CHECK size_sea 2015, Q4: "<<size_sea(2015,4,1,17)<<endl;
   
  if (multi==2 ) { 
    ofstream oth("op_other_sp.out",ios::out);
    oth<<"Species.n Year Quarter Age    N west consum"<<endl;
    y=fy;
    for (q=fq;q<=lq;q++) {
      for (d=1;d<=no_areas;d++) {
        for (s=1;s<=nOthPred;s++) {
          for (a=faq(q);a<=la(s);a++) {
            oth<<s<<" "<<y<<" "<<q<<" "<<" "<<a<<" "<<N_other(fy,q,d,s,a)<<" "<<west_input(q,s,a)<<" "<<consum_input(q,d,s,a)<<endl;
    }}}}
    oth.close();
  }

 ofstream res("op_average_val.out",ios::out);
 res.close();

 ofstream res2("op_indicator_system_avg.out",ios::out);
 res2 <<"run iter  bio.demer bio.small bio.pelag bio.forage comm.Fland comm.Fall comm.M comm.M2 comm.life.expec LFI mweca"<<endl;  
 res2.close();
 
 ofstream res3("op_indicator_species_avg.out",ios::out);
 res3 <<"run iter  life.expectancy"<<endl; 
 res3.close();
 life_expectancy=0;
 
 //ofstream testOut("OP_test.out",ios::out);
 //testOut.close();

 if (test_output>0) cout<<"End of PRELIMINARY CALC SECTION reached:"<<endl;

PROCEDURE_SECTION

 int M2iter,r,s,y,q,a;
 dvariable tmp,penal;
 int cur_phase;
 
 cur_phase=current_phase();
 penalty=0.0;
 
 penalty_number=0; // outputs only
 sum_yield_value=0;
 sum_yield_global=0; 

 
 for (iter=first_no_iter;iter<=last_no_iter;iter++) for (r=1;r<=no_rep;r++) {    // for combinations of F, othetwise no_rep=1 

   if (no_iter>1) cout<<"iter: "<<iter<<"  "; if (no_rep>1) cout<<"r: "<<r<<" out of: "<<no_rep<<endl; 
   //Initialization
   N_global(fy,fq)=N_init;
   if (do_optim==1) iEquiSim=0;
   for (y=fy;y<=ly;y++) {
     if (do_growth_all==1 && y>fy) calc_growth(y);  // Growth has to be calculated before spawning
     calc_SSB(y);  //spawning is allways in the first quarter
     if (do_optim==1) calc_optimal_F(y,first_VPA,nsp); else init_F(y,r);
     for (q=fq;q<=lq;q++) {
       // cout<<"############################"<<endl<<"y:"<<y<<" q:"<<q<<endl;
       if (do_growth_all==1 && y>fy) calc_growth_10(y,q);  // Growth has to be calculated before spawning
       if (q==recq) SSB_recruit(y);
       distribute_stock(y,q);
       for (d=1;d<=no_areas;d++) {
          //cout<<"############"<<endl<<"y:"<<y<<" q:"<<q<<" d:"<<d<<endl;
         if (multi==0) {                        // Single species mode
           M(y,q,d)=M_fixed(q);
           calc_F(y,q,d);
           calc_Z(y,q,d);
           get_N_bar_at_age(y,q,d);             // Calc N within the period
         }
         else {                                 // Multi species mode
           tmp=100.0; M2iter=0;
           M2(y,q,d)=0.0;
           while ((tmp>max_M2_sum2) && (M2iter<max_M2_iteration)) {
           //while ((iter<=max_M2_iteration)) {
             M2iter++;
             calc_F(y,q,d);
             calc_Z(y,q,d);    
             get_N_bar_at_age(y,q,d);       // Calc N within the period
             old_M2=M2(y,q,d);
             if (do_growth_all==1 && y>fy &M2iter>1) calc_growth(y);
             calc_M2(y,q,d);
             tmp=sum(square(M2(y,q,d)-old_M2));
             cout<<"iteration:"<<setprecision(8)<<M2iter<<" tmp: "<<setprecision(8)<<tmp<<"  max_M2_sum2:"<<max_M2_sum2<<"   M2iter:"<<M2iter<<endl;
           }
           calc_Z(y,q,d);                       // update Z with the latest estimate of M2
           get_N_bar_at_age(y,q,d);             // Calc N within the period
         }     // end multi species mode
       }      // end area loop
       get_N_global_at_age(y,q); //calc N for the next period (q=q+1 or y=y+1 and q=1)
     }       // end quarter loop
     calc_C_yield(y);
     calc_yield_value(y);
     if (do_optim==1 && y>=fy_op && y<=ly_op) {
       for (s=first_VPA;s<=nsp;s++) {
         if (phase_target(s)>0 || phase_slope(s)>0 || phase_SSB50(s)>0 || phase_S1(s)>0) if (phase_target(s)<=cur_phase) {
           if (use_price==1) {    
             //calc_yield_value(y); 
             f-=yield_value(y,s); 
             sum_yield_value+=value(yield_value(y,s)); 
           }
           else { 
             f-=YieldWeight(s)*yield_global(y,s); 
             sum_yield_global+=value(YieldWeight(s)*yield_global(y,s));
           }
           if (penalty_use(s)>0) { // use penalty
              if (penalty_use(s)==1) {   // logistic
                  penal= 1.0-(1.0/(1+exp(penalty_S1(s)-penalty_S1(s)/penalty_limit(s)*SSB(y,s))));
                  if (penal > 0.01) penalty_number(s)+=1;  // just book-keeping
                  penal=penal*penalty_factor(s);
              } else  if (penalty_use(s)==2) {  // simple power function
                  if (penalty_limit(s)>SSB(y,s)) {
                     penal= penalty_factor(s)*pow((penalty_limit(s)-SSB(y,s))/1000,penalty_SSB75(s));
                     penalty_number(s)+=1;
                  } else penal=0;
              }  
              
              //cout<<"sp:"<<s<<"  SSB:"<<SSB(y,s)<<" penalty:"<<penal<<endl;
              if (y>=fy_out) { penalty_y(y,s)=value(penal);  penalty(s)+=value(penal); }  // just book-keeping
              f+=penal;  //Optimization with penalty
           } else penal=0;
         }
       } // end species optimazation
     }  // end optimization
    
    
     //cout<<"CHECK  age 3, Q4 "<<"y:"<<y<<" Hake N:"<<N_other(y,4,1,17,3)<<" Saithe: size:"<<size_sea(y,4,1,21,3)<<" consum:"<<consum(y,4,1,21,3)<<endl;
       
   }  // end year-loop
   if (do_optim==1) {
      if (sum(penalty_use)>0) {
        cout<<setprecision(0)<<setfixed()<<"pen:"<<penalty/(ly_op-fy_op+1)<<" all:"<<sum(penalty)/(ly_op-fy_op+1)<<endl;
        cout<<setprecision(0)<<setfixed()<<"  n:"<<penalty_number<<endl;
      }

      cout<<setprecision(2)<<setfixed()<<"tgt:"<<Ftarget<<setprecision(0)<<"  obj func: "<<f/(ly_op-fy_op+1);
      
      if (sum(penalty_use)>0 && sum(penalty)>0.1 ) {
         if (use_price==1) cout<<" ratio obj/penal:"<<setprecision(2)<<sum_yield_value/sum(penalty)<<endl;
         else  cout<<" ratio obj/penal:"<<setprecision(2)<<sum_yield_global/sum(penalty)<<endl; 
      } else cout<<endl;
   }
   if (sd_phase()) for (s=first_VPA;s<=nsp;s++) {
     yield_ly(s)+=yield_global(ly_op,s);
     SSB_ly(s)=SSB(ly_op,s);
     avg_yield(s)=0.0;
     avg_SSB(s)=0.0;
     for (y=fy_op;y<=ly_op;y++) {
       avg_yield(s)+=yield_global(y,s);
       avg_SSB(s)+=SSB(y,s);
     }  
     avg_yield(s)=avg_yield(s)/(ly_op-fy_op+1);
     avg_SSB(s)=avg_SSB(s)/(ly_op-fy_op+1);
   }
   
   if (no_rep>1) {
     print_OP_Fcombinations(r);
      if (MSFD==1 ) {
        print_avg_indicators_system(r);
        // print them
      }
    }   
 }  // end F combination loop


FUNCTION void calc_growth_10(int y, int q);  // Xochitl option
 int s,a,d;
 dvariable k,l;
 d=1; //one area only so far;
 for (s=first_VPA;s<=nsp;s++) {
   if (growth_model(1,s)==10) {
     //cout<<"NOP TSB:"<<TSB(y-1,growth_type10_regr(s,5))<<" "<<TSB(y,growth_type10_regr(s,5))<<" beta:"<<growth_type10_regr(s,4)<<endl;
     //cout<<"length-weight: "<<L_W_ab(s)<<endl;
     k= growth_type10_regr(s,3) + (TSB(y,growth_type10_regr(s,5))+TSB(y-1,growth_type10_regr(s,5)))/2 * growth_type10_regr(s,4); 
     //cout<<"y:"<<y<<"  k:"<<k<<endl; 
     for (a=faq(q);a<=la(s);a++) {
                       // L=Linf*(1-exp(-k*(a-t0)))
       size_sea(y,q,1,s,a)= growth_type10_regr(s,1) * (1.0 - exp(-k * (a+(q-1)*0.25 - growth_type10_regr(s,2))));
       //cout<<"age size_sea y: "<<y<<" q:"<<q<<" a:"<<a<<" Length:"<<size_sea(y,q,d,s,a);
       
       size_sea(y,q,d,s,a)=L_W_ab(s,1)*pow(size_sea(y,q,d,s,a)*10.0,L_W_ab(s,2)); // from length to weight
  
       //cout<<" weight:"<<size_sea(y,q,d,s,a)<<endl;
       west(y,q,s,a)=size_sea(y,q,d,s,a);
       if (consum_op==1) consum(y,q,d,s,a)= consum_ab(q,d,s,1)* pow(west(y,q,s,a),consum_ab(q,d,s,2));
       weca(y,q,d,s,a)=size_sea(y,q,d,s,a);  
     }
     //if (q==4) cout<<"size_sea y: "<<y<<" q:"<<q<<size_sea(y,q,1,s)<<endl;
   }  
 }


FUNCTION void calc_growth(int y);
 int s, gs, q, qq;
 dvariable stock_N;
 for (s=first_VPA;s<=nsp;s++) {
 
   if (growth_model(1,s)==1) {
     // calc stock density 1 January
     stock_N=0;
     for (gs=growth_model(4,s);gs<=growth_model(5,s);gs++) stock_N+= sum(N(y,fq,1,gs)); // ERROR AREA 1 ONLY !!!!!!!!!!!
     // Calc weight 1. January      4darray   west(fy,ly_plus_1,fq,lq,first_VPA,nsp,fa,max_a)
     //cout<<"y:"<<y<<" s:"<<s<<" Clup_N: "<<stock_N<<endl;

     a=growth_model(2,s); // first age
     if (a==fa) qq=recq; else qq=fq;
     west(y,qq,s,a)= growth_type1_regr(s,a,1)+growth_type1_regr(s,a,3)*stock_N;

     a=growth_model(2,s)+1; // second age
     west(y,fq,s,a)= growth_type1_regr(s,a,1)+growth_type1_regr(s,a,2)*west(y-1,qq,s,a-1)+growth_type1_regr(s,a,3)*stock_N;

     for (a=growth_model(2,s)+2;a<=growth_model(3,s);a++) {
       west(y,fq,s,a)= growth_type1_regr(s,a,1)+growth_type1_regr(s,a,2)*west(y-1,fq,s,a-1)+growth_type1_regr(s,a,3)*stock_N;
       //if (s==3) cout<<"y:"<<y<<" s:"<<s<<" a:"<<a<<"w(t-1):"<<west(y-1,fq,s,a-1)<<" w(t):"<<west(y,fq,s,a)<<endl;
     }

    // adjust Q1 weight (min max)
     q=fq;
     for (a=growth_model(2,s)+1;a<=growth_model(3,s);a++) {
         if (west(y,fq,s,a) < growth_min_w(s,a)) west(y,fq,s,a)=growth_min_w(s,a);
         else if (west(y,fq,s,a) > growth_max_w(s,a)) west(y,fq,s,a)=growth_max_w(s,a);
     }

     // calc W for quarter 2-4
     a=growth_model(2,s); // first age
     if (a==fa) qq=recq; else qq=fq;
     for (q=recq+1;q<=lq;q++) west(y,q,s,a)=west(y,qq,s,a)*growth_type1_in_year(s,a,q);

     for (a=growth_model(2,s)+1;a<=growth_model(3,s);a++) for (q=fq;q<=lq;q++) {
       west(y,q,s,a)=west(y,fq,s,a)*growth_type1_in_year(s,a,q);
     }

     // weight into weca and size_sea
     for (a=growth_model(2,s);a<=growth_model(3,s);a++) for (q=fq;q<=lq;q++) {
        size_sea(y,q,1,s,a)=west(y,q,s,a);    //ERROR AREA 1 only
       weca(y,q,1,s,a)=west(y,q,s,a)*growth_ratio_weca_west(s,q,a);   //     ERROR AREA 1 ONLY !!!!!!!!!!!    5darray   weca(fy,ly_plus_1,fq,lq,1,no_areas,first_VPA,nsp,fa,max_a) // to allow growth
     }
   }
 }

FUNCTION void calc_optimal_F(int y,int first_sp, int last_sp);
 int q,d,s;
 for (s=first_sp;s<=last_sp;s++) {
   switch(HCR(s)) {
    case 1: Factual(s)=Ftarget(s);
            break;
    case 2: if (SSB(y,s)> T2(s)) Factual(s)=Ftarget(s);
            else if (SSB(y,s)>= T1(s)) Factual(s)=Ftarget(s)*(SSB(y,s)-T1(s))/(T2(s)-T1(s));
            else Factual(s)=0.0;
            break;
    case 22: if (TSB(y,s)> T2(s)) Factual(s)=Ftarget(s);
            else if (TSB(y,s)>= T1(s)) Factual(s)=Ftarget(s)*(TSB(y,s)-T1(s))/(T2(s)-T1(s));
            else Factual(s)=0.0;
            break;
    case 3: if (SSB(y,s)> T2(s)) Factual(s)=Ftarget(s)+(SSB(y,s)-T2(s))/T2(s)*exp(log_Fslope(s));
            else if (SSB(y,s)>= T1(s)) Factual(s)=Ftarget(s)*(SSB(y,s)-T1(s))/(T2(s)-T1(s));
            else Factual(s)=0.0;
            break;
    case 33: if (TSB(y,s)> T2(s)) Factual(s)=Ftarget(s)+(TSB(y,s)-T2(s))/T2(s)*exp(log_Fslope(s));
            else if (TSB(y,s)>= T1(s)) Factual(s)=Ftarget(s)*(TSB(y,s)-T1(s))/(T2(s)-T1(s));
            else Factual(s)=0.0;
            break;
   case 4:  Factual(s)=Ftarget(s)/(1+exp(S1(s)-S1(s)/SSB50(s)/1000*SSB(y,s)));
            break;
   case 44: Factual(s)=Ftarget(s)/(1+exp(S1(s)-S1(s)/SSB50(s)/1000*TSB(y,s)));
            break;
   }

   for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++) {
     F(y,q,d,s)=exploitation_pattern(q,d,s)*Factual(s);
   }
   Fbar(y,s)=Factual(s);
   // if (s==26) cout<<"y:"<<y<<" s:"<<s<<" Fbar:" <<setprecision(3)<<setfixed()<<Fbar(y,s)<<"  TSB: "<<TSB(y,s)<<"  SSB: "<<SSB(y,s)<<endl;
 }

FUNCTION void init_F(int y,int r);
 int q,d,s,a ;
 for (s=first_VPA;s<=nsp;s++) {
   if (F_or_C(s)==1) { for(q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  F(y,q,d,s)=F_input(y,q,d,s);}
   else  if (F_or_C(s)==11) { for(q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  F(y,q,d,s)=F_input(fy,q,d,s);}
   else  if (F_or_C(s)==31) { 
     if (HCR(s)==1) {
       for(q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  for (a=faq(q);a<=la(s);a++) F(y,q,d,s,a)=exploitation_pattern(q,d,s,a)*Fcombination(r,s);
       Fbar(y,s)=Fcombination(r,s);
     }
     else if (HCR(s)==2 || HCR(s)==22) {
       Ftarget(s)=Fcombination(r,s);
       calc_optimal_F(y,s,s); // get F level
       //cout<<"s:"<<s<<" HCR:"<<HCR(s)<<" TSB:"<<TSB(y,s)<<" SSB:"<<SSB(y,s)<<" Ftarget:"<<Ftarget(s)<<" Fcatual:"<<Factual(s)<<endl;
     }
   } // else cout<<"Something is wrong with the option value of for F_or_C:"<< F_or_C(s)<<endl;
 }

 
FUNCTION void calc_F(int y, int q, int d);
 int s;
 if (do_optim==0) for (s=first_VPA;s<=nsp;s++) if (F_or_C(s)==2) calc_F_from_Catch(y,q,d,s);



FUNCTION void calc_anno_mortality_and_weight(int my_fy, int my_ly);
 int y,q,d,s,a;
 
 deadM2w=0; deadMw=0;
 for (y=my_fy;y<=my_ly;y++) {
   deadM1(y)=0;
   deadM2(y)=0;
   deadM(y)=0;
   deadF(y)=0;
   deadZ(y)=0;
   mean_weca(y)=0;
   annoC(y)=0;

   for(q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  for (s=first_VPA;s<=nsp;s++) for (a=faq(q);a<=la(s);a++) {
     if (multi==2 ) {
       deadM1(y,s,a)+=Nbar(y,q,d,s,a)*M1(  q,d,s,a);
       deadM2(y,s,a)+=Nbar(y,q,d,s,a)*M2(y,q,d,s,a);
       deadM2w(y,s)+=Nbar(y,q,d,s,a)*M2(y,q,d,s,a)*size_sea(y,q,d,s,a);
      }
      deadM(y,s,a)+=Nbar(y,q,d,s,a)* M(y,q,d,s,a);
      deadMw(y,s)+=Nbar(y,q,d,s,a)*M(y,q,d,s,a)*size_sea(y,q,d,s,a);
      deadF(y,s,a)+=Nbar(y,q,d,s,a)* F(y,q,d,s,a);
      deadZ(y,s,a)+=Nbar(y,q,d,s,a)* Z(y,q,d,s,a);

      mean_weca(y,s,a)+=weca(y,q,d,s,a)*C(y,q,d,s,a);
      annoC(y,s,a)+= value(C(y,q,d,s,a));
    }
   for (s=first_VPA;s<=nsp;s++) for (a=fa;a<=la(s);a++) {
      if (annoC(y,s,a)<1E-3) annoC(y,s,a)=0;
     if (multi==2 ) {
       annoM1(y,s,a)= deadM1(y,s,a)/deadZ(y,s,a)*annoZ(y,s,a);
       annoM2(y,s,a)= deadM2(y,s,a)/deadZ(y,s,a)*annoZ(y,s,a);
     }
     annoM(y,s,a)= deadM(y,s,a)/deadZ(y,s,a)*annoZ(y,s,a);
     annoF(y,s,a)= deadF(y,s,a)/deadZ(y,s,a)*annoZ(y,s,a);
     if (annoC(y,s,a)>0) mean_weca(y,s,a)=mean_weca(y,s,a)/annoC(y,s,a); else mean_weca(y,s,a)=0.0;
   }
 }
  
 
 //FUNCTION void write_M1M2_output();
 // adstring txt;
 // ofstream out("OP_M1M2.out",ios::out);
 // out<<"# Natural mortaleties";
 // y=ly;
 // for(q=fq;q<=lq;q++) {
 //   out<<"# quarter:"<<q;
 //   for (d=1;d<=no_areas;d++) {
 //     if (no_areas>1) out<<" area:"<<d<<endl; else out<<endl;
 //     for (s=first_VPA;s<=nsp;s++){
 //       for (a=faq(q);a<=la(s);a++) {
 //         out<<setprecision(5)<<setfixed()<< M2(y,q,d,s,a) +M1(q,d,s,a)<<" ";
 //       }
 //       out<<"species:"<<species_names(s)<<endl;
 //     }
 //   }
 // }
 // out.close();
 // if (test_output==10) cout<<"file OP_N.out is done"<<endl;
 
   
FUNCTION void write_N_output();
 adstring txt;
 ofstream out("op_n.out",ios::out);
 out<<"# Stock number 1. January, except for recruits which are by recruiting season";
 for (y=fy+1;y<=ly+1;y++) {
   out<<" Year:"<<y<<endl;
   for (s=first_VPA;s<=nsp;s++){
    N_global(y,fq,s,fa)=N_global(y,recq,s,fa);  // copy recruits to first quarter
    out<<setprecision(3)<<setfixed()<<N_global(y,fq,s)<<"  # "<<species_names(s)<<endl;
   }
 }
 
 out.close();
 if (test_output==10) cout<<"file op_n.out is done"<<endl;
 
 
 
  
FUNCTION void write_N_output_dll();
 y=ly+1;
 for (s=first_VPA;s<=nsp;s++){
    N_global(y,fq,s,fa)=N_global(y,recq,s,fa);  // copy recruits to first quarter
    for (a=fa ;a<=la(s);a++) {
       N_dll(s,a)= N_global(y,fq,s,fa);
   }
 }
 

FUNCTION void calc_SSB(int y);
 if (fa >0 && y>fy) SSB_recruit(y);
 for (int s=first_VPA;s<=nsp;s++) {
   SSB(y,s)=sum(elem_prod(elem_prod(N_global(y,fq,s),west(y,fq,s)),propmat(fq,s)));
   TSB(y,s)=sum(elem_prod(N_global(y,fq,s),west(y,fq,s)));
 }

FUNCTION void get_N_global_at_age(int y, int q);
 //calc N for the next period (q=q+1 or y=y+1 and q=1)
 int d,s,a;
 dvar_matrix plusses(first_VPA,nsp,la_VPA-1,la_VPA);
 // calculate N values (excluding recruits)
 //  cout<<"get_N_global_at_age  y:"<<y<<" q:"<<q<<endl;
 plusses=0;
 for (d=1;d<=no_areas;d++) {
   for (s=first_VPA;s<=nsp;s++){
     if (q==lq) { // birthday
       if (do_optim==0) {
         plusses(s,la(s)-1)+= N(y,q,d,s,la(s)-1)*exp(-Z(y,q,d,s,la(s)-1));
         plusses(s,la(s))  += N(y,q,d,s,la(s))  *exp(-Z(y,q,d,s,la(s)));
       }
       for (a=fa;a<la(s);a++)  N(y+1,fq,d,s,a+1)=N(y,q,d,s,a)*exp(-Z(y,q,d,s,a));
       if (nplus(s)==1) N(y+1,fq,d,s,la(s))+=N(y,q,d,s,la(s))*exp(-Z(y,q,d,s,la(s))); // plusgroup
     }
     else {  // quarter step
       for (a=faq(q);a<=la(s);a++) N(y,q+1,d,s,a)=N(y,q,d,s,a)*exp(-Z(y,q,d,s,a));
     }
   }
 }
 if (q<lq) {
   for (s=first_VPA;s<=nsp;s++) {
     N_global(y,q+1,s)=N(y,q+1,1,s);
     for (d=2;d<=no_areas;d++) N_global(y,q+1,s)+=N(y,q+1,d,s);
   }
 } else {  // birthday
   for (s=first_VPA;s<=nsp;s++) {
     N_global(y+1,fq,s)=N(y+1,fq,1,s);
     for (d=2;d<=no_areas;d++) N_global(y+1,fq,s)+=N(y+1,fq,d,s);
   }
   if (fy==ly) {
     calc_SSB(y+1);
     SSB_recruit(y+1);
   }
   // calc annual Z
   if (do_optim==0) for (s=first_VPA;s<=nsp;s++) {
     annoZ(y,s,fa)=log(N_global(y,recq,s,fa)/N_global(y+1,fq,s,fa+1));
     for (a=faq(fq);a<la(s);a++) annoZ(y,s,a)=log(N_global(y,fq,s,a)/N_global(y+1,fq,s,a+1));
     if (nplus(s)==1) {
       annoZ(y,s,la(s)-1)=log(N_global(y,fq,s,la(s)-1)/(N_global(y+1,fq,s,la(s))-plusses(s,la(s))));
       annoZ(y,s,la(s))  =log(N_global(y,fq,s,la(s))  /(N_global(y+1,fq,s,la(s))-plusses(s,la(s)-1)));
     } else {
       annoZ(y,s,la(s))=log(N_global(y,fq,s,la(s))/(plusses(s,la(s))));
     }
   }
 }


FUNCTION void calc_Z(int y, int q, int d);
 if (multi==2) {
   M(y,q,d)=M1(q,d)+M2(y,q,d);
   Z(y,q,d)=M(y,q,d)+F(y,q,d);
 }
 else  Z(y,q,d)=M_fixed(q)+F(y,q,d);
 //cout<<"Z: y:"<<y<<" q:"<<q<<" area:"<<d<<setprecision(3)<<endl<<Z(y,q,d)<<endl;
 

FUNCTION void calc_F_from_Catch(int y, int q, int d, int s);
 dvariable Z,FF,MM,NN,CC;
 int a,i;
 for (a=faq(q);a<=la(s);a++) {
   CC=C_input(y,q,d,s,a);
   if(CC==0.0) F(y,q,d,s,a)=0.0;
   else {
       if (multi==2) MM=M1(q,d,s,a)+M2(y,q,d,s,a);  
       else  MM=M_fixed(q,s,a);
     
     NN=N(y,q,d,s,a);
     // if (s==3 && a==3) cout<<"y:"<<y<<" q:"<<q<<"s:"<<s<<" a:"<<a<<" N:"<<NN<<" M:"<<MM<<" C:"<<CC<<endl;
     if ( CC>NN) {
          cout<<" Something is wrong (input catch > N)"<<endl<<
            " y:"<<y<<" q:"<<q<<" species:"<<s<<" age:"<<a<<" C="<<CC<<" N="<<NN<<endl;
            exit(9);
            F(y,q,d,s,a)=1.0;
      }
      else {
         FF=CC/(NN*(1-exp(-MM/2))/(MM/2));  // initial guess;
         for (i=1;i<=5;i++) {   // Newton iteration
           Z=FF+MM;
           FF+= -((FF*NN*(1-exp(-Z))/Z)-CC) / ((NN*(1-exp(-Z)) + FF*NN*exp(-Z))/Z -FF*NN*(1-exp(-Z))/(Z*Z));
         }
         F(y,q,d,s,a)=FF;
      }
   }
 }

FUNCTION void calc_yield_value(int y);
 int q,d,s;
 yield_value(y)=0.0;
 for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++) for (s=first_VPA;s<=nsp;s++)  {
   yield_value(y,s)+=sum(elem_prod(elem_prod(elem_prod(C(y,q,d,s),weca(y,q,d,s)),prop_landed(q,d,s)),price(d,s)));
 }
 

FUNCTION void calc_C_yield(int y);
 int q,d,s;
 yield_global(y)=0.0;
 CWsum_global(y)=0.0;
  
 for (q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++) for (s=first_VPA;s<=nsp;s++)  {
   C(y,q,d,s)=elem_prod(Nbar(y,q,d,s),F(y,q,d,s));
   CWsum(y,q,d,s)=sum(elem_prod(C(y,q,d,s),weca(y,q,d,s)));
   CWsum_global(y,s)+=CWsum(y,q,d,s);

   yield(y,q,d,s)=sum(elem_prod(elem_prod(C(y,q,d,s),weca(y,q,d,s)),prop_landed(q,d,s)));
   yield_global(y,s)+=yield(y,q,d,s);
 }
 
 //cout<<"Y:"<<y<<"  yield_global "<<endl<<yield_global(y)<<endl;
 //cout<<"Nbar:"<<endl<<Nbar(y)<<endl;
 // cout<<"F:"<<endl<<F(y)<<endl;
 // cout<<"C:"<<endl<<C(y)<<endl;
 //cout<<"yield: "<<endl<<yield(y)<<endl;


FUNCTION void get_N_bar_at_age(int y, int q, int d);
 int s,a;
 // problem division with 0  Nbar(y,q,d)=elem_prod(N(y,q,d),elem_div((1-exp(-Z(y,q,d))),Z(y,q,d)));
 for (s=first_VPA;s<=nsp;s++){
   for (a=faq(q);a<=la(s);a++) {
     if (Z(y,q,d,s,a)>0) Nbar(y,q,d,s,a)=  N(y,q,d,s,a)*(1-exp(-Z(y,q,d,s,a)))/Z(y,q,d,s,a);
     else  Nbar(y,q,d,s,a)=N(y,q,d,s,a); 
     
     //Calc N to be used to estimate M2
     if ( multi>0) {
      if (use_Nbar==0) NbarStom(y,q,d,s,a)= N(y,q,d,s,a)* n_proportion_m2(q,s,a);
      else  NbarStom(y,q,d,s,a)= Nbar(y,q,d,s,a)* n_proportion_m2(q,s,a);
     }
 }}
 //cout<<"Nbar: y:"<<y<<" q:"<<q<<" area:"<<d<<endl<<Nbar(y,q,d)<<endl;


FUNCTION void calc_M2_simple(int y,int q, int d,dvector other_food, dvar_matrix size, dmatrix prey_w, dvar_matrix consum, dvar_matrix Nbar, dvar_matrix& M2);
 int pred,pred_a,prey,prey_a,pred_l,prey_l;
 dvariable pred_size,prey_size;
 dvariable tmp;
 dvar_matrix avail_food(1,npr,fa,max_a);
 M2=0.0;
 avail_food=0.0;

 //cout<<"calc_M2_simple  y:"<<y<<" q:"<<q<<" area:"<<d<<endl;
 //cout<<"size:"<<endl<<size<<endl;
 //cout<<"prey_w:"<<endl<<prey_w<<endl;
 //cout<<"consum:"<<endl<<consum<<endl;
 //cout<<"Nbar:"<<endl<<Nbar<<endl;
 
 for (pred=1;pred<=npr;pred++) if (pred_area_present(pred,d)==1){
  for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
    if (Nbar(pred,pred_a)>0) {
      //Calc available food
        pred_size=size(pred,pred_a);
      for (prey=first_VPA;prey<=nsp;prey++) {
         if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
            prey_size=size(prey,prey_a);
            if  ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                avail_food(pred,pred_a)+=Nbar(prey,prey_a)*prey_w(prey,prey_a)*suit(y,q,d,pred,prey,pred_size,prey_size);
      }}}}
      avail_food(pred,pred_a)+=other_food(pred)*other_suit(pred,pred_size,y,q,d);  // add other food
      tmp=Nbar(pred,pred_a)*consum(pred,pred_a)/avail_food(pred,pred_a);
       //calc M2
      for (prey=first_VPA;prey<=nsp;prey++) {
        if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
            prey_size=size(prey,prey_a);
            if ((pred_size*prey_pred_size_fac(pred))>=prey_size) M2(prey,prey_a)+=tmp*suit(y,q,d,pred,prey,pred_size,prey_size);
      }}}
     }
   }
 }
 //cout<<"avail_food:"<<endl<<avail_food<<endl;
 //cout<<"M2:"<<endl<<setprecision(3)<<M2<<endl;



FUNCTION void calc_M2(int y, int q, int d);
  calc_M2_simple(y,q,d,other_food(d),size_sea(y,q,d),prey_w(q,d),consum(y,q,d),NbarStom(y,q,d),M2(y,q,d));    
   

  
FUNCTION void distribute_stock(int y, int q);
 if (no_areas==1) {
  for (s=first_VPA;s<=nsp;s++) N(y,q,1,s)=N_global(y,q,s);
  if (nOthPred>0)  {
    // update other predator stock N  from input option
    if (multi==2) {
      for (s=1;s<=nOthPred;s++) {
        if (other_pred_N_first_year(s)>0) {
         //if (s==2) cout<<"y:"<< y<<" "<<other_pred_N_first_year(s)<<" "<<other_pred_N_last_year(s)<<" "<<other_pred_N_change(s)<< " "<<endl;
         if ((other_pred_N_first_year(s)<=y) && (other_pred_N_last_year(s)>=y) && (y>fy)) {
           for (q=fq;q<=lq;q++)  N_other(y,q,1,s)=N_other(y-1,q,1,s)*other_pred_N_change(s);
         } else {
           if (y>fy) for (q=fq;q<=lq;q++)  N_other(y,q,1,s)=N_other(y-1,q,1,s);
         }
        } 
      }
    }
    for (q=fq;q<=lq;q++) for (s=1;s<=nOthPred;s++) {
     N(y,q,1,s)=N_other(y,q,1,s); 
     Nbar(y,q,1,s)=N(y,q,1,s);
     NbarStom(y,q,1,s)=N(y,q,1,s);
    }
   }
 }
 else for (int d=1;d<=no_areas;d++) N(y,q,d)=elem_prod(N_global(y,q),N_dist(q,d));     // MANGLER OTHER PREd  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

FUNCTION dvariable suit(int y, int q, int d, int pred,int prey,dvariable pred_size,dvariable prey_size)
 dvariable tmp,size_sel,vul,ratio, log_ratio;

 //cout<<"suit  y:"<<y<<" q:"<<q<<" d:"<<d<<" pred:"<<pred<<" prey:"<<prey<<" ";
 vul=season_overlap(d,pred,q,prey)*vulnerability(d,pred,prey);
  
 ratio=pred_size/prey_size;

 if (size_selection(pred)==0) {         // (uniform) no size selection
  if ((ratio >= min_pred_prey_size_ratio(pred,prey)) && (ratio <= max_pred_prey_size_ratio(pred,prey))) return vul;
  else return 0.0;
 }
 else if (size_selection(pred)==4) {         // (confined uniform) no size selection, but within limits
   log_ratio=log(ratio);
   if ((log_ratio >= size_range_a_b(1,pred,prey)+ size_range_a_b(2,pred,prey)*log(pred_size))
     && (log_ratio <= size_range_a_b(3,pred,prey)+ size_range_a_b(4,pred,prey)*log(pred_size))) { return vul;}
   else { return 0.0;}
 }
 else {           //  size selection
  if (ratio >= min_pred_prey_size_ratio(pred,prey) &&  ratio <= max_pred_prey_size_ratio(pred,prey)){
    if (size_selection(pred)==1  || size_selection(pred)==11 ) { //normal distribution or assymmetric normal distribution
      tmp=log(ratio)-(pref_size_ratio(pred)*prey_size_adjustment(prey)+pref_size_ratio_correction(pred)*log(pred_size));
      return vul*exp(-square(tmp)/(2.0* var_size_ratio(pred)));
    }
  }
  else return 0.0;
 }

FUNCTION dvariable other_suit(int pred,dvariable pred_size,int y, int q, int d)
 return(exp(other_suit_slope(pred)*log(pred_size/other_food_size(pred)))*season_overlap(d,pred,q,0));

FUNCTION dvariable calc_recruit(int s, dvariable ssb, dvariable noise, double adjust);
 dvariable rec;
   //cout<<"y:"<<y<<" SSB:"<<ssb<<" noise:"<<noise<<endl;
    switch(SSB_Rec_model(s)) {
             case 1: return(adjust*SSB_R_alfa(s)*ssb*exp(-SSB_R_beta(s)*ssb)*noise);  // Ricker
                      break;
             case 51: return(adjust*SSB_R_alfa(s)*ssb*exp(-SSB_R_beta(s)*ssb+Rec_add_inf(s,1)*Rec_add_inf(s,2))*noise);  // Ricker
                     break;
             case 52:return(adjust*SSB_R_alfa(s)*ssb*exp(-SSB_R_beta(s)*ssb+Rec_add_inf(s,1)*Rec_add_inf(s,2))*noise);  // Ricker
                     break;
             case 2: return(adjust*SSB_R_alfa(s)*ssb/(1+SSB_R_beta(s)*ssb)*noise);     // Betverton & Holt
                     break;
             case 3: return(adjust*exp(SSB_R_alfa(s))*noise);                 // geometric mean
                     break;
             case 4: if (ssb>=SSB_R_beta(s)) {
                       return(adjust*SSB_R_beta(s)*SSB_R_alfa(s)*noise);
                      }
                      return(adjust*exp(SSB_R_alfa(s))*ssb*noise);
                       break;
             case 10: rec=exp(-2.42285E-5*ssb+0.29133E-5*ssb*environment(y,1)+12.18724);
                       if (rec>SSB_R_alfa(s)) return(SSB_R_alfa(s));
                       else return(rec);
                       break;
              case 11: return(5.84E6*pow(environment(y,2),2)-1.74E8*environment(y,2)+1.33E9);
                       break;
              case 100: if (ssb>SSB_Rec_hockey_breakpoint(s)) {
                         return(adjust*SSB_Rec_hockey_breakpoint(s)*exp(SSB_R_alfa(s))*noise);
                       }
                       else return(adjust*exp(SSB_R_alfa(s))*ssb*noise);
                       break;
             default: return(-1.0);  // error
                      break;
    }

FUNCTION double SSB_recruit(int y);
 int recy,ss,i;
 dmatrix noiseFac(1,1,first_VPA,nsp);
 dmatrix covNoise(1,1,first_VPA,nsp);
 dvariable rec,ssb;

 iEquiSim=(iEquiSim+1); if (iEquiSim>nEquiSim) iEquiSim=1 ;
 
 for (s=first_VPA;s<=nsp;s++){
    recy=y-fa;
    if (recy<fy) recy=fy;
    ssb=SSB(recy,s);
    
    if (stochastic_recruitment(s)==0) {       // determenistic recruitment
      if (recruit_adjust_CV(s)==2) noiseFac(1,s)=exp(SSB_R_s2(s)/2.0);
      else noiseFac(1,s)=1.0;
    }
    else if (stochastic_recruitment(s)==1 ) { // stochastic recruitment
       noiseFac(1,s)=rn(iter,s,y);
       if (recruit_adjust_CV(s)==0) noiseFac(1,s)=exp(SSB_R_sd(s)*noiseFac(1,s));
       else if (recruit_adjust_CV(s)==1) noiseFac(1,s)=exp(SSB_R_sd(s)*noiseFac(1,s)-SSB_R_s2(s)/2.0);
    }
    else if (stochastic_recruitment(s)==2) {  // use covariance matrix
      if (s==first_VPA ) { // calculate the covariance for all species
        for (ss=first_VPA;ss<=nsp;ss++) covNoise(1,ss)=rn(iter,ss,y);
        noiseFac=covNoise*chol_coVarianceRec;
      }
       if (recruit_adjust_CV(s)==0) noiseFac(1,s)=exp(noiseFac(1,s));
       else if (recruit_adjust_CV(s)==1) noiseFac(1,s)=exp(noiseFac(1,s)-coVarianceRec(s,s)/2.0);  // does it make sense ???     
    }
    else if (stochastic_recruitment(s)==3) { // stochastic recruitment with input residual
       noiseFac(1,s)=exp(rn(iter,s,y));
       if (recruit_adjust_CV(s)==1) noiseFac(1,s)=noiseFac(1,s)*exp(-SSB_R_s2(s)/2.0);
    }  
  
  if (stochastic_recruitment(s)<=3)  N_global(y,recq,s,fa)=calc_recruit(s,ssb, noiseFac(1,s), recruit_adjust(s));
   
  else if (stochastic_recruitment(s)==20) {  // Equisim, detetministic
     N_global(y,recq,s,fa)=0.0;
     for (i=1;i<=3;i++){   // the three models
       if (i<=2) {
          SSB_Rec_model(s)=i;
        } 
        else if (i==3) {
          SSB_Rec_model(s)=100;
          SSB_Rec_hockey_breakpoint(s)=SSB_Rec_EquiSim(s,i,2);
        }
        SSB_R_alfa(s)=SSB_Rec_EquiSim(s,i,1);
        SSB_R_beta(s)=SSB_Rec_EquiSim(s,i,2);
       
        if (recruit_adjust_CV(s)==2) noiseFac(1,s)=exp(SSB_Rec_EquiSim(s,i,3)/2.0);
        else noiseFac(1,s)=1.0;
       
        //cout<<"EquiSim "<<species_names(s)<< "model:"<<i<<" alfa:"<<SSB_R_alfa(s)<<" beta:"<< SSB_R_beta(s)<<" break:"<< SSB_Rec_hockey_breakpoint(s)<<" SSB:"<<ssb<<" rec:"<<calc_recruit(s,ssb, noiseFac(1,s), recruit_adjust(s))<<endl;
        N_global(y,recq,s,fa)+=calc_recruit(s,ssb, noiseFac(1,s), recruit_adjust(s))*SSB_Rec_EquiSim(s,i,4);
     }
  }
  else if (stochastic_recruitment(s)==21) {  // Equisim, Stochastic
      SSB_Rec_model(s)=SSB_Rec_EquiSim_stoch(s,iEquiSim,4);
      SSB_Rec_hockey_breakpoint(s)=SSB_Rec_EquiSim_stoch(s,iEquiSim,2);
      SSB_R_alfa(s)=SSB_Rec_EquiSim_stoch(s,iEquiSim,1);
      SSB_R_beta(s)=SSB_Rec_EquiSim_stoch(s,iEquiSim,2);   
      noiseFac(1,s)=exp(SSB_Rec_EquiSim_stoch(s,iEquiSim,3)*rn(iter,s,y));

      //cout<<"EquiSim "<<species_names(s)<< "iter:"<<iEquiSim<<" model:"<<SSB_Rec_model(s)<<" alfa:"<<SSB_R_alfa(s)<<" beta:"<< SSB_R_beta(s)<<" break:"<< SSB_Rec_hockey_breakpoint(s)<<" SSB:"<<ssb<<" noise:"<<noiseFac(1,s)<<" rec:"<<calc_recruit(s,ssb, noiseFac(1,s), recruit_adjust(s))<<endl;
      N_global(y,recq,s,fa)=calc_recruit(s,ssb, noiseFac(1,s), recruit_adjust(s));
   }
 }
 //ofstream out("OP_testOut.out",ios::app);
 // out<< noiseFac(1)<<endl;
 
 // cout<<"Recruitment: ";
 //for (s=first_VPA;s<=nsp;s++) cout<<N_global(y,recq,s,fa)<<"  "; cout<<endl;
 //cout<<N_global(y,recq,1,fa)<<"  "; cout<<endl;

 // *****************************************************************************************


FUNCTION void write_M2_simple(int y,int q, int d,dvector other_food, dvar_matrix size, dmatrix prey_w, dvar_matrix consum, dvar_matrix Nbar, dvar_matrix& M2);
 int pred,pred_a,prey,prey_a,pred_l,prey_l;
 dvariable pred_size,prey_size;
 dvariable tmp,partM2;
 dvar_matrix avail_food(1,npr,fa,max_a);
 
 avail_food=0.0;
 ofstream pmo("op_part_m2.out",ios::app);

 for (pred=1;pred<=npr;pred++) if (pred_area_present(pred,d)==1){
  for (pred_a=faq(q);pred_a<=la(pred);pred_a++) {
    if (Nbar(pred,pred_a)>0) {
      //Calc available food
        pred_size=size(pred,pred_a);
      for (prey=first_VPA;prey<=nsp;prey++) {
         if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
            prey_size=size(prey,prey_a);
            if  ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
                avail_food(pred,pred_a)+=Nbar(prey,prey_a)*prey_w(prey,prey_a)*suit(y,q,d,pred,prey,pred_size,prey_size);
      }}}}
      avail_food(pred,pred_a)+=other_food(pred)*other_suit(pred,pred_size,y,q,d);  // add other food
      tmp=Nbar(pred,pred_a)*consum(pred,pred_a)/avail_food(pred,pred_a);
       //calc M2
      for (prey=first_VPA;prey<=nsp;prey++) {
        if (pred_prey_comb(d,pred,prey)>0) {
          for (prey_a=faq(q);prey_a<=la(prey);prey_a++) {
            prey_size=size(prey,prey_a);
            if ((pred_size*prey_pred_size_fac(pred))>=prey_size) {
               partM2=tmp*suit(y,q,d,pred,prey,pred_size,prey_size);
               if (partM2>0)  pmo<<y<<" "<<q<<" "<<d<<" "<<pred<<" "<<pred_a<<" "<<prey<<" "<<prey_a<<" "<<partM2<<endl;
            }
      }}}
     }
   }
 }
 pmo.close();

FUNCTION void write_var_other_sp();
 ofstream othv("op_other_sp_var.out",ios::out);
  othv<<"Species.n Year Quarter Age    N west consum"<<endl;
  for (y=fy_out;y<=ly;y++) {
    for (s=1;s<=nOthPred;s++) {
    //cout<<"s:"<<s<<" "<<other_pred_N_first_year(s)<<" "<<other_pred_N_first_year(s)<<" "<<other_pred_N_last_year(s)<<endl;
    
      if (other_pred_N_first_year(s)>0) {
        for (q=fq;q<=lq;q++) {
          for (d=1;d<=no_areas;d++) {
          for (a=faq(q);a<=la(s);a++) {
            if (N_other(y,q,d,s,a)>0) othv<<s<<" "<<y<<" "<<q<<" "<<" "<<a<<" "<<N_other(y,q,d,s,a)<<" "<<size_sea(y,q,d,s,a)<<" "<<consum(y,q,d,s,a)<<endl;
  }}}}}}
  othv.close();

FUNCTION void write_part_M2();
 int y,q,d,file_do;

 ofstream pmo("op_part_m2.out",ios::out);
 pmo <<"Year Quarter Area Predator.no Predator.age Prey.no Prey.age Part.M2"<<endl;
 pmo.close();
 for (y=fy_out;y<=ly;y++) {
   for (q=fq;q<=lq;q++) {
     for (d=1;d<=no_areas;d++) {
        write_M2_simple(y,q,d,other_food(d),size_sea(y,q,d),prey_w(q,d),consum(y,q,d),NbarStom(y,q,d),M2(y,q,d)); 
 }}}
   
 
 // *****************************************************************************************


FUNCTION void print_summary_qd()
 int s,y,q,a;
 int d;

 ofstream res("op_summary.out",ios::out);
 res <<"Year Quarter Area Species.n Age M1 M2 M F Z N Nbar NbarStom N_dist C west weca Yield CWsum propmat BIO SSB consum"<<endl;
  for (s=first_VPA;s<=nsp;s++)
    for (y=fy_out;y<=ly;y++)
      for (q=fq;q<=lq;q++)
       for (d=1;d<=no_areas;d++)
        for (a=fa;a<=la(s);a++) {
           if (!(a==fa && q <recq) && N(y,q,d,s,a)>0 ) {
             res <<y<<" "<<q<<" "<<d<<" "<<s<<" "<<a<<" ";
             if (multi >=1 ) {
               res <<M1(q,d,s,a)<<" ";
               res <<M2(y,q,d,s,a)<<" ";
             }
             else res << "0 0 ";
             res <<M(y,q,d,s,a)<<" ";
             res <<F(y,q,d,s,a)<<" ";
             res <<Z(y,q,d,s,a)<<" ";
             res <<N(y,q,d,s,a)<<" ";
             res <<Nbar(y,q,d,s,a)<<" ";
             res <<NbarStom(y,q,d,s,a)<<" ";
             if (no_areas==1) res<<" 1 ";
             else res <<N_dist(q,d,s,a)<<" ";
             res <<C(y,q,d,s,a)<<" ";
             res <<west(y,q,s,a)<<" ";
             res <<weca(y,q,d,s,a)<<" ";
             res <<C(y,q,d,s,a)*weca(y,q,d,s,a)*prop_landed(q,d,s,a)<<" ";
             res <<C(y,q,d,s,a)*weca(y,q,d,s,a)<<" ";
             res <<propmat(q,s,a)<<" ";
             res <<N(y,q,d,s,a)*west(y,q,s,a)<<" ";
             res <<N(y,q,d,s,a)*west(y,q,s,a)*propmat(q,s,a)<<" ";
             if (s<=npr) res <<consum(y,q,d,s,a)<<" "; else res<<" 0 ";
             res << endl;
           }
        }
 res.close();
 if (test_output==10) cout<<"file op_summary.out is done"<<endl;

FUNCTION void print_summary_anno()
 int s,y;

 ofstream res("op_summary_anno.out",ios::out);
 res <<"Year Species.n Age M1 M2 M F Z N C west weca  "<<endl;
 res <<"# N and west by 1 st January (except recruit by 3 quarter)"<<endl;
  for (s=first_VPA;s<=nsp;s++)
    for (y=fy_out;y<=ly;y++)
     for (a=fa;a<=la(s);a++) {
       if (!(a==fa && q <recq)) {
         res <<y<<" " <<s<<" "<<a<<" ";
             if (multi >=1 ) {
               res <<annoM1(y,s,a)<<" ";
               res <<annoM2(y,s,a)<<" ";
             }
             else res << "0 0 ";
             res <<annoM(y,s,a)<<" ";
             res <<annoF(y,s,a)<<" ";
             res <<annoZ(y,s,a)<<" ";
             if (!(a==fa && q ==recq)) res <<N_global(y,recq,s,a)<<" ";
             else res <<N_global(y,fq,s,a)<<" ";
             res <<annoC(y,s,a)<<" ";
             if (a>recq) res <<west(y,fq,s,a)<<" ";  else res <<west(y,recq,s,a)<<" ";
             res <<mean_weca(y,s,a)<<" "<<endl;;
           }
        }
  res.close();
  if (test_output==10) cout<<"file op_summary_anno.out is done"<<endl;


FUNCTION void calc_M2bar(int my_fy, int my_ly);
 int y,q,d,s,a;
 ivector ii(first_VPA,3);
 
 for (y=my_fy;y<=my_ly;y++) {
   deadM2(y)=0;
   deadZ(y)=0;
   M2bar(y)=0;
   
   for(q=fq;q<=lq;q++) for (d=1;d<=no_areas;d++)  for (s=first_VPA;s<=nsp;s++) for (a=max(faq(q),M2_bar_ages(1,s));a<=M2_bar_ages(2,s);a++) {
     deadM2(y,s,a)+=Nbar(y,q,d,s,a)*M2(y,q,d,s,a);
      deadZ(y,s,a)+=Nbar(y,q,d,s,a)* Z(y,q,d,s,a);
    }
   for (s=first_VPA;s<=nsp;s++) for (a=M2_bar_ages(1,s);a<=M2_bar_ages(2,s);a++) {
     M2bar(y,s)+= value(deadM2(y,s,a)/deadZ(y,s,a)*annoZ(y,s,a));
   }
   for (s=first_VPA;s<=nsp;s++) for (a=M2_bar_ages(2,s);a<=M2_bar_ages(2,s);a++) {
     M2bar(y,s)/=(M2_bar_ages(2,s)-M2_bar_ages(1,s)+1); 
   }
 }



FUNCTION void print_OP_Fcombinations(int r);
 int s,y,q,a,ny;
 double myield,mvalue,mCW,mSSB,mTSB,mrec,mFbar,belowBpa,belowBlim, Bpa, Blim;
 ofstream res ("op_average_val.out",ios::app);
 for (y=fy_out;y<=ly;y++) calc_yield_value(y);
 ny=ly-fy_out+1;
 for (s=first_VPA;s<=nsp;s++){
    myield=0; mvalue=0; mCW=0; mSSB=0; mTSB=0; mrec=0; mFbar=0;belowBpa=0; belowBlim=0;
    Blim=reference_points(s,3); Bpa=reference_points(s,4);
   for (y=fy_out;y<=ly;y++){
      mvalue+=value(yield_value(y,s));
      myield+=value(yield_global(y,s)); 
      mCW+=value(CWsum_global(y,s)); 
      mSSB+=value(SSB(y,s)); 
      mTSB+=value(TSB(y,s));
      mrec+=value(N_global(y,recq,s,fa));
      mFbar+=value(Fbar(y,s));
      if (SSB(y,s)<Blim) belowBlim++;
      if (SSB(y,s)<Bpa) belowBpa++;
   }
   mvalue=mvalue/ny;
   myield=myield/ny;
   mCW=mCW/ny; 
   mSSB=mSSB/ny; 
   mrec=mrec/ny;
   mFbar=mFbar/ny;
   res<<mvalue<<" "<<myield<<" "<<mCW<<" "<<Fcombination(r,s)<<" "<<mFbar<<" "<<mSSB<<" "<<mTSB<<" "<<mrec<<" "<<belowBlim<<" "<<belowBpa<<" "<<s<<" "<<r+first_no_run-1<<" "<<iter<<" "<<Fcombination(r)<<endl;
 }
 res.close();
  if (test_output==10) cout<<"file op_average_val.out is done"<<endl;


FUNCTION void print_condensed();
 int s,y,q,a;
 double Fbarl;
 ofstream res("op_condensed.out",ios::out);
 res <<"Year Species.n yield CWsum Fbar SSB TSB recruit value";
 if (do_optim==1) res <<" penalty";
 res<<endl;
 
 for (s=first_VPA;s<=nsp;s++){
   for (y=fy_out;y<=ly;y++){
     if (s==first_VPA) calc_yield_value(y); 
     if (do_optim==0) {
       Fbarl=0;
       for (a=avg_F_ages(s,1);a<=avg_F_ages(s,2);a++) Fbarl+=value(annoF(y,s,a));
       Fbarl=Fbarl/ (avg_F_ages(s,2)-avg_F_ages(s,1)+1);
       res<<y<<" "<<s<<" ";
       res<<yield_global(y,s)<<" ";
       res<<CWsum_global(y,s)<<" "<<Fbarl<<" ";
       res<<SSB(y,s)<<" "<<TSB(y,s)<<" ";
       res<<N_global(y,recq,s,fa)<<" ";
       res<<yield_value(y,s)<<endl;
     }
     else res<<y<<" "<<s<<" "<<yield_global(y,s)<<" "<<CWsum_global(y,s)<<" "<<Fbar(y,s)<<" "<<SSB(y,s)<<" "<<TSB(y,s)<<" "<<N_global(y,recq,s,fa)<<" "<<yield_value(y,s)<<" "<<penalty_y(y,s)<<endl;
   }
 }
 res.close();
 if (test_output==10) cout<<"file print_condensed.out is done"<<endl;

FUNCTION void calc_indicators_system();
 int s,y,q,a,d;
 double nw, w;
 double sumM2w, sumMw, sumBioM,sumBioF, sumYield, sumCatch;
 double sumW;
 
 if (do_community_M==1) calc_anno_mortality_and_weight(fy_out,ly); // to obtain deadMw and deadM2w
 
 q=1; //calculate biomass in quater 1
 for (y=fy_out;y<=ly;y++) {
   bio_demer(y)=0; bio_small(y)=0; bio_pelag(y)=0; bio_forage(y)=0;
   sumM2w=0; sumMw=0;  sumBioM=0; sumBioF=0; sumYield=0; sumCatch=0;
   for (s=1;s<=nsp;s++) {
      for (a=1;a<=la(s);a++) {
       for (d=1;d<=no_areas;d++) {
         nw=value(N(y,q,d,s,a)* west(y,q,s,a)); 
         if  (do_bio_demer==1) if (a>=bio_demer_ages(1,s)  &&  a<=bio_demer_ages(2,s)) {
          
          bio_demer(y)+=nw;
          // if (y==2012) cout<<"s:"<<s<<" a:"<<a<<" low:"<<bio_demer_ages(1,s)<<" high:"<<bio_demer_ages(2,s)<<" nw:"<<nw<<endl; 
          }
         if  (do_bio_small==1) {if (a>=bio_small_ages(1,s)  &&  a<=bio_small_ages(2,s)) bio_small(y)+=nw; }
         if  (do_bio_pelag==1) {if (a>=bio_pelag_ages(1,s)  &&  a<=bio_pelag_ages(2,s)) bio_pelag(y)+=nw; }
         if  (do_bio_forage==1){if (a>=bio_forage_ages(1,s) &&  a<=bio_forage_ages(2,s)) bio_forage(y)+=nw; }
       }
     } // end age
     if (s>=first_VPA) {
       if (do_community_F==1 &&  community_F_sp(s)==1) {
         sumCatch+=value(CWsum_global(y,s));
         sumYield+=value(yield_global(y,s));
         sumBioF+=value(TSB(y,s));
       }
       if (do_community_M==1 &&  community_M_sp(s)==1) {
           sumMw+=value(deadMw(y,s));
           sumM2w+=value(deadM2w(y,s));
           sumBioM+=value(TSB(y,s));
        }
     }
   }   // end species
  comm_Fland(y)= sumYield/sumBioF;
  comm_Fall(y)=sumCatch/sumBioF;        
  comm_M(y)=sumMw/sumBioM;
  comm_M2(y)=sumM2w/sumBioM;
 }  // end Year
 
 if (do_LFI==1) calc_LFI(); 
 
 if (do_community_life_expectancy==1) {
    calc_life_expectancy(community_life_expectancy_age);
    //cout<<"life_expectancy(s,y):"<<endl<<life_expectancy<<endl;
    sumW=0; 
    for (s=first_VPA;s<=nsp;s++) if (community_life_expectancy_age(s)>=0)  {
      sumW+=community_life_expectancy_weighting(s);}
    
    for (y=fy_out;y<=ly;y++) {
      sumW=0; 
      community_life_expectancy(y)=0; 
      for (s=first_VPA;s<=nsp;s++) if (community_life_expectancy_age(s)>=0) {
         if (community_life_expectancy_weighting(s)>0 ) w=community_life_expectancy_weighting(s);
         else w=value(N_global(y,recq,s,fa)); // recruits
         sumW+=w;
         community_life_expectancy(y)+=life_expectancy(s,y)*w;
      }
      community_life_expectancy(y)=community_life_expectancy(y)/sumW;
    }    
  }
  if (do_community_mean_C==1) calc_mean_weight_catch();  // calcs mw_C_community(y)
  
   
 
FUNCTION void calc_LFI();    // Large Fish Indicator
 double sumLFI, sumTotal,tmp;
 int y,q,s,a;
 q=1; // Numbers in Quarter 1 is used;
 for (y=fy_out;y<=ly;y++) {
    sumLFI=0; sumTotal=0;
    for (s=first_VPA;s<=nsp;s++) if (LFI_sp(s)==1) {
       for (a=fa;a<=la(s);a++) {
          tmp=value(N_global(y,q,s,a)*west(y,q,s,a));   
          if (a>=LFI_age(s)) sumLFI+=tmp; 
          sumTotal+=tmp; 
       }
    }
    LFI(y)=sumLFI/sumTotal;
 }         
 
   
FUNCTION void print_avg_indicators_system(int r);  
 int ny,y,a,life_sp; 
 double life_exp,sumW,life_exp_avg;        
 ny=ly-fy_out+1;
 calc_indicators_system();
 
 // weigthed average of life expectancy
 life_exp_avg=0; life_sp=0;
 if (do_community_life_expectancy==1) {
    calc_life_expectancy(community_life_expectancy_age);
    //cout<<"life_expectancy(s,y):"<<endl<<life_expectancy<<endl;
    sumW=0; for (s=first_VPA;s<=nsp;s++) if (life_expectancy(s,fy_out)>0) {life_sp+=1; sumW+=community_life_expectancy_weighting(s);}
    for (y=fy_out;y<=ly;y++) {
      life_exp=0; 
      for (s=first_VPA;s<=nsp;s++){
        if (life_expectancy(s,y)>0) life_exp+=life_expectancy(s,y)*community_life_expectancy_weighting(s);
      }
      life_exp_avg+=life_exp/sumW;
    }    
    life_exp_avg= life_exp_avg/ny;
 }      

 ofstream res("op_indicator_system_avg.out",ios::app);
 res<<r+first_no_run-1<<" "<<iter<<" "<<sum(bio_demer)/ny<<" "<<sum(bio_small)/ny<<" "<<sum(bio_pelag)/ny<<" "<<sum(bio_forage)/ny<<" "<<sum(comm_Fland)/ny<<" "<<
            sum(comm_Fall)/ny<<" "<<sum(comm_M)/ny<<" "<<sum(comm_M2)/ny<<" " <<life_exp_avg<<" "<<sum(LFI)/ny<<" "<<sum(mw_C_community)/ny<<endl ;
 res.close();
 if (test_output==10) cout<<"file op_indicator_system_avg.out is done"<<endl;

  
FUNCTION void print_indicators_system(int r);  
 int y;
 calc_indicators_system();
 ofstream res("op_indicator_system.out",ios::out);
 res <<"Year run iter  bio.demer bio.small bio.pelag bio.forage comm.Fland comm.Fall comm.M comm.M2 community.life.expect LFI"<<endl;  //  comm.life.expect size.spec.slope size.spec.interc  comm.mean.weight
 for (y=fy_out;y<=ly;y++) {
  res<<y<<" "<<r+first_no_run-1<<" "<<iter<<" "<< bio_demer(y)<<" "<<bio_small(y)<<" "<<bio_pelag(y)<<" "<< bio_forage(y)<<" "<<comm_Fland(y)<<" "
      <<comm_Fall(y)<<" "<<comm_M(y)<<" "<<comm_M2(y)<<" "<<community_life_expectancy(y)<<" "<<LFI(y)<<endl;
 }
 res.close();
 if (test_output==10) cout<<"file op_indicator_system.out is done"<<endl;


FUNCTION void calc_life_expectancy(ivector options);
 int y,s,q,a,d,nexta,nextq,exemp;

 life_expectancy=0;
 for (y=fy_out;y<=ly;y++) {
   d=1; //assumption (to make life easier) use always area 1 to handle plus group 
   for (s=first_VPA;s<=nsp;s++)  if (options(s)>=fa) { 
     if (options(s)== fa) firstN(s)= value(N_global(y,recq,s,fa));
     else firstN(s)= value(N_global(y,1,s,life_expectancy_age(s)));
    }
   for (s=first_VPA;s<=nsp;s++) {
    exemp=0;
    for (a=fa;a<=la(s);a++) {
     if (options(s)>=0) { 
       for (q=fq;q<=lq;q++)
         if ( (a==fa && q>=recq) || a>fa) {
           if (q==lq) {
             nextq=fq; nexta=a+1;
             if (a==la(s)) exemp=1;  // exemption, special case
           }
           else {
             nextq=q+1;
             nexta=a;
           }
           if (exemp==0) life_expectancy(s,y)+= value(N_global(y,nextq,s,nexta))/firstN(s);
           else value(N(y,q,d,s,a)*exp(-(Z(y,q,d,s,a))))/firstN(s);
         }
       }
      }
     if (options(s)==fa) life_expectancy(s,y)+=(recq-fq); else  if (options(s)>fa) life_expectancy(s,y)+=(recq-fq)+ (options(s)-fa)*(lq-fq+1); // total life expectancy
      
     life_expectancy(s,y)=  life_expectancy(s,y)/(lq-fq+1)+1/(lq-fq+1)/2;  // expectancy was calculated in time steps
   }
 }
 
 
  
FUNCTION void calc_mean_weight_catch();    
 int y,d,q,s,a;
 double sumC;
 mw_C=0;
 tot_C=0;
 // by species
 for (s=first_VPA;s<=nsp;s++) if (mean_weight_C_sp(s)==1 || community_mean_weight_C_sp(s)==1 ) { 
   for (y=fy_out;y<=ly;y++) {
     for (q=fq;q<=lq;q++) {
       for (d=1;d<=no_areas;d++) {
         for (a=faq(q);a<=la(s);a++)  {
             mw_C(y,s)+=value(C(y,q,d,s,a)*weca(y,q,d,s,a));
             tot_C(y,s)+=value(C(y,q,d,s,a)); 
         }
       }
     }
     if (tot_C(y,s)>0) mw_C(y,s)= mw_C(y,s)/ tot_C(y,s); 
     else mw_C(y,s)=0;
  }
 }
 //cout<<"mw_C(y,s):"<<endl<<mw_C<<endl;
 // all spcies combined mean weight
 mw_C_community=0;
 for (y=fy_out;y<=ly;y++) {
   sumC=0;
   for (s=first_VPA;s<=nsp;s++) if (community_mean_weight_C_sp(s)==1) { 
     mw_C_community(y)+= mw_C(y,s)* tot_C(y,s);
     sumC+=tot_C(y,s);
   }
   mw_C_community(y)= mw_C_community(y)/sumC;
 }
 

  
FUNCTION void print_indicators_species()
 int y;
 ofstream res("op_indicator_species.out",ios::out);
 res <<"Year Species.n life.expectancy mweca"<<endl; 

 if (do_life_expectancy==1 ) {calc_life_expectancy(life_expectancy_age); }
 
 if (do_mean_weight_C) { calc_mean_weight_catch();}
 for (y=fy_out;y<=ly;y++) { 
   for (s=first_VPA;s<=nsp;s++) res<<y<<" "<<s<<" "<<life_expectancy(s,y)<<" "<<mw_C(y,s)<<endl;
 }
 res.close();  
 if (test_output==10) cout<<"file indicator_species.out is done"<<endl;


FUNCTION void print_avg_indicators_species(int r);  
 int ny;         

 calc_life_expectancy(community_life_expectancy_age);
  ny=ly-fy_out+1;
 ofstream res("op_indicator_species_avg.out",ios::app);
 for (s=first_VPA;s<=nsp;s++) {
   res<<r+first_no_run-1<<" "<<iter<<" "<<sum(life_expectancy(s))/ny<<endl;
 }
 res.close();
 if (test_output==10) cout<<"file indicator_species_avg.out is done"<<endl;

 
FUNCTION void print_indicators()
 calc_anno_mortality_and_weight(fy_out,ly);
 if (do_bio_demer==1 || do_bio_small==1 || do_bio_pelag==1 || do_bio_forage==1 || do_community_mean_age==1 ||
      do_community_F==1 || do_community_M==1 || do_community_life_expectancy==1 || do_size_spectra==1 || do_community_mean_C==1) {
    print_indicators_system(1);
 }
 if (do_M2_bar==1 || do_life_expectancy==1 ) { 
     print_indicators_species();
 } 
 if (do_mean_weight_at_age==1) {        
     //do_indicators_species_age();
 } 


FUNCTION void print_optim_par();
 int s;
 ofstream res("op_optim_par.out",ios::out);
 res <<"Species.n HCR T1 T2 Ftarget Fslope SSB50 lowSSB50 highSSB50 initSSB50 phaseSSB50 S1 lowS1 highS1 initS1 phaseS1 penaltySSB penaltyUse"<<endl;
 for (s=first_VPA;s<=nsp;s++){
     res<<s<<" "<<HCR(s)<<" "<<T1(s)<<" "<<T2(s)<<" "<<Ftarget(s)<<" "<<exp(log_Fslope(s))<<" "<<SSB50(s)<<" "<<low_SSB50(s)<<" "<<high_SSB50(s)<<" "<<
              init_SSB50(s)<<" "<<phase_SSB50(s)<<" "<<S1(s)<<" "<<low_S1(s)<<" "<<high_S1(s)<<" "<<low_S1(s)*1.2<<" "<<phase_S1(s)<<" "<<penalty_limit(s)<<" "<<penalty_use(s)<< endl;
 }
 res.close();
 if (test_output==10) cout<<"file op_optim_par.out is done"<<endl;


FUNCTION void print_mean_weights();
 if( do_growth_all==1) {
    ofstream g1("op_mean_weights.out",ios::out);
    g1 << "Year Quarter Species.n Age west N "<<endl;
     for (y=fy;y<=ly;y++) for (s=first_VPA;s<=nsp;s++)for (q=fq;q<=lq;q++) for (a=faq(q);a<=la(s);a++)  {
       g1<<y<<" "<<q<<" "<<s<<" "<<a<<" "<<west(y,q,s,a)<<" "<<N(y,q,1,s,a)<<endl;
     }
     g1.close();
     if (test_output==10) cout<<"file op_mean_weight.out is done"<<endl;

 }



TOP_OF_MAIN_SECTION

 arrmblsize=25000000;
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(20000000);
 gradient_structure::set_CMPDIF_BUFFER_SIZE(600000);
 gradient_structure::set_MAX_NVAR_OFFSET(2000);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(300);
 
 
REPORT_SECTION

  if (do_optim==1) {
    report << "objective function (negative log likelihood):  " << f << endl;
    report << "Number of parameters: "<<initial_params::nvarcalc()<<endl;
    report <<"Maximum gradient: "<<objective_function_value::pobjfun->gmax<<endl;
    report <<"penalty: "<<sum(penalty)<<endl;
  }
  if (output==10) {
    write_N_output(); 
    // if (anyC==1) write_M1M2_output(); 
  }
  else if (output==11) { calc_anno_mortality_and_weight(fy_out,ly);  print_summary_anno(); }
  else if (output==12  )  {
    print_summary_qd();
    if (multi==2) write_part_M2();
  }
  else if (output==13) {calc_anno_mortality_and_weight(fy_out,ly);  print_condensed(); print_mean_weights();}
  else if (output==14 ) {cout<<"No detailed output selected"<<endl;  }   // output done for each F-combination
  else if (output==15 ) {
   write_N_output();
   // if (anyC==1) write_M1M2_output();
   print_optim_par();

   calc_anno_mortality_and_weight(fy_out,ly);
   print_condensed();
   print_mean_weights();
   print_summary_anno();
   print_summary_qd();
   if (multi==2) {
     write_part_M2();
     write_var_other_sp();
   }
  }
  else if (output==50 ) {   // Dll
     write_N_output_dll();
  }
  if (MSFD==1 && output!=14) print_indicators();

  report<<setprecision(3)<<setfixed()<<" Ftarget:"<<Ftarget<<endl;
  if (do_optim==1) {
   cout<<setprecision(3)<<setfixed()<<"Ftarget:"<<Ftarget<<endl;
   report<<setprecision(0)<<setfixed()<<"Penalty:"<<penalty<<endl;
  }
 cout<<"done!"<<endl;
 
 
 
  
