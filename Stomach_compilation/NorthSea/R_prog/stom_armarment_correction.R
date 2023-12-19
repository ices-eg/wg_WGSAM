#p <- PREY %>% select(-prey_nodc,-digest,-prey_w_meth,-delta,-rho,-lambda, -xi,-alfa,-k, -Temp,  -n_tot,-n_food,-prey_n )
p <- PREY %>% select(-prey_nodc,-digest,-prey_w_meth, -delta, -rho, -lambda, -xi, alfa, k,  -Temp,  -n_tot,-n_food,-prey_n )

#p$sample_id

pp<-p %>% filter(substr(as.character(sample_id),1,5)=="Gadus"    & prey_name =="N Ammodytidae")
pp$sample_id

p %>% filter(sample_id  %in% c("Gadus morhua_ENG_y:85_q:3_ID:631","Gadus morhua_NED_y:87_q:3_ID:3216","Gadus morhua_NED_y:91_q:2_ID:1459")) %>%
  mutate(stomconGange=prey_w*evacRate, stomconDiv=prey_w/evacRate) %>%
               group_by(sample_id,fish_id) %>%
           mutate(rat=ration1/sum(ration1),prey_ww=prey_w/sum(prey_w),stom_gange=stomconGange/sum(stomconGange),stom_div=stomconDiv/sum(stomconDiv)) %>%
           mutate(stomconGange=NULL, stomconDiv=NULL)

