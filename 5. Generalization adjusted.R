#Generalizing in KHANDLE
library(tidyverse)
library(boot)

#Set seed to get consistent bootstrap estimates.
set.seed(12345)

load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_weights.Rdata")


#Create age categories (65-<75, 75-<85, 85+)
khandle_brfss_harmonized_weights_stack$agecat_h<-
  ifelse(65 <= khandle_brfss_harmonized_weights_stack$age_h & 
           khandle_brfss_harmonized_weights_stack$age_h < 75, 1, 
         ifelse( 75 <= khandle_brfss_harmonized_weights_stack$age_h & 
                   khandle_brfss_harmonized_weights_stack$age_h < 85, 2, 3))


table(khandle_brfss_harmonized_weights_stack$agecat_h, exclude=NULL)

#Step 1: Define age/sex distribution of standard population (full BRFSS sample with CA weights)
brfsss_weights<-khandle_brfss_harmonized_weights_stack %>% 
      filter(brfss_h==1, imp_h==1) 


brfss_total<-sum(brfsss_weights$brfss_sampwt_h)
brfss_total

agesex_totals<-brfsss_weights %>% group_by(agecat_h, male_h) %>% summarise(n = sum(brfss_sampwt_h))
agesex_totals
sum(agesex_totals$n)

agesex_totals$prop_agesex<-agesex_totals$n/brfss_total
agesex_totals

#Step 2. Unweighted KHANDLE results

#limit to khandle data
khandle_weights<-khandle_brfss_harmonized_weights_stack %>% filter(khandle_h==1)

khandle_weights2<-khandle_weights %>% filter(imp_h==1)

khandle_total<-nrow(khandle_weights2)
khandle_total

agesex_totals_k<-khandle_weights2 %>% group_by(agecat_h, male_h) %>% summarise(n = n())
agesex_totals_k
sum(agesex_totals_k$n)

agesex_totals_k$prop_agesex<-agesex_totals_k$n/khandle_total
agesex_totals_k

#get mean estimates by race/ethnicity

#2a. get race, age, and sex-specific estimates of mean
cogimp_race_unw<-khandle_weights %>% group_by(imp_h, race_summary_h, agecat_h, male_h) %>% 
    summarise(pcogimp_race = mean(cogimp_prob_fin_dx))

#2b. merge on standard population distribution
  cogimp_race_unw_stds<-cogimp_race_unw %>% group_by(race_summary_h, agecat_h, male_h) %>% 
    summarise(est_prop_unw=mean(pcogimp_race)) %>% left_join(.,agesex_totals_k, by=c("agecat_h", "male_h"))
cogimp_race_unw_stds
  
  #2c. take sum-product of estimated prev and std population for each race/ethnicity
    est_pcogimp_race_unw<-cogimp_race_unw_stds %>% group_by(race_summary_h) %>% 
                        summarise(est_prop_unw_std = crossprod(est_prop_unw, prop_agesex))
    est_pcogimp_race_unw
  
    #2d. estimate inequalities
    est_PRs_unw_std<- est_pcogimp_race_unw$est_prop_unw_std[1:3]/est_pcogimp_race_unw$est_prop_unw_std[4]  
    est_PRs_unw_std
    
    
    est_PDs_unw_std<- est_pcogimp_race_unw$est_prop_unw_std[1:3]-est_pcogimp_race_unw$est_prop_unw_std[4]  
    est_PDs_unw_std
  

  #bootstrap Inference
      for (i in 1:40){
      dat<- khandle_weights %>% filter(imp_h==i)
      meanfunc<-function(data, indices){
        d<-data[indices,]
        by_race<-d %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_unw=mean(cogimp_prob_fin_dx)) %>%
                    left_join(.,agesex_totals_k, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
                   summarise(pcogimp = crossprod(est_prop_unw, prop_agesex)) %>% data.frame()
        
        PRs<-by_race$pcogimp[1:3]/by_race$pcogimp[4]
        PDs<-by_race$pcogimp[1:3]-by_race$pcogimp[4]
        
        res<-c(by_race$pcogimp, PRs, PDs)
        
        
        return(res)
      }
      bootout<-boot(data=dat, statistic=meanfunc,
                    R=1000)
      
      
      bootout_formatted<-data.frame(bootout$t)
      colnames(bootout_formatted)<-c("Asian", "Black", "Latino", "White", 
                                     "AW_PR", "BW_PR", "LW_PR", 
                                     "AW_PD", "BW_PD", "LW_PD")      
      if (i==1) {bootests_unw<-bootout_formatted} else {bootests_unw<-rbind(bootests_unw,bootout_formatted)}
      
      }

      
      
  #Per reference, can use 2.5th and 97.5th percentiles across all bootstraps
  # (e.g, 1000 bootstraps across m imputed samples) to 
  #     cover between and within imputation variance
  #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5986623/
  
  #Unweighted KHANDLE results: 
      res_unw<-data.frame(group=c("Asian", "Black", "Latino", "White",
                                  "AW_PR", "BW_PR", "LW_PR", 
                                  "AW_PD", "BW_PD", "LW_PD"), 
                          est=c(est_pcogimp_race_unw$est_prop_unw_std, 
                                est_PRs_unw_std, est_PDs_unw_std),
                          LCI=c(quantile(bootests_unw$Asian, 0.025),
                                quantile(bootests_unw$Black, 0.025),
                                quantile(bootests_unw$Latino, 0.025),
                                quantile(bootests_unw$White, 0.025),
                                quantile(bootests_unw$AW_PR, 0.025),
                                quantile(bootests_unw$BW_PR, 0.025),
                                quantile(bootests_unw$LW_PR, 0.025),
                                quantile(bootests_unw$AW_PD, 0.025),
                                quantile(bootests_unw$BW_PD, 0.025),
                                quantile(bootests_unw$LW_PD, 0.025)),
                          UCI=c(quantile(bootests_unw$Asian, 0.975),
                                quantile(bootests_unw$Black, 0.975),
                                quantile(bootests_unw$Latino, 0.975),
                                quantile(bootests_unw$White, 0.975),
                                quantile(bootests_unw$AW_PR, 0.975),
                                quantile(bootests_unw$BW_PR, 0.975),
                                quantile(bootests_unw$LW_PR, 0.975),
                                quantile(bootests_unw$AW_PD, 0.975),
                                quantile(bootests_unw$BW_PD, 0.975),
                                quantile(bootests_unw$LW_PD, 0.975)),
                          weighted=0)
      res_unw                          
      
  
  

#Step 3: Weighted p(cogimp)--this is KHANDLE generalized to BRFSS
  
      
      
      
      cogimp_race_wtd<-khandle_weights %>% group_by(imp_h, race_summary_h, agecat_h, male_h) %>% 
        summarise(pcogimp_race = weighted.mean(cogimp_prob_fin_dx, sw3))
      
      cogimp_race_wtd_stds<-cogimp_race_wtd %>% group_by(race_summary_h, agecat_h, male_h) %>% 
        summarise(est_prop_wtd=mean(pcogimp_race)) %>% left_join(.,agesex_totals, by=c("agecat_h", "male_h"))
      cogimp_race_wtd_stds
      
      
      est_pcogimp_race_wtd<-cogimp_race_wtd_stds %>% group_by(race_summary_h) %>% 
        summarise(est_prop_wtd_std = crossprod(est_prop_wtd, prop_agesex))
      est_pcogimp_race_wtd
  
      est_PRs_wtd_std<- est_pcogimp_race_wtd$est_prop_wtd_std[1:3]/est_pcogimp_race_wtd$est_prop_wtd_std[4]  
      est_PRs_wtd_std
  
  
      est_PDs_wtd_std<- est_pcogimp_race_wtd$est_prop_wtd_std[1:3]-est_pcogimp_race_wtd$est_prop_wtd_std[4]  
      est_PDs_wtd_std
      
      
  #bootstrap Inference
  for (i in 1:40){
    dat<- khandle_weights %>% filter(imp_h==i)
    meanfunc_wtd<-function(data, indices){
      d<-data[indices,]
      by_race<-d %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_wtd=weighted.mean(cogimp_prob_fin_dx, sw3)) %>%
        left_join(.,agesex_totals, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
        summarise(pcogimp = crossprod(est_prop_wtd, prop_agesex)) %>% data.frame()
      
      PRs<-by_race$pcogimp[1:3]/by_race$pcogimp[4]
      PDs<-by_race$pcogimp[1:3]-by_race$pcogimp[4]

      res<-c(by_race$pcogimp, PRs, PDs)
      return(res)
    }
    bootout<-boot(data=dat, statistic=meanfunc_wtd,
                  R=1000)
    
    
    bootout_formatted<-data.frame(bootout$t)
    colnames(bootout_formatted)<-c("Asian", "Black", "Latino", "White", 
                                   "AW_PR", "BW_PR", "LW_PR", 
                                   "AW_PD", "BW_PD", "LW_PD")
    
    if (i==1) {bootests_wtd<-bootout_formatted} else {bootests_wtd<-rbind(bootests_wtd,bootout_formatted)}
    
  }
  
  
  #Per reference, can use 2.5th and 97.5th percentiles across all bootstraps
  # (e.g, 1000 bootstraps across m imputed samples) to 
  #     cover between and within imputation variance
  #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5986623/
  
  #Weighted KHANDLE results (generalized to BRFSS): 
  res_wtd<-data.frame(group=c("Asian", "Black", "Latino", "White",
                              "AW_PR", "BW_PR", "LW_PR", 
                              "AW_PD", "BW_PD", "LW_PD"), 
                      est=c(est_pcogimp_race_wtd$est_prop_wtd_std, 
                                    est_PRs_wtd_std, est_PDs_wtd_std),
                      LCI=c(quantile(bootests_wtd$Asian, 0.025),
                            quantile(bootests_wtd$Black, 0.025),
                            quantile(bootests_wtd$Latino, 0.025),
                            quantile(bootests_wtd$White, 0.025),
                            quantile(bootests_wtd$AW_PR, 0.025),
                            quantile(bootests_wtd$BW_PR, 0.025),
                            quantile(bootests_wtd$LW_PR, 0.025),
                            quantile(bootests_wtd$AW_PD, 0.025),
                            quantile(bootests_wtd$BW_PD, 0.025),
                            quantile(bootests_wtd$LW_PD, 0.025)),
                      UCI=c(quantile(bootests_wtd$Asian, 0.975),
                            quantile(bootests_wtd$Black, 0.975),
                            quantile(bootests_wtd$Latino, 0.975),
                            quantile(bootests_wtd$White, 0.975),
                            quantile(bootests_wtd$AW_PR, 0.975),
                            quantile(bootests_wtd$BW_PR, 0.975),
                            quantile(bootests_wtd$LW_PR, 0.975),
                            quantile(bootests_wtd$AW_PD, 0.975),
                            quantile(bootests_wtd$BW_PD, 0.975),
                            quantile(bootests_wtd$LW_PD, 0.975)),
                      weighted=1)
  res_wtd     
  
  

  all_res_std<-rbind(res_unw, res_wtd)  
  
  save(all_res_std, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/results_final_std.Rdata")
  

 