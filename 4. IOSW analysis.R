library(tidyverse)
library(boot)

set.seed(12345)



# Estimates needed: 
#   naive KHANDLE (overall, by race)
#   naive KHANDLE PR/PD stdized to age/sex in khandle
#   weighted KHANDLE  (overall, by race)
#   weighted KHANDLE PR/PD stdized  to age/sex in BRFSS

#Import data (m=40 imputations)
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized.Rdata")
khandle_brfss_harmonized$khandle_h<-1-khandle_brfss_harmonized$brfss_h

#Create age categories (65-<75, 75-<85, 85+)
khandle_brfss_harmonized$agecat_h<-
  ifelse(65 <= khandle_brfss_harmonized$age_h & 
           khandle_brfss_harmonized$age_h < 75, 1, 
         ifelse( 75 <= khandle_brfss_harmonized$age_h & 
                   khandle_brfss_harmonized$age_h < 85, 2, 3))

#Define age/sex distribution of standard population (full BRFSS sample with CA weights)
#Age and sex had no missing data so just use first imputation.
brfss<-khandle_brfss_harmonized %>% filter(brfss_h==1, imp_h==1) 
brfss_total<-sum(brfss$brfss_sampwt_h)
agesex_totals<-brfss %>% group_by(agecat_h, male_h) %>% 
  summarise(n = sum(brfss_sampwt_h))
agesex_totals$prop_agesex<-agesex_totals$n/brfss_total

#Define age/sex distribution of KHANDLE (unweighted)
#Age and sex had no missing data so just use first imputation.
khandle<-khandle_brfss_harmonized %>% filter(khandle_h==1 & imp_h==1)
khandle_total<-nrow(khandle)
agesex_totals_k<-khandle %>% group_by(agecat_h, male_h) %>% summarise(n = n())
agesex_totals_k$prop_agesex<-agesex_totals_k$n/khandle_total  


#Import dataset that already contains weights for each of the 40 imputations stacked
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_weights.Rdata")

#Create age categories (65-<75, 75-<85, 85+)
khandle_brfss_harmonized_weights_stack$agecat_h<-
  ifelse(65 <= khandle_brfss_harmonized_weights_stack$age_h & 
           khandle_brfss_harmonized_weights_stack$age_h < 75, 1, 
         ifelse( 75 <= khandle_brfss_harmonized_weights_stack$age_h & 
                   khandle_brfss_harmonized_weights_stack$age_h < 85, 2, 3))


#only need to keep KHANDLE
khandle_weights<-khandle_brfss_harmonized_weights_stack %>% filter(khandle_h==1)


#Naive KHANDLE estimates
  #Overall:
  est_pcogimp_unw<-khandle_weights %>% group_by(imp_h) %>% 
    summarise(pcogimp = as.numeric(mean(cogimp_prob_fin_dx))) %>% summarise(mean(pcogimp))
    
  #By race
  est_pcogimp_race_unw<-khandle_weights %>% group_by(imp_h, race_summary_h) %>% 
  summarise(pcogimp_race = mean(cogimp_prob_fin_dx)) %>% 
    group_by(race_summary_h) %>% 
    summarise(est_prop_unw=mean(pcogimp_race))
  est_pcogimp_race_unw
  
  #Inequalities
    #get race, age, and sex-specific estimates of mean
    cogimp_race_unw<-khandle_weights %>% 
        group_by(imp_h, race_summary_h, agecat_h, male_h) %>% 
        summarise(pcogimp_race = mean(cogimp_prob_fin_dx))
    
    #merge on standard population distribution
    cogimp_race_unw_stds<-cogimp_race_unw %>% 
      group_by(race_summary_h, agecat_h, male_h) %>% 
      summarise(est_prop_unw=mean(pcogimp_race)) %>% 
      left_join(.,agesex_totals_k, by=c("agecat_h", "male_h"))
    cogimp_race_unw_stds
    
    #take sum-product of estimated prev and std population for each race/ethnicity
    est_pcogimp_race_unw_std<-cogimp_race_unw_stds %>% group_by(race_summary_h) %>% 
      summarise(est_prop_unw_std = crossprod(est_prop_unw, prop_agesex))
    est_pcogimp_race_unw_std
    
    #Estimate inequalities
    est_PRs_unw_std<- est_pcogimp_race_unw_std$est_prop_unw_std[1:3]/est_pcogimp_race_unw_std$est_prop_unw_std[4]  
    est_PRs_unw_std
    
    est_PDs_unw_std<- est_pcogimp_race_unw_std$est_prop_unw_std[1:3]-est_pcogimp_race_unw_std$est_prop_unw_std[4]  
    est_PDs_unw_std

  
#Weighted  KHANDLE estimates  
  #Overall
  est_pcogimp_wtd<-khandle_weights %>% group_by(imp_h) %>% 
    summarise(weighted.pcogimp = weighted.mean(cogimp_prob_fin_dx, sw3)) %>% 
    summarise(mean(weighted.pcogimp))
  est_pcogimp_wtd
  
  #By race
  est_pcogimp_race_wtd<-khandle_weights %>% group_by(imp_h, race_summary_h) %>% 
    summarise(pcogimp_race = weighted.mean(cogimp_prob_fin_dx, sw3)) %>% 
    group_by(race_summary_h) %>% 
    summarise(est_prop_wtd=mean(pcogimp_race))
  est_pcogimp_race_wtd
  
  
  #Inequalities standardized to age/sex
  cogimp_race_wtd_stds<-khandle_weights %>% group_by(imp_h, race_summary_h, agecat_h, male_h) %>% 
    summarise(pcogimp_race = weighted.mean(cogimp_prob_fin_dx, sw3)) %>% 
    group_by(race_summary_h, agecat_h, male_h) %>% 
    summarise(est_prop_wtd=mean(pcogimp_race)) %>% 
    left_join(.,agesex_totals, by=c("agecat_h", "male_h"))
  cogimp_race_wtd_stds
  
  
  est_pcogimp_race_wtd_std<-cogimp_race_wtd_stds %>% group_by(race_summary_h) %>% 
    summarise(est_prop_wtd_std = crossprod(est_prop_wtd, prop_agesex))
  est_pcogimp_race_wtd_std
  
  est_PRs_wtd_std<- est_pcogimp_race_wtd_std$est_prop_wtd_std[1:3]/est_pcogimp_race_wtd_std$est_prop_wtd_std[4]  
  est_PRs_wtd_std
  
  est_PDs_wtd_std<- est_pcogimp_race_wtd_std$est_prop_wtd_std[1:3]-est_pcogimp_race_wtd_std$est_prop_wtd_std[4]  
  est_PDs_wtd_std
  
  
#Summarize results:
#THis is a terrible way to code this but leaving it for now to get code posted.
  Mean_results <- cbind(est_pcogimp_unw, t(est_pcogimp_race_unw[,2]),
                     (est_pcogimp_wtd), t(est_pcogimp_race_wtd[,2]), 
                     t(est_PRs_unw_std), t(est_PDs_unw_std), 
                     t(est_PRs_wtd_std), t(est_PDs_wtd_std))
  rownames(Mean_results)<-NULL
    colnames(Mean_results)<-c("Overall_unw", "Asian_unw", "Black_unw", "Latino_unw", "White_unw",
                   "Overall_wtd", "Asian_wtd", "Black_wtd", "Latino_wtd", "White_wtd",
                   "AW_PR_unw", "BW_PR_unw", "LW_PR_unw",
                   "AW_PD_unw", "BW_PD_unw", "LW_PD_unw",
                   "AW_PR_wtd", "BW_PR_wtd", "LW_PR_wtd",
                   "AW_PD_wtd", "BW_PD_wtd", "LW_PD_wtd")
  
  Mean_results$Est<-"Mean"
  
#Bootstrap inference
  
  #Analysis function
  analysis<-function(data, indices){
    d<-data[indices,]
    p_khandle<-sum(d$khandle_h)/sum(d$brfss_sampwt_h)

    p3<-glm(khandle_h~race_summary_h+male_h+age_h+education3_h+income_gtmed_pp_h+adl_walking_h+english_interview_h+goodhealth_h+
              race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h+
              race_summary_h*income_gtmed_pp_h+race_summary_h*adl_walking_h+race_summary_h*english_interview_h+race_summary_h*goodhealth_h, 
            data=d, family=binomial(link=logit), 
            weights = d$brfss_sampwt_h)
    
    #generate conditional predicted probabilites for being in khandle
    d$p3<-predict.glm(p3, type="response")
    #inverse odds weight is P(not khandle|K=k) / P(khandle|K=k)
    d$w3<-(1-d$p3)/(d$p3)
    #stabililized inverse odds weight is IOW * odds of being in khandle
    d$sw3<-(d$w3)*(p_khandle/(1-p_khandle))
    d$sw3[d$brfss_h==1]<-1
    
    k<-d %>% filter(brfss_h==0)
    
    res<-as.numeric(rep(NA, 22))
    
    #unweighted
    res[1]<-data.frame(race_summary_h="overall", 
                       pcogimp=mean(k$cogimp_prob_fin_dx))[,"pcogimp"]
    res[2:5] <-k %>% group_by(race_summary_h) %>% summarise(pcogimp = mean(cogimp_prob_fin_dx)) %>% data.frame() %>% select("pcogimp") %>% t() %>% as.numeric()
    
    #weighted
    res[6]<-data.frame(race_summary_h="overall", 
                       pcogimp=weighted.mean(k$cogimp_prob_fin_dx, k$sw3))[,"pcogimp"]
    res[7:10]<-k %>% group_by(race_summary_h) %>% 
      summarise(pcogimp = weighted.mean(cogimp_prob_fin_dx, sw3))  %>% data.frame() %>% select("pcogimp") %>% t() %>% as.numeric()
    
    #unweighted std
    by_race<-k %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_unw=mean(cogimp_prob_fin_dx)) %>%
      left_join(.,agesex_totals_k, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
      summarise(pcogimp = crossprod(est_prop_unw, prop_agesex)) %>% data.frame()
    
    res[11:13]<-by_race$pcogimp[1:3]/by_race$pcogimp[4]
    res[14:16]<-by_race$pcogimp[1:3]-by_race$pcogimp[4]
    
    
    #weighted std.
    by_race_w<-k %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_wtd=weighted.mean(cogimp_prob_fin_dx, sw3)) %>%
      left_join(.,agesex_totals, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
      summarise(pcogimp = crossprod(est_prop_wtd, prop_agesex)) %>% data.frame()
    
    res[17:19]<-by_race_w$pcogimp[1:3]/by_race_w$pcogimp[4]
    res[20:22]<-by_race_w$pcogimp[1:3]-by_race_w$pcogimp[4]
    
    return(res)
  }
  
  
  #Run bootstrap within each imputation:
  for (i in 1:40){
    
    dat<- khandle_brfss_harmonized %>% filter(imp_h==i)
    
    bootout<-boot(data=dat, statistic=analysis, strata=dat$brfss_h,
                  R=1000)
    
    bootout_formatted<-data.frame(bootout$t)
    colnames(bootout_formatted)<-c("Overall_unw", "Asian_unw", "Black_unw", "Latino_unw", "White_unw",
                                   "Overall_wtd", "Asian_wtd", "Black_wtd", "Latino_wtd", "White_wtd",
                                   "AW_PR_unw", "BW_PR_unw", "LW_PR_unw",
                                   "AW_PD_unw", "BW_PD_unw", "LW_PD_unw",
                                   "AW_PR_wtd", "BW_PR_wtd", "LW_PR_wtd",
                                   "AW_PD_wtd", "BW_PD_wtd", "LW_PD_wtd")
    
    if (i==1) {bootests_all<-bootout_formatted} else {bootests_all<-rbind(bootests_all, bootout_formatted)}
    
  }

  #Save results
  #This is also a terrible way to do this.
  
      LCIS<-data.frame(t(data.frame(sapply(bootests_all, function(x) quantile(x,0.025, na.rm=T)))))
      UCIS<-data.frame(t(data.frame(sapply(bootests_all, function(x) quantile(x,0.975, na.rm=T)))))
      
      
      #Merge with means
      colnames(LCIS)<-colnames(bootests_all)
      rownames(LCIS)<-NULL
      LCIS$Est<-"LCI"
      
      colnames(UCIS)<-colnames(bootests_all)
      rownames(UCIS)<-NULL
      UCIS$Est<-"UCI"
      
      Final_results<-pivot_longer(rbind(Mean_results, LCIS, UCIS), !Est) %>% 
        pivot_wider(., names_from="Est")
      
      save(Final_results, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/IOSW_results.Rdata")
      save(bootests_all, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/IOSW_bootests.Rdata")

  