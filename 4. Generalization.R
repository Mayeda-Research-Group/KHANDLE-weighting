#Generalizing in KHANDLE
library(tidyverse)
library(boot)

#Set seed to get consistent bootstrap estimates.
set.seed(12345)

load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_weights.Rdata")


khandle_weights<-khandle_brfss_harmonized_weights_stack %>% filter(khandle_h==1)

#Step 1: Match dan's results with unweighted p(cogimp)
  #Overall:
    cog_imp_unw<-khandle_weights %>% group_by(imp_h) %>% 
        summarise(pcogimp = mean(cogimp_prob_fin_dx))

    est_pcogimp_unw<-mean(cog_imp_unw$pcogimp)
    est_pcogimp_unw
    #24.8%
    
  cogimp_race_unw<-khandle_weights %>% group_by(imp_h, race_summary_h) %>% 
    summarise(pcogimp_race = mean(cogimp_prob_fin_dx))

  est_pcogimp_race_unw<-cogimp_race_unw %>% group_by(race_summary_h) %>% 
    summarise(est_prop_unw=mean(pcogimp_race))
  est_pcogimp_race_unw
  
  


  #bootstrap Inference
      se_unw<-rep(NA,40)
      
      for (i in 1:40){
      dat<- khandle_weights %>% filter(imp_h==i)
      meanfunc<-function(data, indices){
        d<-data[indices,]
        overall<-data.frame(race_summary_h="overall", 
                            pcogimp=mean(d$cogimp_prob_fin_dx))
        by_race<-d %>% group_by(race_summary_h) %>% summarise(pcogimp = mean(cogimp_prob_fin_dx)) %>% data.frame()
        res<-as.numeric(rbind(overall, by_race)[,2])
        return(res)
      }
      bootout<-boot(data=dat, statistic=meanfunc,
                    R=1000)
      
      
      bootout_formatted<-data.frame(bootout$t)
      colnames(bootout_formatted)<-c("Overall", "Asian", "Black", "Latino", "White")
      
      if (i==1) {bootests_unw<-bootout_formatted} else {bootests_unw<-rbind(bootests_unw,bootout_formatted)}
      
      }

      
      
  #Per reference, can use 2.5th and 97.5th percentiles across all bootstraps
  # (e.g, 1000 bootstraps across m imputed samples) to 
  #     cover between and within imputation variance
  #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5986623/
  
  #Unweighted KHANDLE results: 
      res_unw<-data.frame(group=c("Overall", "Asian", "Black", "Latino", "White"), 
                          est_pcogimp=c(est_pcogimp_unw,est_pcogimp_race_unw$est_prop_unw),
                          LCI=c(quantile(bootests_unw$Overall, 0.025),
                                quantile(bootests_unw$Asian, 0.025),
                                quantile(bootests_unw$Black, 0.025),
                                quantile(bootests_unw$Latino, 0.025),
                                quantile(bootests_unw$White, 0.025)),
                          UCI=c(quantile(bootests_unw$Overall, 0.975),
                                quantile(bootests_unw$Asian, 0.975),
                                quantile(bootests_unw$Black, 0.975),
                                quantile(bootests_unw$Latino, 0.975),
                                quantile(bootests_unw$White, 0.975)),
                          weighted=0)
      res_unw                          
      
  
  

#Step 2: Get weighted p(cogimp)--this is KHANDLE generalized to BRFSS
  cog_imp<-khandle_weights %>% group_by(imp_h) %>% 
      summarise(weighted.pcogimp = weighted.mean(cogimp_prob_fin_dx, sw3))
  cog_imp
  
  est_pcogimp_wtd<-mean(cog_imp$weighted.pcogimp)
  est_pcogimp_wtd
  #20.3%
  
  cogimp_race_wtd<-khandle_weights %>% group_by(imp_h, race_summary_h) %>% 
    summarise(pcogimp_race = weighted.mean(cogimp_prob_fin_dx, sw3))
  
  est_pcogimp_race_wtd<-cogimp_race_wtd %>% group_by(race_summary_h) %>% 
    summarise(est_prop_wtd=mean(pcogimp_race))
  est_pcogimp_race_wtd
  


  
  
  #bootstrap Inference
  se_wtd<-rep(NA,40)
  
  for (i in 1:40){
    dat<- khandle_weights %>% filter(imp_h==i)
    meanfunc_wtd<-function(data, indices){
      d<-data[indices,]
      overall<-data.frame(race_summary_h="overall", 
                          pcogimp=weighted.mean(d$cogimp_prob_fin_dx, d$sw3))
      by_race<-d %>% group_by(race_summary_h) %>% 
        summarise(pcogimp = weighted.mean(cogimp_prob_fin_dx, sw3)) %>% data.frame()
      res<-as.numeric(rbind(overall, by_race)[,2])
      return(res)
    }
    bootout<-boot(data=dat, statistic=meanfunc_wtd,
                  R=1000)
    
    
    bootout_formatted<-data.frame(bootout$t)
    colnames(bootout_formatted)<-c("Overall", "Asian", "Black", "Latino", "White")
    
    if (i==1) {bootests_wtd<-bootout_formatted} else {bootests_wtd<-rbind(bootests_wtd,bootout_formatted)}
    
  }
  
  
  
  #Per reference, can use 2.5th and 97.5th percentiles across all bootstraps
  # (e.g, 1000 bootstraps across m imputed samples) to 
  #     cover between and within imputation variance
  #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5986623/
  
  #Weighted KHANDLE results (generalized to BRFSS): 
  res_wtd<-data.frame(group=c("Overall", "Asian", "Black", "Latino", "White"), 
                      est_pcogimp=c(est_pcogimp_wtd,est_pcogimp_race_wtd$est_prop_wtd),
                      LCI=c(quantile(bootests_wtd$Overall, 0.025),
                            quantile(bootests_wtd$Asian, 0.025),
                            quantile(bootests_wtd$Black, 0.025),
                            quantile(bootests_wtd$Latino, 0.025),
                            quantile(bootests_wtd$White, 0.025)),
                      UCI=c(quantile(bootests_wtd$Overall, 0.975),
                            quantile(bootests_wtd$Asian, 0.975),
                            quantile(bootests_wtd$Black, 0.975),
                            quantile(bootests_wtd$Latino, 0.975),
                            quantile(bootests_wtd$White, 0.975)),
                      weighted=1)
  res_wtd                          
  
  
  

  all_res<-rbind(res_unw, res_wtd)  
  
  save(all_res, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/results_final.Rdata")
  

 