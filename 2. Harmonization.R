#Harmonization
library(spatstat)
library(dplyr)

#------------------------------------------------------------------
# KHANDLE
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_analysis_pmm.Rdata")

#studyid
khandle_analysis_pmm_harm<-data.frame(id_h=rep(NA, nrow(khandle_analysis_pmm)))
khandle_analysis_pmm_harm$id_h<-khandle_analysis_pmm$`khandle_clean$studyid`

#age
khandle_analysis_pmm_harm$age_h<-khandle_analysis_pmm$w1_interview_age

#male
khandle_analysis_pmm_harm$male_h<-ifelse(khandle_analysis_pmm$male==1,1,
                                         ifelse(khandle_analysis_pmm$male==0,0,NA))


#race_summary
khandle_analysis_pmm_harm$race_summary_h<-khandle_analysis_pmm$race_summary

#english_interview
khandle_analysis_pmm_harm$english_interview_h<-ifelse(khandle_analysis_pmm$english_interview==1,1,
                                                      ifelse(khandle_analysis_pmm$english_interview==0,0,NA))

#marital_status
khandle_analysis_pmm_harm$marital_h<-ifelse(khandle_analysis_pmm$marital_status %in% c(1:2), 1, 
                                              ifelse(khandle_analysis_pmm$marital_status %in% c(3:6),0,NA))
    #check recoding
    table(khandle_analysis_pmm_harm$marital_h, 
          khandle_analysis_pmm$marital_status, 
          exclude=NULL)


#education
khandle_analysis_pmm_harm$education_h<-factor(ifelse(khandle_analysis_pmm$education %in% c(0,1),1,
                                            ifelse(khandle_analysis_pmm$education==2,2,
                                                ifelse(khandle_analysis_pmm$education==7,3,
                                                  ifelse(khandle_analysis_pmm$education %in%
                                                         c(3:4), 4, 
                                                    ifelse(khandle_analysis_pmm$education %in%
                                                         c(5:6), 5, NA))))))
    #check recoding
    table(khandle_analysis_pmm_harm$education_h, 
          khandle_analysis_pmm$education, 
          exclude=NULL)


#income
khandle_analysis_pmm$income_cont<-ifelse(khandle_analysis_pmm$income_range==1, 10000,
                                  ifelse(khandle_analysis_pmm$income_range==2, 15000,
                                  ifelse(khandle_analysis_pmm$income_range==3, 20000,
                                  ifelse(khandle_analysis_pmm$income_range==4, 25000,
                                  ifelse(khandle_analysis_pmm$income_range==5, 35000,
                                  ifelse(khandle_analysis_pmm$income_range==6, 45000,
                                  ifelse(khandle_analysis_pmm$income_range==7, 55000,
                                  ifelse(khandle_analysis_pmm$income_range==8, 65000,
                                  ifelse(khandle_analysis_pmm$income_range==9, 75000,
                                  ifelse(khandle_analysis_pmm$income_range==10, 100000,
                                  ifelse(khandle_analysis_pmm$income_range==11, 125000,
                                  ifelse(khandle_analysis_pmm$income_range==12, 150000,
                                  ifelse(khandle_analysis_pmm$income_range==13, 175000,NA
                                  )))))))))))))
                                                                            
  #check recoding
   table(khandle_analysis_pmm$income_cont, 
         khandle_analysis_pmm$income_range, 
         exclude=NULL)
                                         

khandle_analysis_pmm$peopleperincome<-ifelse(khandle_analysis_pmm$marital_status
                                             %in% c(1:2),2,1) 
    #check recoding
    table(khandle_analysis_pmm$peopleperincome, 
          khandle_analysis_pmm$marital_status, 
          exclude=NULL)
                                                 
khandle_analysis_pmm_harm$income_pp_h<-khandle_analysis_pmm$income_cont/
                                        sqrt(khandle_analysis_pmm$peopleperincome)                                             

summary(khandle_analysis_pmm_harm$income_pp_h)

hist(khandle_analysis_pmm_harm$income_pp_h, col=rgb(0,0,1,0.5), freq = F, ylim=c(0,2e-05))
                                                                                            
#military
khandle_analysis_pmm_harm$military_h<-ifelse(khandle_analysis_pmm$military==1,1,
                                             ifelse(khandle_analysis_pmm$military==0,0,NA))

#working
# khandle_analysis_pmm_harm$working_h<-ifelse(khandle_analysis_pmm$working==1,1,
#                                             ifelse(khandle_analysis_pmm$working==0,0,NA))

#3/17/21 EHL post code review: removed working

#health
khandle_analysis_pmm_harm$goodhealth_h<-ifelse(khandle_analysis_pmm$health %in% c(1:3), 1, 
                                               ifelse(khandle_analysis_pmm$health %in% c(4:5),0,NA))
#check recoding
table(khandle_analysis_pmm_harm$goodhealth_h, 
      khandle_analysis_pmm$health, 
      exclude=NULL)

#blind
khandle_analysis_pmm_harm$blind_h<-as.numeric(ifelse(khandle_analysis_pmm$sensimp_vision
                                          %in% c(1:4), 0, 
                                          ifelse(khandle_analysis_pmm$sensimp_vision
                                                 %in% c(5,6), 1,NA)))
                                        
  #check recoding
  table(khandle_analysis_pmm_harm$blind_h, 
        khandle_analysis_pmm$sensimp_vision, 
        exclude=NULL)

#smoke_status
khandle_analysis_pmm_harm$smoke_status_h<-ifelse(khandle_analysis_pmm$smoke_status==2,1,0)
  
  #check recoding
  table(khandle_analysis_pmm_harm$smoke_status_h,
        khandle_analysis_pmm$smoke_status, 
        exclude=NULL)



#adl_walking
table(khandle_analysis_pmm$adl1, khandle_analysis_pmm$adl2, exclude=NULL)
  #Very few "5s." Lump these with people who have serious difficulty.

khandle_analysis_pmm_harm$adl_walking_h<-as.numeric(ifelse((khandle_analysis_pmm$adl1 
                                                 %in% c(3:5) |
                                                   khandle_analysis_pmm$adl2
                                                   %in% c(3:5)),1,0))
  #check recoding
  table(khandle_analysis_pmm_harm$adl_walking_h, 
        khandle_analysis_pmm$adl1,
        khandle_analysis_pmm$adl2,
        exclude=NULL)

#adl_dressing
table(khandle_analysis_pmm$adl9, exclude=NULL)
  #Very few "5s." Lump these with people who have serious difficulty.
  
khandle_analysis_pmm_harm$adl_dressing_h<-as.numeric(ifelse((khandle_analysis_pmm$adl9 %in% 
                                                    c(3:5)),1,0))
  #check recoding
  table(khandle_analysis_pmm_harm$adl_dressing_h, 
        khandle_analysis_pmm$adl9,
        exclude=NULL)
  

#exercise
khandle_analysis_pmm_harm$exercise_h<-as.numeric(ifelse((khandle_analysis_pmm$pa_lt_ex %in% 
                                                      c(1:3) | 
                                              khandle_analysis_pmm$pa_vig_ex %in% 
                                                c(1:3)),1,0))
  #check recoding
  table(khandle_analysis_pmm_harm$exercise_h, 
        khandle_analysis_pmm$pa_lt_ex,
        khandle_analysis_pmm$pa_vig_ex,
        exclude=NULL)
  
  table(khandle_analysis_pmm_harm$exercise_h, exclude=NULL)
  

#CA sampling weights
khandle_analysis_pmm_harm$brfss_sampwt_h<-1 

#CA county code
khandle_analysis_pmm_harm$county_code<-NA
  
#brfss indicator
khandle_analysis_pmm_harm$brfss_h<-0

#imputation indicator
khandle_analysis_pmm_harm$imp_h<-khandle_analysis_pmm$imp

#Post code review: adding cog impairment variables
#khandle_analysis_pmm_harm$cogimp_prob_adj_bl_dx<-khandle_analysis_pmm$w1_cogimp_prob_adj_bl_dx
#khandle_analysis_pmm_harm$cogimp_prob_adj_bl_nodx<-khandle_analysis_pmm$w1_cogimp_prob_adj_bl_nodx
khandle_analysis_pmm_harm$cogimp_prob_fin_dx<-khandle_analysis_pmm$w1_cogimp_prob_fin_dx
#khandle_analysis_pmm_harm$cogimp_prob_fin_nodx<-khandle_analysis_pmm$w1_cogimp_prob_fin_nodx
  
#3/17/21 EHL post code review: removed extra cog impairment vars

#------------------------------------------------------------------
# BRFSS
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/brfss_analysis_pmm.Rdata")

#studyid
brfss_analysis_pmm_harm<-data.frame(id_h=rep(NA, nrow(brfss_analysis_pmm)))
brfss_analysis_pmm_harm$id_h<-factor(rep(1:sum(brfss_analysis_pmm$imp==1),40))

#age
brfss_analysis_pmm_harm$age_h<-ifelse(brfss_analysis_pmm$age<=90,brfss_analysis_pmm$age, 90)

  #check recoding 
  summary(brfss_analysis_pmm$age)
  summary(brfss_analysis_pmm_harm$age_h)
  table(brfss_analysis_pmm$age, brfss_analysis_pmm_harm$age_h, exclude=NULL)

#male
brfss_analysis_pmm_harm$male_h<-ifelse(brfss_analysis_pmm$male==1,1,
                                       ifelse(brfss_analysis_pmm$male==0,0,NA))


#race_summary
brfss_analysis_pmm_harm$race_summary_h<-brfss_analysis_pmm$race_summary

#english_interview
brfss_analysis_pmm_harm$english_interview_h<-ifelse(brfss_analysis_pmm$english_interview==1,1,
                                                   ifelse(brfss_analysis_pmm$english_interview==0,0,NA))

#marital_status
brfss_analysis_pmm_harm$marital_h<-ifelse(brfss_analysis_pmm$marital_status %in% c(1,6), 1, 
                                            ifelse(brfss_analysis_pmm$marital_status %in% c(2:5),0,NA))

#check recoding
table(brfss_analysis_pmm_harm$marital_h, 
      brfss_analysis_pmm$marital_status, 
      exclude=NULL)


#education
brfss_analysis_pmm_harm$education_h<-factor(ifelse(brfss_analysis_pmm$education %in% c(1,2), 1, 
                                                     ifelse(brfss_analysis_pmm$education==3,2,
                                                            ifelse(brfss_analysis_pmm$education==4,3,
                                                                   ifelse(brfss_analysis_pmm$education==5, 4, 
                                                                          ifelse(brfss_analysis_pmm$education==6, 5, NA))))))
#check recoding
table(brfss_analysis_pmm_harm$education_h, 
      brfss_analysis_pmm$education, 
      exclude=NULL)


#income
brfss_analysis_pmm$income_cont<-ifelse(brfss_analysis_pmm$income==1, 10000,
                                  ifelse(brfss_analysis_pmm$income==2, 15000,
                                  ifelse(brfss_analysis_pmm$income==3, 20000,
                                  ifelse(brfss_analysis_pmm$income==4, 25000,
                                  ifelse(brfss_analysis_pmm$income==5, 35000,
                                  ifelse(brfss_analysis_pmm$income==6, 50000,
                                  ifelse(brfss_analysis_pmm$income==7, 75000,
                                  ifelse(brfss_analysis_pmm$income==8, 100000,
                                  ifelse(brfss_analysis_pmm$income==9, 125000,
                                  ifelse(brfss_analysis_pmm$income==10, 175000,NA
                                  ))))))))))

#check recoding
table(brfss_analysis_pmm$income_cont, 
      brfss_analysis_pmm$income, 
      exclude=NULL)


livingwage<-read.csv("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/livingwage_MIT.csv")
livingwage$county_code<-factor(livingwage$FIPS.code)
livingwage <- livingwage %>% select(-FIPS.code,-County.name)

brfss_analysis_pmm<-left_join(brfss_analysis_pmm, livingwage, by="county_code")

brfss_analysis_pmm$income_corrected<-brfss_analysis_pmm$income_cont/
  (brfss_analysis_pmm$Living.wage/16.5310111) #county income divided by weighted average of KHANDLE counties to get scale factor

summary(brfss_analysis_pmm$income_corrected)

brfss_analysis_pmm$peopleperincome<-brfss_analysis_pmm$hhsize_v2

#check recoding
table(brfss_analysis_pmm$peopleperincome, 
      brfss_analysis_pmm$hhsize_v2, 
      exclude=NULL)

brfss_analysis_pmm_harm$income_pp_h<-brfss_analysis_pmm$income_corrected/
  sqrt(brfss_analysis_pmm$peopleperincome)                                             
summary(brfss_analysis_pmm_harm$income_pp_h)

hist(brfss_analysis_pmm_harm$income_pp_h, col=rgb(1,0,0,0.5), add=T, freq = F)


#military
brfss_analysis_pmm_harm$military_h<-ifelse(brfss_analysis_pmm$military==1,1,
                                           ifelse(brfss_analysis_pmm$military==0,0,NA))


#working
# brfss_analysis_pmm_harm$working_h<-ifelse(brfss_analysis_pmm$working==1,1,
#                                            ifelse(brfss_analysis_pmm$working==0,0,NA))
#3/17/21 EHL post code review: removed working


#health
brfss_analysis_pmm_harm$goodhealth_h<-ifelse(brfss_analysis_pmm$health %in% c(1:3), 1, 
                                               ifelse(brfss_analysis_pmm$health %in% c(4:5),0,NA))
#check recoding
table(brfss_analysis_pmm_harm$goodhealth_h, 
      brfss_analysis_pmm$health, 
      exclude=NULL)

#blind
brfss_analysis_pmm_harm$blind_h<-ifelse(brfss_analysis_pmm$vision_impair==1,1,
                                        ifelse(brfss_analysis_pmm$vision_impair==0,0,NA))

    #check recoding
    table(brfss_analysis_pmm_harm$blind_h, 
          brfss_analysis_pmm$vision_impair, 
          exclude=NULL)
    class(brfss_analysis_pmm_harm$blind_h)

#smoke_status
brfss_analysis_pmm_harm$smoke_status_h<-ifelse(brfss_analysis_pmm$smoke_status==2,1,0)

  #check recoding
  table(brfss_analysis_pmm_harm$smoke_status_h, 
      brfss_analysis_pmm$smoke_status, 
      exclude=NULL)

#adl_walking
brfss_analysis_pmm_harm$adl_walking_h<-ifelse(brfss_analysis_pmm$diff_walk==1,1,
                                              ifelse(brfss_analysis_pmm$diff_walk==0,0,NA))

    #check recoding
    table(brfss_analysis_pmm_harm$adl_walking_h, 
          brfss_analysis_pmm$diff_walk,
          exclude=NULL)
    class(brfss_analysis_pmm_harm$adl_walking_h)
    
#adl_dressing
brfss_analysis_pmm_harm$adl_dressing_h<-ifelse(brfss_analysis_pmm$diff_dress==1,1,
                                               ifelse(brfss_analysis_pmm$diff_dress==0,0,NA))

    #check recoding
    table(brfss_analysis_pmm_harm$adl_dressing_h, 
          brfss_analysis_pmm$diff_dress,
          exclude=NULL)
    class(brfss_analysis_pmm_harm$adl_dressing_h)
    

#exercise
brfss_analysis_pmm_harm$exercise_h<-ifelse(brfss_analysis_pmm$phys_activity==1,1,
                                           ifelse(brfss_analysis_pmm$phys_activity==0,0,NA))
    
    #check recoding
    table(brfss_analysis_pmm_harm$exercise_h, 
          brfss_analysis_pmm$phys_activity,
          exclude=NULL)
    class(brfss_analysis_pmm_harm$exercise_h)
    

#CA sampling weights
brfss_analysis_pmm_harm$brfss_sampwt_h<-brfss_analysis_pmm$cdc_finalwt   

#CA county code
brfss_analysis_pmm_harm$county_code<-brfss_analysis_pmm$county_code   
    
#brfss indicator
brfss_analysis_pmm_harm$brfss_h<-1

#imputation indicator
brfss_analysis_pmm_harm$imp_h<-brfss_analysis_pmm$imp

#Post code review: adding cog impairment variables (missing in BRFSS but adding so merge works)
#brfss_analysis_pmm_harm$cogimp_prob_adj_bl_dx<-NA
#brfss_analysis_pmm_harm$cogimp_prob_adj_bl_nodx<-NA
brfss_analysis_pmm_harm$cogimp_prob_fin_dx<-NA
#brfss_analysis_pmm_harm$cogimp_prob_fin_nodx<-NA
#3/17/21 EHL post code review: removed extra cog impairment vars


#check brfss income distribution to set thresholds
quantile(brfss_analysis_pmm_harm$income_pp_h, probs = seq(0, 1, 0.05))
weighted.quantile(brfss_analysis_pmm_harm$income_pp_h, brfss_analysis_pmm_harm$brfss_sampwt_h, probs=seq(0,1,0.05))
mean(brfss_analysis_pmm_harm$income_pp_h)
weighted.mean(brfss_analysis_pmm_harm$income_pp_h, brfss_analysis_pmm_harm$brfss_sampwt_h)

#check khandle income distribution to set thresholds
quantile(khandle_analysis_pmm_harm$income_pp_h, probs = seq(0, 1, 0.05))
mean(khandle_analysis_pmm_harm$income_pp_h)


#COmbine harmonized datasets
khandle_brfss_harmonized<-rbind(khandle_analysis_pmm_harm,
                                brfss_analysis_pmm_harm)

#Dichotomizing income
khandle_brfss_harmonized$income_gt50k_pp_h<-ifelse(khandle_brfss_harmonized$income_pp_h >50000,1,
                                                   ifelse(khandle_brfss_harmonized$income_pp_h <=50000,0,NA))
    
khandle_brfss_harmonized$income_gt65k_pp_h<-ifelse(khandle_brfss_harmonized$income_pp_h >65000,1,
                                                                                   ifelse(khandle_brfss_harmonized$income_pp_h <=65000,0,NA))

khandle_brfss_harmonized$income_gtmed_pp_h<-ifelse(khandle_brfss_harmonized$income_pp_h>weighted.median(brfss_analysis_pmm_harm$income_pp_h, w=brfss_analysis_pmm_harm$brfss_sampwt_h),1,
                                                   ifelse(khandle_brfss_harmonized$income_pp_h <=weighted.median(brfss_analysis_pmm_harm$income_pp_h, w=brfss_analysis_pmm_harm$brfss_sampwt_h),0,NA))


#Post code review 3/30/21 -- creating two versions of education that is more collapsed, and an additional quartiles of income variable.

weighted_quarts<-weighted.quantile(brfss_analysis_pmm_harm$income_pp_h, 
                                   brfss_analysis_pmm_harm$brfss_sampwt_h, probs=c(0.25, 0.5, 0.75), na.rm = TRUE)
khandle_brfss_harmonized$income_quartile<-factor(ifelse(khandle_brfss_harmonized$income_pp_h <= weighted_quarts[1], 1,
                                                         ifelse(khandle_brfss_harmonized$income_pp_h <= weighted_quarts[2], 2,
                                                                ifelse(khandle_brfss_harmonized$income_pp_h <= weighted_quarts[3], 3, 4))))
                                 
table(khandle_brfss_harmonized$income_gtmed_pp_h, khandle_brfss_harmonized$income_quartile)


khandle_brfss_harmonized$education2_h<-factor(ifelse(khandle_brfss_harmonized$education_h %in% c(1:3), 1,
                                              ifelse(khandle_brfss_harmonized$education_h == 4, 2,
                                                     ifelse(khandle_brfss_harmonized$education_h==5, 3, NA))))

table(khandle_brfss_harmonized$education_h, khandle_brfss_harmonized$education2_h, exclude=NULL)



khandle_brfss_harmonized$education3_h<-factor(ifelse(khandle_brfss_harmonized$education_h %in% c(1:2), 1,
                                              ifelse(khandle_brfss_harmonized$education_h == 3, 2,
                                                     ifelse(khandle_brfss_harmonized$education_h == 4, 3,
                                                     ifelse(khandle_brfss_harmonized$education_h==5, 4, NA)))))

table(khandle_brfss_harmonized$education_h, khandle_brfss_harmonized$education3_h, exclude=NULL)


#Save final dataset
save(khandle_brfss_harmonized, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized.Rdata")
