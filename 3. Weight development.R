#Pre-weighting covariate balance 

library(twang)
library(survey)
library(tableone)
library(Hmisc)
library(openxlsx)
library(tidyverse)
library(spatstat)

load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized.Rdata")

#Things needed throughout program:
    #Final list of harmonized variables
        harmonized<-colnames(khandle_brfss_harmonized)[!(colnames(khandle_brfss_harmonized)
                                                         %in% c("id_h","brfss_h", "brfss_sampwt_h", 
                                                                "income_quartile", "income_gt50k_pp_h", 
                                                                "income_gt65k_pp_h", "imp_h",
                                                                "cogimp_prob_adj_bl_dx", "cogimp_prob_adj_bl_nodx",
                                                                "cogimp_prob_fin_dx","cogimp_prob_fin_nodx", 
                                                                "county_code", "working_h", 
                                                                
                                                              "education_h", "education2_h",
                                                              "income_pp_h"))]
                                                          #Note we decided that working_h wasn't well-harmonized so dropping it. 
    
    
    #Variable list for checking race-ethnicity specific balance:
      harmonized_norace<-colnames(khandle_brfss_harmonized)[!(colnames(khandle_brfss_harmonized)
                                                              %in% c("id_h","brfss_h", "brfss_sampwt_h", "income_quartile", "income_gt50k_pp_h", 
                                                                     "income_gt65k_pp_h","imp_h", "cogimp_prob_adj_bl_dx", "cogimp_prob_adj_bl_nodx",
                                                                     "cogimp_prob_fin_dx","cogimp_prob_fin_nodx", 
                                                                     "county_code", "working_h", "race_summary_h", 
                                                                     
                                                                     "education_h", "education2_h",
                                                                     "income_pp_h"))]
 
    #Function for running race/eth-specific covariate balance
    
          Checkcovbal<-function(data, weightvar){
            
            #Create empty object for storing cov bal by race results
            covbal_race<-list()
            
            #Run covariate balance for each racial group
            for (i in levels(data$race_summary_h)){
              
              temp<-bal.stat(data=data[data$race_summary_h==i,], 
                             vars=harmonized_norace,
                             w.all=data[data$race_summary_h==i,weightvar],
                             treat.var='brfss_h', 
                             sampw=data$brfss_sampwt_h[data$race_summary_h==i], 
                             estimand="ATT", 
                             multinom = F)
              
              covbal_race[[i]]<-temp$results
              
            }
            
            #Format for saving
            for (i in 1:4){
              covbal_race[[i]][,"var"]<-rownames(covbal_race[[i]])
              covbal_race[[i]]<-covbal_race[[i]][,c("var",colnames(covbal_race[[i]])[1:length(covbal_race[[i]])-1])]
              covbal_race[[i]][,"race"]<-ifelse(i==1,"Asian", ifelse(i==2, "Black", ifelse
                                                                     (i==3, "Latino", ifelse(i==4, "White"))))                                                    
            }
            
            names(covbal_race)<-c("Asian", "Black", "Latino", "White")
            
            #Generate data to plot covariate balance
            covbal_race_toplot<-do.call("rbind", covbal_race)
            covbal_race_toplot<-covbal_race_toplot[order(covbal_race_toplot$var),] #Fix order here!
            
            #covbal_race_toplot$var<-fct_reorder(covbal_race_toplot$var,
            #                                       covbal_race_toplot$std.eff.sz)
            covbal_race_toplot$std.eff.sz<-covbal_race_toplot$std.eff.sz*(-1) #Changes effect size to khandle-brfss
            
            covbal_plot<-ggplot(covbal_race_toplot, aes(x=var, y=std.eff.sz), group=race) + 
              geom_point(size=3, aes(colour=factor(race), shape=factor(race), fill=factor(race))) +
              geom_hline(yintercept=0) +
              ylab("Standardized mean difference (KHANDLE-BRFSS)/SD[BRFSS]") +
              scale_shape_manual(values=c("Asian"=16,"Black"=16,"Latino"=16,"White"=16)) +
              scale_color_manual(values=c("Asian"="purple", "Black"="forestgreen","Latino"="navy", "White"="hotpink")) + 
              theme_bw() +
              theme(axis.title.y=element_blank()) + 
              theme(legend.position = "right") + 
              theme(aspect.ratio = 5/3) + ylim(-1,1.25)+
              coord_flip()
            
            return(list("covbal_race"=covbal_race_toplot, "covbal_plot"=covbal_plot))
            
          }


#Step 1. use full imputed dataset (all 40 imps together) to decide what goes in the weights

    test_imp<-khandle_brfss_harmonized
    
    #A. Run unweighted covbal check
      unw_test<-Checkcovbal(test_imp, "brfss_sampwt_h")
      unw_test$covbal_plot
    
    
    #B. Create and apply first weights: Main sociodemographics (age, sex, edu, race, and race intxns)
    
        #create khandle indicator
        test_imp$khandle_h<- 1-test_imp$brfss_h
        
        #create marginal probability of being in khandle (n in khandle/total n)
        p_khandle<-sum(test_imp$khandle_h)/sum(test_imp$brfss_sampwt_h)
        p_khandle
        
        #use logistic model to predict khandle=1 based on race, sex/gender, age, education, 
        # and 2-way interactions between race and other variables 
        p1<-glm(khandle_h~race_summary_h+male_h+age_h+education3_h+
                  race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h, 
                data=test_imp, family=binomial(link=logit), 
                weights = test_imp$brfss_sampwt_h)
        summary(p1)
        
        #generate conditional predicted probabilites for being in khandle
        test_imp$p1<-predict.glm(p1, type="response")
        summary(test_imp$p1)
        
        #inverse odds weight is P(not khandle|K=k) / P(khandle|K=k)
        test_imp$w1<-(1-test_imp$p1)/(test_imp$p1)
        
        #stabililized inverse odds weight is IOW * odds of being in khandle, wts for BRFSS sample = 1
        test_imp$sw1<-(test_imp$w1)*(p_khandle/(1-p_khandle))
        test_imp$sw1[test_imp$brfss_h==1]<-1
        
        #Check distribution of stabilized weights in khandle sample
        summary(test_imp$sw1[test_imp$khandle_h==1])
        
        #weight to go in covariate balance analysis is multiplied brfss_samp weight and sIOW
        test_imp$sw1_twang<-test_imp$sw1*test_imp$brfss_sampwt_h
        
        
        #Run first weighted covbal check
        w1_test<-Checkcovbal(test_imp, "sw1_twang")
        w1_test$covbal_plot
        #Still looks imbalanced on income, adl_walking, 
        #     goodhealth, english_interview
        
        #Based on this plot, take the most extreme positive and negative differences
        #(walking, income) to weight on for second weights
        
    
    #C. Create and apply second weights (adding income, walking and race intxns)
    
        #use logistic model to predict khandle=1 based on race, sex/gender, age, education, 
        # and 2-way interactions between race and other variables 
        #Add income, adl_walking
        p2<-glm(khandle_h~race_summary_h+male_h+age_h+education3_h+income_gtmed_pp_h+adl_walking_h+
                  race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h+
                  race_summary_h*income_gtmed_pp_h+race_summary_h*adl_walking_h, 
                data=test_imp, family=binomial(link=logit), 
                weights = test_imp$brfss_sampwt_h)
        summary(p2)
        
        #generate conditional predicted probabilites for being in khandle
        test_imp$p2<-predict.glm(p2, type="response")
        summary(test_imp$p2)
        
        #inverse odds weight is P(not khandle|K=k) / P(khandle|K=k)
        test_imp$w2<-(1-test_imp$p2)/(test_imp$p2)
        
        
        #stabililized inverse odds weight is IOW * odds of being in khandle
        test_imp$sw2<-(test_imp$w2)*(p_khandle/(1-p_khandle))
        test_imp$sw2[test_imp$brfss_h==1]<-1
        
        #Check distribution of stabilized weights in khandle sample
        summary(test_imp$sw2[test_imp$khandle_h==1])
        
        #weight to go in covariate balance analysis is mutlitplied brfss_samp weight and sIOW
        test_imp$sw2_twang<-test_imp$sw2*test_imp$brfss_sampwt_h
        
        
        
        #Run second weighted covbal check
        w2_test<-Checkcovbal(test_imp, "sw2_twang")
        w2_test$covbal_plot
        #Still looks imbalanced on english_interview, good health
        
        #Based on this plot, take the most extreme differences
        #(english interview, good health) to weight on for third weights
        
        #Create and apply third weights (and english_interview and race intxns)
        
    #D. Create and apply second weights (adding income, walking and race intxns)
        #use logistic model to predict khandle=1 based on race, sex/gender, age, education, income, walking
        # add english_interview, good health
        # and 2-way interactions between race and other variables 
        p3<-glm(khandle_h~race_summary_h+male_h+age_h+education3_h+income_gtmed_pp_h+adl_walking_h+english_interview_h+goodhealth_h+
                  race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h+
                  race_summary_h*income_gtmed_pp_h+race_summary_h*adl_walking_h+race_summary_h*english_interview_h+race_summary_h*goodhealth_h, 
                data=test_imp, family=binomial(link=logit), 
                weights = test_imp$brfss_sampwt_h)
        summary(p3)
        
        #generate conditional predicted probabilites for being in khandle
        test_imp$p3<-predict.glm(p3, type="response")
        summary(test_imp$p3)
        
        #inverse odds weight is P(not khandle|K=k) / P(khandle|K=k)
        test_imp$w3<-(1-test_imp$p3)/(test_imp$p3)
        
        #stabililized inverse odds weight is IOW * odds of being in khandle
        test_imp$sw3<-(test_imp$w3)*(p_khandle/(1-p_khandle))
        test_imp$sw3[test_imp$brfss_h==1]<-1
        
        #Check distribution of stabilized weights in khandle sample
        summary(test_imp$sw3[test_imp$khandle_h==1])
        
        #weight to go in covariate balance analysis is mutlitplied brfss_samp weight and sIOW
        test_imp$sw3_twang<-test_imp$sw3*test_imp$brfss_sampwt_h
        
        #Run third weighted covbal check
        w3_test<-Checkcovbal(test_imp, "sw3_twang")
        w3_test$covbal_plot
        #Looks balanced (all std mean diff < abs(0.25) except good health still
        # a bit off, but not much to be done about that since we already weight on it. 
        #We're done! We'll use weights based on these variables. 








#Step 2. Run unweighted and final weighted cov balances iterating over imputations
        #to generate output data (weights for next step of analysis) and plots


khandle_brfss_harmonized_weights<-list()
khandle_brfss_harmonized_covbal_weights<-list()
khandle_brfss_harmonized_covbal_noweights<-list()

for (j in 1:40){
  data_imp<-khandle_brfss_harmonized[khandle_brfss_harmonized$imp_h==j,]
  
  data_imp$khandle_h<- 1-data_imp$brfss_h
  #marginal probability of being in khandle is n in khandle/total n
  p_khandle<-sum(data_imp$khandle_h)/sum(data_imp$brfss_sampwt_h)
  p_khandle
  
  #Unweighted analysis
  unw_MI<-Checkcovbal(data_imp, "brfss_sampwt_h")
  khandle_brfss_harmonized_covbal_noweights[[j]]<-unw_MI$covbal_race
  
  
  
  #Weighted analysis using third set of weights from above:
  
  #use logistic model to predict khandle=1 based on race, sex/gender, age, education, income, walking, english_interview, goodhealth
  # and 2-way interactions between race and other variables 
  p3<-glm(khandle_h~race_summary_h+male_h+age_h+education3_h+income_gtmed_pp_h+adl_walking_h+english_interview_h+goodhealth_h+
            race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h+
            race_summary_h*income_gtmed_pp_h+race_summary_h*adl_walking_h+race_summary_h*english_interview_h+race_summary_h*goodhealth_h, 
          data=data_imp, family=binomial(link=logit), 
          weights = data_imp$brfss_sampwt_h)
  summary(p3)
  
  #generate conditional predicted probabilites for being in khandle
  data_imp$p3<-predict.glm(p3, type="response")
  summary(data_imp$p3)
  
  #inverse odds weight is P(not khandle|K=k) / P(khandle|K=k)
  data_imp$w3<-(1-data_imp$p3)/(data_imp$p3)
  
  #stabililized inverse odds weight is IOW * odds of being in khandle
  data_imp$sw3<-(data_imp$w3)*(p_khandle/(1-p_khandle))
  data_imp$sw3[data_imp$brfss_h==1]<-1
  
  #Check distribution of stabilized weights in khandle sample
  summary(data_imp$sw3[data_imp$khandle_h==1])
  
  #weight to go in covariate balance analysis is mutlitplied brfss_samp weight and sIOW
  data_imp$sw3_twang<-data_imp$sw3*data_imp$brfss_sampwt_h
  
  
  #Run weighted covbal and store data
  wt_MI<-Checkcovbal(data_imp, "sw3_twang")
  khandle_brfss_harmonized_covbal_weights[[j]]<-wt_MI$covbal_race
  
  #Store final data_imp dataset with sw3_twang weights
  khandle_brfss_harmonized_weights[[j]]<-data_imp
  
}

#Saving results across all 40 imputations 
khandle_brfss_harmonized_weights_stack<-do.call(rbind,khandle_brfss_harmonized_weights)
save(khandle_brfss_harmonized_weights_stack, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_weights.Rdata")

khandle_brfss_harmonized_covbal_weights_stack<-do.call(rbind,khandle_brfss_harmonized_covbal_weights)

for (i in 1:40){
  if (i==1) {imps<-rep(i,64)} else {imps<-c(imps, rep(i,64))}}
khandle_brfss_harmonized_covbal_weights_stack$imp<-imps
save(khandle_brfss_harmonized_covbal_weights_stack, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_covbal_weights.Rdata")


khandle_brfss_harmonized_covbal_noweights_stack<-do.call(rbind,khandle_brfss_harmonized_covbal_noweights)
khandle_brfss_harmonized_covbal_noweights_stack$imp<-imps
save(khandle_brfss_harmonized_covbal_noweights_stack, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_covbal_noweights.Rdata")
