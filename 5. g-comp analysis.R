library(tidyverse)
library(boot)

# NOTE: This script does g-computation (outcome modeling) treating race/ethnicity
# as a point-treatment, creating replicates of the target sample for all levels of 
# the exposure (race/ethnicity) to estimate inequalities by race/ethnicity. 
# Because there are mediators in the outcome model, a time-varying g-formula 
# implementation may be required to estimate the total effect of race/ethnicity,
# but there is little existing work on this.
# We also implement another analytic approach, using the outcome model to predict 
# outcomes in the target population, and standardizing by age/sex to obtain the 
# total effect, but this is not a standard analytic approach in g-computation.


set.seed(12345)

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




# Estimates needed: 
#   naive KHANDLE (overall, by race)
#   naive KHANDLE PR/PD stdized to all (using OM)
#   naive KHANDLE PR/PD stdized just for age/sex in khandle
#   est BRFSS  (overall, by race)
#   est BRFSS PR/PD stdized to all (using OM)
#   est BRFSS PR/PD stdized just for age/sex in BRFSS



#For each imputation, develop outcome model to predict prevalence of cognitive impairment
#For purposes of demonstration, using same variables as used in IOSW weighting models

for (i in 1:40){
  
data<-khandle_brfss_harmonized %>% filter(imp_h==i)

#run Outcome Model (OM)
outmod<-lm(cogimp_prob_fin_dx~race_summary_h+male_h+age_h+education3_h+
             income_gtmed_pp_h+adl_walking_h+english_interview_h+goodhealth_h+
          race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h+
          race_summary_h*income_gtmed_pp_h+race_summary_h*adl_walking_h+
            #race_summary_h*english_interview_h+ (removed due to sparse data)
            race_summary_h*goodhealth_h, 
        data=data[data$brfss_h==0,] 
        )

data$pred_p<-predict.lm(outmod, data)
data$imposs<-ifelse(data$pred_p<0, -1, ifelse(data$pred_p>1, 1, 0))
table(data$imposs[data$brfss_h==1])
hist(data$pred_p[data$brfss_h==1 & data$pred_p<0])
summary(data$pred_p[data$brfss_h==1])

pred_overall <- data %>% filter(brfss_h==1) %>%  group_by(brfss_h) %>%
  summarise(p_est = weighted.mean(pred_p, brfss_sampwt_h)) %>% 
  bind_rows(data %>% filter(brfss_h==0) %>%  group_by(brfss_h) %>%
                summarise(p_est = mean(cogimp_prob_fin_dx)))

pred_re <- data %>% filter(brfss_h==1) %>% group_by(brfss_h, race_summary_h) %>% 
  summarise(p_est = weighted.mean(pred_p, brfss_sampwt_h)) %>%
  bind_rows(data %>% filter(brfss_h==0) %>%  group_by(brfss_h, race_summary_h) %>%
              summarise(p_est = mean(cogimp_prob_fin_dx)))


#Create replicates to estimate inequality stdzd to all variables in OM 
reps<-rbind(data %>% mutate(race_summary_h=1, rep="Asian"), 
                  data %>% mutate(race_summary_h=2, rep="Black"),
                  data %>% mutate(race_summary_h=3, rep="Latino"),
                  data %>% mutate(race_summary_h=4, rep="White"))

reps$race_summary_h<-as.factor(reps$race_summary_h)

reps$pred_p<-predict.lm(outmod, reps)

pred_stdall <- reps %>% group_by(brfss_h, rep) %>% 
  summarise(p_est = weighted.mean(pred_p, brfss_sampwt_h))
pred_stdall<-pred_stdall %>% mutate(PRall=p_est/p_est[rep=="White"],
                                                PDall=p_est-p_est[rep=="White"]) %>%
  select(-p_est)

#Standardize only to age/sex categories 
k<-data %>% filter(brfss_h==0)
by_race_k<-k %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_unw=mean(cogimp_prob_fin_dx)) %>%
  left_join(.,agesex_totals_k, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
  summarise(pcogimp = crossprod(est_prop_unw, prop_agesex)) %>% data.frame()


b<-data %>% filter(brfss_h==1)
by_race_b<-b %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_unw=mean(pred_p)) %>%
  left_join(.,agesex_totals, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
  summarise(pcogimp = crossprod(est_prop_unw, prop_agesex)) %>% data.frame()

  
pred_stdall$PRas<- c(by_race_k$pcogimp/by_race_k$pcogimp[4],
                                     by_race_b$pcogimp/by_race_b$pcogimp[4])
                       
pred_stdall$PDas<- c(by_race_k$pcogimp-by_race_k$pcogimp[4],
                                     by_race_b$pcogimp-by_race_b$pcogimp[4])
  
#Format results
pred_overall$race_summary_h<-0
crude_results<-rbind(pred_overall, pred_re)
crude_results$Group<-ifelse(crude_results$race_summary_h==0, "Overall", 
                            ifelse(crude_results$race_summary_h==1, "Asian", 
                                   ifelse(crude_results$race_summary_h==2, "Black",
                                          ifelse(crude_results$race_summary_h==3, "Latino", "White"))))
crude_results <- crude_results %>% rename(Pcogimp=p_est) %>% select(-race_summary_h)
pred_stdall <- pred_stdall %>% rename(Group=rep)

Results<-full_join(pred_stdall, crude_results, 
                   by=c("Group" = "Group", "brfss_h"="brfss_h")) %>%
                  rename(BRFSS=brfss_h) %>% select(BRFSS, Group, Pcogimp, 
                                                   PRall, PDall, PRas, PDas)

Results_wide<-pivot_wider(Results,names_from = c("BRFSS", "Group"), values_from = c("Pcogimp", "PRall", "PDall", "PRas", "PDas"))

if (i==1){ Results_all<-Results_wide} else {
  Results_all<-rbind(Results_all, Results_wide)}

#Need to save results from all imputations
}



analysis<-function(data,indices){
  data<-data[indices,]
  outmod<-lm(cogimp_prob_fin_dx~race_summary_h+male_h+age_h+education3_h+
                       income_gtmed_pp_h+adl_walking_h+english_interview_h+goodhealth_h+
                       race_summary_h*male_h+race_summary_h*age_h+race_summary_h*education3_h+
                       race_summary_h*income_gtmed_pp_h+race_summary_h*adl_walking_h+
                       #race_summary_h*english_interview_h+ (removed due to sparse data)
                       race_summary_h*goodhealth_h, 
                     data=data[data$brfss_h==0,] 
)

data$pred_p<-predict.lm(outmod, data)
data$imposs<-ifelse(data$pred_p<0, -1, ifelse(data$pred_p>1, 1, 0))

pred_overall <- data %>% filter(brfss_h==1) %>%  group_by(brfss_h) %>%
  summarise(p_est = weighted.mean(pred_p, brfss_sampwt_h)) %>% 
  bind_rows(data %>% filter(brfss_h==0) %>%  group_by(brfss_h) %>%
              summarise(p_est = mean(cogimp_prob_fin_dx)))

pred_re <- data %>% filter(brfss_h==1) %>% group_by(brfss_h, race_summary_h) %>% 
  summarise(p_est = weighted.mean(pred_p, brfss_sampwt_h)) %>%
  bind_rows(data %>% filter(brfss_h==0) %>%  group_by(brfss_h, race_summary_h) %>%
              summarise(p_est = mean(cogimp_prob_fin_dx)))

#Create replicates to estimate inequality stdzd to all variables in OM 
reps<-rbind(data %>% mutate(race_summary_h=1, rep="Asian"), 
            data %>% mutate(race_summary_h=2, rep="Black"),
            data %>% mutate(race_summary_h=3, rep="Latino"),
            data %>% mutate(race_summary_h=4, rep="White"))

reps$race_summary_h<-as.factor(reps$race_summary_h)

reps$pred_p<-predict.lm(outmod, reps)

pred_stdall <- reps %>% group_by(brfss_h, rep) %>% 
  summarise(p_est = weighted.mean(pred_p, brfss_sampwt_h))
pred_stdall<-pred_stdall %>% mutate(PRall=p_est/p_est[rep=="White"],
                                    PDall=p_est-p_est[rep=="White"]) %>%
  select(-p_est)

#Standardize only to age/sex categories 
k<-data %>% filter(brfss_h==0)
by_race_k<-k %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_unw=mean(cogimp_prob_fin_dx)) %>%
  left_join(.,agesex_totals_k, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
  summarise(pcogimp = crossprod(est_prop_unw, prop_agesex)) %>% data.frame()


b<-data %>% filter(brfss_h==1)
by_race_b<-b %>% group_by(race_summary_h, agecat_h, male_h) %>%  summarise(est_prop_unw=mean(pred_p)) %>%
  left_join(.,agesex_totals, by=c("agecat_h", "male_h")) %>% group_by(race_summary_h) %>%
  summarise(pcogimp = crossprod(est_prop_unw, prop_agesex)) %>% data.frame()


pred_stdall$PRas<- c(by_race_k$pcogimp/by_race_k$pcogimp[4],
                     by_race_b$pcogimp/by_race_b$pcogimp[4])

pred_stdall$PDas<- c(by_race_k$pcogimp-by_race_k$pcogimp[4],
                     by_race_b$pcogimp-by_race_b$pcogimp[4])

#Format results
pred_overall$race_summary_h<-0
crude_results<-rbind(pred_overall, pred_re)
crude_results$Group<-ifelse(crude_results$race_summary_h==0, "Overall", 
                            ifelse(crude_results$race_summary_h==1, "Asian", 
                                   ifelse(crude_results$race_summary_h==2, "Black",
                                          ifelse(crude_results$race_summary_h==3, "Latino", "White"))))
crude_results <- crude_results %>% rename(Pcogimp=p_est) %>% select(-race_summary_h)
pred_stdall <- pred_stdall %>% rename(Group=rep)

Results<-full_join(pred_stdall, crude_results, 
                   by=c("Group" = "Group", "brfss_h"="brfss_h")) %>%
  rename(BRFSS=brfss_h) %>% select(BRFSS, Group, Pcogimp, 
                                   PRall, PDall, PRas, PDas)

Results_wide<-pivot_wider(Results,names_from = c("BRFSS", "Group"), values_from = c("Pcogimp", "PRall", "PDall", "PRas", "PDas"))
res<-as.numeric(t(Results_wide))
return(res)
}

#Bootstrap inference



for (i in 1:40){
  
  dat <- khandle_brfss_harmonized %>% filter(imp_h==i)
  bootout<-boot(data=dat, statistic=analysis, strata=dat$brfss_h,
                R=1000)
  
  
  bootout_formatted<-data.frame(bootout$t)
  colnames(bootout_formatted)<-colnames(Results_wide)
  
  if (i==1) {bootests<-bootout_formatted} else {bootests<-rbind(bootests,bootout_formatted)}
  
}



#Combine results:
Est<-sapply(Results_all, function(x) mean(x, na.rm=T))
LCIS<-sapply(bootests, function(x) quantile(x,0.025, na.rm=T))
UCIS<-sapply(bootests, function(x) quantile(x,0.975, na.rm=T))


All_res_wide<-data.frame(rbind(Est, LCIS, UCIS))
All_res_wide$Var<-c("Mean", "LCI", "UCI")

Final_results<-All_res_wide %>% 
  pivot_longer(., !"Var", names_to=c("Est_type", "BRFSS", "Group"), 
               names_sep = "_") %>% pivot_wider(., names_from="Var")



save(bootests, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/gcomp_bootests.Rdata")
save(Final_results, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/gcomp_results.Rdata")



#Results figures
#Prevalence estimates

Prev_plot<-Final_results %>% filter(Est_type=="Pcogimp")

final_results<-ggplot(data=Prev_plot)+
  geom_pointrange(aes(x=factor(Group, levels = c("Overall", "Asian",
                                                 "Black", "Latino",
                                                 "White")), 
                      y=Mean, ymin=LCI, ymax=UCI, group=factor(BRFSS, levels=c(1,0)), 
                      color=factor(BRFSS, levels=c(1,0))), position=position_dodge(width=.4), size=1.5, shape=15)+
  scale_color_manual(name="", breaks=c(1,0),
                     labels=c("CA-BRFSS based on KHANDLE OM", "KHANDLE"), 
                     values=c("gray17","gray68")) +
  xlab("")+ ylab("Estimated prevalence of \ncognitive impairment (95% CI)")+ 
  scale_y_continuous(labels = scales::percent, )+
  theme_bw()+ 
  guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  theme(axis.text.x = element_text(size=12, face="bold"), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        legend.position = "bottom"
  )
final_results

ggsave(final_results, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/final_results_gcomp.jpg")



#PRs

PR_plot<-Final_results %>% filter(Est_type %in% c("PRall", "PRas") & Group != "Overall" & Group != "White") %>% mutate(
    Stdz = case_when(Est_type=="PRall" ~ "All", 
                    Est_type == "PRas" ~ "ASonly"), 
    Group2 = paste0(BRFSS,Stdz))
 
final_results_stdPR<-ggplot(data=PR_plot)+
  geom_pointrange(aes(x=factor(Group, levels = c("Asian", "Black", 
                                                 "Latino")), 
                      y=Mean, ymin=LCI, ymax=UCI,
                      color=factor(Group2, levels=c("1ASonly", "1All", "0ASonly", "0All")),  
                      group=factor(Group2, levels=c("1ASonly", "1All", "0ASonly", "0All")), 
                      shape=factor(Group2, levels=c("1ASonly", "1All", "0ASonly", "0All"))), 
                  position=position_dodge(width=.4), size=1.5)+
  
  scale_colour_manual(name = "Sample and standardization variables",
                      labels = c("CA-BRFSS, age/sex only", 
                                 "CA-BRFSS, all vars in OM", 
                                 "KHANDLE, age/sex only", "KHANDLE, all vars in OM"),
                      values = c("gray17","gray17","gray68","gray68")) +   
  scale_shape_manual(name = "Sample and standardization variables",
                     labels = c("CA-BRFSS, age/sex only", 
                                "CA-BRFSS, all vars in OM", 
                                "KHANDLE, age/sex only", "KHANDLE, all vars in OM"),
                     values = c(15, 19, 15, 19))+
  
  
  #scale_shape_manual(name="", values=c(15,19), labels=c("Standardized to all vars in outcome model", "Standardized to age/sex only"))+
  scale_x_discrete(labels=c("Asian vs. White", "Black vs. White", "Latino vs. White"))+
  #scale_color_manual(name="", breaks=c(1,0),
         #            labels=c("CA-BRFSS based on KHANDLE OM", "KHANDLE"), 
          #           values=c("gray17","gray68")) +
  xlab("")+ ylab("Estimated prevalence ratios for \ncognitive impairment (95% CI)")+ 
  scale_y_continuous(breaks=seq(0.8, 2.2, .2))+
  theme_bw()+ 
  #guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  #guides(shape = guide_legend(override.aes = list(linetype = 0, size=1)))+
  geom_abline(intercept=1, slope=0)+
  theme(axis.text.x = element_text(size=12, face="bold"), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.position = "bottom"
  )+
  guides(color=guide_legend(nrow=2, title.position = "top", title.hjust = .5),
         shape=guide_legend(nrow=2, title.position = "top", title.hjust = .5))
final_results_stdPR


ggsave(final_results_stdPR, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/final_results_PR_gcomp.jpg")



#COmparison to IOSW results

load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/results_final_cogimp_prob_fin_nodx.Rdata")


Prev_plot_comp<-Prev_plot %>% bind_rows(all_res %>% filter(weighted==1) %>% 
                                          mutate(Est_type="Pcogimp", BRFSS="2") %>% 
                                          rename(Mean=est_pcogimp, Group=group) %>% 
                                          select(Est_type, BRFSS, Group, Mean, LCI, UCI))

final_results_comp<-ggplot(data=Prev_plot_comp)+
  geom_pointrange(aes(x=factor(Group, levels = c("Overall", "Asian",
                                                 "Black", "Latino",
                                                 "White")), 
                      y=Mean, ymin=LCI, ymax=UCI, group=factor(BRFSS, levels=c(2,1,0)), 
                      color=factor(BRFSS, levels=c(2,1,0))), position=position_dodge(width=.4), size=1.5, shape=15)+
  scale_color_manual(name="", breaks=c(2,1,0),
                     labels=c("IOSW estimate", "OM estimate", "Original KHANDLE estimate"), 
                     values=c("black", "gray60","gray85")) +
  xlab("")+ ylab("Estimated prevalence of \ncognitive impairment (95% CI)")+ 
  scale_y_continuous(labels = scales::percent, )+
  theme_bw()+ 
  guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  theme(axis.text.x = element_text(size=12, face="bold"), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        legend.position = "bottom"
  )
final_results_comp

ggsave(final_results_comp, file="C:/Users/ehlarson/Box/KHANDLE weighting/Psy-mca materials/Output/final_results_comp.jpg")




