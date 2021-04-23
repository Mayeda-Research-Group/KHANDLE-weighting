#Results summary
library(twang)
library(openxlsx)
library(cowplot)
library(tidyverse)
library(ggpubr)

load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized.Rdata")
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_weights.Rdata")
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_covbal_noweights.Rdata")
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_brfss_harmonized_covbal_weights.Rdata")
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/results_final.Rdata")
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/results_final_std.Rdata")

#Final list of harmonized variables
harmonized<-colnames(khandle_brfss_harmonized)[!(colnames(khandle_brfss_harmonized)
                                                 %in% c("id_h","brfss_h", "brfss_sampwt_h", 
                                                        "income_pp_h", "income_gt50k_pp_h", 
                                                        "income_gt65k_pp_h", "imp_h",
                                                        "cogimp_prob_adj_bl_dx", "cogimp_prob_adj_bl_nodx",
                                                        "cogimp_prob_fin_dx","cogimp_prob_fin_nodx", 
                                                        "county_code", "working_h", "education_h", "education2_h"))]
#Note we decided that working_h wasn't well-harmonized so dropping it. 


#Calculate mean of harmonizable variables across all imputations (for table 1)
temp<-bal.stat(data=khandle_brfss_harmonized, 
               vars=harmonized,
               w.all=khandle_brfss_harmonized$brfss_sampwt_h,
               treat.var='brfss_h', 
               sampw=khandle_brfss_harmonized$brfss_sampwt_h, 
               estimand="ATT", 
               multinom = F)

unw_covbal_MIcomb<-temp$results
unw_covbal_MIcomb$var<-rownames(unw_covbal_MIcomb)

#Save results For Table 1
write.xlsx(unw_covbal_MIcomb, file ="C:/Users/ehlarson/Box/KHANDLE weighting/Output/unw_covbal_MIcomb.xlsx")

#Step 2: Weighted and unweighted covbal plots across MI

#Stack weighted and unweighted and keep relevant vars
khandle_brfss_harmonized_covbal_noweights_stack$wtd<-"A. Unweighted KHANDLE"
khandle_brfss_harmonized_covbal_weights_stack$wtd<-"B. Weighted KHANDLE"
indata<-rbind(khandle_brfss_harmonized_covbal_noweights_stack, khandle_brfss_harmonized_covbal_weights_stack)

test<-indata[,c("var", "std.eff.sz", "imp", "race", "wtd")]

#Get mean across imputations to plot
toplot<-  test %>% group_by(var, race, wtd) %>%
  summarise(mean_std.eff.sz=mean(std.eff.sz))

#Getting variable  order right for plot
order_vars<-c("age_h", "male_h","education3_h:1", 
              "education3_h:2", "education3_h:3", "education3_h:4",
                "income_gtmed_pp_h", 
              "english_interview_h", "goodhealth_h", "adl_walking_h",
              
              "marital_h","military_h", 
               "blind_h", "adl_dressing_h", 
              "smoke_status_h", "exercise_h")

toplot$var2<-factor(toplot$var, levels=order_vars)
final<-toplot[order(toplot$var2),]

wtvars<-c("age_h", "male_h","education3_h.1", 
          "education3_h.2", "education3_h.3", "education3_h.4",
          "income_gtmed_pp_h", 
          "english_interview_h", "adl_walking_h")


var_labels<-c("Age, years", "Male", "Less than high school",  "High school/GED", 
              "Some college or trade/technical school", 
              "College graduate or more", "Income above BRFSS median",
              "Interviewed in English", "Self-rated health 'good' or better", 
              "Difficulty walking",
              
              "Married/living with partner", 
              "Military service",  
              "Blind/serious vision impairment", "Difficulty dressing", 
              "Current smoker", "Exercise in last month")

#Make plot                  
final_covbal_plot<-ggplot(final, aes(x=var, y=as.numeric(mean_std.eff.sz))) +  
  geom_rect(data = data.frame(wtd = "B. Weighted KHANDLE", labvar="Variable not included in weights"), 
            aes(xmin = 0, xmax = 6.5, ymin = -Inf, ymax = Inf, fill=labvar), 
            alpha = 0.3, inherit.aes = FALSE)+
  scale_fill_manual("",values="grey")+
  geom_point(size=3, aes(colour=factor(race))) +
  geom_hline(yintercept=0, size=1) +
  ylab("Standardized mean difference (KHANDLE-CABRFSS)/SD[CABRFSS]") +
  scale_color_manual(name="Race/ethnicity", 
                     values=c("Asian"="purple", "Black"="forestgreen","Latino"="navy", "White"="hotpink")) +
  scale_x_discrete(limits=rev(order_vars), labels=rev(var_labels))+
  guides(fill = guide_legend(order = 2),col = guide_legend(order = 1))+
  facet_grid(.~wtd)+
  theme_bw() +
  theme(axis.title.y=element_blank()) + 
  theme(legend.position = "bottom", legend.box="vertical", legend.margin=margin()) + 
   ylim(-1,1.25)+
  coord_flip()
final_covbal_plot

ggsave(final_covbal_plot, file="C:/Users/ehlarson/Box/KHANDLE weighting/Output/covbal_MIcomb_final.jpg")


#Step 3: Propensity score overlap                                    
#Get mean propensity score for each person so we can plot dist of mean score
meanpscore<-data.frame(khandle_brfss_harmonized_weights_stack %>%  group_by(id_h) %>% 
                         summarise(mean_pscore=mean(p3), brfss_h=mean(brfss_h), race_summary_h=mean(as.numeric(race_summary_h))))


#ratio of sample sizes
oneimp<-khandle_brfss_harmonized %>% filter(imp_h==1) 
ratio<-nrow(oneimp[oneimp$brfss_h==0,])/sum(oneimp$brfss_sampwt_h[oneimp$brfss_h==1])

meanpscore$mean_pscore_scaled<-meanpscore$mean_pscore/(ratio)

head(meanpscore)

#Density plots of propensity scores
pscoredens_MIcomb<-ggplot(meanpscore) + 
  geom_density(aes(x=mean_pscore,linetype=factor(brfss_h)), size=1)+xlim(0,0.005)+
  scale_linetype_manual(name="Sample", values=c("0"=1, "1"=2), labels=c("KHANDLE", "CA BRFSS"))+
  xlab("Propensity score (truncated at 0.005)")+ylab("Density")+
  theme_bw()
pscoredens_MIcomb

ggsave(pscoredens_MIcomb, file="C:/Users/ehlarson/Box/KHANDLE weighting/Output/pscoredensity_MIcomb.jpg")

#by race/ethnicity
meanpscore$race_labels<-ifelse(meanpscore$race_summary_h==1, "Asian",
                               ifelse(meanpscore$race_summary_h==2, "Black", 
                                      ifelse(meanpscore$race_summary_h==3, "Latino", 
                                             ifelse(meanpscore$race_summary_h==4, "White", NA))))
  
pscoredens_race_MIcomb<-ggplot(meanpscore) + 
  geom_density(aes(x=mean_pscore,linetype=factor(brfss_h)), size=1)+xlim(0,0.005)+
  facet_wrap("race_labels")+
  scale_linetype_manual(name="Sample", values=c("0"=1, "1"=2), labels=c("KHANDLE", "CA BRFSS"))+
  xlab("Propensity score (truncated at 0.005)")+ylab("Density")+
  theme_bw()
pscoredens_race_MIcomb

ggsave(pscoredens_race_MIcomb, file="C:/Users/ehlarson/Box/KHANDLE weighting/Output/pscoredensity_race_MIcomb.jpg")


summary(khandle_brfss_harmonized_weights_stack %>% filter(brfss_h==0) %>% select(sw3))
summary(khandle_brfss_harmonized_weights_stack %>% filter(brfss_h==0, race_summary_h==1) %>% select(sw3))
summary(khandle_brfss_harmonized_weights_stack %>% filter(brfss_h==0, race_summary_h==2) %>% select(sw3))
summary(khandle_brfss_harmonized_weights_stack %>% filter(brfss_h==0, race_summary_h==3) %>% select(sw3))
summary(khandle_brfss_harmonized_weights_stack %>% filter(brfss_h==0, race_summary_h==4) %>% select(sw3))


#Step 4: Results plot of main results of paper
final_results<-ggplot(data=all_res)+
  geom_pointrange(aes(x=factor(group, levels = c("Overall", "Asian",
                                                 "Black", "Latino",
                                                 "White")), 
                      y=est_pcogimp, ymin=LCI, ymax=UCI, group=factor(weighted, levels=c(1,0)), 
                      color=factor(weighted, levels=c(1,0))), position=position_dodge(width=.4), size=1.5, shape=15)+
  scale_color_manual(name="", breaks=c(1,0),
                     labels=c("KHANDLE generalized to CA BRFSS", "Unweighted KHANDLE"), 
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

ggsave(final_results, file="C:/Users/ehlarson/Box/KHANDLE weighting/Output/final_results.jpg")




#Step 5: Results plot of main results of paper, age- and sex-adjusted



final_results_stdPR<-ggplot(data=all_res_std %>% filter(group %in% c("AW_PR", "BW_PR", 
                                                                   "LW_PR")))+
  geom_pointrange(aes(x=factor(group, levels = c("AW_PR", "BW_PR", 
                                                 "LW_PR")), 
                      y=est, ymin=LCI, ymax=UCI,
                      color=factor(weighted, levels = c(1,0)), group=factor(weighted, levels=c(1,0))), 
                  position=position_dodge(width=.4), size=1.5, shape=15)+
  scale_x_discrete(labels=c("Asian vs. White", "Black vs. White", "Latino vs. White"))+
  scale_color_manual(name="", breaks=c(1,0),
                     labels=c("KHANDLE generalized to CA BRFSS", "Unweighted KHANDLE"), 
                     values=c("gray17","gray68")) +
  xlab("")+ ylab("Estimated prevalence ratios for \ncognitive impairment (95% CI)")+ 
  scale_y_continuous(breaks=seq(0.8, 2.2, .2))+
  theme_bw()+ 
  #guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  geom_abline(intercept=1, slope=0)+
  theme(axis.text.x = element_text(size=12, face="bold"), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.position = "none"
  )
final_results_stdPR



final_results_stdPD<-ggplot(data=all_res_std %>% filter(group %in% c("AW_PD", "BW_PD", 
                                                                     "LW_PD")))+
  geom_pointrange(aes(x=factor(group, levels = c("AW_PD", "BW_PD", 
                                                 "LW_PD")), 
                      y=est, ymin=LCI, ymax=UCI,
                      color=factor(weighted, levels = c(1,0)), group=factor(weighted, levels=c(1,0))), 
                  position=position_dodge(width=.4), size=1.5, shape=15)+
  scale_x_discrete(labels=c("Asian vs. White", "Black vs. White", "Latino vs. White"))+
  scale_color_manual(name="", breaks=c(1,0),
                     labels=c("KHANDLE generalized to CA BRFSS", "Unweighted KHANDLE"), 
                     values=c("gray17","gray68")) +
  xlab("")+ ylab("Estimated prevalence differences for \ncognitive impairment (95% CI)")+ 
  scale_y_continuous(labels = scales::percent, )+
  geom_abline(intercept=0, slope=0)+
  theme_bw()+ 
  #guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  theme(axis.text.x = element_text(size=12, face="bold"), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.position = "none"
  )
final_results_stdPD

combined_std<-ggarrange(final_results_stdPR, final_results_stdPD,
          align='h', labels=c('A', 'B'),
          common.legend = T, 
          legend="bottom")

combined_std

ggsave(combined_std, file="C:/Users/ehlarson/Box/KHANDLE weighting/Output/final_results_stdPRPD.jpg")




#Post code review 4/21/2021
#Generate Table 1 by race/ethnicity
covbal_race<-list()

#Run covariate balance for each racial group
data<-khandle_brfss_harmonized

harmonized_norace<-harmonized[!harmonized %in% "race_summary_h"]


for (i in levels(data$race_summary_h)){
  
  temp<-bal.stat(data=data[data$race_summary_h==i,], 
                 vars=harmonized_norace,
                 w.all=data$brfss_sampwt_h[data$race_summary_h==i],
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

#Save results For Table 1
write.xlsx(covbal_race, file ="C:/Users/ehlarson/Box/KHANDLE weighting/Output/unw_covbal_MIcomb_race.xlsx")

#Get n's by race/ethnicity for header of Table S3 (just use one imputation because we didn't have to impute race)
table(data$race_summary_h[data$brfss_h==0 & data$imp_h==1])
data %>% filter(imp_h==1, brfss_h==1) %>% group_by(race_summary_h) %>% summarise(n=sum(brfss_h))

# final_results_std1<-ggplot(data=all_res_std %>% filter(group %in% c("Asian",
#                                                                     "Black",
#                                                                     "Latino",
#                                                                     "White")))+
#   geom_pointrange(aes(x=factor(group, levels = c("Asian",
#                                                  "Black", "Latino",
#                                                  "White")), 
#                       y=est, ymin=LCI, ymax=UCI, group=factor(weighted, levels=c(0,1)), 
#                       color=factor(weighted, levels=c(0,1))), position=position_dodge(width=.4), size=1.5, shape=15)+
#   scale_color_manual(name="", breaks=c(0,1),
#                      labels=c("Unweighted KHANDLE", "KHANDLE generalized to BRFSS"), 
#                      values=c("gray68","gray17")) +
#   xlab("")+ ylab("Estimated prevalence of \ncognitive impairment (95% CI)")+ 
#   scale_y_continuous(labels = scales::percent, )+
#   theme_bw()+ 
#   guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
#   theme(axis.text.x = element_text(size=12, face="bold"), 
#         axis.text.y = element_text(size=12), 
#         axis.title.x = element_text(size=14), 
#         axis.title.y = element_text(size=14), 
#         legend.position = "bottom"
#   )
# 
# final_results_std1
# 
# final_results_std<-ggplot(data=all_res_std %>% filter(group %in% c("Asian",
#                                                                    "Black",
#                                                                    "Latino",
#                                                                    "White") & weighted==1))+
#   geom_pointrange(aes(x=factor(group, levels = c("Asian",
#                                                  "Black", "Latino",
#                                                  "White")), 
#                       
#                       y=est, ymin=LCI, ymax=UCI, 
#                       color=factor(group, levels = c("Asian",
#                                                      "Black", "Latino",
#                                                      "White"))), position=position_dodge(width=.4), size=1.5, shape=15)+
#   scale_x_discrete(labels=c("Asian", "Black", "Latino", "White"))+
#   scale_color_manual(name="", 
#                      labels=c("Asian",
#                               "Black", "Latino",
#                               "White"),
#                      values=c("Asian"="purple", "Black"="forestgreen","Latino"="navy", "White"="hotpink")) +
#   xlab("")+ ylab("Estimated prevalence of \ncognitive impairment (95% CI)")+ 
#   scale_y_continuous(labels = scales::percent, )+
#   theme_bw()+ 
#   guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
#   theme(axis.text.x = element_text(size=12, face="bold"), 
#         axis.text.y = element_text(size=12), 
#         axis.title.x = element_text(size=14), 
#         axis.title.y = element_text(size=14), 
#         legend.position = "bottom"
#   )
# final_results_std
