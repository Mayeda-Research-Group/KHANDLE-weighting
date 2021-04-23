#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse","ggplot2","survey", "tableone", "mice", "openxlsx")

#------------------------------------------------------------------
# KHANDLE
#------------------------------------------------------------------

#load and examine dataset
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/KW_KHANDLE_analysis.Rdata")
khandle_clean<-clean_data
str(khandle_clean)
vars<-colnames(khandle_clean)


#Assess missingness in vars we want to harmonize on / use for analysis
harmonizable.vars<-c("w1_interview_age", "male", "race_summary", 
                     "marital_status", "education", "income_range", "military", 
                     "sensimp_vision", "english_interview", "health", "smoke_status",
                     "adl1", "adl2", "adl9", 
                     "pa_lt_ex", "pa_vig_ex", 
                     "w1_cogimp_prob_fin_dx")
#3/17/21 EHL post code review: removed other 3 cogimp vars. 
                   #Moved concerned_thinking_v2 and working to auxiliary (line 72).

missingpattern<-md.pattern(khandle_clean[,harmonizable.vars], plot=F)


missingsummary<-data.frame(varname=harmonizable.vars, pctmiss=NA)
row.names(missingsummary)<-harmonizable.vars
for (i in harmonizable.vars){
  missingsummary[i,"pctmiss"]<-100*sum(is.na(khandle_clean[,i]))/nrow(khandle_clean)
  
  print(i)
  print(table(clean_data[,i], exclude=NULL))
}

missingsummary<-missingsummary[order(missingsummary$pctmiss),]  
missingsummary

#Assess missingness in auxiliary vars 

#First, check which equipment variables are useful
equips<-c("w1_equipment_brace","w1_equipment_cane", "w1_equipment_crutches", 
          "w1_equipment_dk", "w1_equipment_none", "w1_equipment_orthopedic", 
          "w1_equipment_other",  "w1_equipment_oxygen",
          "w1_equipment_prosthesis", "w1_equipment_railings", 
          "w1_equipment_refused",  "w1_equipment_walker", 
          "w1_equipment_wheelchair")

for (i in equips){
  print(i)
  print(table(khandle_clean[,i], exclude=NULL))
}

#Decided to keep none cane walker railings based on being indicators 
#with less sparsity 

#Then order auxiliary variables by missingness
aux.vars<-c('income_worry', "born_us", "growingup_finance", 
            "growingup_gohungry", "growingup_housing", "ladder", "adl3", "adl4", 
            "adl5", "adl6", "adl7", "adl8", "iadl1", "iadl2", "iadl3", 
            "w1_equipment_cane", "w1_equipment_none", 
            "w1_equipment_railings", "w1_equipment_walker", 
            "w1_nihtlbx_depr_theta", 
            "w1_d_senas_exec_z", "w1_d_senas_sem_z", "w1_d_senas_vrmem_z", 
            "pain", "pa_hvy_wrk", "pa_lt_hse", "pa_lt_wrk", 
            "pa_vig_hse", "concerned_thinking_v2", "working")
length(aux.vars)

auxmissingsummary<-data.frame(varname=aux.vars, pctmiss=NA)
row.names(auxmissingsummary)<-aux.vars
for (i in aux.vars){
  auxmissingsummary[i,"pctmiss"]<-100*sum(is.na(khandle_clean[,i]))/nrow(khandle_clean)
  
  print(i)
  print(table(khandle_clean[,i], exclude=NULL))
}

auxmissingsummary<-auxmissingsummary[order(auxmissingsummary$pctmiss),]  
auxmissingsummary


#Combine to get vector of all variables that will go in imputation model, 
#Ordered by increasing missingness in vars.to.impute, and subsequenty, aux.vars. 
all.vars<-c(paste(missingsummary$varname), paste(auxmissingsummary$varname))
length(all.vars)
all.vars

#check that variables aren't listed twice
sum(duplicated(all.vars))


## Run single imputation
#prep data by dropping vars we don't need, ordering by missingness
in.data<-khandle_clean[,all.vars]

#Set variable types
continuous.vars<-c("w1_interview_age", 
                   "w1_nihtlbx_depr_theta", 
                   "w1_d_senas_exec_z", "w1_d_senas_sem_z", "w1_d_senas_vrmem_z",
                   "w1_cogimp_prob_fin_dx")
binary.vars<-c("male","english_interview", "military", "working",
               "w1_equipment_cane", "w1_equipment_none", "w1_equipment_railings",
               "w1_equipment_walker","concerned_thinking_v2", "pain", "born_us")
ordinal.vars<-c("education", "income_range", "sensimp_vision", "health",
                "pa_lt_ex", "pa_vig_ex",
                "adl1", "adl2", "adl9",
                'income_worry', 
                "growingup_finance", "growingup_gohungry", "ladder",
                "adl3", "adl4", 
                "adl5", "adl6", "adl7", "adl8", "iadl1", "iadl2", "iadl3",
                "pa_hvy_wrk", "pa_lt_hse", "pa_lt_wrk", "pa_vig_hse")
categorical.vars<-c("race_summary", "marital_status",
                    "growingup_housing", "smoke_status")

#Check that have all variables, no duplicates
length(c(continuous.vars, binary.vars, ordinal.vars, categorical.vars))
sum(duplicated(c(continuous.vars, binary.vars, ordinal.vars, categorical.vars)))    

#Set variable classes by type
for (i in binary.vars){
  in.data[,i]<-factor(in.data[,i])}
for (i in ordinal.vars){
  in.data[,i]<-factor(in.data[,i], ordered=T)}
for (i in categorical.vars){
  in.data[,i]<-factor(in.data[,i],ordered=F)}

#recheck classes
classsum<-data.frame(varname=all.vars, class=NA)
for (i in all.vars){
  classsum[classsum$varname==i,"class"]<-(class(in.data[,i])[1])
}
classsum

#Initiate imputations
ini<-mice(in.data, maxit=0, defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

ini$method
meth<-ini$method
meth

ini$predictorMatrix
pred<-ini$predictorMatrix


#Run imputations
imp_pmm_all<-mice(in.data, m=40, maxit=10, pred=pred, meth=meth, 
                  defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

#examine diagnostics
imp_pmm_all$loggedEvents
plot(imp_pmm_all)
densityplot(imp_pmm_all, ~income_range)

#Save final imputed datasetss
temp_KHANDLE<-list()
for (i in 1:40){
  temp_KHANDLE[[i]]<-complete(imp_pmm_all,action=i)
  
  temp_KHANDLE[[i]]<-cbind(khandle_clean$studyid, temp_KHANDLE[[i]][,harmonizable.vars])
  temp_KHANDLE[[i]][,"imp"]<-i
}

khandle_analysis_pmm<-do.call(rbind,temp_KHANDLE)

save(khandle_analysis_pmm, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/khandle_analysis_pmm.Rdata")




#------------------------------------------------------------------
# BRFSS
#------------------------------------------------------------------

#load and examine dataset
load("C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/KW_BRFSS_analysis.Rdata")
brfss_clean<-clean_data

str(brfss_clean)
vars<-colnames(brfss_clean)

#Assess missingness in vars we want to harmonize on
harmonizable.vars_brfss<-c("age", "male", "race_summary", 
                           "marital_status", "education", "income", "military", 
                           "vision_impair", "english_interview", "health", "smoke_status",
                           "diff_walk", "diff_dress",
                           "phys_activity", "hhsize_v2")
#3/17/21 EHL post code review: moved diff_remember and working to auxiliary (line 216)

missingpattern_brfss<-md.pattern(brfss_clean[,harmonizable.vars_brfss], plot=F)


missingsummary_brfss<-data.frame(varname=harmonizable.vars_brfss, pctmiss=NA)
row.names(missingsummary_brfss)<-harmonizable.vars_brfss
for (i in harmonizable.vars_brfss){
  missingsummary_brfss[i,"pctmiss"]<-100*sum(is.na(brfss_clean[,i]))/nrow(brfss_clean)
  
  print(i)
  print(table(brfss_clean[,i], brfss_clean$year, exclude=NULL))
}

missingsummary_brfss<-missingsummary_brfss[order(missingsummary_brfss$pctmiss),]  
missingsummary_brfss


#Assess missingness in auxiliary vars 
aux.vars_brfss<-c("county_code", "ownhome_v3", 
                  "insured", "diff_errands", "mental_health", "physical_health", 
                  "stroke_v2", "angina_v2", "arthritis_v2", "kidney_v2", "diabetes_v2", 
                  "depress_v2", "weight_v2","cdc_finalwt", "diff_remember", "working")
length(aux.vars_brfss)
#removed height because coded unusably (e.g. 5'9" = 509)

auxmissingsummary_brfss<-data.frame(varname=aux.vars_brfss, pctmiss=NA)
row.names(auxmissingsummary_brfss)<-aux.vars_brfss
for (i in aux.vars_brfss){
  auxmissingsummary_brfss[i,"pctmiss"]<-100*sum(is.na(brfss_clean[,i]))/nrow(brfss_clean)
  
  print(i)
  print(table(brfss_clean[,i], brfss_clean$year, exclude=NULL))
}

auxmissingsummary_brfss<-auxmissingsummary_brfss[order(auxmissingsummary_brfss$pctmiss),]  
auxmissingsummary_brfss


#Combine to get vector of all variables that will go in imputation model, 
#Ordered by increasing missingness in vars.to.impute, and subsequenty, aux.vars. 
all.vars_brfss<-c(paste(missingsummary_brfss$varname), 
                  paste(auxmissingsummary_brfss$varname))
length(all.vars_brfss)
all.vars_brfss

#check that variables aren't listed twice
sum(duplicated(all.vars_brfss))


## Run single imputation
#prep data by dropping vars we don't need, ordering by missingness
in.data_brfss<-brfss_clean[,all.vars_brfss]

#Set variable types
continuous.vars_brfss<-c("age", "cdc_finalwt", "mental_health", "physical_health",
                         "weight_v2", "hhsize_v2")
binary.vars_brfss<-c("male","english_interview", "military", "working", "vision_impair",
                     "diff_walk", "diff_dress", "diff_remember",'phys_activity',
                     "diabetes_v2", "insured", "depress_v2", "kidney_v2", "stroke_v2",
                     "arthritis_v2", "angina_v2","ownhome_v3", "diff_errands") 
ordinal.vars_brfss<-c("education", "income", "health")
categorical.vars_brfss<-c("race_summary", "marital_status","county_code", "smoke_status")

#Check that have all variables, no duplicates
length(c(continuous.vars_brfss, binary.vars_brfss, ordinal.vars_brfss, 
         categorical.vars_brfss))
sum(duplicated(c(continuous.vars_brfss, binary.vars_brfss, ordinal.vars_brfss, 
                 categorical.vars_brfss)))    

#Set variable classes by type
for (i in binary.vars_brfss){
  in.data_brfss[,i]<-factor(in.data_brfss[,i])}
for (i in ordinal.vars_brfss){
  in.data_brfss[,i]<-factor(in.data_brfss[,i], ordered=T)}
for (i in categorical.vars_brfss){
  in.data_brfss[,i]<-factor(in.data_brfss[,i],ordered=F)}

#recheck classes
classsum<-data.frame(varname=all.vars_brfss, class=NA)
for (i in all.vars_brfss){
  classsum[classsum$varname==i,"class"]<-(class(in.data_brfss[,i])[1])
}
classsum

#Initiate imputations
ini<-mice(in.data_brfss, maxit=0, defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

ini$method
meth<-ini$method
meth

ini$predictorMatrix
pred<-ini$predictorMatrix

#Run imputations
imp_pmm_all_brfss<-mice(in.data_brfss, m=40, maxit=10, pred=pred, meth=meth, 
                        defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

#examine diagnostics
imp_pmm_all_brfss$loggedEvents
plot(imp_pmm_all_brfss)
densityplot(imp_pmm_all_brfss, ~income)


#Save final imputed dataset
#Save final imputed dataset
temp_brfss<-list()
for (i in 1:40){
  temp_brfss[[i]]<-complete(imp_pmm_all_brfss,action=i)
  
  temp_brfss[[i]]<-temp_brfss[[i]][,c(harmonizable.vars_brfss, "county_code","cdc_finalwt")]
  temp_brfss[[i]][,"imp"]<-i
}

brfss_analysis_pmm<-do.call(rbind,temp_brfss)

save(brfss_analysis_pmm, file="C:/Users/ehlarson/Box/KHANDLE weighting/Data/Analysis_datasets/Multiple imputed/brfss_analysis_pmm.Rdata")


#Export tables summarizing missingess:
missingtabs<-list(KHANDLE=missingsummary, BRFSS=missingsummary_brfss)
write.xlsx(missingtabs, file = "C:/Users/ehlarson/Box/KHANDLE weighting/Output/missing_summaries.xlsx")


