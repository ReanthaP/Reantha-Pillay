library(gtsummary)  # this library is used for summary
library(dplyr)      #this library is used for combination
library(readxl)     #this one is used for reading xlsx file

#Create Merged Rows for Model from Q45 therefore for each patricipant you will know whether they had a missed or delayed ANYTHING (taking into account that NA should remain NA and not become 0)
spear_community$q45_merged<-rowSums(spear_community[,257:267], na.rm=TRUE) * NA^(rowSums(is.na(spear_community[,257:267])) == length(spear_community[,257:267]))


#take merged rows: if any row sum is more than or equal to 1 take it to be 1 ie. ANYTHING was missed or delays (OPTION 2)
spear_community$q45_merged_cat<-ifelse(spear_community$q45_merged >1,"1","0")


#Visualization of how many records missing or visible (SEEMS LIKE Q44 may be better to run the model with)
tb1_all<-spear_community%>% select("correct_DAG", "q44_missed_app")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_DAG) #table for three countries


#make categorical variables into factors (CONTINUE MAKING THESE FACTORS)
spear_community$q45_merged_cat <- as.factor(spear_community$q45_merged_cat)
spear_community$q2_age_group <- as.factor(spear_community$q2_age_group)
spear_community$q44_missed_app <-as.factor(spear_community$q44_missed_app)
spear_community$q1_sex_bin <-as.factor(spear_community$q1_sex_bin)
spear_community$q4_min_ethnicity <-as.factor(spear_community$q4_min_ethnicity)
spear_community$education_3 <-as.factor(spear_community$education_3)
spear_community$q6_employment_cat <-as.factor(spear_community$q6_employment_cat)
spear_community$income_dollar <-as.factor(spear_community$income_dollar)
spear_community$q12_urban <-as.factor(spear_community$q12_urban) #location
spear_community$depression_severity <-as.factor(spear_community$depression_severity)
spear_community$anxiety_severity <-as.factor(spear_community$anxiety_severity)
spear_community$stress_severity <-as.factor(spear_community$stress_severity)
spear_community$q11_income_sources <-as.factor(spear_community$q11_income_sources)
spear_community$q43___1 <-as.factor(spear_community$q43___1) #DM
spear_community$q43___2 <-as.factor(spear_community$q43___2) #HPT
spear_community$q43___3 <-as.factor(spear_community$q43___3) #Obesity
spear_community$q43___4 <-as.factor(spear_community$q43___4) #CVD
spear_community$q43___merged_CVD <-as.factor(spear_community$q43___4)
spear_community$q43___merged_Chronic_Infectious_Disease <-as.factor(spear_community$q43___merged_Chronic_Infectious_Disease)
spear_community$q43___merged_lung_disease <-as.factor(spear_community$q43___merged_lung_disease)
spear_community$q43___merged_Mental_Health_Disorder <-as.factor(spear_community$q43___merged_Mental_Health_Disorder)
spear_community$q43___10 <-as.factor(spear_community$q43___10) #CKD
spear_community$q43___11 <-as.factor(spear_community$q43___11) #Cancer
spear_community$q43___15 <-as.factor(spear_community$q43___15) #Smoker
spear_community$q43___16 <-as.factor(spear_community$q43___16) #Pregnant
spear_community$q43___17 <-as.factor(spear_community$q43___17) #Other
spear_community$q43___18 <-as.factor(spear_community$q43___18) #None



###################NOT GOING TO USE DUE TO MISSING DATA##################
#Fit model for each possible determinant: uses q45 merged
log.model_age<- glm(spear_community$q45_merged_cat ~ spear_community$q2_age_group, data=spear_community, family=binomial)
log.model_sex<- glm(spear_community$q45_merged_cat ~ spear_community$q1_sex_bin, data=spear_community, family=binomial)
log.model_ethnicity<- glm(spear_community$q45_merged_cat ~ spear_community$q4_min_ethnicity, data=spear_community, family=binomial)
log.model_edu<- glm(spear_community$q45_merged_cat ~ spear_community$education_3, data=spear_community, family=binomial)
log.model_employment<- glm(spear_community$q45_merged_cat ~ spear_community$q6_employment_cat, data=spear_community, family=binomial)
log.model_income<- glm(spear_community$q45_merged_cat ~ spear_community$income_dollar, data=spear_community, family=binomial)
log.model_location<- glm(spear_community$q45_merged_cat ~ spear_community$q12_urban, data=spear_community, family=binomial)
log.model_depression<- glm(spear_community$q45_merged_cat ~ spear_community$depression_severity, data=spear_community, family=binomial)
log.model_anxiety<- glm(spear_community$q45_merged_cat ~ spear_community$anxiety_severity, data=spear_community, family=binomial)
log.model_stress<- glm(spear_community$q45_merged_cat ~ spear_community$stress_severity, data=spear_community, family=binomial)
log.model_income_sources<- glm(spear_community$q45_merged_cat ~ spear_community$q11_income_sources, data=spear_community, family=binomial)
log.model_CVD<- glm(spear_community$q45_merged_cat ~ spear_community$q43___merged_CVD, data=spear_community, family=binomial)
log.model_ChronicInfectious<- glm(spear_community$q45_merged_cat ~ spear_community$q43___merged_Chronic_Infectious_Disease, data=spear_community, family=binomial)
log.model_LungDisease<- glm(spear_community$q45_merged_cat ~ spear_community$q43___merged_lung_disease, data=spear_community, family=binomial)
log.model_Mental<- glm(spear_community$q45_merged_cat ~ spear_community$q43___merged_Mental_Health_Disorder, data=spear_community, family=binomial)
log.model_CKD<- glm(spear_community$q45_merged_cat ~ spear_community$q43___10, data=spear_community, family=binomial)
log.model_Cancer<- glm(spear_community$q45_merged_cat ~ spear_community$q43___11, data=spear_community, family=binomial)
log.model_Smoker<- glm(spear_community$q45_merged_cat ~ spear_community$q43___15, data=spear_community, family=binomial)
log.model_Pregnant<- glm(spear_community$q45_merged_cat ~ spear_community$q43___16, data=spear_community, family=binomial)
log.model_Other<- glm(spear_community$q45_merged_cat ~ spear_community$q43___17, data=spear_community, family=binomial)
log.model_NoComobidities<- glm(spear_community$q45_merged_cat ~ spear_community$q43___18, data=spear_community, family=binomial)

log.model_EconomicInsecurity<- glm(spear_community$q45_merged_cat ~ spear_community$econ_insecurity_score, data=spear_community, family=binomial)
log.model_BiologicalRisk<- glm(spear_community$q45_merged_cat ~ spear_community$bio_risk_score, data=spear_community, family=binomial)
log.model_PercievedRisk<- glm(spear_community$q45_merged_cat ~ spear_community$perceived_risk_score, data=spear_community, family=binomial)


#Alternative model: uses q44
log.model_2_age<- glm(spear_community$q44_missed_app ~ spear_community$q2_age_group, data=spear_community, family=binomial) 
log.model_2_sex<- glm(spear_community$q44_missed_app ~ spear_community$q1_sex_bin, data=spear_community, family=binomial)
log.model_2_ethnicity<- glm(spear_community$q44_missed_app ~ spear_community$q4_min_ethnicity, data=spear_community, family=binomial)

#Change reference category for education
table(spear_community$education_3)
spear_community$education_3<-relevel(spear_community$education_3, ref="Primary") 
table(spear_community$education_3)
spear_community$education_3 <-as.factor(spear_community$education_3) #education as a factor

log.model_2_edu<- glm(spear_community$q44_missed_app ~ spear_community$education_3, data=spear_community, family=binomial)

#####Alt model uses q44 cont
log.model_2_employment<- glm(spear_community$q44_missed_app ~ spear_community$q6_employment_cat, data=spear_community, family=binomial)
log.model_2_income<- glm(spear_community$q44_missed_app ~ spear_community$income_dollar, data=spear_community, family=binomial)
############


#######Change reference category for location
spear_community$q12_urban<-relevel(spear_community$q12_urban, ref="Urban") 
table(spear_community$q12_urban)
spear_community$q12_urban <-as.factor(spear_community$q12_urban) #location

#Alternative model: uses q44 CONT
log.model_2_spear_location<- glm(spear_community$q44_missed_app ~ spear_community$q12_urban, data=spear_community, family=binomial)

spear_community$depression_severity<-relevel(spear_community$depression_severity, ref="Normal") #change reference category to Normal
table(spear_community$depression_severity)
log.model_2_depression<- glm(spear_community$q44_missed_app ~ spear_community$depression_severity, data=spear_community, family=binomial)

spear_community$anxiety_severity<-relevel(spear_community$anxiety_severity, ref="Normal") #change ref category
log.model_2_anxiety<- glm(spear_community$q44_missed_app ~ spear_community$anxiety_severity, data=spear_community, family=binomial)
log.model_2_stress<- glm(spear_community$q44_missed_app ~ spear_community$stress_severity, data=spear_community, family=binomial)
log.model_2_income_sources<- glm(spear_community$q44_missed_app ~ spear_community$q11_income_sources, data=spear_community, family=binomial)
log.model_2_CVD<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_CVD, data=spear_community, family=binomial)
log.model_2_ChronicInfectious<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_Chronic_Infectious_Disease, data=spear_community, family=binomial)
log.model_2_LungDisease<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_lung_disease, data=spear_community, family=binomial)
log.model_2_Mental<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_Mental_Health_Disorder, data=spear_community, family=binomial)
log.model_2_CKD<- glm(spear_community$q44_missed_app ~ spear_community$q43___10, data=spear_community, family=binomial)
log.model_2_Cancer<- glm(spear_community$q44_missed_app ~ spear_community$q43___11, data=spear_community, family=binomial)
log.model_2_Smoker<- glm(spear_community$q44_missed_app ~ spear_community$q43___15, data=spear_community, family=binomial)
log.model_2_Pregnant<- glm(spear_community$q44_missed_app ~ spear_community$q43___16, data=spear_community, family=binomial)
log.model_2_Other<- glm(spear_community$q44_missed_app ~ spear_community$q43___17, data=spear_community, family=binomial)
log.model_2_NoComorbidities<- glm(spear_community$q44_missed_app ~ spear_community$q43___18, data=spear_community, family=binomial)

log.model_2_EconomicInsecuirity<- glm(spear_community$q44_missed_app ~ spear_community$econ_insecurity_score, data=spear_community, family=binomial)
log.model_2_BiologicalRisk<- glm(spear_community$q44_missed_app ~ spear_community$bio_risk_score, data=spear_community, family=binomial)
log.model_2_PercievedRisk<- glm(spear_community$q44_missed_app ~ spear_community$perceived_risk_score, data=spear_community, family=binomial)



#show model
summary(log.model_age) #not used as missing data from 45
summary(log.model_2_age) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_age), confint.default(log.model_2_age), level = 0.95)) #take OR and CI out

summary(log.model_sex) #not used as missing data from 45
summary(log.model_2_sex) #take p value out
exp(cbind("Odds ratio" = coef(log.model_2_sex), confint.default(log.model_2_sex), level = 0.95)) #take OR and CI out

summary(log.model_ethnicity)#not used as missing data from 45
summary(log.model_2_ethnicity) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ethnicity), confint.default(log.model_2_ethnicity), level = 0.95)) #take OR and CI out


summary(log.model_edu) #not used as missing data from 45
summary(log.model_2_edu) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_edu), confint.default(log.model_2_edu), level = 0.95)) #take OR and CI out


summary(log.model_employment)#not used as missing data from 45
summary(log.model_2_employment) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_employment), confint.default(log.model_2_employment), level = 0.95)) #take OR and CI out


summary(log.model_income)#not used as missing data from 45
summary(log.model_2_income) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income), confint.default(log.model_2_income), level = 0.95)) #take OR and CI out

summary(log.model_location)#not used as missing data from 45
summary(log.model_2_location) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_location), confint.default(log.model_2_location), level = 0.95)) #take OR and CI out

summary(log.model_depression)#not used as missing data from 45
summary(log.model_2_depression)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_depression), confint.default(log.model_2_depression), level = 0.95)) #take OR and CI out


summary(log.model_anxiety)#not used as missing data from 45
summary(log.model_2_anxiety)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_anxiety), confint.default(log.model_2_anxiety), level = 0.95)) #take OR and CI out

###########OPTED NOT TO INCLUDE STRESS AS VERY SMALL NUMBERS#############
summary(log.model_stress)#not used as missing data from 45
summary(log.model_2_stress)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_stress), confint.default(log.model_2_stress), level = 0.95)) #take OR and CI out

table(spear_community$q11_income_sources)
summary(log.model_income_sources)#not used as missing data from 45
summary(log.model_2_income_sources)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_sources), confint.default(log.model_2_income_sources), level = 0.95)) #take OR and CI out


summary(log.model_CVD)#not used as missing data from 45
summary(log.model_2_CVD)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CVD), confint.default(log.model_2_CVD), level = 0.95)) #take OR and CI out


summary(log.model_ChronicInfectious)#not used as missing data from 45
summary(log.model_2_ChronicInfectious)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious), confint.default(log.model_2_ChronicInfectious), level = 0.95)) #take OR and CI out


summary(log.model_LungDisease) #not used as missing data from 45
summary(log.model_2_LungDisease)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease), confint.default(log.model_2_LungDisease), level = 0.95)) #take OR and CI out


summary(log.model_Mental)#not used as missing data from 45
summary(log.model_2_Mental)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Mental), confint.default(log.model_2_Mental), level = 0.95)) #take OR and CI out


summary(log.model_CKD)#not used as missing data from 45
summary(log.model_2_CKD)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CKD), confint.default(log.model_2_CKD), level = 0.95)) #take OR and CI out


summary(log.model_Cancer)#not used as missing data from 45
summary(log.model_2_Cancer)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Cancer), confint.default(log.model_2_Cancer), level = 0.95)) #take OR and CI out


summary(log.model_Smoker)#not used as missing data from 45
summary(log.model_2_Smoker)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Smoker), confint.default(log.model_2_Smoker), level = 0.95)) #take OR and CI out


summary(log.model_Pregnant)#not used as missing data from 45
summary(log.model_2_Pregnant)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant), confint.default(log.model_2_Pregnant), level = 0.95)) #take OR and CI out


summary(log.model_Other)#not used as missing data from 45
summary(log.model_2_Other)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Other), confint.default(log.model_2_Other), level = 0.95)) #take OR and CI out


summary(log.model_NoComobidities)#not used as missing data from 45
summary(log.model_2_NoComorbidities)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities), confint.default(log.model_2_NoComorbidities), level = 0.95)) #take OR and CI out


summary(log.model_EconomicInsecurity)#not used as missing data from 45
summary(log.model_2_EconomicInsecuirity)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity), confint.default(log.model_2_EconomicInsecuirity), level = 0.95)) #take OR and CI out


summary(log.model_BiologicalRisk)#not used as missing data from 45
summary(log.model_2_BiologicalRisk)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk), confint.default(log.model_2_BiologicalRisk), level = 0.95)) #take OR and CI out


summary(log.model_PercievedRisk)#not used as missing data from 45
summary(log.model_2_PercievedRisk)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk), confint.default(log.model_2_PercievedRisk), level = 0.95)) #take OR and CI out




#####Combined Model: adjusting for age, Location Sex
log.model_2_depression_adj<- glm(spear_community$q44_missed_app ~ spear_community$depression_severity + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_depression_adj) #use p values for depression categories
exp(cbind("Odds ratio" = coef(log.model_2_depression_adj), confint.default(log.model_2_depression_adj), level = 0.95)) #take OR and CI out for depression categories


log.model_2_anxiety_adj<- glm(spear_community$q44_missed_app ~ spear_community$anxiety_severity + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_anxiety_adj)
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_adj), confint.default(log.model_2_anxiety_adj), level = 0.95)) #take OR and CI out

####did not run variable
log.model_2_stress_adj<- glm(spear_community$q44_missed_app ~ spear_community$stress_severity + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_stress_adj)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk), confint.default(log.model_2_PercievedRisk), level = 0.95)) #take OR and CI out



log.model_2_EconomicInsecuirity_adj<- glm(spear_community$q44_missed_app ~ spear_community$econ_insecurity_score + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_EconomicInsecuirity_adj)
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_adj), confint.default(log.model_2_EconomicInsecuirity_adj), level = 0.95)) #take OR and CI out


log.model_2_BiologicalRisk_adj<- glm(spear_community$q44_missed_app ~ spear_community$bio_risk_score + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_BiologicalRisk_adj)
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_adj), confint.default(log.model_2_BiologicalRisk_adj), level = 0.95)) #take OR and CI out

log.model_2_PercievedRisk_adj<- glm(spear_community$q44_missed_app ~ spear_community$perceived_risk_score + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_PercievedRisk_adj)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_adj), confint.default(log.model_2_PercievedRisk_adj), level = 0.95)) #take OR and CI out


log.model_2_CVD_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_CVD + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_CVD_adj)
exp(cbind("Odds ratio" = coef(log.model_2_CVD_adj), confint.default(log.model_2_CVD_adj), level = 0.95)) #take OR and CI out


log.model_2_ChronicInfectious_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_Chronic_Infectious_Disease + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_ChronicInfectious_adj)
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_adj), confint.default(log.model_2_ChronicInfectious_adj), level = 0.95)) #take OR and CI out


log.model_2_LungDisease_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_lung_disease + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_LungDisease_adj)
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_adj), confint.default(log.model_2_LungDisease_adj), level = 0.95)) #take OR and CI out



log.model_2_CKD_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___10 + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_CKD_adj)
exp(cbind("Odds ratio" = coef(log.model_2_CKD_adj), confint.default(log.model_2_CKD_adj), level = 0.95)) #take OR and CI out


log.model_2_Cancer_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___11 + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_Cancer_adj)
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_adj), confint.default(log.model_2_Cancer_adj), level = 0.95)) #take OR and CI out


log.model_2_Mental_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___merged_Mental_Health_Disorder + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_Mental_adj)
exp(cbind("Odds ratio" = coef(log.model_2_Mental_adj), confint.default(log.model_2_Mental_adj), level = 0.95)) #take OR and CI out


log.model_2_Smoker_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___15 + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_Smoker_adj)
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_adj), confint.default(log.model_2_Smoker_adj), level = 0.95)) #take OR and CI out


log.model_2_Pregnant_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___16 + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_Pregnant_adj)
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_adj), confint.default(log.model_2_Pregnant_adj), level = 0.95)) #take OR and CI out


log.model_2_Other_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___17 + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_Other_adj)
exp(cbind("Odds ratio" = coef(log.model_2_Other_adj), confint.default(log.model_2_Other_adj), level = 0.95)) #take OR and CI out


log.model_2_NoComorbidities_adj<- glm(spear_community$q44_missed_app ~ spear_community$q43___18 + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_NoComorbidities_adj)
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_adj), confint.default(log.model_2_NoComorbidities_adj), level = 0.95)) #take OR and CI out



############# PER COUNTRY REGRESSIONS
### START BY MAKING PER COUNTRY DATA SET
####Create Subset for Indonesia
spear_community_Indonesia <- subset(spear_community, spear_community$correct_DAG == "Indonesia", )
####TO CHECK IF ITS WORKING###
table(spear_community$q6_employment_cat, spear_community$correct_DAG)
table(spear_community_Indonesia$q6_employment_cat)
#######Create subset for Nepal
spear_community_Nepal <- subset(spear_community, spear_community$correct_DAG == "Nepal", )
####TO CHECK IF ITS WORKING###
table(spear_community$q6_employment_cat, spear_community$correct_DAG)
table(spear_community_Nepal$q6_employment_cat)
#######
#######Create subset for Vietnam
spear_community_Vietnam <- subset(spear_community, spear_community$correct_DAG == "Vietnam", )
####TO CHECK IF ITS WORKING###
table(spear_community$q6_employment_cat, spear_community$correct_DAG)
table(spear_community_Vietnam$q6_employment_cat)
#######



#NOW RUN ADJUSTED OR FOR COUNTRIES

######Indonesia

spear_community_Indonesia$depression_severity<-relevel(spear_community_Indonesia$depression_severity, ref="Normal")

log.model_2_depression_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$depression_severity + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_depression_adj_Indo) #use p values for depression categories
exp(cbind("Odds ratio" = coef(log.model_2_depression_adj_Indo), confint.default(log.model_2_depression_adj_Indo), level = 0.95)) #take OR and CI out for depression categories


spear_community_Indonesia$anxiety_severity<-relevel(spear_community_Indonesia$anxiety_severity, ref="Normal") #change ref category
log.model_2_anxiety_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$anxiety_severity + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_anxiety_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_adj_Indo), confint.default(log.model_2_anxiety_adj_Indo), level = 0.95)) #take OR and CI out

####did not run variable
log.model_2_stress_adj<- glm(spear_community$q44_missed_app ~ spear_community$stress_severity + spear_community$q12_urban + spear_community$q1_sex_bin + spear_community$q2_age_group, data=spear_community, family=binomial)
summary(log.model_2_stress_adj)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk), confint.default(log.model_2_PercievedRisk), level = 0.95)) #take OR and CI out



log.model_2_EconomicInsecuirity_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$econ_insecurity_score + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_EconomicInsecuirity_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_adj_Indo), confint.default(log.model_2_EconomicInsecuirity_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_BiologicalRisk_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$bio_risk_score + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_BiologicalRisk_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_adj_Indo), confint.default(log.model_2_BiologicalRisk_adj_Indo), level = 0.95)) #take OR and CI out

log.model_2_PercievedRisk_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$perceived_risk_score + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_PercievedRisk_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_adj_Indo), confint.default(log.model_2_PercievedRisk_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_CVD_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_CVD + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_CVD_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_CVD_adj_Indo), confint.default(log.model_2_CVD_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_ChronicInfectious_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_Chronic_Infectious_Disease + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_ChronicInfectious_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_adj_Indo), confint.default(log.model_2_ChronicInfectious_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_LungDisease_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_lung_disease + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_LungDisease_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_adj_Indo), confint.default(log.model_2_LungDisease_adj_Indo), level = 0.95)) #take OR and CI out



log.model_2_CKD_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___10 + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_CKD_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_CKD_adj_Indo), confint.default(log.model_2_CKD_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Cancer_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___11 + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Cancer_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_adj_Indo), confint.default(log.model_2_Cancer_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Mental_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_Mental_Health_Disorder + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Mental_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Mental_adj_Indo), confint.default(log.model_2_Mental_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Smoker_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___15 + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Smoker_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_adj_Indo), confint.default(log.model_2_Smoker_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Pregnant_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___16 + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Pregnant_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_adj_Indo), confint.default(log.model_2_Pregnant_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Other_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___17 + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Other_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Other_adj_Indo), confint.default(log.model_2_Other_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_NoComorbidities_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___18 + spear_community_Indonesia$q12_urban + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_NoComorbidities_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_adj_Indo), confint.default(log.model_2_NoComorbidities_adj_Indo), level = 0.95)) #take OR and CI out

######Nepal


spear_community_Nepal$depression_severity<-relevel(spear_community_Nepal$depression_severity, ref="Normal") #change ref category

log.model_2_depression_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$depression_severity + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_depression_adj_Nepal) #use p values for depression categories
exp(cbind("Odds ratio" = coef(log.model_2_depression_adj_Nepal), confint.default(log.model_2_depression_adj_Nepal), level = 0.95)) #take OR and CI out for depression categories


spear_community_Nepal$anxiety_severity<-relevel(spear_community_Nepal$anxiety_severity, ref="Normal") #change ref category
log.model_2_anxiety_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$anxiety_severity + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_anxiety_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_adj_Nepal), confint.default(log.model_2_anxiety_adj_Nepal), level = 0.95)) #take OR and CI out

####did not run variable
log.model_2_stress_adj<- glm(spear_community$q44_missed_app ~ spear_community$stress_severity + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_stress_adj)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk), confint.default(log.model_2_PercievedRisk), level = 0.95)) #take OR and CI out



log.model_2_EconomicInsecuirity_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$econ_insecurity_score + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_EconomicInsecuirity_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_adj_Nepal), confint.default(log.model_2_EconomicInsecuirity_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_BiologicalRisk_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$bio_risk_score + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_BiologicalRisk_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_adj_Nepal), confint.default(log.model_2_BiologicalRisk_adj_Nepal), level = 0.95)) #take OR and CI out

log.model_2_PercievedRisk_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$perceived_risk_score + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_PercievedRisk_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_adj_Nepal), confint.default(log.model_2_PercievedRisk_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_CVD_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_CVD + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_CVD_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_CVD_adj_Nepal), confint.default(log.model_2_CVD_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_ChronicInfectious_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_Chronic_Infectious_Disease + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_ChronicInfectious_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_adj_Nepal), confint.default(log.model_2_ChronicInfectious_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_LungDisease_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_lung_disease + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_LungDisease_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_adj_Nepal), confint.default(log.model_2_LungDisease_adj_Nepal), level = 0.95)) #take OR and CI out



log.model_2_CKD_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___10 + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_CKD_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_CKD_adj_Nepal), confint.default(log.model_2_CKD_adj_Nepal), level = 0.95)) #take OR and CI out

#######################
##### ERROR MESSAGE AS NO PARTICIANTS WITH CANCER IN NEPAL THEREFORE CANNOT RUN REGRESSION############# table(spear_community_Nepal$q43___11) to see
log.model_2_Cancer_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___11 + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Cancer_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_adj_Nepal), confint.default(log.model_2_Cancer_adj_Nepal), level = 0.95)) #take OR and CI out
##### ERROR MESSAGE AS NO PARTICIANTS WITH CANCER IN NEPAL THEREFORE CANNOT RUN REGRESSION############# table(spear_community_Nepal$q43___11) to see
################
####Cont Nepal Adj OR

log.model_2_Mental_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_Mental_Health_Disorder + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Mental_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_Mental_adj_Nepal), confint.default(log.model_2_Mental_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_Smoker_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___15 + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Smoker_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_adj_Nepal), confint.default(log.model_2_Smoker_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_Pregnant_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___16 + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Pregnant_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_adj_Nepal), confint.default(log.model_2_Pregnant_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_Other_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___17 + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Other_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_Other_adj_Nepal), confint.default(log.model_2_Other_adj_Nepal), level = 0.95)) #take OR and CI out


log.model_2_NoComorbidities_adj_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___18 + spear_community_Nepal$q12_urban + spear_community_Nepal$q1_sex_bin + spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial)
summary(log.model_2_NoComorbidities_adj_Nepal)
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_adj_Nepal), confint.default(log.model_2_NoComorbidities_adj_Nepal), level = 0.95)) #take OR and CI out


##########VIETNAM###########################

spear_community_Vietnam$depression_severity<-relevel(spear_community_Vietnam$depression_severity, ref="Normal") #change ref category

log.model_2_depression_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$depression_severity + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_depression_adj_Vietnam) #use p values for depression categories
exp(cbind("Odds ratio" = coef(log.model_2_depression_adj_Vietnam), confint.default(log.model_2_depression_adj_Vietnam), level = 0.95)) #take OR and CI out for depression categories

####

spear_community_Vietnam$anxiety_severity<-relevel(spear_community_Vietnam$anxiety_severity, ref="Normal") #change ref category

log.model_2_anxiety_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$anxiety_severity + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_anxiety_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_adj_Vietnam), confint.default(log.model_2_anxiety_adj_Vietnam), level = 0.95)) #take OR and CI out

####did not run variable
log.model_2_stress_adj<- glm(spear_community$q44_missed_app ~ spear_community$stress_severity + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_stress_adj)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk), confint.default(log.model_2_PercievedRisk), level = 0.95)) #take OR and CI out
#########


log.model_2_EconomicInsecuirity_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$econ_insecurity_score + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_EconomicInsecuirity_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_adj_Vietnam), confint.default(log.model_2_EconomicInsecuirity_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_BiologicalRisk_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$bio_risk_score + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_BiologicalRisk_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_adj_Vietnam), confint.default(log.model_2_BiologicalRisk_adj_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_PercievedRisk_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$perceived_risk_score + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_PercievedRisk_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_adj_Vietnam), confint.default(log.model_2_PercievedRisk_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_CVD_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_CVD + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_CVD_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_CVD_adj_Vietnam), confint.default(log.model_2_CVD_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_ChronicInfectious_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_Chronic_Infectious_Disease + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_ChronicInfectious_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_adj_Vietnam), confint.default(log.model_2_ChronicInfectious_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_LungDisease_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_lung_disease + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_LungDisease_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_adj_Vietnam), confint.default(log.model_2_LungDisease_adj_Vietnam), level = 0.95)) #take OR and CI out



log.model_2_CKD_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___10 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_CKD_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_CKD_adj_Vietnam), confint.default(log.model_2_CKD_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Cancer_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___11 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Cancer_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_adj_Vietnam), confint.default(log.model_2_Cancer_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Mental_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_Mental_Health_Disorder + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Mental_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Mental_adj_Vietnam), confint.default(log.model_2_Mental_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Smoker_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___15 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Smoker_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_adj_Vietnam), confint.default(log.model_2_Smoker_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Pregnant_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___16 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Pregnant_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_adj_Vietnam), confint.default(log.model_2_Pregnant_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Other_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___17 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Other_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Other_adj_Vietnam), confint.default(log.model_2_Other_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_NoComorbidities_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___18 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$q1_sex_bin + spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_NoComorbidities_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_adj_Vietnam), confint.default(log.model_2_NoComorbidities_adj_Vietnam), level = 0.95)) #take OR and CI out



############# N column cross tabulations 2/0+2
table (spear_community$q12_urban, spear_community$q44_missed_app)
table (spear_community$q2_age_group, spear_community$q44_missed_app)
table (spear_community$q1_sex_bin, spear_community$q44_missed_app)
table (spear_community$q4_min_ethnicity, spear_community$q44_missed_app)
table (spear_community$education_3 , spear_community$q44_missed_app)
table (spear_community$q6_employment_cat, spear_community$q44_missed_app)
table (spear_community$income_dollar, spear_community$q44_missed_app)
table (spear_community$q11_income_sources, spear_community$q44_missed_app)
table (spear_community$depression_severity, spear_community$q44_missed_app)
table (spear_community$anxiety_severity, spear_community$q44_missed_app)

table (spear_community$q43___merged_CVD, spear_community$q44_missed_app)
table (spear_community$q43___merged_Chronic_Infectious_Disease, spear_community$q44_missed_app)
table (spear_community$q43___merged_lung_disease, spear_community$q44_missed_app)
table (spear_community$q43___10, spear_community$q44_missed_app) #CKD
table (spear_community$q43___11, spear_community$q44_missed_app) #cancer
table (spear_community$q43___merged_Mental_Health_Disorder, spear_community$q44_missed_app) 
table (spear_community$q43___15, spear_community$q44_missed_app) #smoker
table (spear_community$q43___16, spear_community$q44_missed_app) #pregnant
table (spear_community$q43___17, spear_community$q44_missed_app) #other
table (spear_community$q43___18, spear_community$q44_missed_app) #none


##### for Indonesia n columns
table (spear_community_Indonesia$depression_severity, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$anxiety_severity, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q43___merged_CVD, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q43___merged_Chronic_Infectious_Disease, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q43___merged_lung_disease, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q43___10, spear_community_Indonesia$q44_missed_app) #CKD
table (spear_community_Indonesia$q43___11, spear_community_Indonesia$q44_missed_app) #cancer
table (spear_community_Indonesia$q43___merged_Mental_Health_Disorder, spear_community_Indonesia$q44_missed_app) 
table (spear_community_Indonesia$q43___15, spear_community_Indonesia$q44_missed_app) #smoker
table (spear_community_Indonesia$q43___16, spear_community_Indonesia$q44_missed_app) #pregnant
table (spear_community_Indonesia$q43___17, spear_community_Indonesia$q44_missed_app) #other
table (spear_community_Indonesia$q43___18, spear_community_Indonesia$q44_missed_app) #none


########N columns for nepal
table (spear_community_Nepal$q12_urban, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q2_age_group, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q1_sex_bin, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q4_min_ethnicity, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$education_3 , spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q6_employment_cat, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$income_dollar, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q11_income_sources, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$depression_severity, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$anxiety_severity, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q43___merged_CVD, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q43___merged_Chronic_Infectious_Disease, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q43___merged_lung_disease, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q43___10, spear_community_Nepal$q44_missed_app) #CKD
table (spear_community_Nepal$q43___11, spear_community_Nepal$q44_missed_app) #cancer
table (spear_community_Nepal$q43___merged_Mental_Health_Disorder, spear_community_Nepal$q44_missed_app) 
table (spear_community_Nepal$q43___15, spear_community_Nepal$q44_missed_app) #smoker
table (spear_community_Nepal$q43___16, spear_community_Nepal$q44_missed_app) #pregnant
table (spear_community_Nepal$q43___17, spear_community_Nepal$q44_missed_app) #other
table (spear_community_Nepal$q43___18, spear_community_Nepal$q44_missed_app) #none



##### for Vietnam n columns
table (spear_community_Vietnam$depression_severity, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$anxiety_severity, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q43___merged_CVD, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q43___merged_Chronic_Infectious_Disease, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q43___merged_lung_disease, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q43___10, spear_community_Vietnam$q44_missed_app) #CKD
table (spear_community_Vietnam$q43___11, spear_community_Vietnam$q44_missed_app) #cancer
table (spear_community_Vietnam$q43___merged_Mental_Health_Disorder, spear_community_Vietnam$q44_missed_app) 
table (spear_community_Vietnam$q43___15, spear_community_Vietnam$q44_missed_app) #smoker
table (spear_community_Vietnam$q43___16, spear_community_Vietnam$q44_missed_app) #pregnant
table (spear_community_Vietnam$q43___17, spear_community_Vietnam$q44_missed_app) #other
table (spear_community_Vietnam$q43___18, spear_community_Vietnam$q44_missed_app) #none






#######
library(MASS) #install MASS
confint(log.model_2_depression_adj) #this is 95% CI for co-efficient (log odds), need 95% CI for odds




################NOT FOING TO USE THIS PREDICTIVE MODEL AS TOO MANY ADJUSTMENTS
#Fit model for combo of determinants with p>0,2
log.model_combined<- glm(spear_community$q45_merged_cat ~ spear_community$q1_sex_bin + spear_community$q4_min_ethnicity + spear_community$q6_employment_cat + spear_community$q12_urban + spear_community$q43___merged_CVD + spear_community$q43___merged_Chronic_Infectious_Disease + spear_community$q43___merged_lung_disease + spear_community$q43___merged_Mental_Health_Disorder + spear_community$q43___10 + spear_community$q43___11 + spear_community$q43___15 + spear_community$q43___16 + spear_community$q43___17 +spear_community$q43___18 + spear_community$econ_insecurity_score, data=spear_community, family=binomial)

log_model_2_combined <- glm(spear_community$q44_missed_app ~ spear_community$q2_age_group + spear_community$q1_sex_bin + spear_community$q4_min_ethnicity + spear_community$q5_education + spear_community$q6_employment_cat + spear_community$q9_income + spear_community$q12_urban + spear_community$anxiety_severity + spear_community$q11_income_sources + spear_community$q43___merged_CVD + spear_community$q43___merged_Chronic_Infectious_Disease + spear_community$q43___merged_lung_disease + spear_community$q43___merged_Mental_Health_Disorder + spear_community$q43___10 +spear_community$q43___11 + spear_community$q43___15 + spear_community$q43___16 + spear_community$q43___17 + spear_community$q43___18 + spear_community$econ_insecurity_score + spear_community$bio_risk_score + spear_community$perceived_risk_score, data=spear_community, family=binomial)

#Show combined models
summary(log.model_combined)
summary(log_model_2_combined)



###########
#Optional code
#take merged rows: if any row sum is more than or equal to 1 take it to be 1 ie. ANYTHING was missed or delays (OPTION 1)
for (i in 1:nrow(spear_community)) spear_community$q45_merged[i]<- sum(spear_community[i,(257:267)])
for (i in 1:nrow(spear_community)){
  if (spear_community$q45_merged[i] >=1) {spear_community$q45_merged[i] <- 1
  }
}
###############Show model 2 as ODDS RATIOS: ####NOT USED AS BETTER COMBINED CODE USED INSTEAD
exp(log.model_2_location$coefficients)
exp(log.model_2_age$coefficients)
exp(log.model_2_sex$coefficients)
exp(log.model_2_ethnicity$coefficients)
exp(log.model_2_edu$coefficients)
exp(log.model_2_employment$coefficients)
exp(log.model_2_income$coefficients) 
exp(log.model_2_depression$coefficients)
exp(log.model_2_anxiety$coefficients) 
exp(log.model_2_stress$coefficients) 
exp(log.model_2_EconomicInsecuirity$coefficients)
exp(log.model_2_BiologicalRisk$coefficients)
exp(log.model_2_PercievedRisk$coefficients)
exp(log.model_2_income_sources$coefficients)
exp(log.model_2_CVD$coefficients)
exp(log.model_2_ChronicInfectious$coefficients)
exp(log.model_2_LungDisease$coefficients)
exp(log.model_2_CKD$coefficients)
exp(log.model_2_Cancer$coefficients)
exp(log.model_2_Mental$coefficients)
exp(log.model_2_Smoker$coefficients)
exp(log.model_2_Pregnant$coefficients)
exp(log.model_2_Other$coefficients)
exp(log.model_2_NoComorbidities$coefficients)
