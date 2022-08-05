

###########Run for Indonesia##########
######Crude OR ############
spear_community_Indonesia$q44_missed_app <-as.factor(spear_community_Indonesia$q44_missed_app) 


log.model_2_age_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q2_age_group, data=spear_community_Indonesia, family=binomial) 
#show model
summary(log.model_2_age_Indonesia) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_age_Indonesia), confint.default(log.model_2_age_Indonesia), level = 0.95)) #take OR and CI out

log.model_2_sex_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q1_sex_bin, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_sex_Indonesia) #take p value out
exp(cbind("Odds ratio" = coef(log.model_2_sex_Indonesia), confint.default(log.model_2_sex_Indonesia), level = 0.95)) #take OR and CI out

log.model_2_ethnicity_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q4_min_ethnicity, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_ethnicity_Indonesia) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ethnicity_Indonesia), confint.default(log.model_2_ethnicity_Indonesia), level = 0.95)) #take OR and CI out

#Change reference category for education
table(spear_community_Indonesia$education_3)
spear_community_Indonesia$education_3<-relevel(spear_community_Indonesia$education_3, ref="Primary") 
table(spear_community_Indonesia$education_3)
spear_community_Indonesia$education_3 <-as.factor(spear_community_Indonesia$education_3) #education as a factor

log.model_2_edu_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$education_3, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_edu_Indonesia) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_edu_Indonesia), confint.default(log.model_2_edu_Indonesia), level = 0.95)) #take OR and CI out

log.model_2_employment_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q6_employment_cat, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_employment_Indonesia) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_employment_Indonesia), confint.default(log.model_2_employment_Indonesia), level = 0.95)) #take OR and CI out

log.model_2_income_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_income_Indonesia) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_Indonesia), confint.default(log.model_2_income_Indonesia), level = 0.95)) #take OR and CI out

#######Change reference category for location
spear_community_Indonesia$q12_urban<-relevel(spear_community_Indonesia$q12_urban, ref="Urban") 
table(spear_community_Indonesia$q12_urban)
spear_community_Indonesia$q12_urban <-as.factor(spear_community_Indonesia$q12_urban) #location
log.model_2_spear_location_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q12_urban, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_spear_location_Indonesia) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_spear_location_Indonesia), confint.default(log.model_2_spear_location_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q11_income_sources <-as.factor(spear_community_Indonesia$q11_income_sources) #location
log.model_2_income_sources_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q11_income_sources, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_income_sources_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_sources_Indonesia), confint.default(log.model_2_income_sources_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$depression_severity <-as.factor(spear_community_Indonesia$depression_severity)
spear_community_Indonesia$depression_severity<-relevel(spear_community_Indonesia$depression_severity, ref="Normal") #change reference category to Normal
table(spear_community_Indonesia$depression_severity)
log.model_2_depression_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$depression_severity, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_depression_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_depression_Indonesia), confint.default(log.model_2_depression_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$anxiety_severity <-as.factor(spear_community_Indonesia$anxiety_severity)
spear_community_Indonesia$anxiety_severity<-relevel(spear_community_Indonesia$anxiety_severity, ref="Normal") #change ref category
table(spear_community_Indonesia$anxiety_severity)
log.model_2_anxiety_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$anxiety_severity, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_anxiety_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_Indonesia), confint.default(log.model_2_anxiety_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___merged_CVD <-as.factor(spear_community_Indonesia$q43___merged_CVD)
log.model_2_CVD_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_CVD, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_CVD_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CVD_Indonesia), confint.default(log.model_2_CVD_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___merged_Chronic_Infectious_Disease <-as.factor(spear_community_Indonesia$q43___merged_Chronic_Infectious_Disease)
log.model_2_ChronicInfectious_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_Chronic_Infectious_Disease, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_ChronicInfectious_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_Indonesia), confint.default(log.model_2_ChronicInfectious_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___merged_lung_disease <-as.factor(spear_community_Indonesia$q43___merged_lung_disease)
log.model_2_LungDisease_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_lung_disease, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_LungDisease_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_Indonesia), confint.default(log.model_2_LungDisease_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___merged_Mental_Health_Disorder <-as.factor(spear_community_Indonesia$q43___merged_Mental_Health_Disorder)
log.model_2_Mental_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_Mental_Health_Disorder, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Mental_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Mental_Indonesia), confint.default(log.model_2_Mental_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___10 <-as.factor(spear_community_Indonesia$q43___10)
log.model_2_CKD_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___10, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_CKD_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CKD_Indonesia), confint.default(log.model_2_CKD_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___11 <-as.factor(spear_community_Indonesia$q43___11)
log.model_2_Cancer_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___11, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Cancer_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_Indonesia), confint.default(log.model_2_Cancer_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___15 <-as.factor(spear_community_Indonesia$q43___15)
log.model_2_Smoker_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___15, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Smoker_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_Indonesia), confint.default(log.model_2_Smoker_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___16 <-as.factor(spear_community_Indonesia$q43___16)
log.model_2_Pregnant_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___16, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Pregnant_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_Indonesia), confint.default(log.model_2_Pregnant_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___17 <-as.factor(spear_community_Indonesia$q43___17)
log.model_2_Other_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___17, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Other_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Other_Indonesia), confint.default(log.model_2_Other_Indonesia), level = 0.95)) #take OR and CI out

spear_community_Indonesia$q43___18 <-as.factor(spear_community_Indonesia$q43___18)
log.model_2_NoComorbidities_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___18, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_NoComorbidities_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_Indonesia), confint.default(log.model_2_NoComorbidities_Indonesia), level = 0.95)) #take OR and CI out

log.model_2_EconomicInsecuirity_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$econ_insecurity_score, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_EconomicInsecuirity_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_Indonesia), confint.default(log.model_2_EconomicInsecuirity_Indonesia), level = 0.95)) #take OR and CI out

log.model_2_BiologicalRisk_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$bio_risk_score, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_BiologicalRisk_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_Indonesia), confint.default(log.model_2_BiologicalRisk_Indonesia), level = 0.95)) #take OR and CI out


log.model_2_PercievedRisk_Indonesia<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$perceived_risk_score, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_PercievedRisk_Indonesia)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_Indonesia), confint.default(log.model_2_PercievedRisk_Indonesia), level = 0.95)) #take OR and CI out


###########Run for NEPAL ##########
######Crude OR ############

spear_community_Nepal$q44_missed_app <-as.factor(spear_community_Nepal$q44_missed_app) 


log.model_2_age_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q2_age_group, data=spear_community_Nepal, family=binomial) 
#show model
summary(log.model_2_age_Nepal) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_age_Nepal), confint.default(log.model_2_age_Nepal), level = 0.95)) #take OR and CI out

log.model_2_sex_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q1_sex_bin, data=spear_community_Nepal, family=binomial)
summary(log.model_2_sex_Nepal) #take p value out
exp(cbind("Odds ratio" = coef(log.model_2_sex_Nepal), confint.default(log.model_2_sex_Nepal), level = 0.95)) #take OR and CI out

log.model_2_ethnicity_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q4_min_ethnicity, data=spear_community_Nepal, family=binomial)
summary(log.model_2_ethnicity_Nepal) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ethnicity_Nepal), confint.default(log.model_2_ethnicity_Nepal), level = 0.95)) #take OR and CI out

#Change reference category for education
table(spear_community_Nepal$education_3)
spear_community_Nepal$education_3<-relevel(spear_community_Nepal$education_3, ref="Primary") 
table(spear_community_Nepal$education_3)
spear_community_Nepal$education_3 <-as.factor(spear_community_Nepal$education_3) #education as a factor

log.model_2_edu_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$education_3, data=spear_community_Nepal, family=binomial)
summary(log.model_2_edu_Nepal) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_edu_Nepal), confint.default(log.model_2_edu_Nepal), level = 0.95)) #take OR and CI out

log.model_2_employment_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q6_employment_cat, data=spear_community_Nepal, family=binomial)
summary(log.model_2_employment_Nepal) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_employment_Nepal), confint.default(log.model_2_employment_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$income_dollar <-as.factor(spear_community_Nepal$income_dollar) 
log.model_2_income_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$income_dollar, data=spear_community_Nepal, family=binomial)
summary(log.model_2_income_Nepal) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_Nepal), confint.default(log.model_2_income_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q11_income_sources <-as.factor(spear_community_Nepal$q11_income_sources) #location
log.model_2_income_sources_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q11_income_sources, data=spear_community_Nepal, family=binomial)
summary(log.model_2_income_sources_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_sources_Nepal), confint.default(log.model_2_income_sources_Nepal), level = 0.95)) #take OR and CI out

#######Change reference category for location
spear_community_Nepal$q12_urban<-relevel(spear_community_Nepal$q12_urban, ref="Urban") 
table(spear_community_Nepal$q12_urban)
spear_community_Nepal$q12_urban <-as.factor(spear_community_Nepal$q12_urban) #location
log.model_2_spear_location_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q12_urban, data=spear_community_Nepal, family=binomial)
summary(log.model_2_spear_location_Nepal) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_spear_location_Nepal), confint.default(log.model_2_spear_location_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$depression_severity <-as.factor(spear_community_Nepal$depression_severity)
spear_community_Nepal$depression_severity<-relevel(spear_community_Nepal$depression_severity, ref="Normal") #change reference category to Normal
table(spear_community_Nepal$depression_severity)
log.model_2_depression_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$depression_severity, data=spear_community_Nepal, family=binomial)
summary(log.model_2_depression_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_depression_Nepal), confint.default(log.model_2_depression_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$anxiety_severity <-as.factor(spear_community_Nepal$anxiety_severity)
spear_community_Nepal$anxiety_severity<-relevel(spear_community_Nepal$anxiety_severity, ref="Normal") #change ref category
table(spear_community_Nepal$anxiety_severity)
log.model_2_anxiety_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$anxiety_severity, data=spear_community_Nepal, family=binomial)
summary(log.model_2_anxiety_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_Nepal), confint.default(log.model_2_anxiety_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___merged_CVD <-as.factor(spear_community_Nepal$q43___merged_CVD)
log.model_2_CVD_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_CVD, data=spear_community_Nepal, family=binomial)
summary(log.model_2_CVD_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CVD_Nepal), confint.default(log.model_2_CVD_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___merged_Chronic_Infectious_Disease <-as.factor(spear_community_Nepal$q43___merged_Chronic_Infectious_Disease)
log.model_2_ChronicInfectious_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_Chronic_Infectious_Disease, data=spear_community_Nepal, family=binomial)
summary(log.model_2_ChronicInfectious_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_Nepal), confint.default(log.model_2_ChronicInfectious_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___merged_lung_disease <-as.factor(spear_community_Nepal$q43___merged_lung_disease)
log.model_2_LungDisease_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_lung_disease, data=spear_community_Nepal, family=binomial)
summary(log.model_2_LungDisease_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_Nepal), confint.default(log.model_2_LungDisease_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___merged_Mental_Health_Disorder <-as.factor(spear_community_Nepal$q43___merged_Mental_Health_Disorder)
log.model_2_Mental_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___merged_Mental_Health_Disorder, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Mental_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Mental_Nepal), confint.default(log.model_2_Mental_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___10 <-as.factor(spear_community_Nepal$q43___10)
log.model_2_CKD_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___10, data=spear_community_Nepal, family=binomial)
summary(log.model_2_CKD_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CKD_Nepal), confint.default(log.model_2_CKD_Nepal), level = 0.95)) #take OR and CI out

###NIL CA IN NEPAL####
spear_community_Nepal$q43___11 <-as.factor(spear_community_Nepal$q43___11)
log.model_2_Cancer_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___11, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Cancer_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_Nepal), confint.default(log.model_2_Cancer_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___15 <-as.factor(spear_community_Nepal$q43___15)
log.model_2_Smoker_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___15, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Smoker_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_Nepal), confint.default(log.model_2_Smoker_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___16 <-as.factor(spear_community_Nepal$q43___16)
log.model_2_Pregnant_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___16, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Pregnant_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_Nepal), confint.default(log.model_2_Pregnant_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___17 <-as.factor(spear_community_Nepal$q43___17)
log.model_2_Other_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___17, data=spear_community_Nepal, family=binomial)
summary(log.model_2_Other_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Other_Nepal), confint.default(log.model_2_Other_Nepal), level = 0.95)) #take OR and CI out

spear_community_Nepal$q43___18 <-as.factor(spear_community_Nepal$q43___18)
log.model_2_NoComorbidities_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$q43___18, data=spear_community_Nepal, family=binomial)
summary(log.model_2_NoComorbidities_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_Nepal), confint.default(log.model_2_NoComorbidities_Nepal), level = 0.95)) #take OR and CI out

log.model_2_EconomicInsecuirity_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$econ_insecurity_score, data=spear_community_Nepal, family=binomial)
summary(log.model_2_EconomicInsecuirity_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_Nepal), confint.default(log.model_2_EconomicInsecuirity_Nepal), level = 0.95)) #take OR and CI out

log.model_2_BiologicalRisk_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$bio_risk_score, data=spear_community_Nepal, family=binomial)
summary(log.model_2_BiologicalRisk_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_Nepal), confint.default(log.model_2_BiologicalRisk_Nepal), level = 0.95)) #take OR and CI out


log.model_2_PercievedRisk_Nepal<- glm(spear_community_Nepal$q44_missed_app ~ spear_community_Nepal$perceived_risk_score, data=spear_community_Nepal, family=binomial)
summary(log.model_2_PercievedRisk_Nepal)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_Nepal), confint.default(log.model_2_PercievedRisk_Nepal), level = 0.95)) #take OR and CI out


###########Run for Vietnam ##########
######Crude OR ############

spear_community_Vietnam$q44_missed_app <-as.factor(spear_community_Vietnam$q44_missed_app) 


log.model_2_age_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q2_age_group, data=spear_community_Vietnam, family=binomial) 
#show model
summary(log.model_2_age_Vietnam) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_age_Vietnam), confint.default(log.model_2_age_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_sex_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q1_sex_bin, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_sex_Vietnam) #take p value out
exp(cbind("Odds ratio" = coef(log.model_2_sex_Vietnam), confint.default(log.model_2_sex_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_ethnicity_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q4_min_ethnicity, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_ethnicity_Vietnam) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ethnicity_Vietnam), confint.default(log.model_2_ethnicity_Vietnam), level = 0.95)) #take OR and CI out

#Change reference category for education
table(spear_community_Vietnam$education_3)
spear_community_Vietnam$education_3<-relevel(spear_community_Vietnam$education_3, ref="Primary") 
table(spear_community_Vietnam$education_3)
spear_community_Vietnam$education_3 <-as.factor(spear_community_Vietnam$education_3) #education as a factor

log.model_2_edu_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$education_3, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_edu_Vietnam) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_edu_Vietnam), confint.default(log.model_2_edu_Nepal), level = 0.95)) #take OR and CI out

log.model_2_employment_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q6_employment_cat, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_employment_Vietnam) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_employment_Vietnam), confint.default(log.model_2_employment_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$income_dollar <-as.factor(spear_community_Vietnam$income_dollar) 
log.model_2_income_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$income_dollar, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_income_Vietnam) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_Vietnam), confint.default(log.model_2_income_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q11_income_sources <-as.factor(spear_community_Vietnam$q11_income_sources) #location
log.model_2_income_sources_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_income_sources_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_income_sources_Vietnam), confint.default(log.model_2_income_sources_Vietnam), level = 0.95)) #take OR and CI out

#######Change reference category for location
spear_community_Vietnam$q12_urban<-relevel(spear_community_Vietnam$q12_urban, ref="Urban") 
table(spear_community_Vietnam$q12_urban)
spear_community_Vietnam$q12_urban <-as.factor(spear_community_Vietnam$q12_urban) #location
log.model_2_spear_location_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q12_urban, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_spear_location_Vietnam) #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_spear_location_Vietnam), confint.default(log.model_2_spear_location_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$depression_severity <-as.factor(spear_community_Vietnam$depression_severity)
spear_community_Vietnam$depression_severity<-relevel(spear_community_Vietnam$depression_severity, ref="Normal") #change reference category to Normal
table(spear_community_Vietnam$depression_severity)
log.model_2_depression_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$depression_severity, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_depression_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_depression_Vietnam), confint.default(log.model_2_depression_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$anxiety_severity <-as.factor(spear_community_Vietnam$anxiety_severity)
spear_community_Vietnam$anxiety_severity<-relevel(spear_community_Vietnam$anxiety_severity, ref="Normal") #change ref category
table(spear_community_Vietnam$anxiety_severity)
log.model_2_anxiety_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$anxiety_severity, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_anxiety_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_Vietnam), confint.default(log.model_2_anxiety_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___merged_CVD <-as.factor(spear_community_Vietnam$q43___merged_CVD)
log.model_2_CVD_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_CVD, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_CVD_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CVD_Vietnam), confint.default(log.model_2_CVD_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___merged_Chronic_Infectious_Disease <-as.factor(spear_community_Vietnam$q43___merged_Chronic_Infectious_Disease)
log.model_2_ChronicInfectious_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_Chronic_Infectious_Disease, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_ChronicInfectious_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_Vietnam), confint.default(log.model_2_ChronicInfectious_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___merged_lung_disease <-as.factor(spear_community_Vietnam$q43___merged_lung_disease)
log.model_2_LungDisease_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_lung_disease, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_LungDisease_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_Vietnam), confint.default(log.model_2_LungDisease_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___merged_Mental_Health_Disorder <-as.factor(spear_community_Vietnam$q43___merged_Mental_Health_Disorder)
log.model_2_Mental_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_Mental_Health_Disorder, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Mental_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Mental_Vietnam), confint.default(log.model_2_Mental_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___10 <-as.factor(spear_community_Vietnam$q43___10)
log.model_2_CKD_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___10, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_CKD_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_CKD_Vietnam), confint.default(log.model_2_CKD_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___11 <-as.factor(spear_community_Vietnam$q43___11)
log.model_2_Cancer_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___11, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Cancer_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_Vietnam), confint.default(log.model_2_Cancer_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___15 <-as.factor(spear_community_Vietnam$q43___15)
log.model_2_Smoker_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___15, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Smoker_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_Vietnam), confint.default(log.model_2_Smoker_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___16 <-as.factor(spear_community_Vietnam$q43___16)
log.model_2_Pregnant_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___16, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Pregnant_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_Vietnam), confint.default(log.model_2_Pregnant_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___17 <-as.factor(spear_community_Vietnam$q43___17)
log.model_2_Other_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___17, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Other_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_Other_Vietnam), confint.default(log.model_2_Other_Vietnam), level = 0.95)) #take OR and CI out

spear_community_Vietnam$q43___18 <-as.factor(spear_community_Vietnam$q43___18)
log.model_2_NoComorbidities_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___18, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_NoComorbidities_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_Vietnam), confint.default(log.model_2_NoComorbidities_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_EconomicInsecuirity_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$econ_insecurity_score, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_EconomicInsecuirity_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_Vietnam), confint.default(log.model_2_EconomicInsecuirity_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_BiologicalRisk_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$bio_risk_score, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_BiologicalRisk_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_Vietnam), confint.default(log.model_2_BiologicalRisk_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_PercievedRisk_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$perceived_risk_score, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_PercievedRisk_Vietnam)  #take p values out
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_Vietnam), confint.default(log.model_2_PercievedRisk_Vietnam), level = 0.95)) #take OR and CI out



######CROSS TABS for CRUDE ODDS RATIOS BY COUNTRY 2/0+2
#Indonesia
table (spear_community_Indonesia$q12_urban, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q2_age_group, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q1_sex_bin, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q4_min_ethnicity, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$education_3, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q6_employment_cat, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$income_dollar, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$q11_income_sources, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$depression_severity, spear_community_Indonesia$q44_missed_app)
table (spear_community_Indonesia$anxiety_severity, spear_community_Indonesia$q44_missed_app)

#2/1+2
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

#Nepal 2/0+2
table (spear_community_Nepal$q12_urban, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q2_age_group, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q1_sex_bin, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q4_min_ethnicity, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$education_3, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q6_employment_cat, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$income_dollar, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$q11_income_sources, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$depression_severity, spear_community_Nepal$q44_missed_app)
table (spear_community_Nepal$anxiety_severity, spear_community_Nepal$q44_missed_app)
#2/1+2
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

#Vietnam 2/0+2
table (spear_community_Vietnam$q12_urban, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q2_age_group, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q1_sex_bin, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q4_min_ethnicity, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$education_3 , spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q6_employment_cat, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$income_dollar, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$q11_income_sources, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$depression_severity, spear_community_Vietnam$q44_missed_app)
table (spear_community_Vietnam$anxiety_severity, spear_community_Vietnam$q44_missed_app)
#2/1+2
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


#NOW RUN ADJUSTED OR FOR COUNTRIES

######Indonesia (sex, ethnic group, education, household income)

spear_community_Indonesia$depression_severity<-relevel(spear_community_Indonesia$depression_severity, ref="Normal")

log.model_2_depression_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$depression_severity+ spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_depression_adj_Indo) #use p values for depression categories
exp(cbind("Odds ratio" = coef(log.model_2_depression_adj_Indo), confint.default(log.model_2_depression_adj_Indo), level = 0.95)) #take OR and CI out for depression categories

spear_community_Indonesia$anxiety_severity <-as.factor(spear_community_Indonesia$anxiety_severity)
spear_community_Indonesia$anxiety_severity<-relevel(spear_community_Indonesia$anxiety_severity, ref="Normal") #change ref category
log.model_2_anxiety_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$anxiety_severity + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_anxiety_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_adj_Indo), confint.default(log.model_2_anxiety_adj_Indo), level = 0.95)) #take OR and CI out

log.model_2_EconomicInsecuirity_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$econ_insecurity_score + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar,  data=spear_community_Indonesia, family=binomial)
summary(log.model_2_EconomicInsecuirity_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_adj_Indo), confint.default(log.model_2_EconomicInsecuirity_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_BiologicalRisk_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$bio_risk_score + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar,  data=spear_community_Indonesia, family=binomial)
summary(log.model_2_BiologicalRisk_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_adj_Indo), confint.default(log.model_2_BiologicalRisk_adj_Indo), level = 0.95)) #take OR and CI out

log.model_2_PercievedRisk_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$perceived_risk_score + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_PercievedRisk_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_adj_Indo), confint.default(log.model_2_PercievedRisk_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_CVD_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_CVD + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_CVD_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_CVD_adj_Indo), confint.default(log.model_2_CVD_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_ChronicInfectious_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_Chronic_Infectious_Disease + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_ChronicInfectious_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_adj_Indo), confint.default(log.model_2_ChronicInfectious_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_LungDisease_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_lung_disease + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar,  data=spear_community_Indonesia, family=binomial)
summary(log.model_2_LungDisease_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_adj_Indo), confint.default(log.model_2_LungDisease_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_CKD_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___10 + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_CKD_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_CKD_adj_Indo), confint.default(log.model_2_CKD_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Cancer_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___11 + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Cancer_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_adj_Indo), confint.default(log.model_2_Cancer_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Mental_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___merged_Mental_Health_Disorder + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Mental_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Mental_adj_Indo), confint.default(log.model_2_Mental_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Smoker_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___15 + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Smoker_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_adj_Indo), confint.default(log.model_2_Smoker_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Pregnant_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___16 + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Pregnant_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_adj_Indo), confint.default(log.model_2_Pregnant_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_Other_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___17 + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_Other_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_Other_adj_Indo), confint.default(log.model_2_Other_adj_Indo), level = 0.95)) #take OR and CI out


log.model_2_NoComorbidities_adj_Indo<- glm(spear_community_Indonesia$q44_missed_app ~ spear_community_Indonesia$q43___18 + spear_community_Indonesia$q1_sex_bin + spear_community_Indonesia$q4_min_ethnicity + spear_community_Indonesia$education_3 + spear_community_Indonesia$income_dollar, data=spear_community_Indonesia, family=binomial)
summary(log.model_2_NoComorbidities_adj_Indo)
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_adj_Indo), confint.default(log.model_2_NoComorbidities_adj_Indo), level = 0.95)) #take OR and CI out


###############No ADJUSTED Or done for nepal as no statistically significant cofounders###########

##########VIETNAM (location, income, income sources)

spear_community_Vietnam$depression_severity<-relevel(spear_community_Vietnam$depression_severity, ref="Normal") #change ref category

log.model_2_depression_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$depression_severity + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources , data=spear_community_Vietnam, family=binomial)
summary(log.model_2_depression_adj_Vietnam) #use p values for depression categories
exp(cbind("Odds ratio" = coef(log.model_2_depression_adj_Vietnam), confint.default(log.model_2_depression_adj_Vietnam), level = 0.95)) #take OR and CI out for depression categories


spear_community_Vietnam$anxiety_severity<-relevel(spear_community_Vietnam$anxiety_severity, ref="Normal") #change ref category
log.model_2_anxiety_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$anxiety_severity + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_anxiety_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_anxiety_adj_Vietnam), confint.default(log.model_2_anxiety_adj_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_EconomicInsecuirity_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$econ_insecurity_score + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_EconomicInsecuirity_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_EconomicInsecuirity_adj_Vietnam), confint.default(log.model_2_EconomicInsecuirity_adj_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_BiologicalRisk_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$bio_risk_score + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_BiologicalRisk_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_BiologicalRisk_adj_Vietnam), confint.default(log.model_2_BiologicalRisk_adj_Vietnam), level = 0.95)) #take OR and CI out

log.model_2_PercievedRisk_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$perceived_risk_score + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_PercievedRisk_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_PercievedRisk_adj_Vietnam), confint.default(log.model_2_PercievedRisk_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_CVD_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_CVD + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_CVD_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_CVD_adj_Vietnam), confint.default(log.model_2_CVD_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_ChronicInfectious_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_Chronic_Infectious_Disease + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_ChronicInfectious_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_ChronicInfectious_adj_Vietnam), confint.default(log.model_2_ChronicInfectious_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_LungDisease_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_lung_disease + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_LungDisease_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_LungDisease_adj_Vietnam), confint.default(log.model_2_LungDisease_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_CKD_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___10 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_CKD_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_CKD_adj_Vietnam), confint.default(log.model_2_CKD_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Cancer_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___11 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Cancer_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Cancer_adj_Vietnam), confint.default(log.model_2_Cancer_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Mental_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___merged_Mental_Health_Disorder + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Mental_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Mental_adj_Vietnam), confint.default(log.model_2_Mental_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Smoker_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___15 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Smoker_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Smoker_adj_Vietnam), confint.default(log.model_2_Smoker_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Pregnant_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___16 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Pregnant_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Pregnant_adj_Vietnam), confint.default(log.model_2_Pregnant_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_Other_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___17 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_Other_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_Other_adj_Vietnam), confint.default(log.model_2_Other_adj_Vietnam), level = 0.95)) #take OR and CI out


log.model_2_NoComorbidities_adj_Vietnam<- glm(spear_community_Vietnam$q44_missed_app ~ spear_community_Vietnam$q43___18 + spear_community_Vietnam$q12_urban + spear_community_Vietnam$income_dollar + spear_community_Vietnam$q11_income_sources, data=spear_community_Vietnam, family=binomial)
summary(log.model_2_NoComorbidities_adj_Vietnam)
exp(cbind("Odds ratio" = coef(log.model_2_NoComorbidities_adj_Vietnam), confint.default(log.model_2_NoComorbidities_adj_Vietnam), level = 0.95)) #take OR and CI out

