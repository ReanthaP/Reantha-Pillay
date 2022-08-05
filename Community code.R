#Project SPEAR Placement
#####Imported data spear_community
library(readr)
library(gtsummary)  # this library is used for summary
library(dplyr)      #this library is used for combination
library(readxl)     #this one is used for reading xlsx file

#load the data
library(haven)
spear_community <- read_csv("~/Desktop/Dissertation/QUANTITATIVE/Healthcare access data/spear_community.csv")
View(spear_community)


###Total community records 
table(spear_community$correct_DAG)

#Sex for all Community members
table(spear_community$q1_sex_bin)

#Sex for Community members by country
table(spear_community$q1_sex_bin, spear_community$correct_DAG)

#Age group for all community members
table(spear_community$q2_age_group)

#Age group for community members by country
table(spear_community$q2_age_group,spear_community$correct_DAG)


#Marital status for all community members
table(spear_community$q3_married)

#Marital status for community members by country
table(spear_community$q3_married,spear_community$correct_DAG)


#Ethnic Minority for all community members
table(spear_community$q4_min_ethnicity)


#Ethnic Minority for community members by country
table(spear_community$q4_min_ethnicity,spear_community$correct_DAG)


#Education for all community members 
table(spear_community$q5_education)
#must create a new column and a new set of categories (primary, secondary, higher)
spear_community$education_3 <- recode(spear_community$q5_education, "0"="Primary", "1"="Primary", "2"="Primary", "3"="Secondary", "4"="Higher", "5"="Higher", "6"="Higher")
table(spear_community$education_3)

#Education for community members divided by county 
table(spear_community$education_3, spear_community$correct_DAG)


#Employment status for all community members
table(spear_community$q6_employment_cat)

#Employment status for community members by country
table(spear_community$q6_employment_cat, spear_community$correct_DAG)
table(spear_community_Indonesia$q6_employment_cat)

#Occupation category for all community members
table(spear_community$q7_occupation_cat)
table(spear_community$q7_occupation)

#Occupation category for community members by country 
table(spear_community$q7_occupation_cat, spear_community$correct_DAG) 
table(spear_community$q7_occupation, spear_community$correct_DAG)

#Location for all community members
table(spear_community$q12_urban)

#Location for all community members by country
table(spear_community$q12_urban, spear_community$correct_DAG) 

#Monthly income for all community members
#must create a new column and a new set of categories (<$100, $100-$399, $400-$1000, >$1000, Unknown)

spear_community$income_dollar <- recode(spear_community$q9_income, "1"="<$100", "2"="$100-$399", "3"="$100-$399", "4"="$400-$1000", "5"="$400-$1000", "6"=">$1000", "97"="Unknown","98"="Unknown")
table(spear_community$income_dollar)

#Monthly income for community members by country
table(spear_community$income_dollar,spear_community$correct_DAG)

#Type of dwelling for all community members
table(spear_community$q14_dwelling)

#Type of dwelling community members by country
table(spear_community$q14_dwelling, spear_community$correct_DAG) 

#Living situation community members for all countries
table(spear_community$q13_current_2) 

#Living situation community members by countries
table(spear_community$q13_current_2, spear_community$correct_DAG) 

#Rent or Own for all community members
table(spear_community$q19_accomm)

#Rent or Own for community members by country
table(spear_community$q19_accomm, spear_community$correct_DAG)


#CROWDING###################
#make new binomial variable for Crowding where >2 is crowded and =< is not crowded
spear_community$crowdingcat<-ifelse(spear_community$crowding >2,"Crowded","Not crowded")

#Crowding for all community members 
table(spear_community$crowdingcat)

#Crowding for all community members by country 
table(spear_community$crowdingcat, spear_community$correct_DAG) 


#Household member in HCW for all community members (0=No All other numbers added =yes)
table(spear_community$q17_hcws)

#Household member HCW for community members by country (0=No All other numbers added =yes)
table(spear_community$q17_hcws, spear_community$correct_DAG)

#Household member working in high contact role for all community (0=No All other numbers added =yes)
table(spear_community$q18_services)

#Household member working in high contact role community members by country (0=No All other numbers added =yes)
table(spear_community$q18_services, spear_community$correct_DAG) 



################OPTION  function for summary (number and percentage) ######## Table 3 Part 1
tb1_all<-spear_community%>% select("q2_age_group", "q3_married","q1_sex_bin", "q4_min_ethnicity", "education_3", "q6_employment_cat","q7_occupation_cat", "q12_urban","income_dollar", "q14_dwelling", "q13_current_2", "q19_accomm", "q17_hcws", "q18_services","crowdingcat", "correct_DAG")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_DAG) #table for three countries



#Chronic conditions for all community members--option 1: Diabetes
table(spear_community$q43___1)

#Chronic conditions for community members by country --option 1: Diabetes
table(spear_community$q43___1, spear_community$correct_DAG)


#Chronic conditions for all community members--option 2: Hypertension
table(spear_community$q43___2)

#Chronic conditions for community members by country --option 2:Hypertension
table(spear_community$q43___2, spear_community$correct_DAG)

#Chronic conditions for all community members--option 3:Obesity
table(spear_community$q43___3)

#Chronic conditions for community members by country --option 4: Obesity
table(spear_community$q43___3, spear_community$correct_DAG)

#Chronic conditions for all community members--option 4:CVS
table(spear_community$q43___4)

#Chronic conditions for community members by country --option 4: CVS
table(spear_community$q43___4, spear_community$correct_DAG)

#Chronic conditions for all community members--option 5:Chronic infectious disease
table(spear_community$q43___5)

#Chronic conditions for community members by country --option 5:Chronic infectious disease
table(spear_community$q43___5, spear_community$correct_DAG)

#Chronic conditions for all community members--option 6:TB
table(spear_community$q43___6)

#Chronic conditions for community members by country --option 6: TB
table(spear_community$q43___6, spear_community$correct_DAG)


#Chronic conditions for all community members--option 7:Asthma
table(spear_community$q43___7)

#Chronic conditions for community members by country --option 7:Asthma
table(spear_community$q43___7, spear_community$correct_DAG)

#Chronic conditions for all community members--option 8:COPD
table(spear_community$q43___8)

#Chronic conditions for community members by country --option 8:COPD
table(spear_community$q43___8, spear_community$correct_DAG)

#Chronic conditions for all community members--option 9:Other lung disease
table(spear_community$q43___9)

#Chronic conditions for community members by country --option 9: Other lung disease
table(spear_community$q43___9, spear_community$correct_DAG)


#Chronic conditions for all community members--option 10:CKD
table(spear_community$q43___10)

#Chronic conditions for community members by country --option 10: CKD
table(spear_community$q43___10, spear_community$correct_DAG)

#Chronic conditions for all community members--option 11:Cancer
table(spear_community$q43___11)

#Chronic conditions for community members by country --option 11: Cancer
table(spear_community$q43___11, spear_community$correct_DAG)


#Chronic conditions for all community members--option 12:Depression
table(spear_community$q43___12)

#Chronic conditions for community members by country --option 12:Depression
table(spear_community$q43___12, spear_community$correct_DAG)

#Chronic conditions for all community members--option 13:Alcohol and substance use d/o
table(spear_community$q43___13)

#Chronic conditions for community members by country --option 13:Alcohol and substance use d/o
table(spear_community$q43___13, spear_community$correct_DAG)


#Chronic conditions for all community members--option 14:Other mental condition
table(spear_community$q43___14)

#Chronic conditions for community members by country --option 14:Other mental condition
table(spear_community$q43___14, spear_community$correct_DAG)

#Chronic conditions for all community members--option 15:current smoker
table(spear_community$q43___15)

#Chronic conditions for community members by country --option 15:current smoker
table(spear_community$q43___15, spear_community$correct_DAG)

#Chronic conditions for all community members--option 16:Pregnant
table(spear_community$q43___16)

#Chronic conditions for community members by country --option 16:Pregnant
table(spear_community$q43___16, spear_community$correct_DAG)

#Chronic conditions for all community members--option 17:Other
table(spear_community$q43___17)

#Chronic conditions for community members by country --option 17:Other
table(spear_community$q43___17, spear_community$correct_DAG)

#Chronic conditions for all community members--option 18:None
table(spear_community$q43___18)

#Chronic conditions for community members by country --option 18:None
table(spear_community$q43___18, spear_community$correct_DAG)




#################

#Reasons for missed appointments for all community members : Healthcare provider cancelled 1=yes
table(spear_community$q46___1)

#Reasons for missed appointments for community members by country: Healthcare provider cancelled 1=yes
table(spear_community$q46___1, spear_community$correct_DAG)

#Reasons for missed appointments for all community members : Healthcare provider closed 1=yes
table(spear_community$q46___2)

#Reasons for missed appointments for community members by country: Healthcare provider closed 1=yes
table(spear_community$q46___2, spear_community$correct_DAG)

#Reasons for missed appointments for all community members : Pharmacy closed 1=yes
table(spear_community$q46___3)

#Reasons for missed appointments for community members by country: pharmacy closed 1=yes
table(spear_community$q46___3, spear_community$correct_DAG)


#Reasons for missed appointments for all community members : Travel Restrictions 1=yes
table(spear_community$q46___4)

#Reasons for missed appointments for community members by country: Travel Restrictions 1=yes
table(spear_community$q46___4, spear_community$correct_DAG)


#Reasons for missed appointments for all community members : Fear of exposure  1=yes
table(spear_community$q46___6)

#Reasons for missed appointments for community members by country: fear of exposure 1=yes
table(spear_community$q46___6, spear_community$correct_DAG)

#Reasons for missed appointments for all community members : Fear of exposure on public transport 1=yes
table(spear_community$q46___7)

#Reasons for missed appointments for community members by country: fear of exposure on public transport 1=yes
table(spear_community$q46___7, spear_community$correct_DAG)

#Reasons for missed appointments for all community members : No public transport 1=yes
table(spear_community$q46___10)

#Reasons for missed appointments for community members by country:No public transport 1=yes
table(spear_community$q46___10, spear_community$correct_DAG)


#Reasons for missed appointments for all community members :Felt okay 1=yes
table(spear_community$q46___8)

#Reasons for missed appointments for community members by country:Felt okayt 1=yes
table(spear_community$q46___8, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :Financial strain  1=yes
table(spear_community$q46___9)

#Reasons for missed appointments for community members by country:Financial strain  1=yes
table(spear_community$q46___9, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :No permission from fam/spouse  1=yes
table(spear_community$q46___11)

#Reasons for missed appointments for community members by country:No permission from fam/spouse 1=yes
table(spear_community$q46___9, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :Forgot  1=yes
table(spear_community$q46___12)

#Reasons for missed appointments for community members by country:forgot  1=yes
table(spear_community$q46___12, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :Disrespected by HCW  1=yes
table(spear_community$q46___13)

#Reasons for missed appointments for community members by country:disrespected by HCW1=yes
table(spear_community$q46___13, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :Substance use  1=yes
table(spear_community$q46___14)

#Reasons for missed appointments for community members by country:substance use 1=yes
table(spear_community$q46___14, spear_community$correct_DAG)


#Reasons for missed appointments for all community members :Self isolating  1=yes
table(spear_community$q46___5)

#Reasons for missed appointments for community members by country:self isolating 1=yes
table(spear_community$q46___5, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :dont know  1=yes
table(spear_community$q46___97)

#Reasons for missed appointments for community members by country:dont know 1=yes
table(spear_community$q46___97, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :other  1=yes
table(spear_community$q46___99)

#Reasons for missed appointments for community members by country:other 1=yes
table(spear_community$q46___99, spear_community$correct_DAG)

#Reasons for missed appointments for all community members :Prefer not to say  1=yes
table(spear_community$q46___98)

#Reasons for missed appointments for community members by country:Prefer not to say 1=yes
table(spear_community$q46___98, spear_community$correct_DAG)


########################CREATE MERGED COLUMNS Q43 (Co-morbids) and Q46 (Reasons for missed visits)
library(gtsummary)  # this library is used for summary
library(dplyr)      #this library is used for combination
library(readxl)     #this one is used for reading xlsx file

#Create a merged column for co-morbidity: lung disease
spear_community["q43___merged_lung_disease"] <- NA
columns <- spear_community %>% select("q43___7","q43___8","q43___9")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 3 comorbidity columns into merged lung disease column 
for (i in 1:nrow(spear_community)) spear_community$q43___merged_lung_disease[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q43___merged_lung_disease[i] ==2) {spear_community$q43___merged_lung_disease[i] <- 1
  }
}

table(spear_community$q43___merged_lung_disease)

##Create a merged column for co-morbidity: Chronic Infectious Disease
spear_community["q43___merged_Chronic_Infectious_Disease"] <- NA
columns <- spear_community %>% select("q43___5", "q43___6")
col_indices <- which(names(spear_community)%in%names(columns))

#sum the values in 2 comorbidity columns into Chronic Infectious Disease
for (i in 1:nrow(spear_community)) spear_community$q43___merged_Chronic_Infectious_Disease[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q43___merged_Chronic_Infectious_Disease[i] ==2) {spear_community$q43___merged_Chronic_Infectious_Disease[i] <- 1
  }
}   

table(spear_community$q43___merged_Chronic_Infectious_Disease)

#Create a merged column for co-morbidity: Mental Health Disorder
spear_community["q43___merged_Mental_Health_Disorder"] <- NA
columns <- spear_community %>% select("q43___12", "q43___13","q43___14")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 3 comorbidity columns into Mental Health Disorder 
for (i in 1:nrow(spear_community)) spear_community$q43___merged_Mental_Health_Disorder[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q43___merged_Mental_Health_Disorder[i] ==2) {spear_community$q43___merged_Mental_Health_Disorder[i] <- 1
  }
}   

table(spear_community$q43___merged_Chronic_Infectious_Disease)


#Create merged column for co-morbidity: CVD
spear_community["q43___merged_CVD"] <- NA
columns <- spear_community %>% select("q43___1", "q43___2","q43___3","q43___4")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 4 comorbidity columns into CVD 
for (i in 1:nrow(spear_community)) spear_community$q43___merged_CVD[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q43___merged_CVD[i] >=2) {spear_community$q43___merged_CVD[i] <- 1
  }
}   

table(spear_community$q43___merged_CVD)

#Create a merged column for Reasons for missed appointments Facility Closed
spear_community["q46___merged__Facility Closed"] <- NA
columns <- spear_community %>% select("q46___2", "q46___3")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 2 reasons missed/delayed columns into Facility Closed 
for (i in 1:nrow(spear_community)) spear_community$q46___merged__FacilityClosed[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q46___merged__FacilityClosed[i] >=2) {spear_community$q46___merged__FacilityClosed[i] <- 1
  }
}   

table(spear_community$q46___merged__FacilityClosed)

#Create a merged column for Reasons for missed appointments Public Health Restrictions
spear_community["q46___merged__PublicHealth_Restrictions"] <- NA
columns <- spear_community %>% select("q46___4", "q46___10", "q46___5")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 3 reasons missed/delayed columns into Public Health Restrictions 
for (i in 1:nrow(spear_community)) spear_community$q46___merged__PublicHealth_Restrictions[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q46___merged__PublicHealth_Restrictions[i] >=2) {spear_community$q46___merged__PublicHealth_Restrictions[i] <- 1
  }
}   

table(spear_community$q46___merged__PublicHealth_Restrictions)



#Create a merged column for Reasons for missed appointments Fear of Exposure
spear_community["q46___merged__Fear_of_Exposure"] <- NA
columns <- spear_community %>% select("q46___6", "q46___7")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 2 reasons missed/delayed columns into Fear of Exposure 
for (i in 1:nrow(spear_community)) spear_community$q46___merged__Fear_of_Exposure[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q46___merged__Fear_of_Exposure[i] >=2) {spear_community$q46___merged__Fear_of_Exposure[i] <- 1
  }
}   

table(spear_community$q46___merged__Fear_of_Exposure)


#Create a merged column for Reasons for missed appointments: Personal Reasons
spear_community["q46___merged__Personal_Reasons"] <- NA
columns <- spear_community %>% select("q46___8", "q46___12")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 2 reasons missed/delayed columns into Personal Reasons 
for (i in 1:nrow(spear_community)) spear_community$q46___merged__Personal_Reasons[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q46___merged__Personal_Reasons[i] >=2) {spear_community$q46___merged__Personal_Reasons[i] <- 1
  }
}   

table(spear_community$q46___merged__Personal_Reasons)

#Create a merged column for Reasons for missed appointments:Social Reasons
spear_community["q46___merged__Social"] <- NA
columns <- spear_community %>% select("q46___9", "q46___11", "q46___13", "q46___14")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 4 reasons missed/delayed columns into Social Reasons 
for (i in 1:nrow(spear_community)) spear_community$q46___merged__Social[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q46___merged__Social[i] >=2) {spear_community$q46___merged__Social[i] <- 1
  }
} 

table(spear_community$q46___merged__Social)

#Create a merged column for Reasons for missed appointments:Reason not stated
spear_community["q46___merged__not_stated"] <- NA
columns <- spear_community %>% select("q46___97", "q46___98", "q46___99")
col_indices <- which(names(spear_community)%in%names(columns))


#sum the values in 2 reasons missed/delayed columns into Reasons not stated
for (i in 1:nrow(spear_community)) spear_community$q46___merged__not_stated[i]<- sum(spear_community[i,col_indices])
for (i in 1:nrow(spear_community)){
  if(spear_community$q46___merged__not_stated[i] >=2) {spear_community$q46___merged__not_stated[i] <- 1
  }
} 

table(spear_community$q46___merged__not_stated)

##############summary function for comorbidities and reasons for missed appointments, communities (TABLE 3 pat 2)
tb1_all<-spear_community%>% select("q43___1", "q43___2", "q43___3", "q43___4", "q43___10", "q43___11", "q43___15", "q43___16",  "q43___merged_lung_disease","q43___merged_Chronic_Infectious_Disease", "q43___merged_Mental_Health_Disorder","q43___merged_CVD","q43___17", "q43___18", "q46___1", "q46___merged__FacilityClosed","q46___merged__PublicHealth_Restrictions", "q46___merged__Fear_of_Exposure", "q46___merged__Personal_Reasons", "q46___merged__Social", "q46___merged__not_stated", "q18_services", "correct_DAG")
tb1_all%>% tbl_summary() #table for all
tb1_all %>% tbl_summary(by=correct_DAG) #table for three countries  



#########Table 5. Changes in Health seeking behaviour############
#summary function
tb1_all<-spear_community%>% select("q23_15_flu_jab", "q23_16_avoid_hf","q23_17_avoid_crowds", "q23_18_avoid_transport", "q23_19_avoid_hcw", "q23_5_drugs","q23_6_hands", "q23_10_symptom_advice","q23_11_hotline", "q23_2_herbal", "q23_3_vitaminc", "correct_DAG")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_DAG) #table for three countries


#Health seeking behavior: Get Flu vaccine for all community members
table(spear_community$q23_15_flu_jab)

#Health seeking behavior: Get Flu vaccine for community members by country
table(spear_community$q23_15_flu_jab, spear_community$correct_DAG)

#Health seeking behavior: Get Flu vaccine for all community members
table(spear_community$q23_15_flu_jab)

#Health seeking behavior: Get Flu vaccine for community members by country
table(spear_community$q23_15_flu_jab, spear_community$correct_DAG)

#Health seeking behavior: avoid health facilities for all community members
table(spear_community$q23_16_avoid_hf)

#Health seeking behavior: avoid health facilities for community members by country
table(spear_community$q23_16_avoid_hf, spear_community$correct_DAG)

#Health seeking behavior: avoid crowds for all community members
table(spear_community$q23_17_avoid_crowds)

#Health seeking behavior: avoid crowds for community members by country
table(spear_community$q23_17_avoid_crowds, spear_community$correct_DAG)

#Health seeking behavior: avoid public transport for all community members
table(spear_community$q23_18_avoid_transport)

#Health seeking behavior: avoid public transport for community members by country
table(spear_community$q23_18_avoid_transport, spear_community$correct_DAG)

#Health seeking behavior: avoid HCW for all community members
table(spear_community$q23_19_avoid_hcw)

#Health seeking behavior: avoid HCW for community members by country
table(spear_community$q23_19_avoid_hcw, spear_community$correct_DAG)

#Health seeking behavior: buying drugs from pharmacy for all community members
table(spear_community$q23_5_drugs)

#Health seeking behavior: buying drugs from the pharmacy for community members by country
table(spear_community$q23_5_drugs, spear_community$correct_DAG)

#Health seeking behavior: hand hygiene practices for all community members
table(spear_community$q23_6_hands)

#Health seeking behavior: hand hygiene practice for community members by country
table(spear_community$q23_6_hands, spear_community$correct_DAG)

#Health seeking behavior: seeking advice for covid sx for all community members
table(spear_community$q23_10_symptom_advice)

#Health seeking behavior: seeking advice for covid sx  for community members by country
table(spear_community$q23_10_symptom_advice, spear_community$correct_DAG)

#Health seeking behavior: call hotline covid sx for all community members
table(spear_community$q23_11_hotline)

#Health seeking behavior: call hotline for covid sx  for community members by country
table(spear_community$q23_11_hotline, spear_community$correct_DAG)

#Health seeking behavior: herbal supplements for all community members
table(spear_community$q23_2_herbal)

#Health seeking behavior: herbal supplement for community members by country
table(spear_community$q23_2_herbal, spear_community$correct_DAG)

#Health seeking behavior: Vit C supplements for all community members
table(spear_community$q23_3_vitaminc)

#Health seeking behavior: Vit C supplement for community members by country
table(spear_community$q23_3_vitaminc, spear_community$correct_DAG)


#############NOT USED
#############Logistic Regression: MODEL################

library(gtsummary)  # this library is used for summary
library(dplyr)      #this library is used for combination
library(readxl)     #this one is used for reading xlsx file

#Decide whether combing outcomes 1:m/d in last month and 2:m/d more than 1 mo ago or keeping separate (su the value, id >0)

#Creating a merged column for Q45: delayed/ Missed Outcome (Q45_1_operation all the way to Q45_10-_vacc) 
data[spear_community$q45_merged] <- NA
columns <- raw_data %>% select(starts_with (spear_community$q45_1_operation))
col_indices <- which(names(data)%in%names(columns))


#make categorical variables into factors 

#Fit model: determinants.of.forgone.healthcare<- glm(spear_community_Q45_combined ~ spear_communit$age + ...sex + ...location... ,family=binomial)

