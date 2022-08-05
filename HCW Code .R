
library(gtsummary)  # this library is used for summary
library(dplyr)      #this library is used for combination
library(readxl)     #this one is used for reading xlsx file

#load the data
library(haven)


#Project SPEAR Placement
# Imported spear_health worker

#Summary data: Community
summary(spear_community)
summary(spear_healthworker)

#Install dplayer
install.packages("dplyr")

#Frequency of different Sex in HCW data all countries (male 1, female 2)

table(spear_healthworker['q1_sex'])


#Frequency of different ages in HCW data all countries                                                                                
table(spear_healthworker["q2_birth_year"])


#Frequency of education levels in HCW data all countries
table(spear_healthworker["q3_education"])


#Frequency of locations in HCW data all countries
table(spear_healthworker["q5_location"])
###only nepalese with 2199 NA##################

#Frequency of professions in HCW data all countries
table(spear_healthworker["q4_profession"])

#Frequency of Types of medical Facilities in HCW data all countries
table(spear_healthworker["q6_facility_type"])

#Frequency of education levels in HCW data in Vn                                                                                
table(spear_healthworker["q3_educationvn"])


###############################
########HCW Descriptive Table: SPEAR #######
###cleaned data####

#Total cleaned record
table(spear_healthworker$correct_dag)

#OPTION  function for summary (number and percentage)
tb1_all<-spear_healthworker%>% select("q2_age_simplified", "q3_educationmerged","q4_professionmerged", "q1_sex_adapted_n", "q5_locationmerged", "q6_facility_type_merged","q7_dept_merged", "correct_dag")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_dag) #table for three countries

#Sex for all HCW 
table(spear_healthworker$q1_sex_adapted_n)

#Sex for HCW broken down by country
table(spear_healthworker$q1_sex_adapted_n, spear_healthworker$correct_dag)


#Age categories for all HCW
table(spear_healthworker$q2_age_simplified_n)

####FOR AGE CATEGORY NAMES
table(spear_healthworker$q2_age_simplified)

#Age categories for HCW broken down by country
table(spear_healthworker$q2_age_simplified, spear_healthworker$correct_dag)


#Education for all HCW
table(spear_healthworker$q3_educationmerged)

#Education for HCW broken down by country
table(spear_healthworker$q3_educationmerged_n, spear_healthworker$correct_dag)


#Profession for all HCW
table(spear_healthworker$q4_professionmerged)

#Profession for HCW broken down by country
table(spear_healthworker$q4_professionmerged, spear_healthworker$correct_dag)

#####Location by HCW all not done#######

#Location for HCW broken down by country
table(spear_healthworker$q5_locationmerged, spear_healthworker$correct_dag)

#####use no _n to get names #### ?????missing nepal site breakdown???? see comments in table

table(spear_healthworker$q5_locationmerged, spear_healthworker$correct_dag)


#Facility type for all HCW
table(spear_healthworker$q6_facility_type_merged)

#Facility type for HCW broken down by country
table(spear_healthworker$q6_facility_type_merged,  spear_healthworker$correct_dag)


#Hospital department for all HCW #####
table(spear_healthworker$q7_dept_merged)

#Hospital department HCW broken down by country ####
table(spear_healthworker$q7_dept_merged,  spear_healthworker$correct_dag)



######Descriptive table 2: Disruptions to Health service provision#####

#Summary function (table 2)
tb1_all<-spear_healthworker%>% select("q11_happened___1", "q11_happened___2", "q11_happened___3", "q11_happened___4","q11_happened___5","q11_happened___6", "q11_happened___7", "correct_dag")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_dag) #table for three countries


#Summary function divided by prim/sec/tertiary (table2B)
tb1_all<-spear_healthworker%>% select("q11_happened___1", "q11_happened___2", "q11_happened___3", "q11_happened___4","q11_happened___5","q11_happened___6", "q11_happened___7", "q6_facility_type_merged")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=q6_facility_type_merged) #table for facilities

#Summary Function divided by prim/sec/tertiary for each country (2c, d, e)
### START BY MAKING PER COUNTRY DATA SET
####Create Subset for Indonesia
spear_healthworker_Indonesia <- subset(spear_healthworker, spear_healthworker$correct_dag == "Indonesia", )
####TO CHECK IF ITS WORKING###
table(spear_healthworker$q1_sex_adapted_n, spear_healthworker$correct_dag)
table(spear_healthworker_Indonesia$q1_sex_adapted_n)
#######Create subset for Nepal
spear_healthworker_Nepal <- subset(spear_healthworker, spear_healthworker$correct_dag == "Nepal", )
####TO CHECK IF ITS WORKING###
table(spear_healthworker$q1_sex_adapted_n, spear_healthworker$correct_dag)
table(spear_healthworker_Nepal$q1_sex_adapted_n)
#######Create subset for Vietnam
spear_healthworker_Vietnam <- subset(spear_healthworker, spear_healthworker$correct_dag == "Vietnam", )
####TO CHECK IF ITS WORKING###
table(spear_healthworker$q1_sex_adapted_n, spear_healthworker$correct_dag)
table(spear_healthworker_Vietnam$q1_sex_adapted_n)

#######Summary Function table: Disruptions to health services disggregated by level of service, for Indonesia
tb1_all<-spear_healthworker_Indonesia%>% select("spear_healthworker_Indonesia$q11_happened___1", "spear_healthworker_Indonesia$q11_happened___2", "spear_healthworker_Indonesia$q11_happened___3", "spear_healthworker_Indonesia$q11_happened___4","spear_healthworker_Indonesia$q11_happened___5","spear_healthworker_Indonesia$q11_happened___6", "spear_healthworker_Indonesia$q11_happened___7", "spear_healthworker_Indonesia$q6_facility_type_merged")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=q6_facility_type_merged) #table for facilities

#######Summary Function table: Disruptions to health services disggregated by level of service, for Nepal
tb1_all<-spear_healthworker_Nepal%>% select("q11_happened___1", "q11_happened___2", "q11_happened___3", "q11_happened___4","q11_happened___5","q11_happened___6", "q11_happened___7", "q6_facility_type_merged")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=q6_facility_type_merged) #table for facilities

#######Summary Function table: Disruptions to health services disggregated by level of service, for Vietnam
tb1_all<-spear_healthworker_Vietnam%>% select("q11_happened___1", "q11_happened___2", "q11_happened___3", "q11_happened___4","q11_happened___5","q11_happened___6", "q11_happened___7", "q6_facility_type_merged")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=q6_facility_type_merged) #table for facilities


#Survey Question 11# option 1-non-essential surgery cancelled/delayed all countries
table(spear_healthworker$q11_happened___1)

#Survey Question 11# option 1-non-essential surgery cancelled/delayed by country
table(spear_healthworker$q11_happened___1, spear_healthworker$correct_dag)

#Survey Question 11# option 2-non-essential procedures cancelled/delayed all countries
table(spear_healthworker$q11_happened___2)

#Survey Question 11# option 2-non-essential procedures cancelled/delayed by country
table(spear_healthworker$q11_happened___2, spear_healthworker$correct_dag)

#Survey Question 11# option 3- other appointments cancelled/delayed all countries
table(spear_healthworker$q11_happened___3)

#Survey Question 11# option 3- other appointments cancelled/delayed by country
table(spear_healthworker$q11_happened___3,spear_healthworker$correct_dag)

#Survey Question 11# option 4- oappointments on the phone all countries
table(spear_healthworker$q11_happened___4)

#Survey Question 11# option 4- oappointments on the phone by country
table(spear_healthworker$q11_happened___4, spear_healthworker$correct_dag)

#Survey Question 11# option 5- vaccination days or services delayed all countries
table(spear_healthworker$q11_happened___5)

#Survey Question 11# option 5- vaccination days or services delayed bu country
table(spear_healthworker$q11_happened___5,spear_healthworker$correct_dag)

#Survey Question 11# option 6- stock outs of essential medicines for gen pt all countries
table(spear_healthworker$q11_happened___6)

#Survey Question 11# option 6- stock outs of essential medicines for gen pt by country
table(spear_healthworker$q11_happened___6, spear_healthworker$correct_dag)

#Survey Question 11# option 7- stock outs of essential medicines for covid pt all countries
table(spear_healthworker$q11_happened___7)

#Survey Question 11# option 7- stock outs of essential medicines for covid pt by country
table(spear_healthworker$q11_happened___7, spear_healthworker$correct_dag)

#Summary function (CANNOT USE FOR q15 as table produces values for 1, however need to use 0 as 0= inadequate HCW)
tb1_all<-spear_healthworker%>% select("q11_happened___1", "q11_happened___2", "q11_happened___3", "q11_happened___4","q11_happened___5","q11_happened___6", "q11_happened___7", "q15_1_doctors", "q15_2_nurses", "q15_3_midwives", "q15_4_nonclinical", "q15_5_social_workers","q15_6_pharmacists","q15_7_chw", "q15_8_related_staff","correct_dag")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_dag) #table for three countries


####Survey Question 15 #option 1- inadequate staffing Drs/dr assistants for all countries
table(spear_healthworker$q15_1_doctors)

#Survey Question 15 #option 1- inadequate staffing Drs/dr assistants by country
table(spear_healthworker$q15_1_doctors, spear_healthworker$correct_dag)

#Survey Question 15 #option 2- inadequate staffing nurses/nurse assistants for all countries
table(spear_healthworker$q15_2_nurses)

#Survey Question 15 #option 2- inadequate staffing nurses/nurse assistants for by country
table(spear_healthworker$q15_2_nurses, spear_healthworker$correct_dag)

#Survey Question 15 #option 3- inadequate staffing midwives for all countries
table(spear_healthworker$q15_3_midwives)


#Survey Question 15 #option 3- inadequate staffing midwives for by country
table(spear_healthworker$q15_3_midwives,spear_healthworker$correct_dag)

#Survey Question 15 #option 4- inadequate staffing staff in non-clinical dept for all countries
table(spear_healthworker$q15_4_nonclinical)

#Survey Question 15 #option 4- inadequate staffing staff in non-clinical dept by country
table(spear_healthworker$q15_4_nonclinical, spear_healthworker$correct_dag)


#Survey Question 15 #option 5- inadequate staffing medical social worker for all countries
table(spear_healthworker$q15_5_social_workers)

#Survey Question 15 #option 5- inadequate staffing medical social worker by country
table(spear_healthworker$q15_5_social_workers,spear_healthworker$correct_dag)

#Survey Question 15 #option 6- inadequate staffing pharmacists for all countries
table(spear_healthworker$q15_6_pharmacists)

#Survey Question 15 #option 6- inadequate staffing pharmacist by country
table(spear_healthworker$q15_6_pharmacists,spear_healthworker$correct_dag)


#Survey Question 15 #option 7- inadequate staffing CHW for all countries
table(spear_healthworker$q15_7_chw)

#Survey Question 15 #option 7- inadequate staffing CHW by country
table(spear_healthworker$q15_7_chw,spear_healthworker$correct_dag)


#Survey Question 15 #option 8- inadequate staffing health related staff for all countries
table(spear_healthworker$q15_8_related_staff)

#Survey Question 15 #option 8- inadequate staffing health related staff by country
table(spear_healthworker$q15_8_related_staff,spear_healthworker$correct_dag)


#Summary function (For Q 16 add 0:Not available in my dept + 2: stock out )
tb1_all<-spear_healthworker%>% select("q16_1_sanitiser", "q16_2_masks","q16_3_n95", "q16_4_gowns", "q16_5_suits","q16_6_head_cover","q16_7_shoe_cover", "q16_8_gloves","q16_9_visor", "q16_10_goggles", "q16_11_ventilators", "q16_12_isolation_rooms", "q16_13_beds", "q16_99_other", "correct_dag")
tb1_all%>% tbl_summary() #table for all 
tb1_all %>% tbl_summary(by=correct_dag) #table for three countries

#####Survey Question 16 #option 1- equipment stock out sanitizer for all countries ????how to handle variable (yes stock out =2)
table(spear_healthworker$q16_1_sanitiser)

#Survey Question 16 #option 1- equipment stock out sanitizer by country
table(spear_healthworker$q16_1_sanitiser, spear_healthworker$correct_dag)

#Survey Question 16 #option 2- equipment stock out surgical masks for all countries
table(spear_healthworker$q16_2_masks)

#Survey Question 16 #option 2- equipment stock out surgical masks for all countries
table(spear_healthworker$q16_2_masks,spear_healthworker$correct_dag)

#Survey Question 16 #option 3- equipment stock out N95 for all countries
table(spear_healthworker$q16_3_n95) 

#Survey Question 16 #option 3- equipment stock out N95 by country
table(spear_healthworker$q16_3_n95, spear_healthworker$correct_dag)

#Survey Question 16 #option 4- equipment stock out gowns for all countries
table(spear_healthworker$q16_4_gowns)

#Survey Question 16 #option 4- equipment stock out gowns by country
table(spear_healthworker$q16_4_gowns,spear_healthworker$correct_dag)

#Survey Question 16 #option 5- equipment stock out hazmat suit for all countries
table(spear_healthworker$q16_5_suits)

#Survey Question 16 #option 5- equipment stock out hazmat suit fby countries
table(spear_healthworker$q16_5_suits,spear_healthworker$correct_dag)

#Survey Question 16 #option 6- equipment stock out headcover for all countries
table(spear_healthworker$q16_6_head_cover)

#Survey Question 16 #option 6- equipment stock out headcover by country
table(spear_healthworker$q16_6_head_cover,spear_healthworker$correct_dag)

#Survey Question 16 #option 7- equipment stock out shoecover for all countries
table(spear_healthworker$q16_7_shoe_cover)

#Survey Question 16 #option 7- equipment stock out shoecover by country
table(spear_healthworker$q16_7_shoe_cover,spear_healthworker$correct_dag)

#Survey Question 16 #option 8- equipment stock out gloves for all countries
table(spear_healthworker$q16_8_gloves) 

#Survey Question 16 #option 8- equipment stock out gloves by country
table(spear_healthworker$q16_8_gloves,spear_healthworker$correct_dag)

#Survey Question 16 #option 9- equipment stock out visor for all countries
table(spear_healthworker$q16_9_visor)

#Survey Question 16 #option 9- equipment stock out visor by country
table(spear_healthworker$q16_9_visor,spear_healthworker$correct_dag)

#Survey Question 16 #option 10- equipment stock out goggles for all countries
table(spear_healthworker$q16_10_goggles)

#Survey Question 16 #option 10- equipment stock out goggles by country
table(spear_healthworker$q16_10_goggles,spear_healthworker$correct_dag)

#Survey Question 16 #option 11- equipment stock out ventilators for all countries
table(spear_healthworker$q16_11_ventilators)

#Survey Question 16 #option 11- equipment stock out ventilators by country
table(spear_healthworker$q16_11_ventilators,spear_healthworker$correct_dag)

#Survey Question 16 #option 12- equipment stock out isolation rooms for all countries
table(spear_healthworker$q16_12_isolation_rooms)

#Survey Question 16 #option 12- equipment stock out isolation rooms by country
table(spear_healthworker$q16_12_isolation_rooms,spear_healthworker$correct_dag)

#Survey Question 16 #option 13- equipment stock out beds for all countries
table(spear_healthworker$q16_13_beds)

#Survey Question 16 #option 13- equipment stock out beds by country
table(spear_healthworker$q16_13_beds,spear_healthworker$correct_dag)

#Survey Question 16 #option 99- equipment stock out other for all countries
table(spear_healthworker$q16_99_other)

#Survey Question 16 #option 99- equipment stock out other by country
table(spear_healthworker$q16_99_other,spear_healthworker$correct_dag)


