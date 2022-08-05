###### Plot graph using ggplot

library(ggplot2)
library(dplyr)
library(gridExtra)

spear_community<-read.csv("C:/Users/halt.OUCRU/PH Epidemiology Dropbox/SPEAR/SPEAR study quantitative analysis/Analysis/Healthcare access analyses/spear_community.csv")


#Dealing with q35-before symptoms
Q35_total <- data.frame(sums = apply(spear_community %>% select(q35___1:q35___9),2,sum))
Q35_total <- Q35_total %>% mutate(prop = round((sums/sum(Q35_total)), 3))

Q35_VN <- data.frame(sums = apply(spear_community %>% filter(correct_DAG == "Vietnam") %>% select(q35___1:q35___9),2,sum))
Q35_VN <- Q35_VN %>% mutate(prop = round((sums/sum(Q35_VN)), 3))

Q35_INDO <- data.frame(sums = apply(spear_community %>% filter(correct_DAG == "Indonesia") %>% select(q35___1:q35___9),2,sum))
Q35_INDO <- Q35_INDO %>% mutate(prop = round((sums/sum(Q35_INDO)), 3))

Q35_NP <- data.frame(sums = apply(spear_community %>% filter(correct_DAG == "Nepal") %>% select(q35___1:q35___9),2,sum))
Q35_NP <- Q35_NP %>% mutate(prop = round((sums/sum(Q35_NP)), 3))

#Dealing with q37-during sypmtoms 
Q37_total <- data.frame(sums = apply(spear_community %>% select(q37___1:q37___9),2,sum))
Q37_total <- Q37_total %>% mutate(prop = round((sums/sum(Q37_total)), 3))

Q37_VN <- data.frame(sums = apply(spear_community %>% filter(correct_DAG == "Vietnam") %>% select(q37___1:q37___9),2,sum))
Q37_VN <- Q37_VN %>% mutate(prop = round((sums/sum(Q37_VN)), 3))

Q37_INDO <- data.frame(sums = apply(spear_community %>% filter(correct_DAG == "Indonesia") %>% select(q37___1:q37___9),2,sum))
Q37_INDO <- Q37_INDO %>% mutate(prop = round((sums/sum(Q37_INDO)), 3))

Q37_NP <- data.frame(sums = apply(spear_community %>% filter(correct_DAG == "Nepal") %>% select(q37___1:q37___9),2,sum))
Q37_NP <- Q37_NP %>% mutate(prop = round((sums/sum(Q37_NP)), 3))

par(mfrow=c(2,2))

#Combine the answer (1-9) into one table
ways <- c("1","1","2","2","3","3","4","4","5","5","6","6","7","7","8","8","9","9")
time<-c(rep(c("Before Covid-19 Pandemic", "During Covid-19 Pandemic"), 9))
time<-relevel(as.factor(time), ref = "Before")


#total
Q35_total$sums
Q37_total$sums
count_total <- c(614,267,650,277,520,199,395,138,331,107,155,71,520,100,294,89,346,99)
total <- data.frame(time, ways,count_total)

a<-ggplot(total, aes(x=ways, y=count_total, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Frequency")+ylim(0,650)+
  labs(title = "Health-seeking for respiratory symptoms across all three countries", subtitle = "1= Stayed at home, 2= More fluids, 3= Herbal Medicines, 4= OTC Antibiotics, 5= OTC Medication, 
       6= Telephone Consultation, 7= PHC visit, 8= Public Hospital visit, 9= Private Hospital Visit")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        plot.subtitle = element_text(face = "italic",  size = 10), #subtitle in italic
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title = element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"
show(a) #DID NOT USE AS Q37 needs to ne treated as a proportion

#VN
Q35_VN$sums
Q37_VN$sums
count_VN<-c(250,85,157,50,157,45,123,44,73,28,91,27,152,32,132,36,105,21)
VN <- data.frame(time, ways,count_VN)

b<-ggplot(total, aes(x=ways, y=count_VN, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Frequency")+ ylim(0,650)+
  labs(title = "Health-seeking for respiratory symptoms in VIETNAM")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title = element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"

show(b) #DID NOT USE AS Q37 needs to ne treated as a proportion

#Nepal
Q35_NP$sums
Q37_NP$sums
count_NP<-c(148,106,252,157,128,89,174,69,131,48,26,12,131,22,128,37,151,51)
NP <- data.frame(time, ways,count_NP)

c<-ggplot(total, aes(x=ways, y=count_NP, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Frequency")+ ylim(0,650)+
  labs(title = "Health-seeking for respiratory symptoms in NEPAL")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title = element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"
show(c) #DID NOT USE AS Q37 needs to ne treated as a proportion

#Indo
Q35_INDO$sums
Q37_INDO$sums
count_INDO<-c (216,76,241,70,235,65,98,25,127,31,38,32,237,46,34,16,90,27)

INDO <- data.frame(time, ways,count_INDO)

d<-ggplot(total, aes(x=ways, y=count_INDO, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Frequency")+ ylim(0,650)+
  labs(title = "Health-seeking for respiratory symptoms in INDONESIA")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title= element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"
show(d) #DID NOT USE AS Q37 needs to ne treated as a proportion

###############GRAPHS REDONE TREATING Q37 as a proportion

#Q35 as a proportion
Q35_sum <-data.frame(Q35_sums =apply(spear_community %>% select(starts_with("q35___")),2,sum))
Q35_sum <-Q35_sum %>% mutate(Q35_pro=round(Q35_sums/sum(Q35_sum)*100,2))

Q35_sum_VN <- data.frame(Q35_sums = apply(spear_community %>% filter(correct_DAG == "Vietnam") %>% select(q35___1:q35___9),2,sum))
Q35_sum_VN <- Q35_sum_VN %>% mutate(Q35_pro=round(Q35_sums/sum(Q35_sum_VN)*100, 2))

Q35_sum_INDO <- data.frame(Q35_sums = apply(spear_community %>% filter(correct_DAG == "Indonesia") %>% select(q35___1:q35___9),2,sum))
Q35_sum_INDO <- Q35_sum_INDO %>% mutate(pro = round(Q35_sums/sum(Q35_sum_INDO)*100,2))

Q35_sum_NP <- data.frame(Q35_sums = apply(spear_community %>% filter(correct_DAG == "Nepal") %>% select(q35___1:q35___9),2,sum))
Q35_sum_NP <- Q35_sum_NP %>% mutate(pro = round(Q35_sums/sum(Q35_sum_NP)*100, 2))


#Q37 as a proportion
q37_analysis <-subset (spear_community,q36___0==0)
Q37_sum <-data.frame(Q37_sums =apply(spear_community %>% select(starts_with("q37___")),2,sum))
Q37_sum <-Q37_sum %>% mutate(Q37_pro=round(Q37_sums/sum(Q37_sum)*100,2))

q37_analysis <-subset (spear_community,q36___0==0)
Q37_sum_VN <- data.frame(Q37_sums = apply(spear_community %>% filter(correct_DAG == "Vietnam") %>% select(q37___1:q37___9),2,sum))
Q37_sum_VN <- Q37_sum_VN %>% mutate(prop = round(Q37_sums/sum(Q37_sum_VN)*100,2))

q37_analysis <-subset (spear_community,q36___0==0)
Q37_sum_INDO <- data.frame(Q37_sums = apply(spear_community %>% filter(correct_DAG == "Indonesia") %>% select(q37___1:q37___9),2,sum))
Q37_sum_INDO <- Q37_sum_INDO %>% mutate(pro = round(Q37_sums/sum(Q37_sum_INDO)*100,2))

q37_analysis <-subset (spear_community,q36___0==0)
Q37_sum_NP <- data.frame(Q37_sums = apply(spear_community %>% filter(correct_DAG == "Nepal") %>% select(q37___1:q37___9),2,sum))
Q37_sum_NP <- Q37_sum_NP %>% mutate(pro = round(Q37_sums/sum(Q37_sum_NP)*100,2))



Q35_sum
Q37_sum
prop_total <- c(15.95,19.75,16.88,20.49,13.51,14.72,10.26,10.21,8.6,7.9,4.03,5.25,13.51,7.4,7.64,6.58,8.99,7.32)
total.prop <- data.frame(time, ways, prop_total)

a<-ggplot(total.prop, aes(x=ways, y=prop_total, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Percentage")+ylim(0,100)+
  labs(title = "Health-seeking for respiratory symptoms across all three countries", subtitle = "1= Stayed at home, 2= More fluids, 3= Herbal Medicines, 4= OTC Antibiotics, 5= OTC Medication, 
       6= Telephone Consultation, 7= PHC visit, 8= Public Hospital visit, 9= Private Hospital Visit")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        plot.subtitle = element_text(face = "italic",  size = 10), #subtitle in italic
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title = element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"
show(a)


#VN
Q35_sum_VN
Q37_sum_VN
prop_VN<-c(20.16, 23.10, 12.66, 13.59, 12.66, 12.23, 9.92, 11.96, 5.89, 7.61, 7.34, 7.34, 12.26, 8.70, 10.65, 9.78,8.47,5.71)
VN.prop <- data.frame(time, ways,prop_VN)

b<-ggplot(VN.prop, aes(x=ways, y=prop_VN, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Percentage")+ ylim(0,100)+
  labs(title = "Health-seeking for respiratory symptoms in VIETNAM")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title = element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"

show(b)

#Indo
Q35_sum_INDO
Q37_sum_INDO
prop_INDO<-c (16.41, 19.59, 18.31, 18.04, 17.86, 16.75, 7.45, 6.44, 9.65, 7.99, 2.89, 8.25, 18.01, 11.86, 2.58, 4.12, 6.84, 6.96)
INDO.prop <- data.frame(time, ways,prop_INDO)

c<-ggplot(INDO.prop, aes(x=ways, y=prop_INDO, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Percentage")+ ylim(0,100)+
  labs(title = "Health-seeking for respiratory symptoms in INDONESIA")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title= element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"
show(c)

#Nepal
Q35_sum_NP
Q37_sum_NP
prop_NP<-c(11.66,17.94, 19.86, 26.57, 10.09, 15.06, 13.71, 11.68, 10.32, 8.12, 2.05, 2.03,10.32, 3.72, 10.09, 6.26, 11.90, 8.63)
NP.prop <- data.frame(time, ways,prop_NP)

d<-ggplot(NP.prop, aes(x=ways, y=prop_NP, fill=time))+geom_bar(stat="identity", position = "dodge")+
  xlab("Health-seeking Behaviours") + ylab("Percentage")+ ylim(0,100)+
  labs(title = "Health-seeking for respiratory symptoms in NEPAL")+theme_bw()+
  theme(plot.title = element_text(size = 10, hjust=0.5, face = "bold"),  #size of title
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title = element_text(size = 9),
        legend.title = element_text(size=0)) #hide the legend title called "time"
show(d)

######DID NOT USE#####
#code to combine 4 into 1. U can use bottom to replace subtitle in each figure
gridExtra::grid.arrange(a,b,c,d, nrow=2, bottom="1=stay at home,....")