
#load libraries: data.table, dplyr, tidyr
#load CPC codes
#files are zipped (unzip?)

CPC<- fread("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/202202_CPC_Classes.7z", header= TRUE, sep= '|', stringsAsFactors = FALSE)
CPC<- fread("/Users/aferloni/Documents/Patent raw data/OECD/2022/Regpat/202202_CPC_Classes.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)

nuts<- read.table(unzip("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/nuts_patents.txt.zip"), header= TRUE, sep= '|', stringsAsFactors = FALSE)
nuts<- fread("/Users/aferloni/Documents/Geographic data/nuts_patents.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)


#-------MATCH LURs TO CPC codes
#reduce CPC codes to only the group level by selecting the first four characters, then match them to the LURs

CPC$CPC_Class<- substr(CPC$CPC_Class, 1, 4)

CPC<- CPC%>%
  distinct(appln_id, CPC_Class)

CPC_nuts<- nuts%>%
  inner_join(CPC, by="appln_id")%>%
  distinct(appln_id, lur, lur_code, CPC_Class, year) #one is counted for every time a code appears in a patent/lur, independently of 
#their relative weight (1 inventor or 1/8)

CPC_nuts<- CPC_nuts%>%
  filter(!CPC_Class %in% c("Y10S", "Y10T")) #I exclude these two codes because they are just tags of former USPC classifications, so they include many different technologies AND are too big.

CPC_nuts<- CPC_nuts%>%
  mutate(time=case_when(
    year < 1980 ~0,
    year >= 1980 & year < 1990 ~1,
    year >= 1990 & year < 2000 ~2,
    year >= 2000 & year < 2010 ~3,
    year >= 2010 & year < 2023 ~4
  ))

city_tech_time<-  CPC_nuts%>% 
  group_by(lur, time, group_id=CPC_Class)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  ungroup()

##---new formula by Mehdi (for all results see 2_regpat_nuts_nonuts)-----

prox_cos_1 <- xtabs(count ~ lur + group_id, data = filter(city_tech_time, time == 1))
prox_cos_1 <- crossprod(prox_cos_1) / sqrt(colSums(prox_cos_1^2) %*% t(colSums(prox_cos_1^2)))

prox_cos_2 <- xtabs(count ~ lur + group_id, data = filter(city_tech_time, time == 2))
prox_cos_2 <- crossprod(prox_cos_2) / sqrt(colSums(prox_cos_2^2) %*% t(colSums(prox_cos_2^2)))

prox_cos_3 <- xtabs(count ~ lur + group_id, data = filter(city_tech_time, time == 3))
prox_cos_3 <- crossprod(prox_cos_3) / sqrt(colSums(prox_cos_3^2) %*% t(colSums(prox_cos_3^2)))

prox_cos_4 <- xtabs(count ~ lur + group_id, data = filter(city_tech_time, time == 4))
prox_cos_4 <- crossprod(prox_cos_4) / sqrt(colSums(prox_cos_4^2) %*% t(colSums(prox_cos_4^2)))

#---current method to calculate specializations (RTA). File to check: lur_data_all_function------

alltechs_city_time<- city_tech_time%>%
  group_by(lur, time)%>%
  summarise(sum_lur=sum(count))%>%
  arrange(time, desc(sum_lur))%>%
  ungroup()

essay1<- alltechs_city_time%>%
  filter(time==1)
essay2<- alltechs_city_time%>%
  filter(time==2)
essay3<- alltechs_city_time%>%
  filter(time==3)
essay4<- alltechs_city_time%>%
  filter(time==4)

#calculate the distribution of sum_lur and select cities that are over 70 percentile

quantile(essay1$sum_lur, prob = seq(0, 1, length = 11))#>70%=792
quantile(essay2$sum_lur, prob = seq(0, 1, length = 11))#>70%=1697
quantile(essay3$sum_lur, prob = seq(0, 1, length = 11))#>70%=3925
quantile(essay4$sum_lur, prob = seq(0, 1, length = 11))#>70%=6481

#here I went back and basically took the cities in time4, with sum_lur over the 70% decile, also in the other three time periods. 99 cities.

essay4<- essay4%>%
  filter(sum_lur>=6481)
keep_cities<- essay4$lur

essay1<- essay1%>%
  filter(lur%in%keep_cities)

essay2<- essay2%>%
  filter(lur%in%keep_cities)

essay3<- essay3%>%
  filter(lur%in%keep_cities)


#put them back together
alltechs_city_time<- rbind(essay1, essay2, essay3, essay4)

city_tech_time<- city_tech_time%>%
  rename(group_id=CPC_Class)

#continue with calculations: codes sum for all lurs and all sum per time period
tech_across_cities<- city_tech_time%>%
  group_by(time, group_id)%>%
  summarise(across=sum(count))%>%
  arrange(time, desc(across))%>%
  ungroup()%>%
  filter(time>0)

size_all<- alltechs_city_time%>%
  group_by(time)%>%
  summarise(all=sum(sum_lur))%>%
  ungroup()

#after calculating all variables, proceed to multiple joins to have all data in the same place:
#calculate COUNT: size activity/city, ACROSS: size activity/all cities, SUM_LUR: each city/all activities, ALL: all activities/all cities
lur_data_all<- city_tech_time%>%
  inner_join(alltechs_city_time, by=c("lur","time"))%>%
  inner_join(tech_across_cities, by=c("time", "group_id"))%>%
  inner_join(size_all, by="time")%>%
  ungroup()

#build function: calculate variables and also RTA
lur_data_all_function<- lur_data_all%>%
  mutate(var1= (count/sum_lur), var2=(across/all))%>%
  mutate(difference=var1-var2)%>%
  mutate(RTA=var1/var2)

#-[OLD METHOD] create all combinations betwen codes, calculate relatedness and put all data together-----
#create four time periods

lur_data_all_function$across<- as.numeric(lur_data_all_function$across)

lur_data_all_function<- lur_data_all_function%>%
  mutate(group_id2=group_id)#this is necessary to build the matrix

time1<- lur_data_all_function%>%
  filter(time==1)%>%
  ungroup()%>%
  select(1,3,6,10,11, 12)

time2<- lur_data_all_function%>%
  filter(time==2)%>%
  ungroup()%>%
  select(1,3,6,10,11, 12)

time3<- lur_data_all_function%>%
  filter(time==3)%>%
  ungroup()%>%
  select(1,3,6,10,11, 12)

time4<- lur_data_all_function%>%
  filter(time==4)%>%
  ungroup()%>%
  select(1,3,6,10,11, 12)

# FIRST STEP
step1x1<- time1%>% 
  group_by(lur)%>%
  expand(group_id, group_id2)%>%
  ungroup()%>%
  filter(group_id != group_id2) #N.B.: expand is a tidyr function (load it)

step1x2<- time2%>% 
  group_by(lur)%>%
  expand(group_id, group_id2)%>%
  ungroup()%>%
  filter(group_id != group_id2)

step1x3<- time3%>% 
  group_by(lur)%>%
  expand(group_id, group_id2)%>%
  ungroup()%>%
  filter(group_id != group_id2)

step1x4<- time4%>% 
  group_by(lur)%>%
  expand(group_id, group_id2)%>%
  ungroup()%>%
  filter(group_id != group_id2)

# SECOND STEP: CALCULATE DUPLICATE COMBINATIONS. a bit long computing time (15 min to ~ 45 min/1 hour for the last call)
gc()

step2x1<- step1x1[!duplicated(t(apply(step1x1[c("lur", "group_id", "group_id2")], 1, sort))), ]
step2x2<- step1x2[!duplicated(t(apply(step1x2[c("lur", "group_id", "group_id2")], 1, sort))), ]
step2x3<- step1x3[!duplicated(t(apply(step1x3[c("lur", "group_id", "group_id2")], 1, sort))), ]

system.time(
  step2x4<- step1x4[!duplicated(t(apply(step1x4[c("lur", "group_id", "group_id2")], 1, sort))), ]
)

#THIRD STEP: JOIN LIST OF COMBINATIONS TO THEIR RESPECTIVE VARIABLES

#take away group_id2 from time
time1<- time1%>%
  select(-6)
time2<- time2%>%
  select(-6)
time3<- time3%>%
  select(-6)
time4<- time4%>%
  select(-6)

period1<- step2x1%>%
  inner_join(time1, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time1, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

period2<- step2x2%>%
  inner_join(time2, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time2, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

period3<- step2x3%>%
  inner_join(time3, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time3, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

period4<- step2x4%>%
  inner_join(time4, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time4, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

#calculate result1 as diff1*diff2, and result2 and 3 as the square of the difference (as they are multiplied by themselves)
period1<- period1%>%
  mutate(result1=diff1*diff2, result2= diff1 ^2, result3= diff2^2)

#THIS IS THE ACTUAL NEW FORMULA
period1_scores<- period1%>%
  group_by(group_id, group_id2)%>%
  summarise(score_old=sum(result), score1=sum(result1), score2= sum(result2), score3= sum(result3))%>%
  ungroup()%>%
  mutate(score_new=score1/(sqrt(score2*score3)), time=1)

#plot the distribution of spatial relatedness for period 1
ggplot(period1_scores, aes(x=score_new))+geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = median(score_new)),linetype = "dashed", size = 0.6) 

EV_rel1<- period1_scores%>%
  filter(group_id=='B60L' | group_id2=='B60L')%>%
  mutate(time=1)
BA_rel1<- period1_scores%>%
  filter(group_id=='H01M' | group_id2=='H01M')%>%
  mutate(time=1)
SG_rel1<- period1_scores%>%
  filter(group_id=='Y04S' | group_id2=='Y04S')%>%
  mutate(time=1)
ICE_rel1<- period1_scores%>%
  filter(group_id=='F02B' | group_id2=='F02B')%>%
  mutate(time=1)

#PERIOD2

period2<- period2%>%
  mutate(result1=diff1*diff2, result2= diff1 ^2, result3= diff2^2)

period2_scores<- period2%>%
  group_by(group_id, group_id2)%>%
  summarise(score_old=sum(result), score1=sum(result1), score2= sum(result2), score3= sum(result3))%>%
  ungroup()%>%
  mutate(score_new=score1/(sqrt(score2*score3)), time=2)

#plot the distribution of spatial relatedness for period 2
ggplot(period2_scores, aes(x=score_new))+geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = median(score_new)),linetype = "dashed", size = 0.6) #ok for the plot

#extract relatedness for each technology
EV_rel2<- period2_scores%>%
  filter(group_id=='B60L' | group_id2=='B60L')%>%
  mutate(time=2)
BA_rel2<- period2_scores%>%
  filter(group_id=='H01M' | group_id2=='H01M')%>%
  mutate(time=2)
SG_rel2<- period2_scores%>%
  filter(group_id=='Y04S' | group_id2=='Y04S')%>%
  mutate(time=2)
ICE_rel2<- period1_scores%>%
  filter(group_id=='F02B' | group_id2=='F02B')%>%
  mutate(time=2)

#PERIOD3

period3<- period3%>%
  mutate(result1=diff1*diff2, result2= diff1 ^2, result3= diff2^2)

period3_scores<- period3%>%
  group_by(group_id, group_id2)%>%
  summarise(score_old=sum(result), score1=sum(result1), score2= sum(result2), score3= sum(result3))%>%
  ungroup()%>%
  mutate(score_new=score1/(sqrt(score2*score3)), time=3)

#plot the distribution of spatial relatedness for period 3
ggplot(period3_scores, aes(x=score_new))+geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = median(score_new)),linetype = "dashed", size = 0.6) #ok for the plot

#extract relatedness for each technology
EV_rel3<- period3_scores%>%
  filter(group_id=='B60L' | group_id2=='B60L')%>%
  mutate(time=3)
BA_rel3<- period3_scores%>%
  filter(group_id=='H01M' | group_id2=='H01M')%>%
  mutate(time=3)
SG_rel3<- period3_scores%>%
  filter(group_id=='Y04S' | group_id2=='Y04S')%>%
  mutate(time=3)
ICE_rel3<- period3_scores%>%
  filter(group_id=='F02B' | group_id2=='F02B')%>%
  mutate(time=3)

#PERIOD4

period4<- period4%>%
  mutate(result1=diff1*diff2, result2= diff1 ^2, result3= diff2^2)

period4_scores<- period4%>%
  group_by(group_id, group_id2)%>%
  summarise(score_old=sum(result), score1=sum(result1), score2= sum(result2), score3= sum(result3))%>%
  ungroup()%>%
  mutate(score_new=score1/(sqrt(score2*score3)), time=4)

#plot the distribution of spatial relatedness for period 3
ggplot(period4_scores, aes(x=score_new))+geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = median(score_new)),linetype = "dashed", size = 0.6) #ok for the plot

#extract relatedness for each technology
EV_rel4<- period4_scores%>%
  filter(group_id=='B60L' | group_id2=='B60L')%>%
  mutate(time=4)
BA_rel4<- period4_scores%>%
  filter(group_id=='H01M' | group_id2=='H01M')%>%
  mutate(time=4)
SG_rel4<- period4_scores%>%
  filter(group_id=='Y04S' | group_id2=='Y04S')%>%
  mutate(time=4)
ICE_rel4<- period3_scores%>%
  filter(group_id=='F02B' | group_id2=='F02B')%>%
  mutate(time=4)

scores_all_periods<- rbind (period1_scores, period2_scores, period3_scores, period4_scores)
EV_all_periods<- rbind(EV_rel1, EV_rel2, EV_rel3, EV_rel4)
BA_all_periods<- rbind(BA_rel1, BA_rel2, BA_rel3, BA_rel4)
SG_all_periods<- rbind(SG_rel1, SG_rel2, SG_rel3, SG_rel4)
ICE_all_periods<- rbind(ICE_rel1, ICE_rel2, ICE_rel3, ICE_rel4)


ggplot(scores_all_periods, aes(x=score_new))+geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = median(score_new)),linetype = "dashed", size = 0.6) + facet_wrap(vars(time))

#write.table(scores_all_periods, "/Users/aferloni/Documents/Thesis-second paper/2_methods/OECD/nuts/nuts_scores.txt", sep = "|", row.names = F )
