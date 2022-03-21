#load files and prepare for analysis

LUR_patents<- fread("/Users/aferloni/Documents/Thesis-first paper/General patent data/A-main files/lur_dist_time.csv", header= TRUE, sep= ',', stringsAsFactors = FALSE)

CPC_patents<- fread("/Users/aferloni/Documents/Thesis-second paper/2_tables/cpc_classes/cpc_topatent_simple.csv", header= TRUE, sep= ',', stringsAsFactors = FALSE)
CPC_patents$patent_id<- as.character(CPC_patents$patent_id)
CPC_patents<- CPC_patents%>%
  select(1, 2)

LUR_CPC<- LUR_patents%>%
  inner_join(CPC_patents, by=c("number"="patent_id"))

LUR_CPC_dist<- LUR_CPC%>%
  distinct(number, country, lur, date, group_id)

#create time periods
LUR_CPC_dist<- LUR_CPC_dist%>%
  mutate(time=case_when(
    date < 1980 ~0,
    date >= 1980 & date < 1990 ~1,
    date >= 1990 & date < 2000 ~2,
    date >= 2000 & date < 2010 ~3,
    date >= 2010 & date < 2021 ~4
  ))

#create variables to calculate spatial relatedness
city_tech_time<-LUR_CPC_dist%>%
  group_by(lur, time, group_id)%>%
  summarise(count=n())%>%
  arrange(desc(count))

alltechs_city_time<- city_tech_time%>%
  group_by(lur, time)%>%
  summarise(sum_lur=sum(count))%>%
  arrange(time, desc(sum_lur))

tech_across_cities<- city_tech_time%>%
  group_by(time, group_id)%>%
  summarise(across=sum(count))%>%
  arrange(time, desc(across))

size_all<- alltechs_city_time%>%
  group_by(time)%>%
  summarise(all=sum(sum_lur))

#after calculating all variables, proceed to multiple joins to have all data in the same place
lur_data_all<- city_tech_time%>%
  inner_join(alltechs_city_time, by=c("lur","time"))%>%
  inner_join(tech_across_cities, by=c("time", "group_id"))%>%
  inner_join(size_all, by="time")

#build function
lur_data_all_function<- lur_data_all%>%
  mutate(var1= (count/sum_lur), var2=(across/all))%>%
  mutate(difference=var1-var2)%>%
  mutate(RTA=var1/var2)

-------#start calculation of relatedness
#drop these collections of former USPC codes
lur_data_all_function<- lur_data_all_function%>%
  filter(!group_id %in% c("Y10S", "Y10T"))

#now if you want filter also by RTA>1
#lur_data_all_function<- lur_data_all_function%>%
# filter(RTA>1)

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

#start first steps for all time periods. Then, launch final step for all periods to let it working through the night (long computing time: >30 min < 2hours for each)

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

# SECOND STEP: CALCULATE DUPLICATE COMBINATIONS. LONG COMPUTING TIME (30 min to 1 hour each call)
gc()

step2x1<- step1x1[!duplicated(t(apply(step1x1[c("lur", "group_id", "group_id2")], 1, sort))), ]
step2x2<- step1x2[!duplicated(t(apply(step1x2[c("lur", "group_id", "group_id2")], 1, sort))), ]
step2x3<- step1x3[!duplicated(t(apply(step1x3[c("lur", "group_id", "group_id2")], 1, sort))), ]

gc(full=TRUE)
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

step3x1<- step2x1%>%
  inner_join(time1, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time1, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

step3x2<- step2x2%>%
  inner_join(time2, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time2, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

step3x3<- step2x3%>%
  inner_join(time3, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time3, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

step3x4<- step2x4%>%
  inner_join(time4, by=c("group_id", "lur"))%>%
  rename(diff1=difference, across1=across)%>%
  inner_join(time4, by=c("lur"="lur", "group_id2"="group_id"))%>%
  rename(diff2=difference, across2=across)%>%
  mutate(product=diff1*diff2)%>%
  mutate(inverse=1/(across1*across2), result= inverse*product)

#WRITE FILES FOR ALL RESULTS

#write.csv(step3x1, file="/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period1.csv", quote =FALSE, row.names = FALSE)
#write.csv(step3x2, file="/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period2.csv", quote =FALSE, row.names = FALSE)
#write.csv(step3x3, file="/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period3.csv", quote =FALSE, row.names = FALSE)
#write.csv(step3x4, file="/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period4.csv", quote =FALSE, row.names = FALSE)

#-----------

#now provided that these calculations are correct, let's now apply the different formula. 

#import data for the four periods

period1<- fread("/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period1.csv", header= TRUE, sep= ',', stringsAsFactors = FALSE)
period2<- fread("/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period2.csv", header= TRUE, sep= ',', stringsAsFactors = FALSE)
period3<- fread("/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period3.csv", header= TRUE, sep= ',', stringsAsFactors = FALSE)
period4<- fread("/Users/aferloni/Documents/Thesis-second paper/2_methods/Geographic/period4.csv", header= TRUE, sep= ',', stringsAsFactors = FALSE)

#calculate result1 as diff1*diff2, and result2 and 3 as the square of the difference (as they are multiplied by themselves)
period1<- period1%>%
  mutate(result1=diff1*diff2, result2= diff1 ^2, result3= diff2^2)

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

#period2

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


#there doesn't appear to be much relatedness between EV, BA and SG. What's more, for some couples, relatedness drops
#before slightly recovering, but always at lower levels than period 1
#this might be due to the fact that many LURs are included even if the only produce 2/3 patents for every code. 

#Therefore, next thing to try: remove LUR in which patenting for some code is insignificant (at least those <10)

lur_data_all_function%>%
  filter(time==3 & count>15)%>%
ggplot(aes(x=count))+geom_boxplot()

#other thing to try could be to calculate different measures of relatedness, for example using the EconGeo package


