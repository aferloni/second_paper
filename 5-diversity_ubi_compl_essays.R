#load: dplyr, EconGeo

#import data: 
all_function<- read.csv("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/lur_data_function.csv", header = T, stringsAsFactors = F)
nodes<- read.csv("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/nodes.csv", header = T, stringsAsFactors = F)

#calculate diversity

#prepare data----
#distinguish periods
period1<- all_function%>%
  filter(time==1)%>%
  distinct(lur, group_id, count)%>%
  filter(group_id %in% nodes$id)%>%
  arrange(lur)
period2<- all_function%>%
  filter(time==2)%>%
  distinct(lur, group_id, count)%>%
  filter(group_id %in% nodes$id)%>%
  arrange(lur)
period3<- all_function%>%
  filter(time==3)%>%
  distinct(lur, group_id, count)%>%
  filter(group_id %in% nodes$id)%>%
  arrange(lur)
period4<- all_function%>%
  filter(time==4)%>%
  distinct(lur, group_id, count)%>%
  filter(group_id %in% nodes$id)%>%
  arrange(lur)

#create a city industry matrix

period1_mat<- get.matrix(period1)
period2_mat<- get.matrix(period2)
period3_mat<- get.matrix(period3)
period4_mat<- get.matrix(period4)

#calculate diversity, ubiquity, kci----
#diversity

div1<- as.data.frame(EconGeo::diversity(period1_mat, RCA=TRUE))%>%
  rownames_to_column("lur")%>%
  rename(diversity=2)%>%
  mutate(time=1)
div2<- as.data.frame(EconGeo::diversity(period2_mat, RCA=TRUE))%>%
  rownames_to_column("lur")%>%
  rename(diversity=2)%>%
  mutate(time=2)
div3<- as.data.frame(EconGeo::diversity(period3_mat, RCA=TRUE))%>%
  rownames_to_column("lur")%>%
  rename(diversity=2)%>%
  mutate(time=3)
div4<- as.data.frame(EconGeo::diversity(period4_mat, RCA=TRUE))%>%
  rownames_to_column("lur")%>%
  rename(diversity=2)%>%
  mutate(time=4)
div_all<- rbind(div1, div2, div3, div4)

#calculate ubiquity
ubi1<- as.data.frame(ubiquity(period1_mat, RCA=TRUE))%>%
  rownames_to_column("code")%>%
  rename(ubiquity=2)%>%
  mutate(time=1)
ubi2<- as.data.frame(ubiquity(period2_mat, RCA=TRUE))%>%
  rownames_to_column("code")%>%
  rename(ubiquity=2)%>%
  mutate(time=2)
ubi3<- as.data.frame(ubiquity(period3_mat, RCA=TRUE))%>%
  rownames_to_column("code")%>%
  rename(ubiquity=2)%>%
  mutate(time=3)
ubi4<- as.data.frame(ubiquity(period4_mat, RCA=TRUE))%>%
  rownames_to_column("code")%>%
  rename(ubiquity=2)%>%
  mutate(time=4)
ubi_all<- rbind(ubi1, ubi2, ubi3, ubi4)

#KCI

kci<- as.data.frame(KCI(period1_mat, RCA=TRUE))
rownames(kci)<- rownames(period1_mat)
kci<- kci%>%
  rownames_to_column("lur")%>%
  rename(kci=2)%>%
  mutate(time=1)

kci2<- as.data.frame(KCI(period2_mat, RCA=TRUE))
rownames(kci2)<- rownames(period2_mat)
kci2<- kci2%>%
  rownames_to_column("lur")%>%
  rename(kci=2)%>%
  mutate(time=2)

kci3<- as.data.frame(KCI(period3_mat, RCA=TRUE))
rownames(kci3)<- rownames(period3_mat)
kci3<- kci3%>%
  rownames_to_column("lur")%>%
  rename(kci=2)%>%
  mutate(time=3)

kci4<- as.data.frame(KCI(period4_mat, RCA=TRUE))
rownames(kci4)<- rownames(period4_mat)
kci4<- kci4%>%
  rownames_to_column("lur")%>%
  rename(kci=2)%>%
  mutate(time=4)

kciall<- rbind(kci, kci2, kci3, kci4)