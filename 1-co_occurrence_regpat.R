
#let's start by loading all NUTS + NO NUTS patents 

nuts<- read.table("/Users/aferloni/Documents/Geographic data/nuts_patents.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)
no_nuts<- read.table("/Users/aferloni/Documents/Geographic data/NO-NUTS/no_nuts.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)

all_patents<- rbind(nuts, no_nuts)

#we should now select the year so that we create four different periods

all_patents<- all_patents%>%
  mutate(time=case_when(
    year < 1980 ~0,
    year >= 1980 & year < 1990 ~1,
    year >= 1990 & year < 2000 ~2,
    year >= 2000 & year < 2010 ~3,
    year >= 2010 & year < 2023 ~4
  ))

# assign patents to periods


patents_1<- all_patents%>%
  filter(time==1)%>%
  distinct(appln_id)
patents_2<- all_patents%>%
  filter(time==2)%>%
  distinct(appln_id)
patents_3<- all_patents%>%
  filter(time==3)%>%
  distinct(appln_id)
patents_4<- all_patents%>%
  filter(time==4)%>%
  distinct(appln_id)

patents_1<- patents_1%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))

patents_2<- patents_2%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))

patents_3<- patents_3%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))

patents_4<- patents_4%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))



#this is the starting point, let's now continue by building a matrix
##-PERIOD 1

Co_occurrence_1<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_1, sparse=TRUE))

###normalize matrix so that all values are comprised between 0 and 1

Mcol<- colSums(Co_occurrence_1)
Mrow<- rowSums(Co_occurrence_1)

M1<- Mrow %*% t(Mcol)
M2<- Co_occurrence_1/sqrt(M1)
diag(M2)<- 1

Co_occurrence_1_mat<- as.matrix(M2) #first transform to regular matrix
#Co_occurrence_1_DF<- as.data.frame(Co_occurrence_1_mat)# if necessary, back to DF

##-PERIOD 2

Co_occurrence_2<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_2, sparse=TRUE))

###normalize matrix so that all values are comprised between 0 and 1

Mcol<- colSums(Co_occurrence_2)
Mrow<- rowSums(Co_occurrence_2)

M1<- Mrow %*% t(Mcol)
M2<- Co_occurrence_2/sqrt(M1)
diag(M2)<- 1

Co_occurrence_2_mat<- as.matrix(M2) #first transform to regular matrix
#Co_occurrence_2_DF<- as.data.frame(Co_occurrence_2_mat)# if necessary, back to DF

##-PERIOD 3

Co_occurrence_3<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_3, sparse=TRUE))

###normalize matrix so that all values are comprised between 0 and 1

Mcol<- colSums(Co_occurrence_3)
Mrow<- rowSums(Co_occurrence_3)

M1<- Mrow %*% t(Mcol)
M2<- Co_occurrence_3/sqrt(M1)
diag(M2)<- 1

Co_occurrence_3_mat<- as.matrix(M2) #first transform to regular matrix
#Co_occurrence_3_DF<- as.data.frame(Co_occurrence_3_mat)# if necessary, back to DF

##-PERIOD 4

Co_occurrence_4<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_4, sparse=TRUE))

###normalize matrix so that all values are comprised between 0 and 1

Mcol<- colSums(Co_occurrence_4)
Mrow<- rowSums(Co_occurrence_4)

M1<- Mrow %*% t(Mcol)
M2<- Co_occurrence_4/sqrt(M1)
diag(M2)<- 1

Co_occurrence_4_mat<- as.matrix(M2) #first transform to regular matrix
#Co_occurrence_4_DF<- as.data.frame(Co_occurrence_4_mat)# if necessary, back to DF

#---now let's transform each period into edgelist to generate the network
##en passant, remove 1 values (duplicated links) because network is undirected (AB=BA). I thought the weights = 0 had to be kept but in reality there's none..

g1<- graph.adjacency(Co_occurrence_1_mat, weighted = T)
period1_edgelist<- get.data.frame(g1, what = "edges")

period1_edgelist<- period1_edgelist%>%
  filter(weight != 1)

links_1<- period1_edgelist[!duplicated(t(apply(period1_edgelist[c("from", "to")], 1, sort))), ]

#2
g2<- graph.adjacency(Co_occurrence_2_mat, weighted = T)
period2_edgelist<- get.data.frame(g2, what = "edges")

period2_edgelist<- period2_edgelist%>%
  filter(weight != 1)

links_2<- period2_edgelist[!duplicated(t(apply(period2_edgelist[c("from", "to")], 1, sort))), ]

#3

g3<- graph.adjacency(Co_occurrence_3_mat, weighted = T)
period3_edgelist<- get.data.frame(g3, what = "edges")

period3_edgelist<- period3_edgelist%>%
  filter(weight != 1)

links_3<- period3_edgelist[!duplicated(t(apply(period3_edgelist[c("from", "to")], 1, sort))), ]
#4

g4<- graph.adjacency(Co_occurrence_4_mat, weighted = T)
period4_edgelist<- get.data.frame(g4, what = "edges")

period4_edgelist<- period4_edgelist%>%
  filter(weight != 1) 

links_4<- period4_edgelist[!duplicated(t(apply(period4_edgelist[c("from", "to")], 1, sort))), ]


#extract also nodes and calculate the relative frequency of each code to map node size

nodes_1<- get.data.frame(g1, what = "vertices")
nodes_2<- get.data.frame(g2, what = "vertices")
nodes_3<- get.data.frame(g3, what = "vertices")
nodes_4<- get.data.frame(g4, what = "vertices")

# write file to export to network
write.csv(links_4, file= "/Users/aferloni/Documents/Thesis-second paper/2_methods/co_occurrence/OECD-regpat/period4_links.csv", quote =FALSE, row.names = FALSE)
write.csv(nodes_4, file= "/Users/aferloni/Documents/Thesis-second paper/2_methods/co_occurrence/OECD-regpat/period4_nodes.csv", quote =FALSE, row.names = FALSE)

