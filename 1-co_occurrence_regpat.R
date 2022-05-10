
#required packages: dplyr, matrix, igraph

#let's start by loading CPC codes + all NUTS + NO NUTS patents (you may want to unzip)

CPC<- fread("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/202202_CPC_Classes.7z", header= TRUE, sep= '|', stringsAsFactors = FALSE)
CPC<- fread("/Users/aferloni/Documents/Patent raw data/OECD/2022/Regpat/202202_CPC_Classes.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)

nuts<- read.table("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/nuts_patents.txt.zip", header= TRUE, sep= '|', stringsAsFactors = FALSE)
no_nuts<- read.table("/Users/aferloni/switchdrive/Andrea-Celine/These Andrea/Paper 2 - Urban relatedness/DATA/no_nuts.txt.zip", header= TRUE, sep= '|', stringsAsFactors = FALSE)

nuts<- fread("/Users/aferloni/Documents/Geographic data/nuts_patents.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)
no_nuts<- fread("/Users/aferloni/Documents/Geographic data/NO-NUTS/no_nuts.txt", header= TRUE, sep= '|', stringsAsFactors = FALSE)

all_patents<- rbind(nuts, no_nuts)

CPC$CPC_Class<- substr(CPC$CPC_Class, 1, 4)

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
  filter((!group_id %in% c("Y10S", "Y10T")))%>%
  distinct(appln_id, group_id)

patents_2<- patents_2%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))%>%
  distinct(appln_id, group_id)

patents_3<- patents_3%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))%>%
  distinct(appln_id, group_id)

patents_4<- patents_4%>%
  inner_join(CPC, by="appln_id")%>%
  rename(group_id=CPC_Class)%>%
  filter((!group_id %in% c("Y10S", "Y10T")))%>%
  distinct(appln_id, group_id)


#this is the starting point, let's now continue by building a matrix
##-PERIOD 1

Co_occurrence_1<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_1, sparse=T))

###calculate cosine similarity

Mrow<- diag(Co_occurrence_1)

M1<- Mrow %*% t(Mrow)
M2<- Co_occurrence_1/sqrt(M1)

Co_occurrence_1_mat<- as.matrix(M2) #first transform to regular matrix
Co_occurrence_1_DF<- as.data.frame(Co_occurrence_1_mat)# then, back to DF

prox_Jaccard <- as.matrix(Co_occurrence_1 / (matrix(rep(Mrow, length(Mrow)), length(Mrow), length(Mrow)) +
                   matrix(rep(Mrow, length(Mrow)), length(Mrow), length(Mrow), byrow = TRUE) -
                   Co_occurrence_1))

plot(prox_Jaccard, Co_occurrence_1_mat)
plot(jaccard, Co_occurrence_1_mat)

jaccard<- as.matrix(1-vegdist(Co_occurrence_1, method = "jaccard"))#alterative method from package vegdist


##-PERIOD 2

Co_occurrence_2<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_2, sparse=TRUE))

###calculate cosine similarity

Mrow<- diag(Co_occurrence_2)

M1<- Mrow %*% t(Mrow)
M2<- Co_occurrence_2/sqrt(M1)

Co_occurrence_2_mat<- as.matrix(M2) #first transform to regular matrix
Co_occurrence_2_DF<- as.data.frame(Co_occurrence_2_mat)# then, back to DF

##-PERIOD 3

Co_occurrence_3<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_3, sparse=TRUE))

###calculate cosine similarity

Mrow<- diag(Co_occurrence_3)

M1<- Mrow %*% t(Mrow)
M2<- Co_occurrence_3/sqrt(M1)

Co_occurrence_3_mat<- as.matrix(M2) #first transform to regular matrix
Co_occurrence_3_DF<- as.data.frame(Co_occurrence_3_mat)# then, back to DF

##-PERIOD 4

Co_occurrence_4<- Matrix::crossprod(xtabs(~ appln_id + group_id, data=patents_4, sparse=TRUE))

###calculate cosine similarity

Mrow<- diag(Co_occurrence_4)

M1<- Mrow %*% t(Mrow)
M2<- Co_occurrence_4/sqrt(M1)

Co_occurrence_4_mat<- as.matrix(M2) #first transform to regular matrix
Co_occurrence_4_DF<- as.data.frame(Co_occurrence_4_mat)# then, back to DF

#check which codes are not in previous periods. Also obtain node list
names_4<- as.data.frame(row.names(Co_occurrence_4_DF))%>%
  rename(code=1)
names_3<- as.data.frame(row.names(Co_occurrence_3_DF))%>%
  rename(code=1)
names_2<- as.data.frame(row.names(Co_occurrence_2_DF))%>%
  rename(code=1)
names_1<- as.data.frame(row.names(Co_occurrence_1_DF))%>%
  rename(code=1)

setdiff(names_4, names_3)# G99Z codes in 4 that are not in 3
setdiff(names_3, names_4)# "B41B" "F21H" "G21Y" codes in 3 that are not in 4
setdiff(names_3, names_2)# "F24J" "G06D" "G16Y" "G21J" "G21Y" "H04T" codes in 3 that are not in 2
setdiff(names_2, names_3)# 0 all codes in 2 are in 3
setdiff(names_2, names_1)# 0 all codes in 2 are in 1
setdiff(names_1, names_2)# "G21J" code in 1 that is not in 2


#---now let's transform each period into edgelist to generate the network
##en passant, remove 1 values (duplicated links) because network is undirected (AB=BA). I thought the weights = 0 had to be kept but in reality there's none..

#network method: does not retain 0s
#g1<- graph.adjacency(Co_occurrence_1_mat, mode="upper", weighted = T, diag=F) #not useful anymore because it doesn't extract the links with weight 0.
#period1_edgelist<- get.data.frame(g1, what = "edges")

#mysterious method: it works but don't know why
period1_edgelist<- data.frame(source= rownames(Co_occurrence_1_DF)[col(Co_occurrence_1_DF)], target=colnames(Co_occurrence_1_DF)[row(Co_occurrence_1_DF)], weight= c(t(Co_occurrence_1_DF)))
period1_edgelist<- period1_edgelist%>%
  filter(weight != 1)

#melt method: safe and reliable
period1_edgelist<- as.data.table(Co_occurrence_1_DF, keep.rownames="source")
period1_edgelist<- melt.data.table(period1_edgelist, id.vars = "source", variable.factor = F, variable.name = "target", value.name = "weight")

period1_edgelist<- as.data.frame(period1_edgelist)%>%
  filter(weight!=1)
links_1<- period1_edgelist[!duplicated(t(apply(period1_edgelist[c("source", "target")], 1, sort))), ] #this works, we get all links including those that are 0. 

#2
period2_edgelist<- as.data.table(Co_occurrence_2_DF, keep.rownames="source")
period2_edgelist<- melt.data.table(period2_edgelist, id.vars = "source", variable.factor = F, variable.name = "target", value.name = "weight")

period2_edgelist<- as.data.frame(period2_edgelist)%>%
  filter(weight!=1)

links_2<- period2_edgelist[!duplicated(t(apply(period2_edgelist[c("source", "target")], 1, sort))), ]

#3

period3_edgelist<- as.data.table(Co_occurrence_3_DF, keep.rownames="source")
period3_edgelist<- melt.data.table(period3_edgelist, id.vars = "source", variable.factor = F, variable.name = "target", value.name = "weight")

period3_edgelist<- as.data.frame(period3_edgelist)%>%
  filter(weight!=1)

links_3<- period3_edgelist[!duplicated(t(apply(period3_edgelist[c("source", "target")], 1, sort))), ]

#4
period4_edgelist<- as.data.table(Co_occurrence_4_DF, keep.rownames="source")
period4_edgelist<- melt.data.table(period4_edgelist, id.vars = "source", variable.factor = F, variable.name = "target", value.name = "weight")

period4_edgelist<- as.data.frame(period4_edgelist)%>%
  filter(weight!=1)

links_4<- period4_edgelist[!duplicated(t(apply(period4_edgelist[c("source", "target")], 1, sort))), ]

# write file to export to network
write.csv(links_4, file= "/Users/aferloni/Documents/Thesis-second paper/2_methods/co_occurrence/OECD-regpat/period4_links.csv", quote =FALSE, row.names = FALSE)
write.csv(names_4, file= "/Users/aferloni/Documents/Thesis-second paper/2_methods/co_occurrence/OECD-regpat/period4_nodes.csv", quote =FALSE, row.names = FALSE)


##TOY------------------------------

toy<- data.frame(patent_id=c("A", "B", "B", "B", "C", "C", "C", "D"), code=c(1, 1, 2, 3, 1, 3, 4, 3))
toy_1<- Matrix::crossprod(xtabs(~ patent_id + code, data=toy, sparse=F))

Mrow_toy_1<- diag(toy_1)

M1_toy<- Mrow_toy_1 %*% t(Mrow_toy_1)
M2<- toy_1/sqrt(M1_toy)

Co_occurrence_toy<- as.matrix(M2) #first transform to regular matrix
Co_occurrence_toy_DF<- as.data.frame(Co_occurrence_toy)# then, back to DF

period1_toy<- data.frame(source= rownames(Co_occurrence_toy_DF)[col(Co_occurrence_toy_DF)], target=colnames(Co_occurrence_toy_DF)[row(Co_occurrence_toy_DF)], weight= c(t(Co_occurrence_toy_DF)))
links_toy<- period1_toy[!duplicated(t(apply(period1_toy[c("source", "target")], 1, sort))), ] #this works, we get all links including those that are 0. 

#this method works fine, whereas if we use the network it will likely drop the 0 case

gtoy<- graph.adjacency(Co_occurrence_toy, weighted = T)
toy_edgelist<- get.data.frame(gtoy, what = "edges")
links_toy2<- toy_edgelist[!duplicated(t(apply(toy_edgelist[c("from", "to")], 1, sort))), ] #indeed, we have one less link with this method

#now the question is why does this method fail when applied to an incidence matrix?
#let's construct one. 

#load igraph
#method pass trhough network
toy<- matrix(c(1, 3, 5, 3, 7, 8, 0, 1, 5, 3, 8, 5, 0, 1, 0, 0), nrow = 4, ncol = 4, byrow = T, dimnames = list(c("city1", "city2", "city3", "city4"), c("code1", "code2", "code3", "code4")))
g1toy<- graph_from_incidence_matrix(toy, directed = F, weighted = T)
period1_toy<- get.data.frame(g1toy, what = "edges") #as you can see, 0 are dropped, in the previous step

#method direct from matrix
toy<- as.data.frame(toy)
period1_toy<- data.frame(source= rownames(toy)[col(toy)], target=colnames(toy)[row(toy)], weight= c(t(toy)))


