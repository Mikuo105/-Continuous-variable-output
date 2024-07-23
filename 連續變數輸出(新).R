dir.create("C:\\Users\\kuo\\Desktop\\r\\degree_cluster_constraint")
for(i in 1:47) {
  dir.create(paste0("C:\\Users\\kuo\\Desktop\\r\\degree_cluster_constraint\\",as.character(x[i])))
}


for(i in 1:47) {
  y<-subset(Agere,Agere[,9]>=(1970+i-1) & Agere[,9]<=(1972+i-1))
  z_a<-y[c(1,4)]
  z_b<-merge(z_a,z_a,by="Document.Number")
  z_c<-z_b[c(2,3)]
  write.csv(z_c, file = paste0("C:\\Users\\kuo\\Desktop\\r\\data_firm_year_Oct2020\\Agere\\temp (",i,").csv"),row.names=F)
  cat("dl n=10000,","format=edgelist1\n","labels embedded\n","data:\n",file =paste0("C:/Users/kuo/Desktop/r/data_firm_year_Oct2020/Agere/Agere (",i,").txt"),append=T)
  write.table(z_c,file = paste0("C:\\Users\\kuo\\Desktop\\r\\data_firm_year_Oct2020\\Agere\\Agere (",i,").txt"),row.names=F,col.names=F,quote=F,append=T )
}

#跑计块
library(igraph)
path <- "C:/Users/kuo/Desktop/r/degree_cluster_constraint/Agere/"
files <- list.files(path = path, pattern = "Agere*")
df1 <- data.frame()
for(file in files) {
  df1 <- read.csv(paste(path, file, sep=""),header=TRUE,row.names=1)
  m=as.matrix(df1)
  net=graph.adjacency(m,mode="undirect",weighted=TRUE,diag=FALSE) 
  summary(net)
  E(net)$weight
  V(net)$name
  #-------------------------------------------------------------------------------
  #import the sample_attributes file:
  #a=read.csv("C:/Users/kuo/Desktop/r/output_y_int_CNST.csv")
  #V(net)$CNST=as.character(a$CNST[match(V(net)$name,a$Inventor_Parse3)])
  #-------------------------------------------------------------------------------
  y_degree<-degree(net,normalized = FALSE)
  y_degree_nor<-degree(net,normalized = TRUE)
  y_contraint<-constraint(net)
  y_transitivity_global<-transitivity(net, type="global",isolates = "nan")  # net is treated as an undirected network
  y_transitivity_local<-transitivity(net, type="local",isolates = "nan")
  CompareDegree <- cbind(y_degree,y_degree_nor,y_transitivity_global,y_transitivity_local,y_contraint,V(net)$CNST,1970+as.integer(substring(file,as.integer(nchar("Agere"))+3,as.integer(nchar("Agere"))+4))-1,1972+as.integer(substring(file,as.integer(nchar("Agere"))+3,as.integer(nchar("Agere"))+4))-1)
  head(CompareDegree)
  write.table(CompareDegree, file =("C:\\Users\\kuo\\Desktop\\r\\degree_cluster_constraint\\Agere_degree_cluster_constraint.CSV"),col.names=NA,append=TRUE,sep=",")
}

#x_closeness<-closeness(net_knowledge_network,mode="out",normalized = FALSE)
#x_closeness_nor<-closeness(net_knowledge_network,normalized = TRUE)
#x_betweenness<-betweenness(net_knowledge_network,normalized = FALSE)
#x_betweenness_nor<-betweenness(net_knowledge_network,normalized = TRUE)
#x_edge_density<-edge_density(net_knowledge_network,loops = FALSE)
#------------------------------------------------------------------------------------------------------------------

#跑计块
library(igraph)
path <- "C:/Users/P65/Desktop/R粂ē絤策/degree_cluster_constraint/Silicon_Labs/"
files <- list.files(path = path, pattern = "Silicon_Labs*")
df1 <- data.frame()
for(file in files) {
  df1 <- read.csv(paste(path, file, sep=""),header=TRUE,row.names=1)
  m=as.matrix(df1)
  net=graph.adjacency(m,mode="undirect",weighted=TRUE,diag=FALSE) 
  summary(net)
  E(net)$weight
  V(net)$name
  #-------------------------------------------------------------------------------
  #import the sample_attributes file:
  a=read.csv("C:/Users/P65/Desktop/R粂ē絤策/output_y_int_CNST.csv")
  V(net)$CNST=as.character(a$CNST[match(V(net)$name,a$Inventor_Parse3)])
  #-------------------------------------------------------------------------------
  y_degree<-degree(net,normalized = FALSE)
  y_degree_nor<-degree(net,normalized = TRUE)
  CompareDegree <- cbind(y_degree,y_degree_nor,y_contraint,V(net)$CNST,1970+as.integer(substring(file,as.integer(nchar("Silicon_Labs"))+3,as.integer(nchar("Silicon_Labs"))+4))-1,1972+as.integer(substring(file,as.integer(nchar("Silicon_Labs"))+3,as.integer(nchar("Silicon_Labs"))+4))-1)
  head(CompareDegree)
  write.table(CompareDegree, file =("C:\\Users\\P65\\Desktop\\R粂ē絤策\\degree_cluster_constraint\\Silicon_Labs_degree_cluster_constraint.CSV"),col.names=NA,append=TRUE,sep=",")
}

