#Delete all variables and plots
rm(list=ls())
if (!is.null(dev.list())) {dev.off()}

library(foreach)
library(doParallel)
library(igraph)
library(microbenchmark)


source("functions.R")

time_taken2<-microbenchmark({
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
#We set seed for freezing results
set.seed(6)
n1<-1000;n2<-1000

#Variables from Gaussian distribution with different parameters
X<-c(rnorm(n1,0),rnorm(n2,1))
#Classes
C<-c(rep(1,n1),rep(2,n2))
#Create a tree
tree<-crt_dtree(X,C)
stopCluster(cl)
},times = 10)
#Create informations for plotting (edges)
hf<-as.character(unlist(tree$HF)) #nodes
nn<-as.numeric(unlist(tree$ValN)[-1]) #values of nodes

edges<-c()
for(i in 1:(length(hf)-1))
{
  bch<-hf[i]
  for (j in (i+1):length(hf))
  {
    ch<-hf[j]
    if ((substr(ch,1,nchar(bch))==bch)&&(nchar(ch)==(nchar(bch)+1)))
    {edges<-c(edges,i,j)}
  }
}

#Create a plot
g <- graph.empty (length(hf), directed = T) #creating empty plot
g<-add.edges(g, edges) #add edges
#Create names
nV<-c()
for(i in 1:(gsize(g)+1))
{
  if(sum(g[i])!=0) nV<-c(nV,paste0(paste0("x<",nn[i])))
  else nV<-c(nV,nn[i])
}
V(g)$name<-nV
#Plotting
#print(X);print(C) #For testing
par(mar = c(0,0,0,0), ps=14,cex=1 )
V(g)$color="white"
plot(g, layout = layout.reingold.tilford(g, root = 1, flip.y = T, circular = F),
     vertex.size=32, vertex.shape="rectangle")

