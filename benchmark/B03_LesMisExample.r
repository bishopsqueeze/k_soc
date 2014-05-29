##------------------------------------------------------------------
## The purpose of this script is to re-create an example set of
## clusters.  The example (the Les Miserables characters) set of
## edges is taken from Ahn, and there is an R package that will
## reproduce that results (linkcomm()).  I want to reproduce it
## as well because I will be modifying the basic result (i.e.,
## a similarity matrix to reproduce the Ye [?] et al. paper.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)                 ## contains graph functions
library(linkcomm)               ## contains link community functions
library(dendextend)             ## contains dendrogram functions
library(caTools)
librart(gdata)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Load utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/social_circle/k_soc/00_Utilities.r")


##------------------------------------------------------------------
## Benchmark: Les Miserables characters connected graph
##------------------------------------------------------------------

## load the raw dataset (from linkcomm())
lm <- lesmiserables

##------------------------------------------------------------------
## Step 1:  This is somewhat redundant, but translate the Les Mis
## factor/edge list into the egonet structure that I've loaded
## from the kaggle competition.
##------------------------------------------------------------------

## convert the data of factors into characters
tmp.lm <- convert.magic(lm, c("V1","V2"), c("character","character"))

## map character names to integers
nodes_l.uniq    <- unique(tmp.lm$V1)
nodes_r.uniq    <- unique(tmp.lm$V2)
characters.uniq <- union(nodes_l.uniq, nodes_r.uniq)
characters.ids  <- 1:length(characters.uniq)

## define the egonet
lm.egonet  <- list()

## load the egonet
for (i in 1:length(nodes_l.uniq)) {
    
    ## get a vertex & assign an id
    tmp.node    <- nodes_l.uniq[i]
    tmp.id      <- characters.ids[ which(characters.uniq == tmp.node)]
    char.id     <- paste("ID_",tmp.id,sep="")
    
    ## get the list of characters (and their ids) associated with this vertex
    lm.egonet[[char.id]] <- paste("ID_", characters.ids[ which(characters.uniq %in% tmp.lm$V2[which(tmp.lm$V1 == tmp.node)]) ], sep="")
}

##------------------------------------------------------------------
## Step 2:  At this point, the Les Mis data are in a format that
## is similar to the loaded kaggle data format. Load the raw data
## into an igraph object
##------------------------------------------------------------------
lm.igraph   <- convEgonetListToIgraphObject(lm.egonet)


##------------------------------------------------------------------
## Step 3:  Compute the dissimilarity measure amongst the clusters.
## The similarity measure is the Jaccard coefficient. The Jaccard
## *distance* is 1 minus the Jaccard coefficient.
##------------------------------------------------------------------

lm.sim      <- calcSimilarityMatrix(lm.igraph)  ## similarity matrix
lm.dis      <- 1 - lm.sim                       ## jaccard distance

##------------------------------------------------------------------
## Step 3:  Compute the cluster
##------------------------------------------------------------------

## create a distance object from the dissimilarity matrix
lm.dist     <- as.dist(lm.dis)

## compute a cluster
lm.clust    <- hclust(lm.dist, method="single")
lm.dend     <- as.dendrogram(lm.clust)


##------------------------------------------------------------------
## Step 4:  Verify cluster output using linkcomm() [ok]
##------------------------------------------------------------------

## use linkcomm() to compute the cluster
lm.lc       <- getLinkCommunities(get.data.frame(lm.igraph,"edges"), hcmethod="single")
comp.clust  <- cbind(lm.lc$hclust$height, lm.clust$height, lm.lc$hclust$height-lm.clust$height)


##------------------------------------------------------------------
## Step 4:  Verify cluster output using linkcomm() [ok]
##------------------------------------------------------------------

hh <- unique(round(lm.clust$height, digits = 5)) # Round to 5 digits to prevent numerical instability affecting community formation.

## just counts the number of edges at each height
## number of heights = 50
## sum of countClusters = 253 = distance lenght
countClusters <- function(x, ht)
{
    return(length(which(ht==x)))
}
clusnums <- sapply(hh, countClusters, ht = round(lm.clust$height, digits = 5)) # Number of clusters at each height.

## want to find the "optimal" height using the cluster density



#for (i in 1:length(unique(lm.clust$height))) {


    tmp.height  <- unique(lm.clust$height)[20]
    tmp.clus    <-  cutree(lm.clust, h=tmp.height)
    
#}



## define a matrix to hold cluster membership at each height
hgts        <- unique(lm.clust$height)
hgts.num    <- length(hgts)
edges.num   <- nrow(get.edgelist(lm.igraph))

## define & load the matrix
tmp.mat <- matrix(0, nrow=edges.num, ncol=hgts.num)
for (i in 1:hgts.num) {
    tmp.mat[,i] <- as.vector(cutree(lm.clust, h=hgts[i]))
}

## get the number of groups at each height
groups.vec <- vector(, length=hgts.num)
for (i in 1:hgts.num) {
    groups.vec[i] <- length(unique(tmp.mat[,i]))
}

## loop over each height and then the groups

for (i in 1:hgts.num) {
    
    groups.uniq <- unique(tmp.mat[,i])
    groups.num  <- length(groups.uniq)

    for (j in 1:groups.num) {
        
        tmp.idx     <- which(tmp.mat[,i] == groups.num[j])
        tmp.memb    <- union(get.edgelist(lm.igraph)[tmp.idx,1], get.edgelist(lm.igraph)[tmp.idx,2])
    }
    
    ## NEXT STEP IS TO REVIEW THE DENSITY FORMULA WITH A CLEAR HEAD
}





