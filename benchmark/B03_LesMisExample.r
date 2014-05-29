##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
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
## <function> :: convert.magic
##------------------------------------------------------------------
## A function to perform a type switch on a data.frame
##------------------------------------------------------------------
convert.magic   <- function(obj, col, type) {
    
    ## isolate the columns to convert
    idx <- which(colnames(obj) %in% col)
    
    ## loop over the columns and convert via a swtich()
    for (i in 1:length(idx)) {
        FUN <- switch(type[i], character = as.character, numeric = as.numeric, factor = as.factor, integer = as.integer)
        obj[, idx[i]]   <- FUN(obj[, idx[i]])
    }
    return(obj)
}



##------------------------------------------------------------------
## Example 1: Les Miserables characters
##
## This example is also included in the Ahn paper on link communities
##------------------------------------------------------------------

## load the dataset
lm <- lesmiserables

##------------------------------------------------------------------
## this is somewhat redundant, but translate the factor/edge list
## into a egonet like what is used in the kaggle competition
##------------------------------------------------------------------

## convert the factors into character
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




convEgonetListToEdgeDataFrame   <- function(myEgonet)
{
    nodes.num   <- length(myEgonet)
    edges.df    <- data.frame()
    
    ## clunky
    for (i in 1:nodes.num) {
        node_l      <- gsub("ID_","",names(myEgonet)[i])
        edges       <- gsub("ID_","",myEgonet[[i]])
        edges.df    <- rbind(edges.df, data.frame(E1=rep(node_l, length(edges)), E2=edges))
    }
    
    return(graph.data.frame(edges.df, directed=FALSE))
    
}

a <- convEgonetListToEdgeDataFrame(lm.egonet)


calcSimilarityMatrix <- function(myIgraph)
{
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.num   <- nrow(edges)
    edges.perm  <- combs(1:edges.num,2)
    #sim         <- vector("numeric",length=nrow(edges.perm))
    sim         <- matrix(0,nrow=edges.num, ncol=edges.num)
    
    for (i in 1:edges.num) {
        for (j in 1:edges.num) {
        
            ## get the sample edges
            ei  <- get.edge(myIgraph,id=i)
            ej  <- get.edge(myIgraph,id=j)
 
            ## keystone
            keystone <- -1
 
            if (ei[1] == ej[1]) {
                keystone <- ei[1]
                tmp.i    <- ei[2]
                tmp.j    <- ej[2]
            } else if (ei[1]==ej[2]) {
                keystone <- ei[1]
                tmp.i    <- ei[2]
                tmp.j    <- ej[1]
            } else if (ei[2]==ej[1]) {
                keystone <- ei[2]
                tmp.i    <- ei[1]
                tmp.j    <- ej[2]
            } else if (ei[2]==ej[2]) {
                keystone <- ei[2]
                tmp.i    <- ei[1]
                tmp.j    <- ej[1]
            }
            
            if (!(keystone == -1)) {
                
                np_i     <- c(keystone, tmp.i, neighbors(myIgraph, tmp.i))
                np_j     <- c(keystone, tmp.j, neighbors(myIgraph, tmp.j))
                sim[i,j] <- (length(intersect(np_i, np_j))) / (length(union(np_i, np_j)))## + 0.000001*runif(1)
                
            }
        }
    }
    return(sim)
}


tmp <- graph.data.frame(rbind(  c(1,3),c(1,2),c(2,4),
                                c(2,5),c(2,6),c(2,7),
                                c(2,8),c(2,9),c(2,10),
                                c(3,8),c(3,9),c(3,10),
                                c(3,11),c(3,12)), directed=FALSE)


tmp <- graph.data.frame(rbind(c(1,3),c(1,2),c(2,3)), directed=FALSE)

b <- calcSimilarityMatrix(a)
d <- as.dist(1-b)
m <- hclust(d, method="single")






##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/social_circle/data/inputs")

loadLinkCommunityMatrix <- function(myId, myFriends, myEgonet)
{
    friends.num <- length(myFriends)
    link.mat    <- matrix(0, nrow=friends.num, ncol=friends.num)
    
    vertex.id   <- as.integer(strsplit(myId, "_")[[1]][2])
    
    for (i in 1:friends.num) {
        for (j in 1:friends.num) {
            
            i.id            <- as.integer(strsplit(myFriends[i], "_")[[1]][2])
            j.id            <- as.integer(strsplit(myFriends[j], "_")[[1]][2])
            
            friends_i.ids   <- myEgonet[[myFriends[i]]]
            friends_j.ids   <- myEgonet[[myFriends[j]]]
            
            np_i    <- c(vertex.id, i.id, friends_i.ids)
            np_j    <- c(vertex.id, j.id, friends_j.ids)
            
            link.mat[i,j]   <- length(intersect(np_i, np_j))/length(union(np_i, np_j))
        }
    }
    colnames(link.mat)  <- myFriends
    rownames(link.mat)  <- myFriends
    return(as.data.frame(link.mat))
}


## at this point, you have an ego network list like what you have in
## in the processed kaggle data

u.egonet    <- lm.ego

## looks like you'll need to reshape the egonet to construct a complete
## set of edges
##
## Current:
##  V1  -> {E11, E12, ... , E1N}
##  V2  -> {E21, E22, ... , E2P}
##  ..
##
## Problem is that not all of the unique edges have a vertex ???

## For example, the following are in the V2 column of the LesMis
## file but not the V1 column
lm.v2notv1  <- unique(lm$V2[which(!(lm$V2 %in% lm$V1))])


## function to take ego network and construct a complete set of edges

ego.ids <- as.integer(gsub("ID_","",names(lm.ego)))

## construct a vertex/edge dataframe
ego.df  <- data.frame()
for (i in 1:length(names(lm.ego))) {
    tmp.vertex  <- as.integer(gsub("ID_","",names(lm.ego)[i]))
    tmp.edges   <- lm.ego[[names(lm.ego)[i]]]
    ego.df      <- rbind(ego.df, data.frame(V=rep(tmp.vertex,length(tmp.edges)), E=tmp.edges))
    
}


ego.ig  <- graph.data.frame(ego.df, directed=FALSE)
vcount(ego.ig)
ecount(ego.ig)


## loop over each EDGE in the network and compute the similarity matrix


loadLinkCommunityMatrix <- function(myVertices, myIgraph)
{
    edges.num   <- length(myVertices)
    link.mat    <- matrix(0, nrow=edges.num, ncol=edges.num)
    
    vertex.id   <- 999999
    
    for (i in 1:edges.num) {
        
        edge.i  <- get.edge(myIgraph, id=i)
        
        for (j in 1:edges.num) {
            
            edge.j  <- get.edge(myIgraph, id=j)
            
            ## The Jaccard similarity coefficient of two vertices is
            ## the number of common neighbors divided by
            ## the number of vertices that are neighbors of at least
            ## one of the two vertices being considered.
            
            np_i    <- neighbors(myIgraph, edge.i[1])
            np_j    <- neighbors(myIgraph, edge.j[1])
            
            link.mat[i,j]   <- length(intersect(np_i, np_j))/length(union(np_i, np_j))
        }
    }
    #colnames(link.mat)  <- paste("ID_",edge.i,".","ID_",edge.j,sep="")
    #rownames(link.mat)  <- colnames(link.mat)
    return(as.data.frame(link.mat))
}

## test graph
#v1 <- data.frame(V=rep(100,7), E=1:7)
#v2 <- data.frame(V=rep(101,5), E=5:9)
#vtest <- rbind(v1, v2)
#gtest <- graph.data.frame(vtest, directed=FALSE, vertices=V)


## isolate the friends
#friends.num <- length(ego.new)
#friends.ids <- names(ego.new)

## load the link community matrix
u.linkCommunity    <- loadLinkCommunityMatrix(ego.df[,1], ego.ig)


## compute a cluster
u.dist  <- dist(u.linkCommunity)
u.clust <- hclust(u.dist, method = "single", members = NULL)
d.clust <- as.dendrogram(u.clust)


## clusters per height
h           <- get_branches_heights(d.clust)
h.num       <- length(h)
h.mat       <- matrix(0,nrow=h.num,ncol=10)

for (i in 1:h.num) {
    
    tmp.h   <- h[i]
    tmp.cut <- cutree(u.clust, h=tmp.h)
    
    tmp.nclust  <- unique(tmp.cut)
    
    
    tmp.count <- sapply(tmp.nclust, function(x) {
        tmp.idx <- which(tmp.cut == x)
        tmp.len <- length(tmp.idx)
        tmp.names <- names(tmp.idx)
        
        tmp.uniq    <- length(unique(as.integer(unlist(sapply(gsub("ID_","",tmp.names), strsplit, "\\.")))))

        return(list(len=tmp.len, unq=tmp.uniq))
    })
    tmp.count   <- t(tmp.count)
    
    mc          <- unlist(tmp.count[,1])
    nc          <- unlist(tmp.count[,2])
    
    dc          <- ifelse(nc == 2, 0, (mc-(nc-1)/(0.5*(nc*(nc-1)) - (nc-1))))
    wc          <- mc / sum(mc)
    
    
    h.mat[i,1]  <- tmp.h
    h.mat[i,2]  <- wc %*% dc
    
}



## similar data from linkcomm() ???
lc <- getLinkCommunities(tmp.lm, hcmethod="single")

lc2 <- getLinkCommunities(tmp.lm, hcmethod="single", dist=as.dist(b))

lc3 <- getLinkCommunities(get.data.frame(a,"edges"), hcmethod="single")





