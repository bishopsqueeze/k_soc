##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(linkcomm)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/social_circle/data/inputs")

##------------------------------------------------------------------
## Read in the raw data files
##------------------------------------------------------------------
load("01_SocialCircle_RawData.Rdata")

##------------------------------------------------------------------
## Goal is to load a set of feature difference vectors for each egonet
##------------------------------------------------------------------
loadSigmap   <- function(inSect=NULL, x=NULL, y=NULL, dict=featureDict)
{
    ## load sigma(x,y)
    for (m in 1:length(inSect)) {
        tmp.val   <- inSect[m]
        tmp.x     <- x[[tmp.val]]
        tmp.y     <- y[[tmp.val]]
        
        ## load the intersection count into the appropriate slots
        if( !is.null(tmp.x) & !is.null(tmp.y) ) {
            if ( length(tmp.intersect <- intersect(tmp.x, tmp.y)) > 0 ) {
                tmp.idx            <- (dict == tmp.val)
                xy.sigmap[tmp.idx] <- length(tmp.intersect)
            }
        }
    }
    return(xy.sigmap)
}




##------------------------------------------------------------------
## Union all of the leaves within a given egonet
##------------------------------------------------------------------

unionEgonetLeaves   <- function(myid, myegonet, myleaves)
{
    vertices.ids    <- c(myid, names(myegonet))
    
    ## error check if only one user?
    return( sort(unique(unlist(sapply(vertices.ids, function(x){myleaves[[x]]})))) )
}



loadLeafMatrix   <- function(myid, myegonet, myleaves, myunion)
{
    vertices.ids    <- c(myid, names(myegonet))
    vertices.num    <- length(vertices.ids)
    union.num       <- length(myunion)
    
    leaf.mat  <- matrix(0,nrow=union.num, ncol=vertices.num)
    
    for (i in 1:vertices.num) {
        tmp.id                  <- vertices.ids[i]
        tmp.leaf                <- myleaves[[tmp.id]]
        tmp.index               <- which(myunion %in% tmp.leaf)
        leaf.mat[tmp.index,i]   <- 1
    }
    leaf.df             <- as.data.frame(leaf.mat)
    colnames(leaf.df)   <- vertices.ids
    rownames(leaf.df)   <- myunion
    
    return(leaf.df)
}


loadProfileSimilarityMatrix <- function(myMatrix)
{
    col.names   <- colnames(myMatrix)
    col.num     <- length(col.names)
    
    sim.mat     <- matrix(0, nrow=col.num, ncol=col.num)
    
    ## compute the cosine similarity measure
    for (i in 1:col.num) {
        for (j in 1:col.num) {
            aTb          <- (myMatrix[,i] %*% myMatrix[,j])
            aTa          <- (myMatrix[,i] %*% myMatrix[,i])
            bTb          <- (myMatrix[,j] %*% myMatrix[,j])
            sim.mat[i,j] <- aTb / sqrt(aTa * bTb)
        }
    }
    colnames(sim.mat)   <- col.names
    rownames(sim.mat)   <- col.names
    
    return(as.data.frame(sim.mat))
}



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



ego.names   <- sort(names(egonets.list))
sigma.list  <- list()

#for (i in 1:length(ego.names)) {
for (i in 1:1) {
   
    ## grab the userid and his egonet
    u.id        <- ego.names[i]
    u.egonet    <- egonets.list[[u.id]]
    
    ## isolate the friends
    friends.num <- length(u.egonet)
    friends.ids <- names(u.egonet)
    
    ## define the superset of granualr features within this
    u.leaves <- unionEgonetLeaves(u.id, u.egonet, leaves.list)

    ## load the (sparse) leaf matrix
    u.leafMatrix <- loadLeafMatrix(u.id, u.egonet, leaves.list, u.leaves)
    
    ## load the profile similarity matrix
    u.profSimilarity    <- loadProfileSimilarityMatrix(u.leafMatrix[,-1])
    
    ## load the link community matrix
    u.linkCommunity    <- loadLinkCommunityMatrix(u.id, friends.ids, u.egonet)
    
    ## combine the two matrices
    alpha   <- 0.5
    u.similarityMatrix  <- alpha*u.profSimilarity + (1-alpha)*u.linkCommunity
    
    ## compute a distance matrix
    u.dist  <- dist(u.similarityMatrix)
    
    u.clust <- hclust(u.dist, method = "single", members = NULL)

    clust.hrange <- quantile(u.clust$height, probs=seq(0,1,0.1))
    
    
}




## debug -- create two-columns G(V,E)

net.df  <- data.frame()
for (i in 1:length(u.egonet)) {
    
    
    tmp.v   <- as.integer(strsplit(names(u.egonet)[i], "_")[[1]][2])
    tmp.e   <- u.egonet[[names(u.egonet)[i]]]
    
    net.df  <- rbind(net.df, data.frame(v=rep(tmp.v, length(tmp.e)), e=tmp.e))
    
}

lc <- getLinkCommunities(net.df, hcmethod="single", plot=FALSE)



##
n <- length(unique(unlist(lesmiserables)))
a <- unique(unlist(lesmiserables))
for (i in 1:n) {
    tmp.a <- a[i]
    
}




