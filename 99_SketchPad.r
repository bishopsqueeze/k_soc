##------------------------------------------------------------------
##
##------------------------------------------------------------------

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
    #u.feat      <- features.list[[u.id]]
    #u.names     <- names(u.feat)
    
    
    
    ## isolate the friends
    friends.num <- length(u.egonet)
    friends.ids <- names(u.egonet)
    
    
    #
    # define the superset of granualr features within this
    #
    u.leaves <- unionEgonetLeaves(u.id, u.egonet, leaves.list)

    #
    # load the (sparse) leaf matrix
    #
    u.leafMatrix <- loadLeafMatrix(u.id, u.egonet, leaves.list, u.leaves)
    
    #
    # load the profile similarity matrix
    #
    u.profSimilarity    <- loadProfileSimilarityMatrix(u.leafMatrix[,-1])
    
    #
    # load the link community matrix
    #
    u.linkCommunity    <- loadLinkCommunityMatrix(u.id, friends.ids, u.egonet)
    

    
    
    ## can just load a matrix of n*((n-1)/2)
    xy.mat      <- matrix(, nrow=length(featureDict), ncol=(friends.num*(friends.num-1)/2))
    xu.mat      <- matrix(, nrow=length(featureDict), ncol=(friends.num*(friends.num-1)/2))
    yu.mat      <- matrix(, nrow=length(featureDict), ncol=(friends.num*(friends.num-1)/2))
    xy.vec      <- vector(, length=(friends.num*(friends.num-1)/2))
    
    rownames(xy.mat) <- rownames(xu.mat) <- rownames(yu.mat) <- featureDict
    
    
    counter <- 1
    ## loop over all unique combos  ## CAN SHORTEN TO SYMMETRIC SIDE
    for (j in 1:(friends.num-1)) {
        for (k in (j+1):friends.num) {
         
            ## identify the friends
            x.id    <- friends.ids[j]
            y.id    <- friends.ids[k]
            
            ## isolate (x,y) combinations
            xy.id   <- paste(x.id,y.id,sep="_")
            xu.id   <- paste(x.id,u.id,sep="_")
            yu.id   <- paste(y.id,u.id,sep="_")
            
            ## extract the x, y feature lists
            x.feat    <- features.list[[x.id]]
            y.feat    <- features.list[[y.id]]

            ## extract names of the features
            x.names   <- names(x.feat)
            y.names   <- names(y.feat)
            
            ## extract the unique set of leaves
            xy.leaves <- unique( c(x.names, y.names) )
            
            ## compute intersections
            xy.intersect    <- intersect(x.names, y.names)
            xu.intersect    <- intersect(x.names, u.names)
            yu.intersect    <- intersect(y.names, u.names)
            
            ## define parent vectors
            xy.sigmap        <- xu.sigmap        <- yu.sigmap        <- vector("integer", length(featureDict))
            names(xy.sigmap) <- names(xu.sigmap) <- names(yu.sigmap) <- featureDict
            
            ## load the parent vectors
            xy.mat[,counter]  <- loadSigmap(inSect=xy.intersect, x=x.feat, y=y.feat, dict=featureDict)
            xy.vec[counter]   <- paste(x.id,y.id,sep=".")
            
            #xy.sigmap <- loadSigmap(inSect=xy.intersect, x=x.feat, y=y.feat, dict=featureDict)
            #xu.sigmap <- loadSigmap(inSect=xu.intersect, x=x.feat, y=u.feat, dict=featureDict)
            #yu.sigmap <- loadSigmap(inSect=yu.intersect, x=y.feat, y=u.feat, dict=featureDict)
  
  
            ## need to consider how to treat the difference vectors
            xy.idx  <- which(featureDict %in% xy.intersect)
            #xu.idx  <- which(featureDict %in% xu.intersect)
            #yu.idx  <- which(featureDict %in% yu.intersect)

            #sigma.list[[u.id]][[x.id]][[y.id]]$sigmap <- xy.sigmap
            #sigma.list[[u.id]][[x.id]][[y.id]]$hindex <- xy.idx
            
            #sigma.list[[u.id]][[x.id]][[u.id]]$sigmap <- xu.sigmap
            #sigma.list[[u.id]][[x.id]][[u.id]]$hindex <- xu.idx
            
            #sigma.list[[u.id]][[y.id]][[u.id]]$sigmap <- yu.sigmap
            #sigma.list[[u.id]][[y.id]][[u.id]]$hindex <- yu.idx
          
            counter <- counter + 1
        }
    }
    colnames(xy.mat) <- xy.vec
}






tmp.sim     <- calcSimilarityMatrix(tmp.edge2)

tmp.dist    <- as.dist(1-tmp.sim)

tmp.clust   <- hclust(tmp.dist, method="single")

tmp.part    <- calcPartitionDensity(tmp.clust, tmp.edge2)
tmp.lc      <- getLinkCommunities(get.data.frame(tmp.edge2, "edges"), hcmethod="single")




comp.edit   <- unlist(tmp.ego)
comp.edit   <- comp.edit[comp.edit > 0]
comp.list   <- list(unique(as.vector(comp.edit)))

tmp.edit    <- circleEdits(known_circles.list[["ID_11364"]], tmp.part$clust.nodes)
tmp.comp    <- circleEdits(known_circles.list[["ID_11364"]], comp.list)

tmp.circ <- convClustersToTextCircle("ID_11364", tmp.part$clust.nodes)

11364 <actual>
11391 11393 11376 11366 11392 11377
11391 11393 11376 11366 11392 11377
11381 11401 11389 11394 11406 11378 11395 11370 11404 11371 11368 11396 11405 11399 11403 11383 11390
11391 11385 11367 11386 11408 11380 11374 11407 11382

11364 <python>
11364,
11392 11393 11394 11395 11396 11397 11399 11400 11401 11402 11403 11404 11405 11406 11407 11408 11365 11366 11367 11368 11369 11370 11371 11372 11373 11375 11376 11377 11378 11379 11380 11381 11382 11383 11384 11385 11386 11387 11388 11389 11390 11391


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




ego.names   <- sort(names(egonets.list))
sigma.list  <- list()

#for (i in 1:length(ego.names)) {
for (i in 1:1) {
    
    ## grab the userid and his egonet
    u.id        <- ego.names[i]
    u.egonet    <- egonets.list[[u.id]]
    u.feat      <- features.list[[u.id]]
    u.names     <- names(u.feat)
    
    ## isolate the friends
    friends.num <- length(tmp.egonet)
    friends.ids <- names(tmp.egonet)
    
    
    ## can just load a matrix of n*((n-1)/2)
    xy.mat      <- matrix(, nrow=length(featureDict), ncol=(friends.num*(friends.num-1)/2))
    xu.mat      <- matrix(, nrow=length(featureDict), ncol=(friends.num*(friends.num-1)/2))
    yu.mat      <- matrix(, nrow=length(featureDict), ncol=(friends.num*(friends.num-1)/2))
    xy.vec      <- vector(, length=(friends.num*(friends.num-1)/2))
    
    rownames(xy.mat) <- rownames(xu.mat) <- rownames(yu.mat) <- featureDict
    
    
    counter <- 1
    ## loop over all unique combos  ## CAN SHORTEN TO SYMMETRIC SIDE
    for (j in 1:(friends.num-1)) {
        for (k in (j+1):friends.num) {
            
            ## identify the friends
            x.id    <- friends.ids[j]
            y.id    <- friends.ids[k]
            
            ## isolate (x,y) combinations
            xy.id   <- paste(x.id,y.id,sep="_")
            xu.id   <- paste(x.id,u.id,sep="_")
            yu.id   <- paste(y.id,u.id,sep="_")
            
            ## extract the x, y feature lists
            x.feat    <- features.list[[x.id]]
            y.feat    <- features.list[[y.id]]
            
            ## extract names of the features
            x.names   <- names(x.feat)
            y.names   <- names(y.feat)
            
            ## extract the unique set of leaves
            xy.leaves <- unique( c(x.names, y.names) )
            
            ## compute intersections
            xy.intersect    <- intersect(x.names, y.names)
            xu.intersect    <- intersect(x.names, u.names)
            yu.intersect    <- intersect(y.names, u.names)
            
            ## define parent vectors
            xy.sigmap        <- xu.sigmap        <- yu.sigmap        <- vector("integer", length(featureDict))
            names(xy.sigmap) <- names(xu.sigmap) <- names(yu.sigmap) <- featureDict
            
            ## load the parent vectors
            xy.mat[,counter]  <- loadSigmap(inSect=xy.intersect, x=x.feat, y=y.feat, dict=featureDict)
            xy.vec[counter]   <- paste(x.id,y.id,sep=".")
            
            #xy.sigmap <- loadSigmap(inSect=xy.intersect, x=x.feat, y=y.feat, dict=featureDict)
            #xu.sigmap <- loadSigmap(inSect=xu.intersect, x=x.feat, y=u.feat, dict=featureDict)
            #yu.sigmap <- loadSigmap(inSect=yu.intersect, x=y.feat, y=u.feat, dict=featureDict)
            
            
            ## need to consider how to treat the difference vectors
            xy.idx  <- which(featureDict %in% xy.intersect)
            #xu.idx  <- which(featureDict %in% xu.intersect)
            #yu.idx  <- which(featureDict %in% yu.intersect)
            
            #sigma.list[[u.id]][[x.id]][[y.id]]$sigmap <- xy.sigmap
            #sigma.list[[u.id]][[x.id]][[y.id]]$hindex <- xy.idx
            
            #sigma.list[[u.id]][[x.id]][[u.id]]$sigmap <- xu.sigmap
            #sigma.list[[u.id]][[x.id]][[u.id]]$hindex <- xu.idx
            
            #sigma.list[[u.id]][[y.id]][[u.id]]$sigmap <- yu.sigmap
            #sigma.list[[u.id]][[y.id]][[u.id]]$hindex <- yu.idx
            
            counter <- counter + 1
        }
    }
    colnames(xy.mat) <- xy.vec
}





