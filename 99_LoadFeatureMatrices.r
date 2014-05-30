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




