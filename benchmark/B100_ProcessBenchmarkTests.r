##------------------------------------------------------------------
## The purpose of this script is to:
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
## Source the utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/social_circle/k_soc/00_Utilities.r")


##------------------------------------------------------------------
## Read in the raw data files
##------------------------------------------------------------------
load("01_SocialCircle_RawData.Rdata")


##------------------------------------------------------------------
## load the connected clusters benchmark
##------------------------------------------------------------------
##  --> connectedClusters.list
##------------------------------------------------------------------
load("B02_ConnectedClustersBenchmark.Rdata")


##------------------------------------------------------------------
## load the similarity and profile benchmarks
##------------------------------------------------------------------
##  --> sweep.res[[]][["p0"]] = profile
##  --> sweep.res[[]][["p1"]] = similarity
##------------------------------------------------------------------
load("B20_SimAndProfBenchmarks.Rdata")

profileClusters.list        <- lapply(sweep.res, function(x) { x[["p0"]] })
similarityClusters.list     <- lapply(sweep.res, function(x) { x[["p1"]] })


##------------------------------------------------------------------
## load the non-partition density benchmarks
##------------------------------------------------------------------
##  --> res[["p0"]] = profile
##  --> res[["p1"]] = similarity
##------------------------------------------------------------------
## These are *not* clusters, rather they are the results of calculating
## clusters and cluster edits for all heights.  With an optimal
## height, we can re-compute the clusters as needed.  However the
## data contains the number of edit as function of cluster height
##------------------------------------------------------------------
load("B22_HeightDensityTest.Rdata")

profileNonPartitionDensityClusters.list     <- lapply(res, function(x) { x[["p0"]] })
similarityNonPartitionDensityClusters.list  <- lapply(res, function(x) { x[["p1"]] })


##******************************************************************
## Main
##******************************************************************


##------------------------------------------------------------------
## loop over the "profile" non-partition density cluster data
##------------------------------------------------------------------
height.ids          <- names(profileNonPartitionDensityClusters.list)
height.num          <- length(height.ids)
profileNonPart.list <- list()

for (i in 1:height.num)
{
    tmp.id          <- height.ids[i]
    tmp.bench       <- profileNonPartitionDensityClusters.list[[tmp.id]]$benchmark

    ## isolate cases where we have fewer edits and a height >= 3/4 of the max
    tmp.index       <- which((tmp.bench$dif.edits <= 0) & (tmp.bench$h > 0.75))
    
    if (length(tmp.index) == 0) {
        tmp.hfrac   <- tmp.bench$hfrac[1]
        tmp.edits   <- tmp.bench$num.pred.edits[1]
        tmp.diff    <- tmp.bench$dif.edits[1]
        
    } else {
        tmp.hfrac   <- tmp.bench$hfrac[ which( tmp.bench$dif.edits == min(tmp.bench$dif.edits[tmp.index]) ) ][1]
        tmp.edits   <- tmp.bench$num.pred.edits[ which( tmp.bench$dif.edits == min(tmp.bench$dif.edits[tmp.index]) ) ][1]
        tmp.diff    <- tmp.bench$dif.edits[ which( tmp.bench$dif.edits == min(tmp.bench$dif.edits[tmp.index]) ) ][1]
    }
    profileNonPart.list[[tmp.id]]$hfrac <- tmp.hfrac
    profileNonPart.list[[tmp.id]]$diff  <- tmp.diff
    profileNonPart.list[[tmp.id]]$edits <- tmp.edits
    
}
#mean(unlist(lapply(profileNonPart.list, function(x){x$hfrac})))    ## 0.8633617
#mean(unlist(lapply(profileNonPart.list, function(x){x$edits})))    ## -63.71186


##------------------------------------------------------------------
## loop over the "similarity" non-partition density cluster data
##------------------------------------------------------------------
height.ids              <- names(similarityNonPartitionDensityClusters.list)
height.num              <- length(height.ids)
similarityNonPart.list  <- list()

for (i in 1:height.num)
{
    tmp.id          <- height.ids[i]
    tmp.bench       <- similarityNonPartitionDensityClusters.list[[tmp.id]]$benchmark
    
    ## isolate cases where we have fewer edits and a height >= 3/4 of the max
    tmp.index       <- which((tmp.bench$dif.edits <= 0) & (tmp.bench$h > 0.75))
    
    if (length(tmp.index) == 0) {
        tmp.hfrac   <- tmp.bench$hfrac[1]
        tmp.edits   <- tmp.bench$num.pred.edits[1]
        tmp.diff    <- tmp.bench$dif.edits[1]
    } else {
        tmp.hfrac   <- tmp.bench$hfrac[ which( tmp.bench$dif.edits == min(tmp.bench$dif.edits[tmp.index]) ) ][1]
        tmp.edits   <- tmp.bench$num.pred.edits[ which( tmp.bench$dif.edits == min(tmp.bench$dif.edits[tmp.index]) ) ][1]
        tmp.diff    <- tmp.bench$dif.edits[ which( tmp.bench$dif.edits == min(tmp.bench$dif.edits[tmp.index]) ) ][1]
    }
    similarityNonPart.list[[tmp.id]]$hfrac  <- tmp.hfrac
    similarityNonPart.list[[tmp.id]]$diff   <- tmp.diff
    similarityNonPart.list[[tmp.id]]$edits  <- tmp.edits
    
}
#mean(unlist(lapply(similarityNonPart.list, function(x){x$hfrac})))    ## 0.8655875
#mean(unlist(lapply(similarityNonPart.list, function(x){x$diff})))     ## -107.6441



##------------------------------------------------------------------
## loop over all of the training IDs and collect edit statistics
##------------------------------------------------------------------
train.ids           <- names(known_circles.list)
train.num           <- length(train.ids)
editsComparison.df  <- data.frame()

for (i in 1:train.num) {
    
    ## extract the id
    tmp.id  <- train.ids[i]
    
    ## extract the various permutations
    tmp.knownCluster        <- known_circles.list[[tmp.id]]
    tmp.connectedCluster    <- connectedClusters.list[[tmp.id]]
    tmp.profileCluster      <- profileClusters.list[[tmp.id]]
    tmp.similarityCluster   <- similarityClusters.list[[tmp.id]]
    
    ## compute circleEdits
    edits.connectedCluster  <- circleEdits(trueCircles=tmp.knownCluster, predCircles=tmp.connectedCluster)
    edits.profileCluster    <- circleEdits(trueCircles=tmp.knownCluster, predCircles=tmp.profileCluster)
    edits.similarityCluster <- circleEdits(trueCircles=tmp.knownCluster, predCircles=tmp.similarityCluster)
    
    ## gather circleEdits from the non-partition density data
    edits.profileNonPart        <- profileNonPart.list[[tmp.id]]$edits
    edits.similarityNonPart     <- similarityNonPart.list[[tmp.id]]$edits
    
    ## aggregate results
    editsComparison.df      <- rbind(editsComparison.df,
                                        data.frame( id=tmp.id,
                                                    num.known=length(tmp.knownCluster),
                                                    num.cc=length(tmp.connectedCluster),
                                                    num.pr=length(tmp.profileCluster),
                                                    num.sim=length(tmp.similarityCluster),
                                                    nedit.cc=edits.connectedCluster,
                                                    nedit.pr=edits.profileCluster,
                                                    nedit.sim=edits.similarityCluster,
                                                    nedit.pr.np=edits.profileNonPart,
                                                    nedit.sim.np=edits.similarityNonPart))
}






