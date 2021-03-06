##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)                 ## contains graph functions
library(caTools)
library(linkcomm)
library(data.table)

library(caret)
library(foreach)
library(doMC)

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
registerDoMC(4)

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
load("01_SocialCircle_RawData.Rdata")       ## raw data
load("02_SocialCircle_Edges.Rdata")         ## edge lists

##------------------------------------------------------------------
## load the connected clusters benchmark
##------------------------------------------------------------------
##  --> connectedClusters.list
##------------------------------------------------------------------
load("./benchmarks/B02_ConnectedClustersBenchmark.Rdata")

##------------------------------------------------------------------
## Directories holding the topological & profile similarity data
##------------------------------------------------------------------
orig.sim.topo            <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/orig.sim.topo"
orig.sim.prof.reduced    <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/orig.sim.prof.reduced"
orig.sim.user.reduced    <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/orig.sim.user.reduced"

##------------------------------------------------------------------
## Available clustering methods in hclust()
##------------------------------------------------------------------
hc.methods <- c("single", "mcquitty") ## ("ward.D", "ward.D2", "median", "centroid")
#hc.methods <- c("average", "single")

##------------------------------------------------------------------
## Loop over the egonets and ...
##  - Load my similarity matrix
##  - Compute distances, clusters
##  - For each cluster height, extract circles and compare to benchmark
##------------------------------------------------------------------

##------------------------------------------------------------------
## Set-up
##------------------------------------------------------------------

## extract egonet data
ego.names       <- names(egoedges.list)
ego.known       <- names(known_circles.list)

## get the edge count for each, so we can work from smallest to largest
egoedges.count  <- unlist(lapply(egoedges.list, ecount))

## isolate the training data only
train.names       <- ego.names[which(ego.names %in% ego.known)]
train.count       <- egoedges.count[which(ego.names %in% ego.known)]
train.num         <- length(train.names)
train.order       <- order(train.count)

## placeholder for output
res  <- list()

##------------------------------------------------------------------
## for the sweep tests, loop over known circles only
##------------------------------------------------------------------
for (i in 1:train.num) { #train.num
    
    ## set-up
    tmp.id          <- train.names[train.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    tmp.known       <- known_circles.list[[tmp.id]]
    tmp.vcount      <- vcount(tmp.edges)
    tmp.ecount      <- ecount(tmp.edges)
    
    ## get the connected cluster benchmark data
    tmp.connectedCluster    <- connectedClusters.list[[tmp.id]]
    
    ## the nodes in the original egonet -- will need to make sure you only have these in circles
    tmp.nodes       <- as.integer(gsub("ID_","",names(egonets.list[[tmp.id]])))
    
    ## gather known circles statistics
    circle.num      <- length(tmp.known)
    circle.tot      <- sum(unlist(lapply(tmp.known, length)))
    circle.uniq     <- sum(unique(unlist(lapply(tmp.known, length))))
    
    ## echo progress
    cat("Iteration", i, "of", train.num, " for ", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
   
    ## load similarity matrix (called tmp.sim)
    load(paste(orig.sim.topo, "/", paste0(tmp.id, ".SimilarityMatrix.Rdata"), sep=""))

    ##------------------------------------------------------------------
    ## loop over the possible methods
    ##------------------------------------------------------------------
    
    for (j in 1:length(hc.methods)) {
        
        ## extract the method
        tmp.method  <- hc.methods[j]
        
        ## test using the original similarity matrix only
        sim.combined    <- tmp.sim
        
        ## calc dissimilarity & generate a cluster
        tmp.dist        <- as.dist(1 - sim.combined)
        tmp.hclust      <- hclust(tmp.dist, method=tmp.method)
        
        ## extract unique heights (reorder from high to low)
        unq.h   <- unique(tmp.hclust$height)
        unq.h   <- unq.h[order(-unq.h)]

        ##------------------------------------------------------------------
        ## loop over the heights and extract fit statistics
        ##------------------------------------------------------------------
        tmp.summary <- list()
        ##
        tmp.summary <- foreach (h=1:length(unq.h)) %dopar% {
    
            ## extract the clusters at each height
            tmp.clust       <- extractHclustClusters(tmp.hclust, unq.h[h], tmp.edges)
            beg.clustnodes  <- tmp.clust$cl.nodes
            end.clustnodes  <- lapply(beg.clustnodes, intersect, tmp.nodes)
            end.tot         <- sum(unlist(lapply(end.clustnodes, length)))
            end.uniq        <- sum(unique(unlist(lapply(end.clustnodes, length))))
            
            ## compute edits :: circleEdits(true, pred)
            tmp.edits       <- circleEdits(tmp.known, end.clustnodes)
            cc.edits        <- circleEdits(tmp.known, tmp.connectedCluster)
            
            ##------------------------------------------------------------------
            ## gather statistics
            ##------------------------------------------------------------------
            tmp.summary[[h]] <- data.frame(
                                    id=tmp.id,
                                    vcount=tmp.vcount,
                                    ecount=tmp.ecount,
                                    method=tmp.method,
                                    h=unq.h[h],
                                    hfrac=unq.h[h]/max(unq.h),
                                    num.pred.cl=tmp.clust$cl.count,
                                    num.known.cl=circle.num,
                                    num.pred.edits=tmp.edits,
                                    num.cc.edits=cc.edits,
                                    dif.edits=(tmp.edits-cc.edits))
        } ## end of the h loop
        
        ## save results
        res[[tmp.id]][[tmp.method]]  <- do.call("rbind", tmp.summary)
        
    } ## end of the j loop
} ## end of the i loop

##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(res, file="./benchmarks/B23_ClusterMethodTests.Rdata")


