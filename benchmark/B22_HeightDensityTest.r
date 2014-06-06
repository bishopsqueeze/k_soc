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
## Directories holding the topological & profile similarity data
##------------------------------------------------------------------
simDirectory    <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/sim"


##------------------------------------------------------------------
## Loop over the egonets and ...
##  - Load my similarity matrix
##  - Compute distances, clusters
##  - Compute partition density
##  - Use linkcomm() to compute similar data
##  - Compare the two
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

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"sweep",sep="/")

## placeholder for output
benchmark.df    <- data.frame()
res       <- list()

##------------------------------------------------------------------
## for these tests loop over known circles only
##------------------------------------------------------------------
#for (i in 1:10) { #train.num
for (i in 1:train.num) { #train.num
    
    ## set-up
    tmp.id          <- train.names[train.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    tmp.known       <- known_circles.list[[tmp.id]]
    
    ## the nodes in the original egonet -- will need to make sure you only have these in circles
    tmp.nodes       <- as.integer(gsub("ID_","",names(egonets.list[[tmp.id]])))
    
    ## gather known circles statistics
    circle.num      <- length(tmp.known)
    circle.tot      <- sum(unlist(lapply(tmp.known, length)))
    circle.uniq     <- sum(unique(unlist(lapply(tmp.known, length))))
    
    ## echo progress
    cat("Iteration", i, "of", train.num, " :: Height scan for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
   
    ## load similarity matrix (called tmp.sim)
    tmp.topoFile     <- paste(simDirectory, "/", paste0(tmp.id, ".SimilarityMatrix.Rdata"), sep="")
    load(tmp.topoFile)
    
    ## load profile matrix (called tmp.profSim)
    tmp.profFile     <- paste(simDirectory, "/", paste0(tmp.id, ".ProfileSimilarityMatrix.Rdata"), sep="")
    load(tmp.profFile)

    ##------------------------------------------------------------------
    ## define weights used to select Similarity and Profile matrices
    ##------------------------------------------------------------------
    w.seq   <- seq(0,1,1)

    ##------------------------------------------------------------------
    ## loop over the set of weights:
    ##  w <- 0 :: Pure "profile" similarity
    ##  w <- 1 :: Pure "topological" similarity
    ##------------------------------------------------------------------
    for (j in 1:length(w.seq)) {
        
        ## weight the matrices
        p               <- w.seq[j]
        sim.combined    <- (p)*(tmp.sim) + (1-p)*(tmp.profSim)
        
        ## calc dissimilarity, generate a cluster, calc parition density
        tmp.dist        <- as.dist(1 - sim.combined)
        tmp.hclust      <- hclust(tmp.dist, method="single")
        tmp.pdens       <- calcPartitionDensity(tmp.hclust, tmp.edges)      ## slow
        
        ##  iterate over all possible heights
        unq.h   <- unique(tmp.hclust$height)
        unq.h   <- unq.h[order(-unq.h)]
        
        ##------------------------------------------------------------------
        ## loop over unique heights, extract clusters, gather stats
        ##------------------------------------------------------------------
        tmp.summary <- list()
        #for (h in 1:length(unq.h)) {
        tmp.summary <- foreach (h=1:length(unq.h)) %dopar% {
            
            ## extract the clusters at each height
            tmp.clust       <- extractHclustClusters(tmp.hclust, unq.h[h], tmp.edges)
            beg.clustnodes  <- tmp.clust$cl.nodes
            end.clustnodes  <- lapply(beg.clustnodes, intersect, tmp.nodes)
            end.tot         <- sum(unlist(lapply(end.clustnodes, length)))
            end.uniq        <- sum(unique(unlist(lapply(end.clustnodes, length))))
            
            ## compute edits :: circleEdits(true, pred)
            tmp.edits       <- circleEdits(tmp.known, end.clustnodes)
            all.edits       <- circleEdits(tmp.known, list(tmp.nodes))
            dif.edits       <- tmp.edits-all.edits
            
            ## gather statistics
            #tmp.summary[[h]]  <-
            data.frame(id=tmp.id,
                                            h=unq.h[h],
                                            hfrac=unq.h[h]/max(unq.h),
                                            num.pred.cl=tmp.clust$cl.count,
                                            num.known.cl=circle.num,
                                            num.pred.uniq=end.uniq,
                                            num.known.uniq=circle.uniq,
                                            num.pred.tot=end.tot,
                                            num.known.tot=circle.tot,
                                            num.pred.edits=tmp.edits,
                                            num.all.edits=all.edits,
                                            dif.edits=(tmp.edits-all.edits))
                                        

        }
        
        ##------------------------------------------------------------------
        ## accumulate results
        ##------------------------------------------------------------------
        benchmark.df <- do.call("rbind", tmp.summary)
        
        ##------------------------------------------------------------------
        ## save results to a list
        ##------------------------------------------------------------------
        p.name  <- paste0("p",p)
        res[[tmp.id]][[p.name]]$benchmark <- benchmark.df
        
        plot(   res[[tmp.id]][[p.name]]$benchmark$hfrac,
                res[[tmp.id]][[p.name]]$benchmark$dif.edits,
                col="steelblue", type="b", pch=20, xlab="Height", ylab="Edit Difference"); abline(h=0)
    }
    
}

## Save results
save(res, file="B22_HeightDensityTest.Rdata")


