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
load("01_SocialCircle_RawData.Rdata")                       ## raw data
##load("02_SocialCircle_Edges.Rdata")                       ## edge lists
load("06_SocialCircle_ReconEdges.Rdata")                    ## edge lists
load("./benchmarks/B02_ConnectedClustersBenchmark.Rdata")   ## connected cluster benchmark

##------------------------------------------------------------------
## Directories holding the topological & profile similarity data
##------------------------------------------------------------------
recon.sim.topo         <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/recon.sim.topo"
recon.sim.prof.full    <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/recon.sim.prof.full"
recon.sim.prof.reduced <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/recon.sim.prof.reduced"

##------------------------------------------------------------------
## Loop over the egonets and ...
##  - Load my similarity matrix
##  - Compute distances, clusters
##  - Use the average optimal height to identify clusters
##------------------------------------------------------------------

##------------------------------------------------------------------
## Set-up
##------------------------------------------------------------------

## extract egonet data
ego.names         <- names(recon_egoedges.list)
ego.known         <- names(known_circles.list)
ego.num           <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
recon_egoedges.count    <- unlist(lapply(recon_egoedges.list, ecount))
#recon_egoedges.order    <- order(recon_egoedges.count)
recon_egoedges.order    <- 1:ego.num

## placeholder for output
sub_predCircleEdits.df      <- data.frame()
sub_predCircleMembers.list  <- list()
sub_submissionFile          <- c("UserId,Predicted")

##------------------------------------------------------------------
## loop over all egonets
##------------------------------------------------------------------
for (i in 1:ego.num) {
    
    ## set-up
    tmp.id          <- ego.names[recon_egoedges.order[i]]
    tmp.edges       <- recon_egoedges.list[[tmp.id]]
    
    ## the nodes in the original egonet -- will need to make sure you only have these in circles
    tmp.nodes       <- as.integer(gsub("ID_","",names(egonets.list[[tmp.id]])))
   
    ## load similarity matrix (called tmp.reconSim)
    tmp.topoFile            <- paste(recon.sim.topo, "/", paste0(tmp.id, ".ReconSimilarityMatrix.Rdata"), sep="")
    load(tmp.topoFile)
    
    ## load profile matrix (called tmp.reducedProfileSim)
    tmp.profFile            <- paste(recon.sim.prof.reduced, "/", paste0(tmp.id, ".ReducedProfileSimilarityMatrix.Rdata"), sep="")
    load(tmp.profFile)

    ##------------------------------------------------------------------
    ## choose weight c(0,1) used to select Similarity || Profile matrices
    ##------------------------------------------------------------------
    ## use a chosen weight:
    ##  p <- 0 :: Pure "profile" similarity
    ##  p <- 1 :: Pure "topological" similarity
    ##------------------------------------------------------------------
    p               <- 1
    sim.combined    <- (p)*(tmp.reconSim) + (1-p)*(tmp.reducedProfileSim)
    
    ## calc dissimilarity, generate a cluster
    tmp.dist        <- as.dist(1 - sim.combined)
    tmp.hclust      <- hclust(tmp.dist, method="single")
    
    ##------------------------------------------------------------------
    ## here we skip the parititon density step because we will use the
    ## the fractional height derived from the benchmark tests
    ##------------------------------------------------------------------
    ##tmp.pdens       <- calcPartitionDensity(tmp.hclust, tmp.edges)
    
    ## cut at the optimal height
    unq.h       <- unique(tmp.hclust$height)
    cut.frac    <- 0.866
    cut.h       <- cut.frac*max(unq.h)
    
    ## extract the clusters at the optimal height
    tmp.clust       <- extractHclustClusters(tmp.hclust, cut.h, tmp.edges)
    beg.clustnodes  <- tmp.clust$cl.nodes
    end.clustnodes  <- lapply(beg.clustnodes, intersect, tmp.nodes)
    
    ## deal with null clusters; substitute all nodes if no hist
    if ( length(end.clustnodes) == 0 ) {
        end.clustnodes <- list(tmp.nodes)
    }
    
    ## compute edits for the training set :: circleEdits(true, pred)
    if ( tmp.id %in% ego.known ) {
        tmp.known       <- known_circles.list[[tmp.id]]
        tmp.edits       <- circleEdits(tmp.known, end.clustnodes)
        tmp.cc.edits    <- circleEdits(tmp.known, connectedClusters.list[[tmp.id]])
        dif.edits       <- tmp.edits-tmp.cc.edits
        
        sub_predCircleEdits.df  <- rbind(sub_predCircleEdits.df,
                                    data.frame(id=tmp.id, num.pred.edits=tmp.edits,
                                    num.cc.edits=tmp.cc.edits, diff.edits=dif.edits))
                                    
        cat("Id=", tmp.id, "Predicted Edits=", tmp.edits, "Connected Edits=", tmp.cc.edits, "Difference=", dif.edits, "\n")
    }
    
    sub_predCircleMembers.list[[tmp.id]] <- end.clustnodes
    

    ## create submission file
    if ( !(tmp.id %in% ego.known) ) {
        
        submissionLine      <- paste(gsub("ID_","",tmp.id), ",", sep="")
        submissionLength    <- length(end.clustnodes)
        for (k in 1:submissionLength) {
            if (k == submissionLength) {
                tmp.cluster       <- tmp.clust$cl.nodes[[k]]
                submissionLine    <- paste(submissionLine, paste(tmp.cluster, collapse=" "), sep="")
            } else {
                tmp.cluster       <- tmp.clust$cl.nodes[[k]]
                submissionLine    <- paste(submissionLine, paste(tmp.cluster, collapse=" "), sep="", ";")
            }
        }
        sub_submissionFile <- c(sub_submissionFile, submissionLine)
        
    }

    cat("Completed iteration",i,"of",ego.num,"\n")
}

##------------------------------------------------------------------
## Write a submission file
##------------------------------------------------------------------
write.table(sub_submissionFile, file="S005_ReconNetwork_Hfrac_0.866_SimilarityOnly.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)

##------------------------------------------------------------------
## Save all contents
##------------------------------------------------------------------
save.image(file="S005_ReconNetwork_Hfrac_0.866_SimilarityOnly.Rdata")


