##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)                 ## contains graph functions
library(caTools)
#library(foreach)
#library(doMC)
#library(MASS)

##------------------------------------------------------------------
## Register the clusters
##------------------------------------------------------------------
#registerDoMC(4)

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
## Loop over the egonets and compute the similarity matrices
##------------------------------------------------------------------

## placeholder for results
similarityMatrix.list   <- list()

## extract egonet data
ego.names   <- sort(names(egonets.list))
ego.num     <- length(ego.names)

## loop over each egonet and compute the similarity matrices
for (i in 1:1) {
    
    ## set-up
    tmp.id          <- ego.names[i]
    tmp.ego         <- egonets.list[[tmp.id]]
    tmp.filename    <- paste0(tmp.id,".SimilarityMatrix.Rdata")
    
    ## extract edges & compute the Jaccard matrix
    edges.raw   <- convEgonetListToIgraphObject(tmp.ego)
    edges.dedup <- dedupEdgeList(edges.raw)
    sim.mat     <- calcSimilarityMatrix(edges.dedup)
    
    ## write intermediate results to a file
    write.csv(sim.mat, file=tmp.filename)
    
    ## record final results
    similarityMatrix.list[[tmp.id]] <- list(edges.raw=edges.raw, edges.dedup=edges.dedup, sim.mat=sim.mat)
}


##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(similarityMatrix.list, "03_SocialCircle_SimilarityMatrices.Rdata")



