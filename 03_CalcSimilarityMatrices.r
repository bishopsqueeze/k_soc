##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)                 ## contains graph functions
library(caTools)
library(linkcomm)


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
load("02_SocialCircle_Edges.Rdata")     ## edge lists


##------------------------------------------------------------------
## Loop over the egonets, define graph objects, save to file
##------------------------------------------------------------------

## extract egonet data
ego.names       <- sort(names(egoedges.list))
ego.num         <- length(ego.names)

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"sim",sep="/")

## loop over each egonet and compute the similarity matrices
for (i in 1:ego.num) {
    
    ## set-up
    tmp.id          <- ego.names[i]
    tmp.ego         <- egonets.list[[tmp.id]]
    tmp.filename    <- paste(output.dir, paste0(tmp.id,".SimilarityMatrix.Rdata"), sep="/")
    
    ## de-duplicate edges
    edges.dedup     <- egoedges.list[[tmp.id]]
    
 }


## placeholder for results
#similarityMatrix.list   <- list()

#edges.dedup <- dedupEdgeList(edges.raw)
#sim.mat     <- calcSimilarityMatrix(edges.dedup)

## write intermediate results to a file
#write.csv(sim.mat, file=tmp.filename)

## record final results
#similarityMatrix.list[[tmp.id]] <- list(edges.raw=edges.raw, edges.dedup=edges.dedup, sim.mat=sim.mat)


##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(egoedges.list, "02_SocialCircle_Edges.Rdata")


