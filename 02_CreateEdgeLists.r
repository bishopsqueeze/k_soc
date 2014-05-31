##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)                 ## contains graph functions
library(caTools)

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
## Loop over the egonets, define graph objects, save to file
##------------------------------------------------------------------

## create a placeholder for the de-duplicated edges
egoedges.list   <- list()

## extract egonet data
ego.names       <- sort(names(egonets.list))
ego.num         <- length(ego.names)

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"edges",sep="/")

## loop over each egonet and compute the similarity matrices
for (i in 1:ego.num) {
    
    ## set-up
    tmp.id          <- ego.names[i]
    tmp.ego         <- egonets.list[[tmp.id]]
    tmp.filename    <- paste(output.dir, paste0(tmp.id,".iGraphObject.Rdata"), sep="/")
    
    ## extract edges
    edges.raw       <- convEgonetListToIgraphObject(tmp.ego)
    
    ## de-duplicate edges
    edges.dedup     <- dedupEdgeList(edges.raw)
    
    ## load the
    egoedges.list[[tmp.id]] <- edges.dedup
    
    ## save results to a file
    save(edges.raw, edges.dedup, file=tmp.filename)
}

##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(egoedges.list, file="02_SocialCircle_Edges.Rdata")


