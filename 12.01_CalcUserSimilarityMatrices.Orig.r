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

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Define the parallel flag
##------------------------------------------------------------------
DOPARALLEL  <- TRUE

##------------------------------------------------------------------
## Register the clusters
##------------------------------------------------------------------
if (DOPARALLEL) {
    library(foreach)
    library(doMC)
    registerDoMC(4)
}

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
## set-up
##------------------------------------------------------------------

## extract egonet data
ego.names       <- names(egoedges.list)
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
egoedges.count  <- unlist(lapply(egoedges.list, ecount))
egoedges.order  <- order(egoedges.count)

##------------------------------------------------------------------
## loop over each egonet and compute the similarity matrices for
## both the full feature vector and the reduced size feature vector
##------------------------------------------------------------------
for (i in 110:1) {
    
    ##------------------------------------------------------------------
    ## set-up
    ##------------------------------------------------------------------
    tmp.id          <- ego.names[egoedges.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    
    ##------------------------------------------------------------------
    ## echo progress
    ##------------------------------------------------------------------
    cat("Iteration", i, "of", ego.num, " :: User Profile Similarity Matrix for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
    
    ##------------------------------------------------------------------
    ## load the profile matrix (called the leafMatrix
    ##------------------------------------------------------------------
    tmp.profFile    <- paste0(getwd(),"/profiles/",paste0(tmp.id,".ProfileMatrix.Rdata"))
    load(tmp.profFile)
    
    ##------------------------------------------------------------------
    ## output files
    ##------------------------------------------------------------------
    tmp.fullFeatureset      <- paste( paste(getwd(),"orig.sim.user.full",sep="/"), paste0(tmp.id, ".UserFullProfileSimilarityMatrix.Rdata"), sep="/")
    tmp.reducedFeatureset   <- paste( paste(getwd(),"orig.sim.user.reduced",sep="/"), paste0(tmp.id, ".UserReducedProfileSimilarityMatrix.Rdata"), sep="/")
    
    ##------------------------------------------------------------------
    ## compute the matrices & immediately write to file
    ##------------------------------------------------------------------
    
    ## full profile
    tmp.fullUserProfileSim      <- calcEgoSimilarityMatrix(tmp.id, tmp.edges, tmp.leafMatrix)
    save(tmp.fullUserProfileSim, file=tmp.fullFeatureset)

    ## reduced profile
    tmp.reducedUserProfileSim   <- calcEgoSimilarityMatrix(tmp.id, tmp.edges, tmp.leafMatrixExDropsRows)
    save(tmp.reducedUserProfileSim, file=tmp.reducedFeatureset)

}







